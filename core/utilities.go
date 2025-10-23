package core

import (
	"fmt"
	"strings"
)

// EqualValues compares two values for equality
func EqualValues(a, b Value) bool {
	// Check nil values first
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}

	// Check by type
	switch aVal := a.(type) {
	case NumberValue:
		if bVal, ok := b.(NumberValue); ok {
			return float64(aVal) == float64(bVal)
		}
	case StringValue:
		if bVal, ok := b.(StringValue); ok {
			return string(aVal) == string(bVal)
		}
		// Also compare string to Class name for backwards compatibility
		if bVal, ok := b.(*Class); ok {
			return string(aVal) == bVal.Name
		}
	case *Class:
		// Compare Class to StringValue or another Class
		if bVal, ok := b.(StringValue); ok {
			return aVal.Name == string(bVal)
		}
		if bVal, ok := b.(*Class); ok {
			return aVal == bVal // Same class instance
		}
	case BoolValue:
		if bVal, ok := b.(BoolValue); ok {
			return bool(aVal) == bool(bVal)
		}
	case NilValue:
		_, ok := b.(NilValue)
		return ok
	case *ListValue:
		if bVal, ok := b.(*ListValue); ok {
			if aVal.Len() != bVal.Len() {
				return false
			}
			aItems := aVal.Items()
			bItems := bVal.Items()
			for i := range aItems {
				if !EqualValues(aItems[i], bItems[i]) {
					return false
				}
			}
			return true
		}
	case TupleValue:
		if bVal, ok := b.(TupleValue); ok {
			if len(aVal) != len(bVal) {
				return false
			}
			for i := range aVal {
				if !EqualValues(aVal[i], bVal[i]) {
					return false
				}
			}
			return true
		}
	case *SetValue:
		if bVal, ok := b.(*SetValue); ok {
			if aVal.Size() != bVal.Size() {
				return false
			}
			// Check if all items in a are in b
			for _, v := range aVal.items {
				if !bVal.Contains(v) {
					return false
				}
			}
			return true
		}
	case *DictValue:
		if bVal, ok := b.(*DictValue); ok {
			if aVal.Size() != bVal.Size() {
				return false
			}
			// Check if all key-value pairs in a are in b
			for k, v1 := range aVal.entries {
				v2, exists := bVal.entries[k]
				if !exists || !EqualValues(v1, v2) {
					return false
				}
			}
			return true
		}
	}

	return false
}

// IsHashable determines if a value can be used as a dictionary key
func IsHashable(v Value) bool {
	switch val := v.(type) {
	case NumberValue, StringValue, BoolValue, NilValue, TupleValue, *FrozenSetValue:
		return true
	case *Instance:
		// Instances of int, str, bool subclasses are hashable
		// Check if the instance's class inherits from a hashable type
		if val.Class != nil {
			// Check if it's a subclass of int, str, or bool
			// For now, we'll check the class name or inheritance
			// TODO: Implement proper inheritance checking
			className := val.Class.Name
			if className == "int" || className == "str" || className == "bool" {
				return true
			}
			// Check if class has int, str, or bool as a parent
			for _, parent := range val.Class.Parents {
				if parent.Name == "int" || parent.Name == "str" || parent.Name == "bool" {
					return true
				}
			}
		}
		return false
	default:
		return false
	}
}

// ValueToKey converts a value to a string key for use in sets and dicts
func ValueToKey(v Value) string {
	switch val := v.(type) {
	case NumberValue:
		return fmt.Sprintf("n:%g", float64(val))
	case StringValue:
		return fmt.Sprintf("s:%s", string(val))
	case BoolValue:
		return fmt.Sprintf("b:%t", bool(val))
	case NilValue:
		return "nil"
	case TupleValue:
		// For tuples, create a key based on their content
		elements := make([]string, len(val))
		for i, elem := range val {
			elements[i] = ValueToKey(elem)
		}
		return fmt.Sprintf("t:(%s)", strings.Join(elements, ","))
	case *FrozenSetValue:
		// For frozensets, use the hash value
		return fmt.Sprintf("fs:%d", val.Hash())
	case *Instance:
		// For instances of hashable types (like int subclasses),
		// use the underlying value if available
		if val.Class != nil {
			// Check if it's an int subclass
			for _, parent := range val.Class.Parents {
				if parent.Name == "int" {
					// Get the __value__ attribute which stores the actual int value
					if numVal, ok := val.Attributes["__value__"].(NumberValue); ok {
						return fmt.Sprintf("n:%g", float64(numVal))
					}
				}
			}
		}
		// For other instances, use pointer address
		return fmt.Sprintf("p:%p", v)
	default:
		// For non-hashable types, use pointer address
		return fmt.Sprintf("p:%p", v)
	}
}

// Compare compares two values and returns:
// -1 if a < b
// 0 if a == b
// 1 if a > b
func Compare(a, b Value) int {
	// Handle same types
	switch aVal := a.(type) {
	case NumberValue:
		if bVal, ok := b.(NumberValue); ok {
			av, bv := float64(aVal), float64(bVal)
			if av < bv {
				return -1
			} else if av > bv {
				return 1
			}
			return 0
		}
	case StringValue:
		if bVal, ok := b.(StringValue); ok {
			as, bs := string(aVal), string(bVal)
			if as < bs {
				return -1
			} else if as > bs {
				return 1
			}
			return 0
		}
	}

	// Fall back to string comparison for different types
	as, bs := a.String(), b.String()
	if as < bs {
		return -1
	} else if as > bs {
		return 1
	}
	return 0
}

// IsTruthy determines if a value is truthy
func IsTruthy(v Value) bool {
	// Nil checks
	if v == nil {
		return false
	}

	// First, check if the value has a __bool__ method
	if obj, ok := v.(Object); ok {
		if boolMethod, hasBool := obj.GetAttr("__bool__"); hasBool {
			if callable, isCallable := boolMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); isCallable {
				// Call __bool__ with no arguments
				result, err := callable.Call([]Value{}, nil)
				if err == nil {
					// __bool__ must return a boolean
					if boolVal, isBool := result.(BoolValue); isBool {
						return bool(boolVal)
					}
					// If __bool__ doesn't return a boolean, that's an error
					// For now, we'll treat it as truthy to avoid breaking existing code
					// In a strict implementation, this should raise an error
					return true
				}
			}
		}

		// If no __bool__, check for __len__ as fallback
		if lenMethod, hasLen := obj.GetAttr("__len__"); hasLen {
			if callable, isCallable := lenMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); isCallable {
				// Call __len__ with no arguments
				result, err := callable.Call([]Value{}, nil)
				if err == nil {
					// __len__ should return a number
					if numVal, isNum := result.(NumberValue); isNum {
						return float64(numVal) != 0
					}
				}
			}
		}
	}

	// Fall back to type-specific behavior
	switch val := v.(type) {
	case BoolValue:
		return bool(val)
	case NumberValue:
		return float64(val) != 0
	case StringValue:
		return string(val) != ""
	case *ListValue:
		return val.Len() > 0
	case TupleValue:
		return len(val) > 0
	case NilValue:
		return false
	case *DictValue:
		// Dictionary is truthy if it has at least one entry
		if val == nil {
			return false
		}
		return val.Size() > 0
	case *SetValue:
		// Set is truthy if it has at least one element
		if val == nil {
			return false
		}
		return val.Size() > 0
	default:
		// For other object types, we consider them truthy if they exist
		return true
	}
}

// PrintValue returns a string representation of a value
func PrintValue(val Value) string {
	if val == nil {
		return "nil"
	}
	return val.String()
}

// PrintValueWithoutQuotes returns a string representation without quotes for strings
func PrintValueWithoutQuotes(val Value) string {
	if str, ok := val.(StringValue); ok {
		return string(str)
	}
	return PrintValue(val)
}

// ProcessEscapeSequences processes escape sequences in a string
func ProcessEscapeSequences(s string) string {
	// For now, just return the string as is
	return s
}

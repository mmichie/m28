package core

import "fmt"

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
	case BoolValue:
		if bVal, ok := b.(BoolValue); ok {
			return bool(aVal) == bool(bVal)
		}
	case NilValue:
		_, ok := b.(NilValue)
		return ok
	case ListValue:
		if bVal, ok := b.(ListValue); ok {
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

	switch val := v.(type) {
	case BoolValue:
		return bool(val)
	case NumberValue:
		return float64(val) != 0
	case StringValue:
		return string(val) != ""
	case ListValue:
		return len(val) > 0
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

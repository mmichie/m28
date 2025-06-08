package core

import "fmt"

// Repr returns the developer-friendly representation of a value
// This checks for __repr__ methods on objects and falls back to default representations
func Repr(val Value) string {
	if val == nil {
		return "nil"
	}

	// Check if value has __repr__ method
	if obj, ok := val.(Object); ok {
		if reprMethod, hasRepr := obj.GetAttr("__repr__"); hasRepr {
			if callable, isCallable := reprMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); isCallable {
				// Call __repr__ with no arguments
				result, err := callable.Call([]Value{}, nil)
				if err == nil {
					if str, ok := result.(StringValue); ok {
						return string(str)
					}
				}
			}
		}
	}

	// Default representations for built-in types
	switch v := val.(type) {
	case StringValue:
		// For strings, return quoted representation
		return fmt.Sprintf("%q", string(v))
	case NumberValue:
		return fmt.Sprintf("%g", float64(v))
	case BoolValue:
		if v {
			return "True"
		}
		return "False"
	case NilValue:
		return "None"
	case ListValue:
		return formatListRepr(v)
	case TupleValue:
		return formatTupleRepr(v)
	case *DictValue:
		return formatDictRepr(v)
	case *SetValue:
		return formatSetRepr(v)
	case *Class:
		return fmt.Sprintf("<class '%s'>", v.Name)
	case *Instance:
		// For instances without __repr__, use default format
		return fmt.Sprintf("<%s object at %p>", v.Class.Name, v)
	case *BuiltinFunction:
		if v.name != "" {
			return fmt.Sprintf("<built-in function %s>", v.name)
		}
		return fmt.Sprintf("<built-in function>")
	case *Module:
		return fmt.Sprintf("<module '%s'>", v.Name)
	default:
		// Check if it's a callable with a name (like UserFunction from eval package)
		if _, ok := val.(Callable); ok {
			// Try to get name through reflection or interface
			if named, ok := val.(interface{ GetName() string }); ok {
				name := named.GetName()
				if name != "" {
					return fmt.Sprintf("<function %s at %p>", name, val)
				}
			}
			// Check if it's a function type
			if val.Type() == FunctionType {
				return fmt.Sprintf("<function at %p>", val)
			}
		}
		// Fall back to String() method
		return val.String()
	}
}

func formatListRepr(list ListValue) string {
	if len(list) == 0 {
		return "[]"
	}
	
	result := "["
	for i, item := range list {
		if i > 0 {
			result += ", "
		}
		result += Repr(item)
	}
	result += "]"
	return result
}

func formatTupleRepr(tuple TupleValue) string {
	if len(tuple) == 0 {
		return "()"
	}
	if len(tuple) == 1 {
		return "(" + Repr(tuple[0]) + ",)"
	}
	
	result := "("
	for i, item := range tuple {
		if i > 0 {
			result += ", "
		}
		result += Repr(item)
	}
	result += ")"
	return result
}

func formatDictRepr(dict *DictValue) string {
	if dict.Size() == 0 {
		return "{}"
	}
	
	result := "{"
	first := true
	for k, v := range dict.entries {
		if !first {
			result += ", "
		}
		first = false
		// Extract the actual key value from the internal key format
		key := dict.keyFromString(k)
		result += Repr(key) + ": " + Repr(v)
	}
	result += "}"
	return result
}

func formatSetRepr(set *SetValue) string {
	if set.Size() == 0 {
		return "set()"
	}
	
	result := "{"
	first := true
	for _, item := range set.items {
		if !first {
			result += ", "
		}
		first = false
		result += Repr(item)
	}
	result += "}"
	return result
}

// Helper method to extract key from internal representation
func (d *DictValue) keyFromString(keyStr string) Value {
	// This is a simple implementation - in practice we might want to store
	// the original key alongside the string representation
	if len(keyStr) > 2 {
		prefix := keyStr[:2]
		value := keyStr[2:]
		switch prefix {
		case "s:":
			return StringValue(value)
		case "n:":
			var num float64
			fmt.Sscanf(value, "%g", &num)
			return NumberValue(num)
		case "b:":
			if value == "true" {
				return True
			}
			return False
		}
	}
	if keyStr == "nil" {
		return Nil
	}
	// Fallback
	return StringValue(keyStr)
}
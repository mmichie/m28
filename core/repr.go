package core

import "fmt"

// Repr returns the developer-friendly representation of a value
// This checks for __repr__ methods on objects and falls back to default representations
func Repr(val Value) string {
	if val == nil {
		return "nil"
	}

	// Try __repr__ method first
	if repr, ok := tryReprMethod(val); ok {
		return repr
	}

	// Fall back to built-in type representations
	return reprBuiltinType(val)
}

// tryReprMethod attempts to call __repr__ method on objects
func tryReprMethod(val Value) (string, bool) {
	obj, ok := val.(Object)
	if !ok {
		return "", false
	}

	reprMethod, hasRepr := obj.GetAttr("__repr__")
	if !hasRepr {
		return "", false
	}

	callable, isCallable := reprMethod.(interface {
		Call([]Value, *Context) (Value, error)
	})
	if !isCallable {
		return "", false
	}

	result, err := callable.Call([]Value{}, nil)
	if err != nil {
		return "", false
	}

	str, ok := result.(StringValue)
	if !ok {
		return "", false
	}

	return string(str), true
}

// reprBuiltinType returns the representation for built-in types
func reprBuiltinType(val Value) string {
	switch v := val.(type) {
	case StringValue:
		return reprString(v)
	case NumberValue:
		return reprNumber(v)
	case BoolValue:
		return reprBool(v)
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
		return reprClass(v)
	case *Instance:
		return reprInstance(v)
	case *BuiltinFunction:
		return reprBuiltinFunction(v)
	case *Module:
		return reprModule(v)
	default:
		return reprDefault(val)
	}
}

// reprString returns quoted string representation
func reprString(s StringValue) string {
	return fmt.Sprintf("%q", string(s))
}

// reprNumber returns numeric representation
func reprNumber(n NumberValue) string {
	return fmt.Sprintf("%g", float64(n))
}

// reprBool returns boolean representation (True/False)
func reprBool(b BoolValue) string {
	if b {
		return "True"
	}
	return "False"
}

// reprClass returns class representation
func reprClass(c *Class) string {
	return fmt.Sprintf("<class '%s'>", c.Name)
}

// reprInstance returns instance representation
func reprInstance(i *Instance) string {
	return fmt.Sprintf("<%s object at %p>", i.Class.Name, i)
}

// reprBuiltinFunction returns builtin function representation
func reprBuiltinFunction(f *BuiltinFunction) string {
	if f.name != "" {
		return fmt.Sprintf("<built-in function %s>", f.name)
	}
	return "<built-in function>"
}

// reprModule returns module representation
func reprModule(m *Module) string {
	return fmt.Sprintf("<module '%s'>", m.Name)
}

// reprDefault handles default representation for unknown types
func reprDefault(val Value) string {
	// Check if it's a callable with a name (like UserFunction from eval package)
	if _, ok := val.(Callable); ok {
		if repr := reprNamedCallable(val); repr != "" {
			return repr
		}

		// Check if it's a function type
		if val.Type() == FunctionType {
			return fmt.Sprintf("<function at %p>", val)
		}
	}

	// Fall back to String() method
	return val.String()
}

// reprNamedCallable attempts to get name from callable
func reprNamedCallable(val Value) string {
	named, ok := val.(interface{ GetName() string })
	if !ok {
		return ""
	}

	name := named.GetName()
	if name == "" {
		return ""
	}

	return fmt.Sprintf("<function %s at %p>", name, val)
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

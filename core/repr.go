package core

import (
	"fmt"
	"sort"
	"strings"
	"unsafe"
)

// Repr returns the developer-friendly representation of a value
// This checks for __repr__ methods on objects and falls back to default representations
func Repr(val Value) string {
	if val == nil {
		return "nil"
	}

	// Handle Class objects specially - they have their own repr format
	// and shouldn't use tryReprMethod which would call an unbound __repr__
	if _, ok := val.(*Class); ok {
		return reprBuiltinType(val)
	}

	// Try __repr__ method first (for instances)
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

	// Call __repr__ method
	// BoundInstanceMethod automatically prepends self, so call with no args
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
	case *ListValue:
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
// Python uses single quotes by default, double quotes if string contains single quotes
func reprString(s StringValue) string {
	str := string(s)

	// Count single and double quotes
	hasSingle := false
	hasDouble := false
	for _, ch := range str {
		if ch == '\'' {
			hasSingle = true
		} else if ch == '"' {
			hasDouble = true
		}
	}

	// Choose quote style: prefer single quotes (Python default)
	if hasSingle && !hasDouble {
		// Use double quotes if string has single quotes but no double quotes
		return fmt.Sprintf("%q", str)
	}

	// Use single quotes (default), escaping them if necessary
	result := "'"
	for _, ch := range str {
		switch ch {
		case '\'':
			result += "\\'"
		case '\\':
			result += "\\\\"
		case '\n':
			result += "\\n"
		case '\r':
			result += "\\r"
		case '\t':
			result += "\\t"
		default:
			if ch < ASCIIPrintableMin || ch == ASCIIDel {
				result += fmt.Sprintf("\\x%02x", ch)
			} else {
				result += string(ch)
			}
		}
	}
	result += "'"
	return result
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

func formatListRepr(list *ListValue) string {
	return listReprWithDepth(list)
}

// listReprWithDepth formats a list repr with depth tracking.
// Returns the repr string, or panics with *RecursionError if depth > maxListReprDepth.
// The panic is caught by the repr() builtin to raise RecursionError to Python.
func listReprWithDepth(list *ListValue) string {
	n := list.Len()
	if n == 0 {
		return "[]"
	}

	depth := pushReprDepth()
	defer popReprDepth()

	if depth > maxListReprDepth {
		panic(&RecursionError{Message: "maximum recursion depth exceeded while getting the repr of an object"})
	}

	return withCycleDetectionPlaceholder(uintptr(unsafe.Pointer(list)), "[...]", func() string {
		// Iterate live against the list's current length (like CPython).
		// This correctly handles mutation during iteration (e.g., test_repr_mutate).
		var elements []string
		for i := 0; i < list.Len(); i++ {
			if i >= len(list.items) {
				break
			}
			elements = append(elements, Repr(list.items[i]))
		}
		return "[" + strings.Join(elements, ", ") + "]"
	})
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
	return withCycleDetection(uintptr(unsafe.Pointer(dict)), func() string {
		result := "{"
		first := true
		for _, k := range dict.orderedKeys {
			v, ok := dict.entries[k]
			if !ok {
				continue
			}
			if !first {
				result += ", "
			}
			first = false
			var key Value
			if orig, has := dict.keys[k]; has {
				key = orig
			} else {
				key = dict.keyFromString(k)
			}
			result += Repr(key) + ": " + Repr(v)
		}
		result += "}"
		return result
	})
}

func formatSetRepr(set *SetValue) string {
	if set.Size() == 0 {
		return "set()"
	}

	// Sort by internal key for deterministic output. Python sets are unordered,
	// but M28 sorts so str()/repr() are reproducible and identical to each other.
	keys := make([]string, 0, len(set.items))
	for k := range set.items {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	result := "{"
	for i, k := range keys {
		if i > 0 {
			result += ", "
		}
		result += Repr(set.items[k])
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

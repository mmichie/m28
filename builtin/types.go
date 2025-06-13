package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterTypes registers type-related functions
func RegisterTypes(ctx *core.Context) {
	// type - returns type of value
	ctx.Define("type", core.NewNamedBuiltinFunction("type", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("type requires 1 argument")
		}

		// Get type descriptor
		desc := core.GetTypeDescriptorForValue(args[0])
		if desc == nil {
			// Fallback for types without descriptors
			typeName := args[0].Type()
			return core.StringValue(fmt.Sprintf("<type '%s'>", string(typeName))), nil
		}

		// Return a string representation of the type
		return core.StringValue(fmt.Sprintf("<type '%s'>", desc.PythonName)), nil
	}))

	// isinstance - check if object is instance of type
	ctx.Define("isinstance", core.NewNamedBuiltinFunction("isinstance", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("isinstance() takes exactly 2 arguments (%d given)", len(args))
		}

		obj := args[0]
		typeArg := args[1]

		// Handle tuple of types
		if tuple, ok := typeArg.(core.TupleValue); ok {
			for _, t := range tuple {
				if isInstance(obj, t) {
					return core.True, nil
				}
			}
			return core.False, nil
		}

		// Single type check
		return core.BoolValue(isInstance(obj, typeArg)), nil
	}))

	// issubclass - check if class is subclass of another
	ctx.Define("issubclass", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("issubclass() takes exactly 2 arguments (%d given)", len(args))
		}

		// For now, just check type equality since we don't have proper classes yet
		return core.BoolValue(args[0].Type() == args[1].Type()), nil
	}))

	// Type conversion functions
	// int - convert to integer
	ctx.Define("int", core.NewNamedBuiltinFunction("int", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NumberValue(0), nil
		}
		if len(args) > 2 {
			return nil, fmt.Errorf("int() takes at most 2 arguments (%d given)", len(args))
		}

		// Handle base parameter for string conversion
		base := 10
		if len(args) == 2 {
			if b, ok := args[1].(core.NumberValue); ok {
				base = int(b)
				if base != 0 && (base < 2 || base > 36) {
					return nil, fmt.Errorf("int() base must be >= 2 and <= 36, or 0")
				}
			} else {
				return nil, fmt.Errorf("int() base must be an integer")
			}
		}

		switch v := args[0].(type) {
		case core.NumberValue:
			return core.NumberValue(int(v)), nil
		case core.StringValue:
			// String to int conversion would go here
			// For now, just return an error
			return nil, fmt.Errorf("int() string conversion not yet implemented")
		case core.BoolValue:
			if bool(v) {
				return core.NumberValue(1), nil
			}
			return core.NumberValue(0), nil
		default:
			return nil, fmt.Errorf("int() argument must be a string or a number, not '%s'", v.Type())
		}
	}))

	// float - convert to float
	ctx.Define("float", core.NewNamedBuiltinFunction("float", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NumberValue(0.0), nil
		}
		if len(args) != 1 {
			return nil, fmt.Errorf("float() takes at most 1 argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.NumberValue:
			return v, nil // Already a number
		case core.StringValue:
			// String to float conversion would go here
			// For now, just return an error
			return nil, fmt.Errorf("float() string conversion not yet implemented")
		case core.BoolValue:
			if bool(v) {
				return core.NumberValue(1.0), nil
			}
			return core.NumberValue(0.0), nil
		default:
			return nil, fmt.Errorf("float() argument must be a string or a number, not '%s'", v.Type())
		}
	}))

	// str - convert to string
	ctx.Define("str", core.NewNamedBuiltinFunction("str", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.StringValue(""), nil
		}
		if len(args) != 1 {
			return nil, fmt.Errorf("str() takes at most 1 argument (%d given)", len(args))
		}

		return core.StringValue(args[0].String()), nil
	}))

	// bool - convert to boolean
	ctx.Define("bool", core.NewNamedBuiltinFunction("bool", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.False, nil
		}
		if len(args) != 1 {
			return nil, fmt.Errorf("bool() takes at most 1 argument (%d given)", len(args))
		}

		return core.BoolValue(core.IsTruthy(args[0])), nil
	}))

	// is_none - check if value is None
	ctx.Define("is_none", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("is_none expects 1 argument, got %d", len(args))
		}
		_, isNil := args[0].(core.NilValue)
		return core.BoolValue(isNil), nil
	}))
}

// isInstance checks if obj is an instance of the given type
func isInstance(obj, typeObj core.Value) bool {
	// Get type name from typeObj
	var typeName string
	switch t := typeObj.(type) {
	case core.StringValue:
		// Handle string type names like "str", "int", etc.
		typeName = string(t)
	default:
		// Handle type objects
		if str, ok := typeObj.(interface{ String() string }); ok {
			s := str.String()
			// Extract type name from "<type 'name'>" format
			if len(s) > 8 && s[:6] == "<type " && s[len(s)-2:] == "'>" {
				typeName = s[7 : len(s)-2]
			}
		}
	}

	// Get the actual type of obj
	desc := core.GetTypeDescriptorForValue(obj)
	if desc != nil {
		return desc.PythonName == typeName
	}

	// Fallback to basic type checking
	switch typeName {
	case "int", "float", "number":
		_, ok := obj.(core.NumberValue)
		return ok
	case "str", "string":
		_, ok := obj.(core.StringValue)
		return ok
	case "list":
		_, ok := obj.(core.ListValue)
		return ok
	case "dict":
		_, ok := obj.(*core.DictValue)
		return ok
	case "set":
		_, ok := obj.(*core.SetValue)
		return ok
	case "tuple":
		_, ok := obj.(core.TupleValue)
		return ok
	case "bool":
		_, ok := obj.(core.BoolValue)
		return ok
	case "NoneType":
		_, ok := obj.(core.NilValue)
		return ok
	default:
		return false
	}
}

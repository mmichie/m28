package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// This function is no longer used - type checking functions have been moved to types.go
// Keeping RegisterTypeCheckingFunctions for backwards compatibility if needed
func RegisterTypeCheckingFunctions(ctx *core.Context) {
	// NOTE: isinstance and issubclass are implemented as special forms in eval/class_forms.go

	// Type conversion functions

	// int() - convert to integer
	ctx.Define("int", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("int", args)
		if err := v.Max(2); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.NumberValue(0), nil
		}

		val := args[0]
		// TODO: Handle optional base parameter for string conversion

		if num, ok := types.AsNumber(val); ok {
			// Truncate to integer
			return core.NumberValue(float64(int64(num))), nil
		}

		if str, ok := types.AsString(val); ok {
			// Try to parse as integer
			var num float64
			if _, err := fmt.Sscanf(str, "%f", &num); err != nil {
				return nil, fmt.Errorf("invalid literal for int() with base 10: '%s'", str)
			}
			return core.NumberValue(float64(int64(num))), nil
		}

		if b, ok := types.AsBool(val); ok {
			if b {
				return core.NumberValue(1), nil
			}
			return core.NumberValue(0), nil
		}

		return nil, fmt.Errorf("int() argument must be a string or a number, not '%s'", val.Type())
	}))

	// float() - convert to float
	ctx.Define("float", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("float", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.NumberValue(0.0), nil
		}

		val := args[0]

		if num, ok := types.AsNumber(val); ok {
			return core.NumberValue(num), nil
		}

		if str, ok := types.AsString(val); ok {
			var num float64
			if _, err := fmt.Sscanf(str, "%f", &num); err != nil {
				return nil, fmt.Errorf("could not convert string to float: '%s'", str)
			}
			return core.NumberValue(num), nil
		}

		if b, ok := types.AsBool(val); ok {
			if b {
				return core.NumberValue(1.0), nil
			}
			return core.NumberValue(0.0), nil
		}

		return nil, fmt.Errorf("float() argument must be a string or a number, not '%s'", val.Type())
	}))

	// str() - convert to string
	strFunc := core.NewNamedBuiltinFunction("str", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("str", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.StringValue(""), nil
		}

		// Use the string representation
		return core.StringValue(core.PrintValueWithoutQuotes(args[0])), nil
	})

	// Add maketrans as a class method
	// maketrans(x[, y[, z]]) creates a translation table
	strFunc.SetAttr("maketrans", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("maketrans", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		// Simple implementation: create a dict mapping
		table := core.NewDict()

		if v.Count() == 1 {
			// Single argument: must be a dict mapping chars to replacement strings or None
			if dict, ok := args[0].(*core.DictValue); ok {
				return dict, nil
			}
			return nil, core.NewTypeError("dict", args[0], "maketrans() argument")
		}

		// Two or three arguments
		x, okx := args[0].(core.StringValue)
		y, oky := args[1].(core.StringValue)
		if !okx || !oky {
			return nil, core.NewTypeError("string", args[0], "maketrans() arguments")
		}

		if len(x) != len(y) {
			return nil, fmt.Errorf("maketrans: the first two maketrans arguments must have equal length")
		}

		// Build the translation table
		for i, char := range string(x) {
			table.Set(string(char), core.StringValue(string([]rune(string(y))[i])))
		}

		// If there's a third argument, it's characters to delete (map to None)
		if v.Count() == 3 {
			if z, ok := args[2].(core.StringValue); ok {
				for _, char := range string(z) {
					table.Set(string(char), core.None)
				}
			}
		}

		return table, nil
	}))

	ctx.Define("str", strFunc)

	// bool() - convert to boolean
	ctx.Define("bool", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("bool", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.False, nil
		}

		return core.BoolValue(types.IsTruthy(args[0])), nil
	}))

	// repr() is now defined in misc.go to avoid duplication

	// bytes() is now a proper type with constructor registered in type_registry_primitives.go
}

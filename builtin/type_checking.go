package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

func registerTypeCheckingBuiltins(ctx *core.Context) {
	// NOTE: isinstance and issubclass are implemented as special forms in eval/class_forms.go

	// Type conversion functions
	
	// int() - convert to integer
	ctx.Define("int", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NumberValue(0), nil
		}
		if len(args) > 2 {
			return nil, fmt.Errorf("int() takes at most 2 arguments (%d given)", len(args))
		}

		val := args[0]
		// TODO: Handle optional base parameter for string conversion

		switch v := val.(type) {
		case core.NumberValue:
			// Truncate to integer
			return core.NumberValue(float64(int64(v))), nil
		case core.StringValue:
			// Try to parse as integer
			var num float64
			if _, err := fmt.Sscanf(string(v), "%f", &num); err != nil {
				return nil, fmt.Errorf("invalid literal for int() with base 10: '%s'", string(v))
			}
			return core.NumberValue(float64(int64(num))), nil
		case core.BoolValue:
			if v {
				return core.NumberValue(1), nil
			}
			return core.NumberValue(0), nil
		default:
			return nil, fmt.Errorf("int() argument must be a string or a number, not '%s'", val.Type())
		}
	}))

	// float() - convert to float
	ctx.Define("float", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NumberValue(0.0), nil
		}
		if len(args) != 1 {
			return nil, fmt.Errorf("float() takes exactly one argument (%d given)", len(args))
		}

		val := args[0]
		switch v := val.(type) {
		case core.NumberValue:
			return v, nil
		case core.StringValue:
			var num float64
			if _, err := fmt.Sscanf(string(v), "%f", &num); err != nil {
				return nil, fmt.Errorf("could not convert string to float: '%s'", string(v))
			}
			return core.NumberValue(num), nil
		case core.BoolValue:
			if v {
				return core.NumberValue(1.0), nil
			}
			return core.NumberValue(0.0), nil
		default:
			return nil, fmt.Errorf("float() argument must be a string or a number, not '%s'", val.Type())
		}
	}))

	// str() - convert to string
	ctx.Define("str", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.StringValue(""), nil
		}
		if len(args) != 1 {
			return nil, fmt.Errorf("str() takes exactly one argument (%d given)", len(args))
		}

		// Use the string representation
		return core.StringValue(core.PrintValueWithoutQuotes(args[0])), nil
	}))

	// bool() - convert to boolean
	ctx.Define("bool", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.False, nil
		}
		if len(args) != 1 {
			return nil, fmt.Errorf("bool() takes exactly one argument (%d given)", len(args))
		}

		return core.BoolValue(core.IsTruthy(args[0])), nil
	}))
}


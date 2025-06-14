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
	ctx.Define("str", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("str", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.StringValue(""), nil
		}

		// Use the string representation
		return core.StringValue(core.PrintValueWithoutQuotes(args[0])), nil
	}))

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

	// bytes() - convert string to bytes representation
	ctx.Define("bytes", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("bytes", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		// For now, we'll represent bytes as a list of numbers
		// In a full implementation, we'd have a proper bytes type

		// Get the string to convert
		str, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// Optional encoding parameter (default to UTF-8)
		encoding, _ := v.GetStringOrDefault(1, "utf-8")

		// For now, only support UTF-8
		if encoding != "utf-8" && encoding != "utf8" {
			return nil, fmt.Errorf("unsupported encoding: %s", encoding)
		}

		// Convert string to bytes
		bytes := []byte(str)
		result := make(core.ListValue, len(bytes))
		for i, b := range bytes {
			result[i] = core.NumberValue(float64(b))
		}

		return result, nil
	}))
}

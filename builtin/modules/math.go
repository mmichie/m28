package modules

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/core"
)

// RegisterMathModule creates and registers the math module
func RegisterMathModule(ctx *core.Context) {
	mathModule := core.NewDict()

	// sqrt - square root
	mathModule.Set("sqrt", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("sqrt() takes exactly one argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.NumberValue:
			return core.NumberValue(math.Sqrt(float64(v))), nil
		default:
			return nil, fmt.Errorf("sqrt() argument must be a number, not '%s'", v.Type())
		}
	}))

	// pow - power function
	mathModule.Set("pow", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("pow() takes exactly two arguments (%d given)", len(args))
		}

		x, ok1 := args[0].(core.NumberValue)
		y, ok2 := args[1].(core.NumberValue)
		if !ok1 || !ok2 {
			return nil, fmt.Errorf("pow() arguments must be numbers")
		}

		return core.NumberValue(math.Pow(float64(x), float64(y))), nil
	}))

	// sin, cos, tan
	mathModule.Set("sin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("sin() takes exactly one argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.NumberValue:
			return core.NumberValue(math.Sin(float64(v))), nil
		default:
			return nil, fmt.Errorf("sin() argument must be a number, not '%s'", v.Type())
		}
	}))

	mathModule.Set("cos", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("cos() takes exactly one argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.NumberValue:
			return core.NumberValue(math.Cos(float64(v))), nil
		default:
			return nil, fmt.Errorf("cos() argument must be a number, not '%s'", v.Type())
		}
	}))

	mathModule.Set("tan", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("tan() takes exactly one argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.NumberValue:
			return core.NumberValue(math.Tan(float64(v))), nil
		default:
			return nil, fmt.Errorf("tan() argument must be a number, not '%s'", v.Type())
		}
	}))

	// Constants
	mathModule.Set("pi", core.NumberValue(math.Pi))
	mathModule.Set("e", core.NumberValue(math.E))

	// Register in the global module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("math", mathModule, "<builtin>", []string{})

	// Also register core math functions in global context
	registerCoreMathFunctions(ctx)
}

// registerCoreMathFunctions registers core math functions in the global context
func registerCoreMathFunctions(ctx *core.Context) {
	// abs, sum, pow are now registered in numeric.go to avoid duplication
	// Note: min and max are registered in numeric.go with full kwargs support

	// Math-specific functions that aren't duplicated elsewhere:

	// sqrt - square root
	ctx.Define("sqrt", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("sqrt() takes exactly one argument (%d given)", len(args))
		}

		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("sqrt() argument must be a number, not '%s'", args[0].Type())
		}

		if float64(num) < 0 {
			return nil, fmt.Errorf("math domain error")
		}

		return core.NumberValue(math.Sqrt(float64(num))), nil
	}))

	// floor - round down
	ctx.Define("floor", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("floor() takes exactly one argument (%d given)", len(args))
		}

		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("floor() argument must be a number, not '%s'", args[0].Type())
		}

		return core.NumberValue(math.Floor(float64(num))), nil
	}))

	// ceil - round up
	ctx.Define("ceil", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("ceil() takes exactly one argument (%d given)", len(args))
		}

		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("ceil() argument must be a number, not '%s'", args[0].Type())
		}

		return core.NumberValue(math.Ceil(float64(num))), nil
	}))
}

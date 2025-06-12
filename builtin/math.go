package builtin

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/core"
)

// RegisterMathModule creates and registers the math module
func RegisterMathModule() {
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
}

// RegisterMathFunctions registers mathematical builtin functions
func RegisterMathFunctions(ctx *core.Context) {
	// Also register as a math module
	RegisterMathModule()
	// abs - absolute value
	ctx.Define("abs", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("abs() takes exactly one argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.NumberValue:
			return core.NumberValue(math.Abs(float64(v))), nil
		default:
			return nil, fmt.Errorf("abs() argument must be a number, not '%s'", v.Type())
		}
	}))

	// min - minimum value
	ctx.Define("min", NewKwargsBuiltinFunction("min", MinWithKwargs))

	// max - maximum value
	ctx.Define("max", NewKwargsBuiltinFunction("max", MaxWithKwargs))

	// sum - sum of values
	ctx.Define("sum", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("sum expected 1 or 2 arguments, got %d", len(args))
		}

		var start float64 = 0
		if len(args) == 2 {
			if num, ok := args[1].(core.NumberValue); ok {
				start = float64(num)
			} else {
				return nil, fmt.Errorf("sum() start argument must be a number")
			}
		}

		// Get the iterable
		var items []core.Value
		switch v := args[0].(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		default:
			return nil, fmt.Errorf("sum() argument must be an iterable, not '%s'", v.Type())
		}

		sum := start
		for _, item := range items {
			if num, ok := item.(core.NumberValue); ok {
				sum += float64(num)
			} else {
				return nil, fmt.Errorf("unsupported operand type(s) for +: 'float' and '%s'", item.Type())
			}
		}

		return core.NumberValue(sum), nil
	}))

	// round is registered in essential_builtins.go with full functionality

	// pow - power function
	ctx.Define("pow", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("pow() takes exactly 2 arguments (%d given)", len(args))
		}

		base, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("pow() base must be a number, not '%s'", args[0].Type())
		}

		exp, ok := args[1].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("pow() exponent must be a number, not '%s'", args[1].Type())
		}

		return core.NumberValue(math.Pow(float64(base), float64(exp))), nil
	}))

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

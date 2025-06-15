package modules

import (
	"math"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitMathModule creates and returns the math module
func InitMathModule() *core.DictValue {
	mathModule := core.NewDict()

	// sqrt - square root
	mathModule.Set("sqrt", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("sqrt", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		if num < 0 {
			return nil, errors.NewRuntimeError("sqrt", "math domain error")
		}

		return core.NumberValue(math.Sqrt(num)), nil
	}))

	// floor - round down
	mathModule.Set("floor", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("floor", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Floor(num)), nil
	}))

	// ceil - round up
	mathModule.Set("ceil", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("ceil", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Ceil(num)), nil
	}))

	// sin - sine
	mathModule.Set("sin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("sin", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Sin(num)), nil
	}))

	// cos - cosine
	mathModule.Set("cos", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("cos", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Cos(num)), nil
	}))

	// tan - tangent
	mathModule.Set("tan", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("tan", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Tan(num)), nil
	}))

	// Constants
	mathModule.Set("pi", core.NumberValue(math.Pi))
	mathModule.Set("e", core.NumberValue(math.E))

	return mathModule
}

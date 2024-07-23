package builtin

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/core"
)

func RegisterMathOps() {
	core.RegisterBuiltin("sin", sinFunc)
	core.RegisterBuiltin("cos", cosFunc)
	core.RegisterBuiltin("tan", tanFunc)
	core.RegisterBuiltin("exp", expFunc)
	core.RegisterBuiltin("log", logFunc)
	core.RegisterBuiltin("sqrt", sqrtFunc)
	core.RegisterBuiltin("pow", powFunc)
	core.RegisterBuiltin("abs", absFunc)
	core.RegisterBuiltin("floor", floorFunc)
	core.RegisterBuiltin("ceiling", ceilingFunc)
}

func sinFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sin requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("sin argument must be a number")
	}
	return math.Sin(num), nil
}

func cosFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cos requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("cos argument must be a number")
	}
	return math.Cos(num), nil
}

func tanFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("tan requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("tan argument must be a number")
	}
	return math.Tan(num), nil
}

func expFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("exp requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("exp argument must be a number")
	}
	return math.Exp(num), nil
}

func logFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 && len(args) != 2 {
		return nil, fmt.Errorf("log requires one or two arguments")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("log argument must be a number")
	}
	if len(args) == 1 {
		return math.Log(num), nil // natural logarithm
	}
	base, ok := args[1].(float64)
	if !ok {
		return nil, fmt.Errorf("log base must be a number")
	}
	return math.Log(num) / math.Log(base), nil // logarithm with custom base
}

func sqrtFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sqrt requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("sqrt argument must be a number")
	}
	return math.Sqrt(num), nil
}

func powFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("pow requires exactly two arguments")
	}
	base, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("pow base must be a number")
	}
	exponent, ok := args[1].(float64)
	if !ok {
		return nil, fmt.Errorf("pow exponent must be a number")
	}
	return math.Pow(base, exponent), nil
}

func absFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("abs requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("abs argument must be a number")
	}
	return math.Abs(num), nil
}

func floorFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("floor requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("floor argument must be a number")
	}
	return math.Floor(num), nil
}

func ceilingFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("ceiling requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("ceiling argument must be a number")
	}
	return math.Ceil(num), nil
}

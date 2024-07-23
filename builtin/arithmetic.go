package builtin

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/core"
)

func RegisterArithmeticFuncs() {
	core.RegisterBuiltin("+", add)
	core.RegisterBuiltin("-", subtract)
	core.RegisterBuiltin("*", multiply)
	core.RegisterBuiltin("/", divide)
	core.RegisterBuiltin("%", modulo)
	core.RegisterBuiltin("mod", modulo)
	core.RegisterBuiltin("max", maxFunc)
	core.RegisterBuiltin("min", minFunc)
	core.RegisterBuiltin("floor", floorFunc)
	core.RegisterBuiltin("ceiling", ceilingFunc)
	core.RegisterBuiltin("round", roundFunc)
	core.RegisterBuiltin("truncate", truncateFunc)
	core.RegisterBuiltin("rem", remFunc)
	core.RegisterBuiltin("abs", absFunc)
	core.RegisterBuiltin("evenp", evenpFunc)
	core.RegisterBuiltin("oddp", oddpFunc)
	core.RegisterBuiltin("zerop", zeropFunc)
	core.RegisterBuiltin("plusp", pluspFunc)
	core.RegisterBuiltin("minusp", minuspFunc)
}

func add(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("+ requires at least two arguments, got %d", len(args))
	}
	result := 0.0
	for _, arg := range args {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("+ requires numeric arguments, got %v", arg)
		}
		result += num
	}
	return result, nil
}

func subtract(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("- requires at least one argument")
	}
	first, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("- requires numeric arguments, got %v", args[0])
	}
	if len(args) == 1 {
		return -first, nil
	}
	result := first
	for _, arg := range args[1:] {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("- requires numeric arguments, got %v", arg)
		}
		result -= num
	}
	return result, nil
}

func multiply(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("* requires at least two arguments")
	}
	result := 1.0
	for _, arg := range args {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("* requires numeric arguments, got %v", arg)
		}
		result *= num
	}
	return result, nil
}

func divide(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("/ requires at least two arguments")
	}
	first, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("/ requires numeric arguments, got %v", args[0])
	}
	result := first
	for _, arg := range args[1:] {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("/ requires numeric arguments, got %v", arg)
		}
		if num == 0 {
			return nil, fmt.Errorf("division by zero")
		}
		result /= num
	}
	return result, nil
}

func modulo(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%% requires exactly two arguments")
	}
	a, ok1 := args[0].(float64)
	b, ok2 := args[1].(float64)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("%% requires numeric arguments, got %v and %v", args[0], args[1])
	}
	if b == 0 {
		return nil, fmt.Errorf("modulo by zero")
	}
	return math.Mod(a, b), nil
}

func maxFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("max requires at least one argument")
	}
	max := args[0].(float64)
	for _, arg := range args[1:] {
		if num, ok := arg.(float64); ok && num > max {
			max = num
		}
	}
	return max, nil
}

func minFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("min requires at least one argument")
	}
	min := args[0].(float64)
	for _, arg := range args[1:] {
		if num, ok := arg.(float64); ok && num < min {
			min = num
		}
	}
	return min, nil
}

func floorFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("floor requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("floor requires a number argument")
	}
	return math.Floor(num), nil
}

func ceilingFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("ceiling requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("ceiling requires a number argument")
	}
	return math.Ceil(num), nil
}

func roundFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("round requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("round requires a number argument")
	}
	return math.Round(num), nil
}

func truncateFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("truncate requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("truncate requires a number argument")
	}
	return math.Trunc(num), nil
}

func remFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("rem requires exactly two arguments")
	}
	num1, ok1 := args[0].(float64)
	num2, ok2 := args[1].(float64)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("rem requires two number arguments")
	}
	if num2 == 0 {
		return nil, fmt.Errorf("division by zero in rem")
	}
	return math.Remainder(num1, num2), nil
}

func absFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("abs requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("abs requires a numeric argument")
	}
	return math.Abs(num), nil
}

func evenpFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("evenp requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("evenp requires a numeric argument")
	}
	return math.Mod(num, 2) == 0, nil
}

func oddpFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("oddp requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("oddp requires a numeric argument")
	}
	return math.Mod(num, 2) != 0, nil
}

func zeropFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("zerop requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("zerop requires a numeric argument")
	}
	return num == 0, nil
}

func pluspFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("plusp requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("plusp requires a numeric argument")
	}
	return num > 0, nil
}

func minuspFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("minusp requires exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("minusp requires a numeric argument")
	}
	return num < 0, nil
}

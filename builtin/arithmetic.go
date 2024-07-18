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
	core.RegisterBuiltin("max", maxFunc)
	core.RegisterBuiltin("min", minFunc)
}

func add(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("+ requires at least two arguments")
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

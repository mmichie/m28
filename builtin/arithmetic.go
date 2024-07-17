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
}

func add(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("+ requires at least two arguments")
	}
	result := 0.0
	for _, arg := range args {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("+ requires numeric arguments")
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
		return nil, fmt.Errorf("- requires numeric arguments")
	}
	if len(args) == 1 {
		return -first, nil
	}
	result := first
	for _, arg := range args[1:] {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("- requires numeric arguments")
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
			return nil, fmt.Errorf("* requires numeric arguments")
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
		return nil, fmt.Errorf("/ requires numeric arguments")
	}
	result := first
	for _, arg := range args[1:] {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("/ requires numeric arguments")
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
		return nil, fmt.Errorf("%% requires numeric arguments")
	}
	if b == 0 {
		return nil, fmt.Errorf("modulo by zero")
	}
	return math.Mod(a, b), nil
}

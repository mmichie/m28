// File: builtin/arithmetic.go

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
	core.RegisterBuiltin("//", floorDivide)
	core.RegisterBuiltin("**", power)
}

func add(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("+ requires at least two arguments")
	}
	result := args[0].(float64)
	for _, arg := range args[1:] {
		result += arg.(float64)
	}
	return result, nil
}

func subtract(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("- requires at least one argument")
	}
	if len(args) == 1 {
		return -args[0].(float64), nil
	}
	result := args[0].(float64)
	for _, arg := range args[1:] {
		result -= arg.(float64)
	}
	return result, nil
}

func multiply(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("* requires at least two arguments")
	}
	result := args[0].(float64)
	for _, arg := range args[1:] {
		result *= arg.(float64)
	}
	return result, nil
}

func divide(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("/ requires exactly two arguments")
	}
	dividend, divisor := args[0].(float64), args[1].(float64)
	if divisor == 0 {
		return nil, fmt.Errorf("division by zero")
	}
	return dividend / divisor, nil
}

func modulo(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%% requires exactly two arguments")
	}
	a, b := args[0].(float64), args[1].(float64)
	if b == 0 {
		return nil, fmt.Errorf("modulo by zero")
	}
	return math.Mod(a, b), nil
}

func floorDivide(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("// requires exactly two arguments")
	}
	a, b := args[0].(float64), args[1].(float64)
	if b == 0 {
		return nil, fmt.Errorf("floor division by zero")
	}
	return math.Floor(a / b), nil
}

func power(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("** requires exactly two arguments")
	}
	base, exponent := args[0].(float64), args[1].(float64)
	return math.Pow(base, exponent), nil
}

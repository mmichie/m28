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
	core.RegisterBuiltin("=", assignFunc)
}

func assignFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("= requires exactly two arguments")
	}
	symbol, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to = must be a symbol")
	}
	value := args[1]
	
	// First try to update an existing variable in any scope
	if !env.SetMutable(symbol, value) {
		// If the variable doesn't exist anywhere, define it in the current scope
		env.Define(symbol, value)
	}
	
	return value, nil
}

func add(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("+ requires at least two arguments")
	}

	// Check if we're dealing with strings
	if _, ok := args[0].(string); ok {
		result := ""
		for _, arg := range args {
			s, ok := arg.(string)
			if !ok {
				return nil, fmt.Errorf("+ cannot mix strings and non-strings")
			}
			result += s
		}
		return result, nil
	}

	// Check if we're dealing with lists (concatenation)
	if list1, ok := args[0].(core.LispList); ok {
		var result core.LispList
		result = append(result, list1...)

		for _, arg := range args[1:] {
			if list2, ok := arg.(core.LispList); ok {
				result = append(result, list2...)
			} else if list2, ok := arg.(core.LispListLiteral); ok {
				// Convert LispListLiteral to LispList
				result = append(result, core.LispList(list2)...)
			} else {
				return nil, fmt.Errorf("+ cannot mix lists and non-lists")
			}
		}
		return result, nil
	}

	// Also handle list literals
	if list1, ok := args[0].(core.LispListLiteral); ok {
		var result core.LispList
		result = append(result, core.LispList(list1)...)

		for _, arg := range args[1:] {
			if list2, ok := arg.(core.LispList); ok {
				result = append(result, list2...)
			} else if list2, ok := arg.(core.LispListLiteral); ok {
				// Convert LispListLiteral to LispList
				result = append(result, core.LispList(list2)...)
			} else {
				return nil, fmt.Errorf("+ cannot mix lists and non-lists")
			}
		}
		return result, nil
	}

	// If not strings or lists, assume numbers
	if num, ok := args[0].(float64); ok {
		result := num
		for _, arg := range args[1:] {
			if val, ok := arg.(float64); ok {
				result += val
			} else {
				return nil, fmt.Errorf("+ cannot mix numbers and non-numbers")
			}
		}
		return result, nil
	}

	return nil, fmt.Errorf("+ operator not supported for %T", args[0])
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

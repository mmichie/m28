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
	core.RegisterBuiltin("*", multiplyWithTupleSupport) // Use the new multiply func
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
			} else if tuple, ok := arg.(core.LispTuple); ok {
				// Convert LispTuple to LispList for append
				result = append(result, core.LispList(tuple)...)
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
			} else if tuple, ok := arg.(core.LispTuple); ok {
				// Convert LispTuple to LispList for append
				result = append(result, core.LispList(tuple)...)
			} else {
				return nil, fmt.Errorf("+ cannot mix lists and non-lists")
			}
		}
		return result, nil
	}

	// Handle tuples (concatenation to new tuple)
	if tuple1, ok := args[0].(core.LispTuple); ok {
		var result core.LispTuple
		result = append(result, tuple1...)

		for _, arg := range args[1:] {
			if tuple2, ok := arg.(core.LispTuple); ok {
				result = append(result, tuple2...)
			} else if list, ok := arg.(core.LispList); ok {
				// Convert LispList to tuple elements
				result = append(result, core.LispTuple(list)...)
			} else if list, ok := arg.(core.LispListLiteral); ok {
				// Convert LispListLiteral to tuple elements
				result = append(result, core.LispTuple(list)...)
			} else {
				return nil, fmt.Errorf("+ cannot mix tuples and non-sequences")
			}
		}
		return result, nil
	}

	// If not strings, lists, or tuples, assume numbers
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

// Original multiply function (keep for reference)
func multiply(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("* requires at least two arguments")
	}
	result := args[0].(float64)
	for _, arg := range args[1:] {
		result *= arg.(float64)
	}
	return result, nil
}

// New multiply function with tuple repetition support
func multiplyWithTupleSupport(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("* requires at least two arguments")
	}

	// If first arg is a tuple and second is a number, handle repetition
	if tuple, isTuple := args[0].(core.LispTuple); isTuple && len(args) == 2 {
		if num, isNum := args[1].(float64); isNum {
			count := int(num)
			if float64(count) != num || count < 0 {
				return nil, fmt.Errorf("tuple repetition requires a non-negative integer, got %v", num)
			}

			result := make(core.LispTuple, 0, len(tuple)*count)
			for i := 0; i < count; i++ {
				result = append(result, tuple...)
			}
			return result, nil
		}
		return nil, fmt.Errorf("cannot multiply tuple by non-number: %T", args[1])
	}

	// If first arg is a number and second is a tuple, handle repetition
	if num, isNum := args[0].(float64); isNum && len(args) == 2 {
		if tuple, isTuple := args[1].(core.LispTuple); isTuple {
			count := int(num)
			if float64(count) != num || count < 0 {
				return nil, fmt.Errorf("tuple repetition requires a non-negative integer, got %v", num)
			}

			result := make(core.LispTuple, 0, len(tuple)*count)
			for i := 0; i < count; i++ {
				result = append(result, tuple...)
			}
			return result, nil
		}
	}

	// Otherwise, treat as regular numeric multiplication
	allNums := true
	for _, arg := range args {
		_, isNum := arg.(float64)
		if !isNum {
			allNums = false
			break
		}
	}

	if allNums {
		num1, _ := args[0].(float64)
		result := num1
		for _, arg := range args[1:] {
			num2, _ := arg.(float64)
			result *= num2
		}
		return result, nil
	}

	// If we get here, we don't know how to handle these args
	types := make([]string, len(args))
	for i, arg := range args {
		types[i] = fmt.Sprintf("%T", arg)
	}
	return nil, fmt.Errorf("* not supported for types: %v", types)
}

func divide(args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

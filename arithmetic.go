package m28

import (
	"fmt"
	"math"
)

type ArithmeticFunc func([]float64) (float64, error)

var arithmeticFuncs = map[string]ArithmeticFunc{
	"+": add,
	"-": subtract,
	"*": multiply,
	"/": divide,
	"%": mod,
}

func registerArithmeticFuncs(env *Environment) {
	for name, fn := range arithmeticFuncs {
		env.Set(LispSymbol(name), LispFunc(makeArithmeticFunc(fn)))
	}
}

func makeArithmeticFunc(fn ArithmeticFunc) BuiltinFunc {
	return func(args []LispValue, _ *Environment) (LispValue, error) {
		numbers, err := convertToNumbers(args)
		if err != nil {
			return nil, err
		}
		result, err := fn(numbers)
		if err != nil {
			return nil, err
		}
		return result, nil
	}
}

func convertToNumbers(args []LispValue) ([]float64, error) {
	numbers := make([]float64, len(args))
	for i, arg := range args {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("argument %d is not a number: %v", i+1, arg)
		}
		numbers[i] = num
	}
	return numbers, nil
}

func add(numbers []float64) (float64, error) {
	result := 0.0
	for _, num := range numbers {
		result += num
		if math.IsInf(result, 0) {
			return 0, fmt.Errorf("overflow in addition")
		}
	}
	return result, nil
}

func subtract(numbers []float64) (float64, error) {
	if len(numbers) == 0 {
		return 0, fmt.Errorf("subtract requires at least one argument")
	}
	result := numbers[0]
	for _, num := range numbers[1:] {
		result -= num
		if math.IsInf(result, 0) {
			return 0, fmt.Errorf("overflow in subtraction")
		}
	}
	return result, nil
}

func multiply(numbers []float64) (float64, error) {
	result := 1.0
	for _, num := range numbers {
		result *= num
		if math.IsInf(result, 0) {
			return 0, fmt.Errorf("overflow in multiplication")
		}
	}
	return result, nil
}

func divide(numbers []float64) (float64, error) {
	if len(numbers) == 0 {
		return 0, fmt.Errorf("divide requires at least one argument")
	}
	result := numbers[0]
	for _, num := range numbers[1:] {
		if num == 0 {
			return 0, fmt.Errorf("division by zero")
		}
		result /= num
		if math.IsInf(result, 0) {
			return 0, fmt.Errorf("overflow in division")
		}
	}
	return result, nil
}

func mod(numbers []float64) (float64, error) {
	if len(numbers) != 2 {
		return 0, fmt.Errorf("modulo requires exactly two arguments")
	}
	if numbers[1] == 0 {
		return 0, fmt.Errorf("modulo by zero")
	}
	return math.Mod(numbers[0], numbers[1]), nil
}

package m28

import (
	"fmt"
)

type ComparisonFunc func([]float64) (bool, error)

var comparisonFuncs = map[string]ComparisonFunc{
	"<":  lessThan,
	">":  greaterThan,
	"=":  equals,
	">=": greaterThanOrEqual,
	"<=": lessThanOrEqual,
	"!=": notEqual,
}

func registerComparisonFuncs(env *Environment) {
	for name, fn := range comparisonFuncs {
		env.Set(LispSymbol(name), LispFunc(makeComparisonFunc(fn)))
	}
}

func makeComparisonFunc(fn ComparisonFunc) BuiltinFunc {
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

func lessThan(numbers []float64) (bool, error) {
	if len(numbers) < 2 {
		return false, fmt.Errorf("< requires at least two arguments")
	}
	for i := 1; i < len(numbers); i++ {
		if numbers[i-1] >= numbers[i] {
			return false, nil
		}
	}
	return true, nil
}

func greaterThan(numbers []float64) (bool, error) {
	if len(numbers) < 2 {
		return false, fmt.Errorf("> requires at least two arguments")
	}
	for i := 1; i < len(numbers); i++ {
		if numbers[i-1] <= numbers[i] {
			return false, nil
		}
	}
	return true, nil
}

func equals(numbers []float64) (bool, error) {
	if len(numbers) < 2 {
		return false, fmt.Errorf("= requires at least two arguments")
	}
	for i := 1; i < len(numbers); i++ {
		if numbers[i-1] != numbers[i] {
			return false, nil
		}
	}
	return true, nil
}

func greaterThanOrEqual(numbers []float64) (bool, error) {
	if len(numbers) < 2 {
		return false, fmt.Errorf(">= requires at least two arguments")
	}
	for i := 1; i < len(numbers); i++ {
		if numbers[i-1] < numbers[i] {
			return false, nil
		}
	}
	return true, nil
}

func lessThanOrEqual(numbers []float64) (bool, error) {
	if len(numbers) < 2 {
		return false, fmt.Errorf("<= requires at least two arguments")
	}
	for i := 1; i < len(numbers); i++ {
		if numbers[i-1] > numbers[i] {
			return false, nil
		}
	}
	return true, nil
}

func notEqual(numbers []float64) (bool, error) {
	if len(numbers) < 2 {
		return false, fmt.Errorf("!= requires at least two arguments")
	}
	for i := 1; i < len(numbers); i++ {
		if numbers[i-1] == numbers[i] {
			return false, nil
		}
	}
	return true, nil
}

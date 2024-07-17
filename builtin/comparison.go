package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func init() {
	core.RegisterBuiltin("<", lessThan)
	core.RegisterBuiltin(">", greaterThan)
	core.RegisterBuiltin("<=", lessOrEqual)
	core.RegisterBuiltin(">=", greaterOrEqual)
	core.RegisterBuiltin("=", equal)
	core.RegisterBuiltin("equal?", equalFunc)
}

func lessThan(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return compareNumbers(args, func(a, b float64) bool { return a < b })
}

func greaterThan(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return compareNumbers(args, func(a, b float64) bool { return a > b })
}

func lessOrEqual(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return compareNumbers(args, func(a, b float64) bool { return a <= b })
}

func greaterOrEqual(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return compareNumbers(args, func(a, b float64) bool { return a >= b })
}

func equal(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("= requires at least two arguments")
	}
	first := args[0]
	for _, arg := range args[1:] {
		if !core.EqualValues(first, arg) {
			return false, nil
		}
	}
	return true, nil
}

func compareNumbers(args []core.LispValue, compare func(float64, float64) bool) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("comparison requires at least two arguments")
	}
	var prev float64
	for i, arg := range args {
		num, ok := arg.(float64)
		if !ok {
			return nil, fmt.Errorf("comparison requires numeric arguments")
		}
		if i > 0 && !compare(prev, num) {
			return false, nil
		}
		prev = num
	}
	return true, nil
}

func equalFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("equal? requires exactly two arguments")
	}
	return core.EqualValues(args[0], args[1]), nil
}

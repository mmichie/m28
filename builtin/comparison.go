package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func RegisterComparisonBuiltins() {
	core.RegisterBuiltin("==", eqFunc)
	core.RegisterBuiltin("!=", neFunc)
	core.RegisterBuiltin("<", ltFunc)
	core.RegisterBuiltin("<=", leFunc)
	core.RegisterBuiltin(">", gtFunc)
	core.RegisterBuiltin(">=", geFunc)
}

func eqFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("== requires exactly two arguments")
	}
	return core.PythonicBool(core.EqualValues(args[0], args[1])), nil
}

func neFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("!= requires exactly two arguments")
	}
	return core.PythonicBool(!core.EqualValues(args[0], args[1])), nil
}

func ltFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("< requires exactly two arguments")
	}
	return compareValues(args[0], args[1], func(a, b float64) bool { return a < b })
}

func leFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("<= requires exactly two arguments")
	}
	return compareValues(args[0], args[1], func(a, b float64) bool { return a <= b })
}

func gtFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("> requires exactly two arguments")
	}
	return compareValues(args[0], args[1], func(a, b float64) bool { return a > b })
}

func geFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf(">= requires exactly two arguments")
	}
	return compareValues(args[0], args[1], func(a, b float64) bool { return a >= b })
}

func compareValues(a, b core.LispValue, comparator func(float64, float64) bool) (core.LispValue, error) {
	switch v1 := a.(type) {
	case float64:
		if v2, ok := b.(float64); ok {
			return core.PythonicBool(comparator(v1, v2)), nil
		}
	case string:
		if v2, ok := b.(string); ok {
			return core.PythonicBool(comparator(float64(len(v1)), float64(len(v2)))), nil
		}
	}
	return nil, fmt.Errorf("cannot compare values of different types")
}

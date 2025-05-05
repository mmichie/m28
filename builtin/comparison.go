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
	core.RegisterBuiltin("and", andFunc)
	core.RegisterBuiltin("or", orFunc)
	core.RegisterBuiltin("not", notFunc)
	core.RegisterBuiltin("eq?", eqFunc) // Alternative equality function
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

func andFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Python-like and function with short-circuit evaluation
	if len(args) == 0 {
		return core.PythonicBool(true), nil
	}

	var result core.LispValue = core.PythonicBool(true)
	for _, arg := range args {
		if !core.IsTruthy(arg) {
			return arg, nil // Return the first falsy value encountered
		}
		result = arg // Keep track of the most recent value
	}
	return result, nil // Return the last value evaluated
}

func orFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Python-like or function with short-circuit evaluation
	if len(args) == 0 {
		return core.PythonicBool(false), nil
	}

	for _, arg := range args {
		if core.IsTruthy(arg) {
			return arg, nil // Return the first truthy value encountered
		}
	}
	return args[len(args)-1], nil // Return the last value if all are falsy
}

func notFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not requires exactly one argument")
	}
	return core.PythonicBool(!core.IsTruthy(args[0])), nil
}

func compareValues(a, b core.LispValue, comparator func(float64, float64) bool) (core.LispValue, error) {
	// Check if we can use the generic Compare function
	_, isComplex1 := a.(core.LispList)
	_, isComplex2 := a.(core.LispListLiteral)
	_, isComplex3 := a.(core.LispTuple)
	_, isComplex4 := a.(*core.PythonicDict)
	_, isComplex5 := a.(*core.PythonicSet)

	_, isComplex6 := b.(core.LispList)
	_, isComplex7 := b.(core.LispListLiteral)
	_, isComplex8 := b.(core.LispTuple)
	_, isComplex9 := b.(*core.PythonicDict)
	_, isComplex10 := b.(*core.PythonicSet)

	// If either value is a complex type, use the Compare function
	if isComplex1 || isComplex2 || isComplex3 || isComplex4 || isComplex5 ||
		isComplex6 || isComplex7 || isComplex8 || isComplex9 || isComplex10 {
		result := core.Compare(a, b)

		// lt: < 0
		if comparator(0, 1) && !comparator(1, 0) {
			return core.PythonicBool(result < 0), nil
		}
		// gt: > 0
		if !comparator(0, 1) && comparator(1, 0) {
			return core.PythonicBool(result > 0), nil
		}
		// le: <= 0
		if comparator(0, 0) && comparator(0, 1) {
			return core.PythonicBool(result <= 0), nil
		}
		// ge: >= 0
		if comparator(0, 0) && comparator(1, 0) {
			return core.PythonicBool(result >= 0), nil
		}
	}

	// For simple numeric types, use the provided comparator directly
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

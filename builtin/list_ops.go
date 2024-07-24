package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func init() {
	core.RegisterBuiltin("any", anyFunc)
	core.RegisterBuiltin("list", list)
	core.RegisterBuiltin("length", length)
	core.RegisterBuiltin("null?", isNull)
	core.RegisterBuiltin("null", isNull)
	core.RegisterBuiltin("car", carFunc)
	core.RegisterBuiltin("cdr", cdrFunc)
	core.RegisterBuiltin("cadr", cadrFunc)
	core.RegisterBuiltin("caddr", caddrFunc)
	core.RegisterBuiltin("cons", consFunc)
	core.RegisterBuiltin("list", listFunc)
	core.RegisterBuiltin("append", appendFunc)
	core.RegisterBuiltin("length", lengthFunc)
	core.RegisterBuiltin("consp", conspFunc)
	core.RegisterBuiltin("first", firstFunc)
	core.RegisterBuiltin("second", secondFunc)
	core.RegisterBuiltin("third", thirdFunc)
	core.RegisterBuiltin("nth", nthFunc)
	core.RegisterBuiltin("last", lastFunc)
	core.RegisterBuiltin("butlast", butlastFunc)
	core.RegisterBuiltin("nthcdr", nthcdrFunc)
}

func caddrFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("caddr requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) < 3 {
		return nil, fmt.Errorf("caddr requires a list with at least three elements")
	}
	return list[2], nil
}

func list(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return core.LispList(args), nil
}

func length(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("length requires exactly one argument")
	}
	switch v := args[0].(type) {
	case core.LispList:
		return float64(len(v)), nil
	case string:
		return float64(len(v)), nil
	default:
		return nil, fmt.Errorf("length requires a list or string argument")
	}
}

func isNull(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("null? expects exactly one argument")
	}
	switch v := args[0].(type) {
	case core.LispList:
		return len(v) == 0, nil
	case core.Nil:
		return true, nil
	default:
		return false, nil
	}
}

func carFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("car requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("car requires a non-empty list")
	}
	return list[0], nil
}

func cdrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cdr requires exactly one argument")
	}
	switch v := args[0].(type) {
	case core.LispList:
		if len(v) == 0 {
			return nil, fmt.Errorf("cdr: cannot take cdr of empty list")
		}
		if len(v) == 2 {
			// This is likely a pair created by cons
			return v[1], nil
		}
		return core.LispList(v[1:]), nil
	default:
		return nil, fmt.Errorf("cdr requires a list argument, got %T", args[0])
	}
}

func cadrFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cadr requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) < 2 {
		return nil, fmt.Errorf("cadr requires a list with at least two elements")
	}
	return list[1], nil
}

func listFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return core.LispList(args), nil
}

func appendFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	result := make(core.LispList, 0)
	for _, arg := range args {
		if list, ok := arg.(core.LispList); ok {
			result = append(result, list...)
		} else {
			result = append(result, arg)
		}
	}
	return result, nil
}

func lengthFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("length requires exactly one argument")
	}
	switch v := args[0].(type) {
	case core.LispList:
		return float64(len(v)), nil
	case string:
		return float64(len(v)), nil
	default:
		return nil, fmt.Errorf("length requires a list or string argument")
	}
}

func anyFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("any requires exactly two arguments")
	}

	predicate, ok := args[0].(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to any must be a function")
	}

	list, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to any must be a list")
	}

	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	for _, item := range list {
		result, err := e.Apply(predicate, []core.LispValue{item}, env)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(result) {
			return true, nil
		}
	}

	return false, nil
}

func conspFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("consp requires exactly one argument")
	}

	switch v := args[0].(type) {
	case core.LispList:
		return len(v) > 0, nil
	default:
		return false, nil
	}
}

func firstFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("first requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("first requires a non-empty list")
	}
	return list[0], nil
}

func secondFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("second requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) < 2 {
		return nil, fmt.Errorf("second requires a list with at least two elements")
	}
	return list[1], nil
}

func thirdFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("third requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) < 3 {
		return nil, fmt.Errorf("third requires a list with at least three elements")
	}
	return list[2], nil
}

func nthFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("nth requires exactly two arguments")
	}
	n, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("nth first argument must be a number")
	}
	list, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("nth second argument must be a list")
	}
	index := int(n)
	if index < 0 || index >= len(list) {
		return nil, fmt.Errorf("nth index out of bounds")
	}
	return list[index], nil
}

func lastFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("last requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("last requires a non-empty list")
	}
	return list[len(list)-1], nil
}

func butlastFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("butlast requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("butlast requires a non-empty list")
	}
	return core.LispList(list[:len(list)-1]), nil
}

func nthcdrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("nthcdr requires exactly two arguments")
	}
	n, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("nthcdr first argument must be a number")
	}
	list, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("nthcdr second argument must be a list")
	}
	index := int(n)
	if index < 0 {
		return nil, fmt.Errorf("nthcdr index must be non-negative")
	}
	if index >= len(list) {
		return core.LispList{}, nil
	}
	return core.LispList(list[index:]), nil
}

func consFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons requires exactly two arguments")
	}

	switch second := args[1].(type) {
	case core.LispList:
		return append(core.LispList{args[0]}, second...), nil
	case core.Nil:
		return core.LispList{args[0]}, nil
	default:
		return core.LispList{args[0], args[1]}, nil
	}
}

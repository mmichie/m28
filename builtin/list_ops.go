package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func init() {
	core.RegisterBuiltin("car", car)
	core.RegisterBuiltin("cdr", cdr)
	core.RegisterBuiltin("cons", cons)
	core.RegisterBuiltin("list", list)
	core.RegisterBuiltin("length", length)
	core.RegisterBuiltin("null?", isNull)
	core.RegisterBuiltin("append", appendFunc)
}

func car(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("car requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("car requires a non-empty list")
	}
	return list[0], nil
}

func cdr(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cdr requires exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("cdr requires a non-empty list")
	}
	return core.LispList(list[1:]), nil
}

func cons(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons requires exactly two arguments")
	}
	if list, ok := args[1].(core.LispList); ok {
		return append(core.LispList{args[0]}, list...), nil
	}
	return core.LispList{args[0], args[1]}, nil
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

func appendFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	result := make(core.LispList, 0)
	for _, arg := range args {
		switch v := arg.(type) {
		case core.LispList:
			result = append(result, v...)
		default:
			result = append(result, v)
		}
	}
	return result, nil
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

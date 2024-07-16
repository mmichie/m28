package m28

import (
	"fmt"
)

func registerListFuncs(env *Environment) {
	env.Set(LispSymbol("car"), LispFunc(car))
	env.Set(LispSymbol("cdr"), LispFunc(cdr))
	env.Set(LispSymbol("cons"), LispFunc(cons))
	env.Set(LispSymbol("list"), LispFunc(list))
	env.Set(LispSymbol("length"), LispFunc(length))
	env.Set(LispSymbol("append"), LispFunc(appendFunc))
}

func car(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("car expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("car expects a non-empty list")
	}
	return list[0], nil
}

func cdr(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cdr expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("cdr expects a non-empty list")
	}
	return LispList(list[1:]), nil
}

func cons(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons expects exactly two arguments")
	}
	head, tail := args[0], args[1]
	switch t := tail.(type) {
	case LispList:
		return LispList(append([]LispValue{head}, t...)), nil
	default:
		return LispList{head, tail}, nil
	}
}

func list(args []LispValue, _ *Environment) (LispValue, error) {
	return LispList(args), nil
}

func length(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("length expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok {
		return nil, fmt.Errorf("length expects a list, got %T", args[0])
	}
	return float64(len(list)), nil
}

func appendFunc(args []LispValue, _ *Environment) (LispValue, error) {
	var result LispList
	for _, arg := range args {
		if list, ok := arg.(LispList); ok {
			result = append(result, list...)
		} else {
			result = append(result, arg)
		}
	}
	return result, nil
}

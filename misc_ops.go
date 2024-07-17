package m28

import (
	"fmt"
)

func registerMiscOps() {
	builtinFuncs["apply"] = applyFunc
	builtinFuncs["caar"] = caarFunc
	builtinFuncs["cadar"] = cadarFunc
	builtinFuncs["assoc"] = assoc
	builtinFuncs["filter"] = filter
	builtinFuncs["match"] = match
	builtinFuncs["exists?"] = exists
}

func applyFunc(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("apply expects at least two arguments")
	}

	fn := args[0]
	lastArg := args[len(args)-1]

	var applyArgs []LispValue
	for _, arg := range args[1 : len(args)-1] {
		applyArgs = append(applyArgs, arg)
	}

	if list, ok := lastArg.(LispList); ok {
		applyArgs = append(applyArgs, list...)
	} else {
		return nil, fmt.Errorf("last argument to apply must be a list")
	}

	return Apply(fn, applyArgs, env)
}

func caarFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("caar expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("caar expects a non-empty list")
	}
	firstElem, ok := list[0].(LispList)
	if !ok || len(firstElem) == 0 {
		return nil, fmt.Errorf("caar expects a list whose first element is a non-empty list")
	}
	return firstElem[0], nil
}

func cadarFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cadar expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) < 2 {
		return nil, fmt.Errorf("cadar expects a list with at least two elements")
	}
	secondElem, ok := list[1].(LispList)
	if !ok || len(secondElem) == 0 {
		return nil, fmt.Errorf("cadar expects a list whose second element is a non-empty list")
	}
	return secondElem[0], nil
}

func assoc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("assoc expects exactly two arguments")
	}
	key := args[0]
	alist, ok := args[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("assoc expects a list as its second argument")
	}
	for _, pair := range alist {
		pairList, ok := pair.(LispList)
		if !ok || len(pairList) < 1 {
			continue
		}
		if EqualValues(key, pairList[0]) {
			return pair, nil
		}
	}
	return nil, nil
}

func filter(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("filter expects exactly two arguments")
	}
	predFunc, ok := args[0].(*Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to filter must be a function")
	}
	list, ok := args[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to filter must be a list")
	}

	var result LispList
	for _, item := range list {
		predicateResult, err := Apply(predFunc, []LispValue{item}, env)
		if err != nil {
			return nil, err
		}
		if IsTruthy(predicateResult) {
			result = append(result, item)
		}
	}
	return result, nil
}

func match(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("match expects exactly two arguments")
	}
	pattern := args[0]
	fact := args[1]

	var matchHelper func(p, f LispValue) bool
	matchHelper = func(p, f LispValue) bool {
		switch pt := p.(type) {
		case LispList:
			ft, ok := f.(LispList)
			if !ok {
				return false
			}
			if len(pt) != len(ft) {
				return false
			}
			for i := range pt {
				if !matchHelper(pt[i], ft[i]) {
					return false
				}
			}
			return true
		case LispSymbol:
			if pt == LispSymbol("?") {
				return true
			}
			return EqualValues(p, f)
		default:
			return EqualValues(p, f)
		}
	}

	return matchHelper(pattern, fact), nil
}

func exists(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("exists? expects exactly two arguments")
	}

	predFunc, ok := args[0].(*Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to exists? must be a function")
	}

	list, ok := args[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to exists? must be a list")
	}

	for _, item := range list {
		result, err := Apply(predFunc, []LispValue{item}, env)
		if err != nil {
			return nil, err
		}
		if IsTruthy(result) {
			return true, nil
		}
	}

	return false, nil
}

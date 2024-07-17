package m28

import (
	"fmt"
)

func registerTypeChecking() {
	builtinFuncs["number?"] = isNumber
	builtinFuncs["string?"] = isString
	builtinFuncs["symbol?"] = isSymbol
	builtinFuncs["list?"] = isList
	builtinFuncs["null?"] = nullFunc
	builtinFuncs["pair?"] = pairFunc
	builtinFuncs["integer?"] = integerFunc
}

func isNumber(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number? expects exactly one argument")
	}
	_, ok := args[0].(float64)
	return ok, nil
}

func isString(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string? expects exactly one argument")
	}
	_, ok := args[0].(string)
	return ok, nil
}

func isSymbol(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol? expects exactly one argument")
	}
	_, ok := args[0].(LispSymbol)
	return ok, nil
}

func isList(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("list? expects exactly one argument")
	}
	_, ok := args[0].(LispList)
	return ok, nil
}

func nullFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("null? expects exactly one argument")
	}
	switch arg := args[0].(type) {
	case LispList:
		return len(arg) == 0, nil
	case nil:
		return true, nil
	default:
		return false, nil
	}
}

func pairFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("pair? expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	return ok && len(list) > 0, nil
}

func integerFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("integer? expects exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return false, nil
	}
	return float64(int(num)) == num, nil
}

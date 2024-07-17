package m28

import "fmt"

func registerLogicalOps() {
	builtinFuncs["and"] = and
	builtinFuncs["or"] = or
	builtinFuncs["not"] = notFunc
}

func and(args []LispValue, _ *Environment) (LispValue, error) {
	for _, arg := range args {
		if !IsTruthy(arg) {
			return false, nil
		}
	}
	return true, nil
}

func or(args []LispValue, _ *Environment) (LispValue, error) {
	for _, arg := range args {
		if IsTruthy(arg) {
			return true, nil
		}
	}
	return false, nil
}

func notFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not expects exactly one argument")
	}
	return !IsTruthy(args[0]), nil
}

package m28

import (
	"fmt"
)

func evalDefine(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("'define' expects exactly two arguments")
	}
	symbol, ok := args[0].(LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to 'define' must be a symbol")
	}
	value, err := EvalExpression(args[1], env)
	if err != nil {
		return nil, err
	}
	env.Define(symbol, value)
	return symbol, nil
}

func evalSet(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("'set!' expects exactly two arguments")
	}
	symbol, ok := args[0].(LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to 'set!' must be a symbol")
	}
	value, err := EvalExpression(args[1], env)
	if err != nil {
		return nil, err
	}
	if env.SetMutable(symbol, value) {
		return value, nil
	}
	return nil, fmt.Errorf("cannot set! undefined variable: %s", symbol)
}

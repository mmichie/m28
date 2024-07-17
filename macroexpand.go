package m28

import (
	"fmt"
)

func macroexpand(expr LispValue, env *Environment) (LispValue, bool, error) {
	list, ok := expr.(LispList)
	if !ok || len(list) == 0 {
		return expr, false, nil
	}

	symbol, ok := list[0].(LispSymbol)
	if !ok {
		return expr, false, nil
	}

	macro, ok := env.Get(symbol)
	if !ok {
		return expr, false, nil
	}

	m, ok := macro.(*Macro)
	if !ok {
		return expr, false, nil
	}

	macroEnv := NewEnvironment(m.Env)

	// Handle regular parameters
	for i, param := range m.Params {
		if i+1 < len(list) {
			macroEnv.Define(param, list[i+1])
		} else {
			return nil, false, fmt.Errorf("not enough arguments for macro %s", symbol)
		}
	}

	// Handle rest parameter
	if m.RestParam != "" {
		restArgs := list[len(m.Params)+1:]
		macroEnv.Define(m.RestParam, LispList(restArgs))
	} else if len(list) > len(m.Params)+1 {
		return nil, false, fmt.Errorf("too many arguments for macro %s", symbol)
	}

	expanded, err := EvalExpression(m.Body, macroEnv)
	if err != nil {
		return nil, false, err
	}

	return expanded, true, nil
}

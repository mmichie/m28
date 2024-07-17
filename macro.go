package m28

import (
	"fmt"
)

func evalDefineMacro(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("define-macro requires at least 2 arguments")
	}

	macroName, ok := args[0].(LispSymbol)
	if !ok {
		return nil, fmt.Errorf("macro name must be a symbol")
	}

	lambdaExpr, ok := args[1].(LispList)
	if !ok || len(lambdaExpr) < 2 {
		return nil, fmt.Errorf("invalid macro definition")
	}

	if lambdaExpr[0] != LispSymbol("lambda") {
		return nil, fmt.Errorf("macro body must be a lambda expression")
	}

	params, restParam, body, err := extractParamsAndBody(lambdaExpr[1:])
	if err != nil {
		return nil, err
	}

	macro := &Macro{
		Params:    params,
		RestParam: restParam,
		Body:      body,
		Env:       env,
	}

	env.Define(macroName, macro)
	return macroName, nil
}

func evalMacroexpand(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("macroexpand requires exactly 1 argument")
	}

	expanded, _, err := macroexpand(args[0], env)
	if err != nil {
		return nil, err
	}

	return expanded, nil
}

func extractParamsAndBody(args []LispValue) ([]LispSymbol, LispSymbol, LispValue, error) {
	if len(args) < 2 {
		return nil, "", nil, fmt.Errorf("lambda requires parameters and body")
	}

	paramList, ok := args[0].(LispList)
	if !ok {
		return nil, "", nil, fmt.Errorf("lambda parameters must be a list")
	}

	var params []LispSymbol
	var restParam LispSymbol
	for i, param := range paramList {
		symbol, ok := param.(LispSymbol)
		if !ok {
			return nil, "", nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		if symbol == "." {
			if i+1 < len(paramList) {
				restParam, ok = paramList[i+1].(LispSymbol)
				if !ok {
					return nil, "", nil, fmt.Errorf("rest parameter must be a symbol")
				}
				break
			}
			return nil, "", nil, fmt.Errorf("invalid rest parameter syntax")
		}
		params = append(params, symbol)
	}

	body := LispList(append([]LispValue{LispSymbol("begin")}, args[1:]...))
	return params, restParam, body, nil
}

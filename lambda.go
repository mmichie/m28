package m28

import (
	"fmt"
)

func evalLambda(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'lambda' expects at least two arguments")
	}
	params, ok := args[0].(LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}
	var paramSymbols []LispSymbol
	var restParam LispSymbol
	for i, param := range params {
		if i == len(params)-2 && param == LispSymbol(".") {
			restParam, ok = params[i+1].(LispSymbol)
			if !ok {
				return nil, fmt.Errorf("rest parameter must be a symbol")
			}
			break
		}
		symbol, ok := param.(LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols = append(paramSymbols, symbol)
	}
	return &Lambda{
		Params:    paramSymbols,
		RestParam: restParam,
		Body:      LispList(append([]LispValue{LispSymbol("begin")}, args[1:]...)),
		Env:       env,
		Closure:   NewEnvironment(env),
	}, nil
}

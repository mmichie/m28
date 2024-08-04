package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least a name and a body")
	}

	funcDef, ok := args[0].(core.LispList)
	if !ok || len(funcDef) == 0 {
		return nil, fmt.Errorf("invalid function definition")
	}

	funcName, ok := funcDef[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("function name must be a symbol")
	}

	params := make([]core.LispSymbol, len(funcDef)-1)
	for i, param := range funcDef[1:] {
		paramSymbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("function parameter must be a symbol")
		}
		params[i] = paramSymbol
	}

	body := core.LispList(args[1:])

	function := &core.Lambda{Params: params, Body: body, Env: env, Closure: env}
	env.Define(funcName, function)
	return function, nil
}

func EvalClass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("class definition requires at least a name and a body")
	}
	className, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	classEnv := env.NewEnvironment(env)
	for _, expr := range args[1:] {
		_, err := e.Eval(expr, classEnv)
		if err != nil {
			return nil, err
		}
	}

	class := &core.Lambda{Params: []core.LispSymbol{}, Body: core.LispList(args[1:]), Env: classEnv}
	env.Define(className, class)
	return class, nil
}

func EvalLambdaPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("lambda requires at least parameters and a body")
	}

	params, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}

	paramSymbols := make([]core.LispSymbol, len(params))
	for i, param := range params {
		symbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols[i] = symbol
	}

	return &core.Lambda{Params: paramSymbols, Body: args[1], Env: env}, nil
}

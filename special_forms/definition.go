package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least a name and a body")
	}

	// Extract function name and parameters
	var funcName core.LispSymbol
	var params []core.LispSymbol
	var defaultValues map[core.LispSymbol]core.LispValue = make(map[core.LispSymbol]core.LispValue)

	// Check if first arg is a list (the function signature)
	funcDef, ok := args[0].(core.LispList)
	if ok && len(funcDef) > 0 {
		// (def (name param1 param2) body...)
		nameVal, ok := funcDef[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("function name must be a symbol")
		}
		funcName = nameVal

		// Extract parameters
		for _, param := range funcDef[1:] {
			// Handle symbol parameter
			if paramSymbol, ok := param.(core.LispSymbol); ok {
				params = append(params, paramSymbol)
			} else if paramStr, ok := param.(string); ok {
				params = append(params, core.LispSymbol(paramStr))
			} else {
				return nil, fmt.Errorf("function parameter must be a symbol, got %T", param)
			}
		}
	} else {
		// Check if first arg is a symbol (simple named function)
		nameVal, ok := args[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("function name must be a symbol")
		}
		funcName = nameVal
		// No parameters
	}

	// Create function body from remaining arguments
	body := core.LispList(args[1:])

	// Create the Lambda function
	function := &core.Lambda{
		Params:        params,
		Body:          body,
		Env:           env,
		Closure:       env,
		DefaultValues: defaultValues,
	}
	
	// Define the function in the current environment
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

package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalDefmacro(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("defmacro requires at least 3 arguments")
	}

	macroName, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("macro name must be a symbol")
	}

	lambda, err := EvalLambda(e, args[1:], env)
	if err != nil {
		return nil, err
	}

	lambdaVal, ok := lambda.(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("macro body must be a lambda")
	}

	macro := &core.Macro{
		Params: lambdaVal.Params,
		Body:   lambdaVal.Body,
		Env:    lambdaVal.Env,
	}

	env.Define(macroName, macro)
	return macroName, nil
}

// EvalLambda is used by EvalDefmacro, so we'll include it here
func EvalLambda(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("lambda requires at least 2 arguments")
	}

	params, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}

	paramInfo, err := parseParameters(params)
	if err != nil {
		return nil, err
	}

	body := core.LispList{core.LispSymbol("progn")}
	body = append(body, args[1:]...)

	lambda := &core.Lambda{
		Params:    paramInfo.required,
		Optional:  paramInfo.optional,
		Rest:      paramInfo.rest,
		KeyParams: paramInfo.keyParams,
		Body:      body,
		Env:       env,
		Closure:   env,
	}

	return lambda, nil
}

// We'll also include a helper function for macro expansion
func ExpandMacro(e core.Evaluator, macro *core.Macro, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	lambdaEnv := env.NewEnvironment(macro.Env)

	// Bind arguments to parameters
	for i, param := range macro.Params {
		if i < len(args) {
			lambdaEnv.Define(param, args[i])
		} else {
			return nil, fmt.Errorf("not enough arguments for macro")
		}
	}

	// Evaluate macro body
	expanded, err := e.Eval(macro.Body, lambdaEnv)
	if err != nil {
		return nil, err
	}

	// Evaluate expanded form
	return e.Eval(expanded, env)
}

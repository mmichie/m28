package m28

import (
	"fmt"
)

// EvalExpression evaluates a LispValue in the given environment
func EvalExpression(expr LispValue, env *Environment) (LispValue, error) {
	switch e := expr.(type) {
	case LispSymbol:
		return evalSymbol(e, env)
	case float64, string, bool:
		return e, nil
	case LispList:
		return evalList(e, env)
	default:
		return nil, fmt.Errorf("unknown expression type: %T", e)
	}
}

func evalSymbol(symbol LispSymbol, env *Environment) (LispValue, error) {
	value, ok := env.Get(symbol)
	if !ok {
		return nil, fmt.Errorf("undefined symbol: %s", symbol)
	}
	return value, nil
}

func evalList(list LispList, env *Environment) (LispValue, error) {
	if len(list) == 0 {
		return nil, fmt.Errorf("empty list")
	}

	first := list[0]
	if symbol, ok := first.(LispSymbol); ok {
		if specialForm, exists := specialForms[symbol]; exists {
			return specialForm(list[1:], env)
		}
	}

	// Function application
	fn, err := EvalExpression(first, env)
	if err != nil {
		return nil, err
	}

	args, err := evalArgs(list[1:], env)
	if err != nil {
		return nil, err
	}

	return Apply(fn, args, env)
}

func evalArgs(args LispList, env *Environment) ([]LispValue, error) {
	evaluated := make([]LispValue, len(args))
	for i, arg := range args {
		value, err := EvalExpression(arg, env)
		if err != nil {
			return nil, err
		}
		evaluated[i] = value
	}
	return evaluated, nil
}

// Apply applies a function to arguments
func Apply(fn LispValue, args []LispValue, env *Environment) (LispValue, error) {
	switch f := fn.(type) {
	case LispFunc:
		return f(args, env)
	case *Lambda:
		return applyLambda(f, args, env)
	default:
		return nil, fmt.Errorf("not a function: %v", fn)
	}
}

func applyLambda(lambda *Lambda, args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < len(lambda.Params) && lambda.RestParam == "" {
		return nil, fmt.Errorf("not enough arguments for lambda")
	}
	callEnv := NewEnvironment(lambda.Closure)
	for i, param := range lambda.Params {
		if i < len(args) {
			callEnv.Define(param, args[i])
		} else {
			callEnv.Define(param, nil)
		}
	}
	if lambda.RestParam != "" {
		restArgs := args[len(lambda.Params):]
		callEnv.Define(lambda.RestParam, LispList(restArgs))
	}
	return EvalExpression(lambda.Body, callEnv)
}

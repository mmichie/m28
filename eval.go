package m28

import (
	"fmt"
)

func EvalExpression(expr LispValue, env *Environment) (LispValue, error) {
	switch e := expr.(type) {
	case LispSymbol:
		return evalSymbol(e, env)
	case float64, string, bool:
		return e, nil
	case LispList:
		expanded, wasExpanded, err := macroexpand(e, env)
		if err != nil {
			return nil, err
		}
		if wasExpanded {
			return EvalExpression(expanded, env)
		}
		return evalList(e, env)
	case Quasiquote:
		return evalQuasiquote(e.Expr, env, 0)
	case Unquote, UnquoteSplicing:
		return nil, fmt.Errorf("unquote or unquote-splicing not in quasiquote")
	default:
		return nil, fmt.Errorf("unknown expression type: %T", e)
	}
}

func evalQuasiquote(expr LispValue, env *Environment, depth int) (LispValue, error) {
	switch e := expr.(type) {
	case Unquote:
		if depth == 0 {
			return EvalExpression(e.Expr, env)
		}
		unquoted, err := evalQuasiquote(e.Expr, env, depth-1)
		return Unquote{Expr: unquoted}, err
	case UnquoteSplicing:
		if depth == 0 {
			return nil, fmt.Errorf("unquote-splicing not in list")
		}
		unquoted, err := evalQuasiquote(e.Expr, env, depth-1)
		return UnquoteSplicing{Expr: unquoted}, err
	case Quasiquote:
		return evalQuasiquote(e.Expr, env, depth+1)
	case LispList:
		result := make(LispList, 0, len(e))
		for _, item := range e {
			if us, ok := item.(UnquoteSplicing); ok && depth == 0 {
				spliced, err := EvalExpression(us.Expr, env)
				if err != nil {
					return nil, err
				}
				splicedList, ok := spliced.(LispList)
				if !ok {
					return nil, fmt.Errorf("unquote-splicing of non-list")
				}
				result = append(result, splicedList...)
			} else {
				evaluated, err := evalQuasiquote(item, env, depth)
				if err != nil {
					return nil, err
				}
				result = append(result, evaluated)
			}
		}
		return result, nil
	default:
		return expr, nil
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

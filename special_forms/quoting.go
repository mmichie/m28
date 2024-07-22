package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalQuote(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quote requires exactly one argument")
	}
	return args[0], nil
}

func EvalBackquote(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("backquote requires exactly one argument")
	}
	return EvalQuasiquote(e, args[0], env, 0)
}

func EvalQuasiquote(e core.Evaluator, expr core.LispValue, env core.Environment, depth int) (core.LispValue, error) {
	switch v := expr.(type) {
	case core.Unquote:
		if depth == 0 {
			return e.Eval(v.Expr, env)
		}
		unquoted, err := EvalQuasiquote(e, v.Expr, env, depth-1)
		return core.Unquote{Expr: unquoted}, err
	case core.UnquoteSplicing:
		if depth == 0 {
			return nil, fmt.Errorf("unquote-splicing not in list")
		}
		unquoted, err := EvalQuasiquote(e, v.Expr, env, depth-1)
		return core.UnquoteSplicing{Expr: unquoted}, err
	case core.Quasiquote:
		return EvalQuasiquote(e, v.Expr, env, depth+1)
	case core.LispList:
		return EvalQuasiquoteList(e, v, env, depth)
	default:
		return expr, nil
	}
}

func EvalQuasiquoteList(e core.Evaluator, list core.LispList, env core.Environment, depth int) (core.LispValue, error) {
	result := make(core.LispList, 0, len(list))
	for _, item := range list {
		if us, ok := item.(core.UnquoteSplicing); ok && depth == 0 {
			spliced, err := e.Eval(us.Expr, env)
			if err != nil {
				return nil, err
			}
			splicedList, ok := spliced.(core.LispList)
			if !ok {
				return nil, fmt.Errorf("unquote-splicing of non-list")
			}
			result = append(result, splicedList...)
		} else {
			evaluated, err := EvalQuasiquote(e, item, env, depth)
			if err != nil {
				return nil, err
			}
			result = append(result, evaluated)
		}
	}
	return result, nil
}

func EvalUnquote(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("unquote requires exactly one argument")
	}
	return e.Eval(args[0], env)
}

func EvalUnquoteSplicing(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("unquote-splicing requires exactly one argument")
	}
	result, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}
	if _, ok := result.(core.LispList); !ok {
		return nil, fmt.Errorf("unquote-splicing of non-list")
	}
	return result, nil
}

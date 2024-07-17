package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

type Evaluator struct {
	specialForms map[core.LispSymbol]SpecialFormFunc
}

func NewEvaluator() *Evaluator {
	return &Evaluator{
		specialForms: GetSpecialForms(),
	}
}

func (e *Evaluator) Eval(expr core.LispValue, env core.Environment) (core.LispValue, error) {
	switch v := expr.(type) {
	case core.LispSymbol:
		return evalSymbol(v, env)
	case float64, string, bool:
		return v, nil
	case core.LispList:
		return e.evalList(v, env)
	case core.Quasiquote:
		return e.evalQuasiquote(v.Expr, env, 0)
	case core.Unquote, core.UnquoteSplicing:
		return nil, fmt.Errorf("unquote or unquote-splicing not in quasiquote")
	default:
		return nil, fmt.Errorf("unknown expression type: %T", expr)
	}
}

func evalSymbol(symbol core.LispSymbol, env core.Environment) (core.LispValue, error) {
	value, ok := env.Get(symbol)
	if !ok {
		return nil, fmt.Errorf("undefined symbol: %s", symbol)
	}
	return value, nil
}

func (e *Evaluator) evalList(list core.LispList, env core.Environment) (core.LispValue, error) {
	if len(list) == 0 {
		return nil, fmt.Errorf("cannot evaluate empty list")
	}

	first := list[0]
	rest := list[1:]

	switch v := first.(type) {
	case core.LispSymbol:
		if specialForm, ok := e.specialForms[v]; ok {
			return specialForm(e, rest, env)
		}
		// Check if it's a built-in function
		if builtinFunc, ok := core.BuiltinFuncs[v]; ok {
			args, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, err
			}
			return builtinFunc(args, env)
		}
	}

	fn, err := e.Eval(first, env)
	if err != nil {
		return nil, err
	}

	args, err := e.evalArgs(rest, env)
	if err != nil {
		return nil, err
	}

	return e.Apply(fn, args, env)
}

func (e *Evaluator) evalArgs(exprs []core.LispValue, env core.Environment) ([]core.LispValue, error) {
	args := make([]core.LispValue, len(exprs))
	for i, expr := range exprs {
		arg, err := e.Eval(expr, env)
		if err != nil {
			return nil, err
		}
		args[i] = arg
	}
	return args, nil
}

func (e *Evaluator) Apply(fn core.LispValue, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	switch f := fn.(type) {
	case core.BuiltinFunc:
		return f(args, env)
	case *core.Lambda:
		return e.applyLambda(f, args, env)
	default:
		return nil, fmt.Errorf("not a function: %v", fn)
	}
}

func (e *Evaluator) applyLambda(lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < len(lambda.Params) && lambda.RestParam == "" {
		return nil, fmt.Errorf("not enough arguments for lambda: expected %d, got %d", len(lambda.Params), len(args))
	}

	callEnv := env.NewEnvironment(lambda.Closure)
	for i, param := range lambda.Params {
		if i < len(args) {
			callEnv.Define(param, args[i])
		} else {
			callEnv.Define(param, nil)
		}
	}

	if lambda.RestParam != "" {
		restArgs := args[len(lambda.Params):]
		callEnv.Define(lambda.RestParam, core.LispList(restArgs))
	}

	return e.Eval(lambda.Body, callEnv)
}

func (e *Evaluator) evalQuasiquote(expr core.LispValue, env core.Environment, depth int) (core.LispValue, error) {
	switch v := expr.(type) {
	case core.Unquote:
		if depth == 0 {
			return e.Eval(v.Expr, env)
		}
		unquoted, err := e.evalQuasiquote(v.Expr, env, depth-1)
		return core.Unquote{Expr: unquoted}, err
	case core.UnquoteSplicing:
		if depth == 0 {
			return nil, fmt.Errorf("unquote-splicing not in list")
		}
		unquoted, err := e.evalQuasiquote(v.Expr, env, depth-1)
		return core.UnquoteSplicing{Expr: unquoted}, err
	case core.Quasiquote:
		return e.evalQuasiquote(v.Expr, env, depth+1)
	case core.LispList:
		return e.evalQuasiquoteList(v, env, depth)
	default:
		return expr, nil
	}
}

func (e *Evaluator) evalQuasiquoteList(list core.LispList, env core.Environment, depth int) (core.LispValue, error) {
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
			evaluated, err := e.evalQuasiquote(item, env, depth)
			if err != nil {
				return nil, err
			}
			result = append(result, evaluated)
		}
	}
	return result, nil
}

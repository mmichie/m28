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
	case float64, int, string, bool:
		return v, nil
	case core.LispList:
		return e.evalList(v, env)
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
		fn, err := e.Eval(v, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating symbol %s: %v", v, err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating arguments: %v", err)
		}
		return e.Apply(fn, args, env)
	default:
		fn, err := e.Eval(first, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating function: %v", err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating arguments: %v", err)
		}
		return e.Apply(fn, args, env)
	}
}

func (e *Evaluator) evalArgs(args []core.LispValue, env core.Environment) ([]core.LispValue, error) {
	evaluated := make([]core.LispValue, len(args))
	for i, arg := range args {
		value, err := e.Eval(arg, env)
		if err != nil {
			return nil, err
		}
		evaluated[i] = value
	}
	return evaluated, nil
}

func (e *Evaluator) Apply(fn core.LispValue, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	switch f := fn.(type) {
	case core.BuiltinFunc:
		return f(args, env)
	case *core.Lambda:
		return e.applyLambda(f, args, env)
	case core.LispList:
		if len(f) > 0 && f[0] == core.LispSymbol("lambda") {
			lambda, err := evalLambda(e, f[1:], env)
			if err != nil {
				return nil, err
			}
			return e.applyLambda(lambda.(*core.Lambda), args, env)
		}
	}
	return nil, fmt.Errorf("not a function: %v", fn)
}

func (e *Evaluator) applyLambda(lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != len(lambda.Params) {
		return nil, fmt.Errorf("wrong number of arguments: expected %d, got %d", len(lambda.Params), len(args))
	}

	lambdaEnv := env.NewEnvironment(lambda.Closure)
	for i, param := range lambda.Params {
		lambdaEnv.Define(param, args[i])
	}

	return e.Eval(lambda.Body, lambdaEnv)
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

func (e *Evaluator) evalBegin(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	var result core.LispValue
	var err error

	for _, arg := range args {
		result, err = e.Eval(arg, env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

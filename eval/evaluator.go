package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/special_forms"
)

type Evaluator struct {
	specialForms map[core.LispSymbol]special_forms.SpecialFormFunc
}

func NewEvaluator() core.Evaluator {
	return &Evaluator{
		specialForms: special_forms.GetSpecialForms(),
	}
}

func (e *Evaluator) Eval(expr core.LispValue, env core.Environment) (core.LispValue, error) {
	switch v := expr.(type) {
	case core.LispSymbol:
		return evalSymbol(v, env)
	case float64, int, string, bool, core.Nil:
		return v, nil
	case core.LispList:
		return e.evalList(v, env)
	default:
		return nil, fmt.Errorf("unknown expression type: %T", expr)
	}
}

func evalSymbol(symbol core.LispSymbol, env core.Environment) (core.LispValue, error) {
	if symbol == "nil" {
		return core.Nil{}, nil
	}
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
		if symbol, ok := arg.(core.LispSymbol); ok {
			if len(symbol) > 1 && symbol[0] == ':' {
				// This is a keyword argument, don't evaluate it
				evaluated[i] = arg
			} else if symbol == "nil" {
				evaluated[i] = core.Nil{}
			} else {
				value, err := e.Eval(arg, env)
				if err != nil {
					return nil, err
				}
				evaluated[i] = value
			}
		} else {
			value, err := e.Eval(arg, env)
			if err != nil {
				return nil, err
			}
			evaluated[i] = value
		}
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
			lambda, err := special_forms.EvalLambda(e, f[1:], env)
			if err != nil {
				return nil, err
			}
			return e.applyLambda(lambda.(*core.Lambda), args, env)
		}
	}
	return nil, fmt.Errorf("not a function: %v", fn)
}

func (e *Evaluator) evalQuasiquote(expr core.LispValue, env core.Environment, depth int) (core.LispValue, error) {
	return special_forms.EvalQuasiquote(e, expr, env, depth)
}

func (e *Evaluator) evalQuasiquoteList(list core.LispList, env core.Environment, depth int) (core.LispValue, error) {
	return special_forms.EvalQuasiquoteList(e, list, env, depth)
}

func (e *Evaluator) evalBegin(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return special_forms.EvalProgn(e, args, env)
}

func (e *Evaluator) applyLambda(lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return special_forms.ApplyLambda(e, lambda, args, env)
}

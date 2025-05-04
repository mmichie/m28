package eval

import (
	"fmt"
	"strings"

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
		value, ok := env.Get(v)
		if !ok {
			return nil, fmt.Errorf("undefined symbol: %s", v)
		}
		return value, nil
	case float64, int, string, core.PythonicBool, core.PythonicNone:
		return v, nil
	case core.LispList:
		if len(v) == 0 {
			return core.LispList{}, nil
		}

		first := v[0]
		rest := v[1:]

		switch f := first.(type) {
		case core.LispSymbol:
			if f == core.LispSymbol("=") {
				// Special handling for assignment
				if len(rest) != 2 {
					return nil, fmt.Errorf("= requires exactly two arguments")
				}
				symbol, ok := rest[0].(core.LispSymbol)
				if !ok {
					return nil, fmt.Errorf("first argument to = must be a symbol")
				}
				value, err := e.Eval(rest[1], env)
				if err != nil {
					return nil, err
				}
				env.Define(symbol, value)
				return value, nil
			}
			if specialForm, ok := e.specialForms[f]; ok {
				return specialForm(e, rest, env)
			}
			fn, err := e.Eval(f, env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating symbol %s: %v", f, err)
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
	default:
		return nil, fmt.Errorf("unknown expression type: %T", expr)
	}
}

func (e *Evaluator) evalArgs(args []core.LispValue, env core.Environment) ([]core.LispValue, error) {
	evaluated := make([]core.LispValue, len(args))
	for i, arg := range args {
		// Check if arg is a symbol that represents a keyword argument
		if symbol, ok := arg.(core.LispSymbol); ok {
			symbolStr := string(symbol)
			if len(symbolStr) > 0 && strings.Contains(symbolStr, "=") {
				// This is a keyword argument in the form name=value
				evaluated[i] = symbolStr // Pass it as a string for the lambda to process
				continue
			} else if len(symbol) > 1 && symbol[0] == ':' {
				// This is a keyword argument, don't evaluate it
				evaluated[i] = arg
				continue
			}
		}

		// Otherwise evaluate normally
		value, err := e.Eval(arg, env)
		if err != nil {
			return nil, err
		}

		// Check if the evaluated value contains a keyword format
		if str, ok := value.(string); ok && strings.Contains(str, "=") {
			// Preserve the string format for keyword detection in ApplyLambda
			evaluated[i] = str
		} else {
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
		return special_forms.ApplyLambda(e, f, args, env)
	case core.LispList:
		if len(f) > 0 && f[0] == core.LispSymbol("lambda") {
			lambda, err := special_forms.EvalLambdaPython(e, f[1:], env)
			if err != nil {
				return nil, err
			}
			return special_forms.ApplyLambda(e, lambda.(*core.Lambda), args, env)
		}
	}
	return nil, fmt.Errorf("not a function: %v", fn)
}

func evalSymbol(symbol core.LispSymbol, env core.Environment) (core.LispValue, error) {
	switch symbol {
	case "None":
		return core.PythonicNone{}, nil
	case "True":
		return core.PythonicBool(true), nil
	case "False":
		return core.PythonicBool(false), nil
	default:
		value, ok := env.Get(symbol)
		if !ok {
			return nil, fmt.Errorf("undefined symbol: %s", symbol)
		}
		return value, nil
	}
}

func (e *Evaluator) evalList(list core.LispList, env core.Environment) (core.LispValue, error) {
	if len(list) == 0 {
		return core.LispList{}, nil
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

func (e *Evaluator) evalDict(dict *core.PythonicDict, env core.Environment) (core.LispValue, error) {
	result := core.NewPythonicDict()

	// Iterate over the original dict and evaluate each key-value pair
	keyFunc := func(key, value core.LispValue) error {
		evaluatedKey, err := e.Eval(key, env)
		if err != nil {
			return err
		}
		evaluatedValue, err := e.Eval(value, env)
		if err != nil {
			return err
		}
		result.Set(evaluatedKey, evaluatedValue)
		return nil
	}

	err := dict.Iterate(keyFunc)
	if err != nil {
		return nil, err
	}

	return result, nil
}

func (e *Evaluator) evalSet(set *core.PythonicSet, env core.Environment) (core.LispValue, error) {
	newSet := core.NewPythonicSet()
	for v := range set.Data() {
		evalValue, err := e.Eval(v, env)
		if err != nil {
			return nil, err
		}
		newSet.Add(evalValue)
	}
	return newSet, nil
}

func (e *Evaluator) applyLambda(lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return special_forms.ApplyLambda(e, lambda, args, env)
}

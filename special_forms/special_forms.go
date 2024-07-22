package special_forms

import (
	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(core.Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

func GetSpecialForms() map[core.LispSymbol]SpecialFormFunc {
	return map[core.LispSymbol]SpecialFormFunc{
		"quote":     EvalQuote,
		"backquote": EvalBackquote,
		"if":        EvalIf,
		"cond":      EvalCond,
		"case":      EvalCase,
		"when":      EvalWhen,
		"unless":    EvalUnless,
		"defun":     EvalDefun,
		"defvar":    EvalDefvar,
		"lambda":    EvalLambda,
		"let":       EvalLet,
		"setq":      EvalSetq,
		"progn":     EvalProgn,
		"do":        EvalDo,
		"and":       EvalAnd,
		"or":        EvalOr,
		"defmacro":  EvalDefmacro,
		"all":       EvalAll,
	}
}

func EvalAnd(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		result, err := e.Eval(arg, env)
		if err != nil {
			return nil, err
		}
		if !core.IsTruthy(result) {
			return false, nil
		}
	}
	if len(args) == 0 {
		return true, nil
	}
	return args[len(args)-1], nil
}

func EvalOr(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		result, err := e.Eval(arg, env)
		if err != nil {
			return nil, err
		}
		if core.IsTruthy(result) {
			return result, nil
		}
	}
	return false, nil
}

// TODO implement this
func EvalDo(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, nil
}

package special_forms

import (
	"fmt"

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
		"let*":      EvalLetStar,
		"setq":      EvalSetq,
		"progn":     EvalProgn,
		"do":        EvalDo,
		"and":       EvalAnd,
		"or":        EvalOr,
		"defmacro":  EvalDefmacro,
		"all":       EvalAll,
		"dolist":    EvalDolist,
		"loop":      EvalLoop,
		"dotimes":   EvalDotimes,
		"setf":      EvalSetf,
		"while":     EvalWhile,
	}
}

func setfCar(e core.Evaluator, args []core.LispValue, value core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("setf (car ...) requires exactly 1 argument")
	}

	list, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	l, ok := list.(core.LispList)
	if !ok || len(l) == 0 {
		return nil, fmt.Errorf("setf (car ...) requires a non-empty list")
	}

	l[0] = value
	return value, nil
}

func setfCdr(e core.Evaluator, args []core.LispValue, value core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("setf (cdr ...) requires exactly 1 argument")
	}

	list, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	l, ok := list.(core.LispList)
	if !ok || len(l) == 0 {
		return nil, fmt.Errorf("setf (cdr ...) requires a non-empty list")
	}

	newCdr, ok := value.(core.LispList)
	if !ok {
		newCdr = core.LispList{value}
	}

	l = append(l[:1], newCdr...)
	return newCdr, nil
}

func setfNth(e core.Evaluator, args []core.LispValue, value core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("setf (nth ...) requires exactly 2 arguments")
	}

	index, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	i, ok := index.(float64)
	if !ok {
		return nil, fmt.Errorf("setf (nth ...) index must be a number")
	}

	list, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	l, ok := list.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("setf (nth ...) requires a list")
	}

	if int(i) < 0 || int(i) >= len(l) {
		return nil, fmt.Errorf("setf (nth ...) index out of bounds")
	}

	l[int(i)] = value
	return value, nil
}

func setfGethash(e core.Evaluator, args []core.LispValue, value core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("setf (gethash ...) requires exactly 2 arguments")
	}

	key, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	hashTable, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	ht, ok := hashTable.(*core.LispHashTable)
	if !ok {
		return nil, fmt.Errorf("setf (gethash ...) requires a hash table")
	}

	ht.Set(key, value)
	return value, nil
}

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

func EvalDo(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("do requires at least 2 arguments")
	}

	// Parse variable specifications
	varSpecs, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("first argument to do must be a list of variable specifications")
	}

	// Create a new environment for the loop
	loopEnv := env.NewEnvironment(env)

	// Initialize variables
	for _, spec := range varSpecs {
		varSpec, ok := spec.(core.LispList)
		if !ok || len(varSpec) < 2 {
			return nil, fmt.Errorf("invalid variable specification in do")
		}
		varName, ok := varSpec[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("variable name must be a symbol")
		}
		initVal, err := e.Eval(varSpec[1], env)
		if err != nil {
			return nil, err
		}
		loopEnv.Define(varName, initVal)
	}

	// Parse end test and result forms
	endTest, ok := args[1].(core.LispList)
	if !ok || len(endTest) < 1 {
		return nil, fmt.Errorf("second argument to do must be a list with at least one element")
	}

	// Main loop
	for {
		// Check end test
		testResult, err := e.Eval(endTest[0], loopEnv)
		if err != nil {
			return nil, err
		}
		if core.IsTruthy(testResult) {
			// Evaluate and return result forms
			var result core.LispValue
			for _, form := range endTest[1:] {
				result, err = e.Eval(form, loopEnv)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}

		// Evaluate body forms
		for _, form := range args[2:] {
			_, err := e.Eval(form, loopEnv)
			if err != nil {
				return nil, err
			}
		}

		// Update variables
		newValues := make(map[core.LispSymbol]core.LispValue)
		for _, spec := range varSpecs {
			varSpec := spec.(core.LispList)
			varName := varSpec[0].(core.LispSymbol)
			if len(varSpec) > 2 {
				newVal, err := e.Eval(varSpec[2], loopEnv)
				if err != nil {
					return nil, err
				}
				newValues[varName] = newVal
			}
		}
		for varName, newVal := range newValues {
			loopEnv.Set(varName, newVal)
		}
	}
}

func EvalDolist(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("dolist requires at least 2 arguments")
	}

	spec, ok := args[0].(core.LispList)
	if !ok || len(spec) != 2 {
		return nil, fmt.Errorf("invalid dolist specification")
	}

	varSymbol, ok := spec[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("dolist variable must be a symbol")
	}

	listExpr, err := e.Eval(spec[1], env)
	if err != nil {
		return nil, err
	}

	list, ok := listExpr.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("dolist requires a list")
	}

	loopEnv := env.NewEnvironment(env)
	var result core.LispValue

	for _, item := range list {
		loopEnv.Set(varSymbol, item)

		for _, form := range args[1:] {
			result, err = e.Eval(form, loopEnv)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalLetStar(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("let* requires at least 2 arguments")
	}

	bindings, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("let* bindings must be a list")
	}

	letStarEnv := env.NewEnvironment(env)

	for _, binding := range bindings {
		bindingList, ok := binding.(core.LispList)
		if !ok || len(bindingList) != 2 {
			return nil, fmt.Errorf("invalid binding in let*")
		}

		symbol, ok := bindingList[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding name must be a symbol")
		}

		// Evaluate the binding value in the current letStarEnv
		value, err := e.Eval(bindingList[1], letStarEnv)
		if err != nil {
			return nil, err
		}

		// Bind the value to the symbol in the letStarEnv
		letStarEnv.Define(symbol, value)
	}

	// Evaluate the body forms in the letStarEnv
	var result core.LispValue
	var err error
	for _, form := range args[1:] {
		result, err = e.Eval(form, letStarEnv)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func EvalDotimes(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("dotimes requires at least 2 arguments")
	}

	spec, ok := args[0].(core.LispList)
	if !ok || len(spec) != 2 {
		return nil, fmt.Errorf("invalid dotimes specification")
	}

	varSymbol, ok := spec[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("dotimes variable must be a symbol")
	}

	countExpr, err := e.Eval(spec[1], env)
	if err != nil {
		return nil, err
	}

	count, ok := countExpr.(float64)
	if !ok {
		return nil, fmt.Errorf("dotimes count must evaluate to a number")
	}

	loopEnv := env.NewEnvironment(env)
	var result core.LispValue

	for i := 0; i < int(count); i++ {
		loopEnv.Set(varSymbol, float64(i))

		for _, form := range args[1:] {
			result, err = e.Eval(form, loopEnv)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalSetf(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("setf requires exactly 2 arguments")
	}

	place := args[0]
	value, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	switch p := place.(type) {
	case core.LispSymbol:
		// Simple variable assignment
		if !env.SetMutable(p, value) {
			return nil, fmt.Errorf("cannot setf undefined variable: %s", p)
		}
		return value, nil

	case core.LispList:
		// Handle structure updates
		if len(p) < 2 {
			return nil, fmt.Errorf("invalid setf place: %v", p)
		}
		accessor, ok := p[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("invalid setf accessor: %v", p[0])
		}

		switch accessor {
		case "car", "first":
			return setfCar(e, p[1:], value, env)
		case "cdr", "rest":
			return setfCdr(e, p[1:], value, env)
		case "nth":
			return setfNth(e, p[1:], value, env)
		case "gethash":
			return setfGethash(e, p[1:], value, env)
		default:
			return nil, fmt.Errorf("unsupported setf accessor: %s", accessor)
		}

	default:
		return nil, fmt.Errorf("invalid setf place: %v", place)
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

func EvalWhile(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("while requires at least 2 arguments")
	}

	condition := args[0]
	body := args[1:]

	var result core.LispValue

	for {
		condResult, err := e.Eval(condition, env)
		if err != nil {
			return nil, err
		}

		if !core.IsTruthy(condResult) {
			break
		}

		for _, expr := range body {
			result, err = e.Eval(expr, env)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

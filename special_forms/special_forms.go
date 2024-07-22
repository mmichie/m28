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
		"setq":      EvalSetq,
		"progn":     EvalProgn,
		"do":        EvalDo,
		"and":       EvalAnd,
		"or":        EvalOr,
		"defmacro":  EvalDefmacro,
		"all":       EvalAll,
		"dolist":    EvalDolist,
		"loop":      EvalLoop,
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

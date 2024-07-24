package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalLoop(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("loop requires at least one argument")
	}

	// Check if it's a simple loop (just body)
	if _, ok := args[0].(core.LispSymbol); !ok {
		return evalSimpleLoop(e, args, env)
	}

	// It's a complex loop with clauses
	return evalComplexLoop(e, args, env)
}

func evalSimpleLoop(e core.Evaluator, body []core.LispValue, env core.Environment) (core.LispValue, error) {
	loopEnv := env.NewEnvironment(env)
	var err error

	for {
		for _, expr := range body {
			_, err = e.Eval(expr, loopEnv)
			if err != nil {
				// Check if the error is a special return signal
				if returnErr, ok := err.(returnError); ok {
					return returnErr.value, nil
				}
				return nil, err
			}
		}
	}
}

func evalComplexLoop(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	loopEnv := env.NewEnvironment(env)
	var result core.LispValue
	var err error

	// Parse loop clauses
	i := 0
	for i < len(args) {
		clause, ok := args[i].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("expected loop clause, got: %v", args[i])
		}
		switch clause {
		case "for":
			i, err = handleForClause(e, args, i, loopEnv)
			if err != nil {
				return nil, err
			}
		case "do":
			i++
			for i < len(args) && args[i] != core.LispSymbol("for") {
				result, err = e.Eval(args[i], loopEnv)
				if err != nil {
					// Check if the error is a special return signal
					if returnErr, ok := err.(returnError); ok {
						return returnErr.value, nil
					}
					return nil, err
				}
				i++
			}
		default:
			return nil, fmt.Errorf("unsupported loop clause: %v", clause)
		}
	}

	return result, nil
}

func handleForClause(e core.Evaluator, args []core.LispValue, i int, env core.Environment) (int, error) {
	if i+3 >= len(args) {
		return 0, fmt.Errorf("incomplete for clause")
	}

	varSymbol, ok := args[i+1].(core.LispSymbol)
	if !ok {
		return 0, fmt.Errorf("for clause requires a symbol")
	}

	if args[i+2] != core.LispSymbol("across") {
		return 0, fmt.Errorf("only 'across' is supported in for clause")
	}

	sequence, err := e.Eval(args[i+3], env)
	if err != nil {
		return 0, err
	}

	var seqList core.LispList
	switch seq := sequence.(type) {
	case core.LispList:
		seqList = seq
	case string:
		seqList = make(core.LispList, len(seq))
		for i, ch := range seq {
			seqList[i] = string(ch)
		}
	default:
		return 0, fmt.Errorf("for across requires a list or string")
	}

	for _, item := range seqList {
		env.Set(varSymbol, item)
		// Evaluate the rest of the loop body
		for j := i + 4; j < len(args); j++ {
			if args[j] == core.LispSymbol("for") {
				break
			}
			_, err = e.Eval(args[j], env)
			if err != nil {
				// Check if the error is a special return signal
				if returnErr, ok := err.(returnError); ok {
					return 0, returnErr
				}
				return 0, err
			}
		}
	}

	return len(args), nil // Return to the end of args to finish the loop
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

// Define a custom error type for signaling a return from the loop
type returnError struct {
	value core.LispValue
}

func (e returnError) Error() string {
	return "return from loop"
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

// Add a new built-in function to allow breaking out of the loop
func init() {
	core.RegisterBuiltin("return-from-loop", returnFromLoopFunc)
}

func returnFromLoopFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("return-from-loop requires exactly one argument")
	}
	return nil, returnError{value: args[0]}
}

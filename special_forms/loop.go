package special_forms

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/core"
)

type loopState struct {
	env           core.Environment
	accumulators  map[core.LispSymbol]core.LispValue
	variables     map[core.LispSymbol]core.LispValue
	whileClause   core.LispValue
	untilClause   core.LispValue
	doClause      []core.LispValue
	finallyClause []core.LispValue
}

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
	var result core.LispValue = core.PythonicNone{}
	var err error

	for {
		for _, expr := range body {
			result, err = e.Eval(expr, loopEnv)
			if err != nil {
				// Check if the error is a break signal
				if IsBreakSignal(err) {
					// Exit the loop completely
					return result, nil
				}

				// Check if the error is a continue signal
				if IsContinueSignal(err) {
					// Skip to the next iteration of the outer loop
					break
				}

				// Check if the error is a special return signal
				if returnErr, ok := err.(returnError); ok {
					return returnErr.value, nil
				}

				// Regular error - propagate it
				return nil, err
			}
		}
	}
}

func evalComplexLoop(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	state := &loopState{
		env:          env.NewEnvironment(env),
		accumulators: make(map[core.LispSymbol]core.LispValue),
		variables:    make(map[core.LispSymbol]core.LispValue),
	}

	// Parse loop clauses
	err := parseLoopClauses(e, args, state)
	if err != nil {
		return nil, err
	}

	// Execute the loop
	var result core.LispValue = core.PythonicNone{}

	for {
		if state.whileClause != nil {
			condition, err := e.Eval(state.whileClause, state.env)
			if err != nil {
				// Handle break/continue in while condition
				if IsBreakSignal(err) {
					break
				}
				if IsContinueSignal(err) {
					continue
				}
				return nil, err
			}
			if !core.IsTruthy(condition) {
				break
			}
		}

		if state.untilClause != nil {
			condition, err := e.Eval(state.untilClause, state.env)
			if err != nil {
				// Handle break/continue in until condition
				if IsBreakSignal(err) {
					break
				}
				if IsContinueSignal(err) {
					continue
				}
				return nil, err
			}
			if core.IsTruthy(condition) {
				break
			}
		}

		for _, doExpr := range state.doClause {
			result, err = e.Eval(doExpr, state.env)
			if err != nil {
				// Handle special control flow signals
				if IsBreakSignal(err) {
					// Exit the loop completely
					goto LoopExit
				}
				if IsContinueSignal(err) {
					// Skip to the next iteration
					break
				}
				if returnErr, ok := err.(returnError); ok {
					return returnErr.value, nil
				}
				return nil, err
			}
		}

		// Update loop variables
		for varName, updateExpr := range state.variables {
			value, err := e.Eval(updateExpr, state.env)
			if err != nil {
				// Handle break/continue in variable updates
				if IsBreakSignal(err) {
					goto LoopExit
				}
				if IsContinueSignal(err) {
					continue
				}
				return nil, err
			}
			state.env.Set(varName, value)
		}
	}

LoopExit:

	// Execute finally clause
	if len(state.finallyClause) > 0 {
		var finalResult core.LispValue
		var err error
		for _, expr := range state.finallyClause {
			finalResult, err = e.Eval(expr, state.env)
			if err != nil {
				// Don't handle break/continue in finally clause -
				// they should propagate out to any enclosing loop
				return nil, err
			}
		}
		return finalResult, nil
	}

	// Return accumulated results
	if len(state.accumulators) > 0 {
		var results core.LispList
		for _, value := range state.accumulators {
			results = append(results, value)
		}
		if len(results) == 1 {
			return results[0], nil
		}
		return results, nil
	}

	return result, nil // Return the last evaluated result
}

func parseLoopClauses(e core.Evaluator, args []core.LispValue, state *loopState) error {
	i := 0
	for i < len(args) {
		clause, ok := args[i].(core.LispSymbol)
		if !ok {
			return fmt.Errorf("expected loop clause, got: %v", args[i])
		}
		i++

		switch clause {
		case "for":
			err := handleForClause(e, args, &i, state)
			if err != nil {
				return err
			}
		case "collect":
			err := handleCollectClause(e, args, &i, state)
			if err != nil {
				return err
			}
		case "sum":
			err := handleSumClause(e, args, &i, state)
			if err != nil {
				return err
			}
		case "maximize":
			err := handleMaximizeClause(e, args, &i, state)
			if err != nil {
				return err
			}
		case "minimize":
			err := handleMinimizeClause(e, args, &i, state)
			if err != nil {
				return err
			}
		case "while":
			if i >= len(args) {
				return fmt.Errorf("while clause requires a condition")
			}
			state.whileClause = args[i]
			i++
		case "until":
			if i >= len(args) {
				return fmt.Errorf("until clause requires a condition")
			}
			state.untilClause = args[i]
			i++
		case "do":
			state.doClause = append(state.doClause, args[i])
			i++
		case "finally":
			state.finallyClause = append(state.finallyClause, args[i:]...)
			i = len(args)
		default:
			return fmt.Errorf("unsupported loop clause: %v", clause)
		}
	}
	return nil
}

func handleForClause(e core.Evaluator, args []core.LispValue, i *int, state *loopState) error {
	if *i+2 >= len(args) {
		return fmt.Errorf("for clause requires at least 3 arguments")
	}

	varName, ok := args[*i].(core.LispSymbol)
	if !ok {
		return fmt.Errorf("for clause requires a symbol as variable name")
	}
	*i++

	fromExpr := args[*i]
	*i++

	var toExpr core.LispValue
	var byExpr core.LispValue = core.LispSymbol("1") // Default step is 1

	if *i < len(args) && args[*i] == core.LispSymbol("to") {
		*i++
		if *i >= len(args) {
			return fmt.Errorf("for clause 'to' requires a value")
		}
		toExpr = args[*i]
		*i++

		if *i < len(args) && args[*i] == core.LispSymbol("by") {
			*i++
			if *i >= len(args) {
				return fmt.Errorf("for clause 'by' requires a value")
			}
			byExpr = args[*i]
			*i++
		}
	}

	fromVal, err := e.Eval(fromExpr, state.env)
	if err != nil {
		return err
	}

	state.env.Set(varName, fromVal)

	updateExpr := core.LispList{core.LispSymbol("+"), varName, byExpr}
	state.variables[varName] = updateExpr

	if toExpr != nil {
		state.whileClause = core.LispList{core.LispSymbol("<="), varName, toExpr}
	}

	return nil
}

func handleCollectClause(e core.Evaluator, args []core.LispValue, i *int, state *loopState) error {
	if *i >= len(args) {
		return fmt.Errorf("collect clause requires an expression")
	}

	collectExpr := args[*i]
	*i++

	collectVar := core.LispSymbol("collect-result")
	if _, exists := state.accumulators[collectVar]; !exists {
		state.accumulators[collectVar] = core.LispList{}
	}

	state.doClause = append(state.doClause, core.LispList{
		core.LispSymbol("setq"),
		collectVar,
		core.LispList{core.LispSymbol("append"), collectVar, core.LispList{core.LispSymbol("list"), collectExpr}},
	})

	return nil
}

func handleSumClause(e core.Evaluator, args []core.LispValue, i *int, state *loopState) error {
	if *i >= len(args) {
		return fmt.Errorf("sum clause requires an expression")
	}

	sumExpr := args[*i]
	*i++

	sumVar := core.LispSymbol("sum-result")
	if _, exists := state.accumulators[sumVar]; !exists {
		state.accumulators[sumVar] = float64(0)
	}

	state.doClause = append(state.doClause, core.LispList{
		core.LispSymbol("setq"),
		sumVar,
		core.LispList{core.LispSymbol("+"), sumVar, sumExpr},
	})

	return nil
}

func handleMaximizeClause(e core.Evaluator, args []core.LispValue, i *int, state *loopState) error {
	if *i >= len(args) {
		return fmt.Errorf("maximize clause requires an expression")
	}

	maxExpr := args[*i]
	*i++

	maxVar := core.LispSymbol("max-result")
	if _, exists := state.accumulators[maxVar]; !exists {
		state.accumulators[maxVar] = float64(math.Inf(-1))
	}

	state.doClause = append(state.doClause, core.LispList{
		core.LispSymbol("setq"),
		maxVar,
		core.LispList{core.LispSymbol("max"), maxVar, maxExpr},
	})

	return nil
}

func handleMinimizeClause(e core.Evaluator, args []core.LispValue, i *int, state *loopState) error {
	if *i >= len(args) {
		return fmt.Errorf("minimize clause requires an expression")
	}

	minExpr := args[*i]
	*i++

	minVar := core.LispSymbol("min-result")
	if _, exists := state.accumulators[minVar]; !exists {
		state.accumulators[minVar] = float64(math.Inf(1))
	}

	state.doClause = append(state.doClause, core.LispList{
		core.LispSymbol("setq"),
		minVar,
		core.LispList{core.LispSymbol("min"), minVar, minExpr},
	})

	return nil
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

	var list core.LispList

	// Check if it's a regular list
	if l, ok := listExpr.(core.LispList); ok {
		list = l
	} else if l, ok := listExpr.(core.LispListLiteral); ok {
		// Convert list literal to regular list
		list = core.LispList(l)
	} else {
		// Try to handle strings by treating them as character lists
		if str, ok := listExpr.(string); ok {
			strList := make(core.LispList, len(str))
			for i, ch := range str {
				strList[i] = string(ch)
			}
			list = strList
		} else {
			return nil, fmt.Errorf("dolist requires a list, list literal, or string; got %T", listExpr)
		}
	}

	loopEnv := env.NewEnvironment(env)
	var result core.LispValue = core.PythonicNone{}

	for _, item := range list {
		loopEnv.Set(varSymbol, item)

		for _, form := range args[1:] {
			result, err = e.Eval(form, loopEnv)
			if err != nil {
				// Handle break and continue signals
				if IsBreakSignal(err) {
					// Break out of the loop completely
					return result, nil
				}
				if IsContinueSignal(err) {
					// Skip to the next iteration
					break
				}
				// Handle explicit return
				if returnErr, ok := err.(returnError); ok {
					return returnErr.value, nil
				}
				// Any other error is propagated
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
	var result core.LispValue = core.PythonicNone{}

	for i := 0; i < int(count); i++ {
		loopEnv.Set(varSymbol, float64(i))

		for _, form := range args[1:] {
			result, err = e.Eval(form, loopEnv)
			if err != nil {
				// Handle break signal
				if IsBreakSignal(err) {
					// Exit the loop completely
					return result, nil
				}

				// Handle continue signal
				if IsContinueSignal(err) {
					// Skip to the next iteration
					break
				}

				// Handle explicit return
				if returnErr, ok := err.(returnError); ok {
					return returnErr.value, nil
				}

				// Any other error is propagated
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

	var result core.LispValue = core.PythonicNone{}

	for {
		condResult, err := e.Eval(condition, env)
		if err != nil {
			// Handle break in condition evaluation
			if IsBreakSignal(err) {
				return result, nil
			}
			// Handle continue in condition evaluation
			if IsContinueSignal(err) {
				continue
			}
			return nil, err
		}

		if !core.IsTruthy(condResult) {
			break
		}

		for _, expr := range body {
			result, err = e.Eval(expr, env)
			if err != nil {
				// Handle break signal
				if IsBreakSignal(err) {
					return result, nil
				}

				// Handle continue signal
				if IsContinueSignal(err) {
					break // Break inner loop, continue outer loop
				}

				// Handle explicit return
				if returnErr, ok := err.(returnError); ok {
					return returnErr.value, nil
				}

				// Any other error is propagated
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

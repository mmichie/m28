package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalIfPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("if requires 2 or 3 arguments")
	}

	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if core.IsTruthy(condition) {
		return e.Eval(args[1], env)
	} else if len(args) == 3 {
		return e.Eval(args[2], env)
	}

	return core.PythonicNone{}, nil
}

func EvalElif(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("elif requires 2 or 3 arguments")
	}

	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if core.IsTruthy(condition) {
		return e.Eval(args[1], env)
	} else if len(args) == 3 {
		return e.Eval(args[2], env)
	}

	return core.PythonicNone{}, nil
}

func EvalElse(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("else requires exactly 1 argument")
	}

	return e.Eval(args[0], env)
}

func EvalFor(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("for loop requires at least 3 arguments")
	}

	iterVar, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("iteration variable must be a symbol")
	}

	iterable, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	// Handle both regular lists and list literals
	var iter core.LispList

	if list, ok := iterable.(core.LispList); ok {
		iter = list
	} else if list, ok := iterable.(core.LispListLiteral); ok {
		iter = core.LispList(list)
	} else {
		return nil, fmt.Errorf("for loop requires a list")
	}

	// Use the parent environment directly instead of creating a nested environment
	// This ensures variables updated in the loop affect the outer scope
	var result core.LispValue
	for _, item := range iter {
		env.Define(iterVar, item)
		for _, expr := range args[2:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				// Check if this is a return signal
				if returnSig, ok := err.(ReturnSignal); ok {
					return returnSig.Value, nil
				}
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalWhilePython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("while loop requires at least 2 arguments")
	}

	var result core.LispValue

	for {
		condition, err := e.Eval(args[0], env)
		if err != nil {
			// Check if this is a return signal
			if returnSig, ok := err.(ReturnSignal); ok {
				return returnSig.Value, nil
			}
			return nil, err
		}

		if !core.IsTruthy(condition) {
			break
		}

		for _, expr := range args[1:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				// Check if this is a return signal
				if returnSig, ok := err.(ReturnSignal); ok {
					return returnSig.Value, nil
				}
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalTry(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("try requires at least a try block and an except block")
	}

	// Extract try block, except blocks, and finally block
	tryBlock := args[0]
	exceptBlocks := []core.LispValue{}
	var finallyBlock core.LispValue

	// Parse except and finally blocks
	for _, block := range args[1:] {
		blockList, ok := block.(core.LispList)
		if !ok || len(blockList) < 1 {
			continue
		}

		blockType, ok := blockList[0].(core.LispSymbol)
		if !ok {
			continue
		}

		if string(blockType) == "finally" {
			if len(blockList) > 1 {
				finallyBlock = blockList[1]
			}
		} else {
			exceptBlocks = append(exceptBlocks, block)
		}
	}

	// Execute try block
	var tryResult core.LispValue = core.PythonicNone{}
	var tryErr error

	tryResult, tryErr = e.Eval(tryBlock, env)

	// If there's a finally block, make sure it's executed at the end
	if finallyBlock != nil {
		defer func() {
			_, finallyErr := e.Eval(finallyBlock, env)
			if finallyErr != nil && tryErr == nil {
				// If try succeeded but finally failed, propagate the finally error
				tryErr = finallyErr
			}
		}()
	}

	// If no error in try block, return its result
	if tryErr == nil {
		return tryResult, nil
	}

	// Handle the exception by matching it with except blocks
	exception, isException := tryErr.(*core.Exception)
	
	// Iterate through except blocks
	for _, exceptBlock := range exceptBlocks {
		exceptClause, ok := exceptBlock.(core.LispList)
		if !ok || len(exceptClause) < 2 {
			continue
		}

		if string(exceptClause[0].(core.LispSymbol)) != "except" {
			continue
		}

		// Handle bare except
		if len(exceptClause) == 2 {
			return e.Eval(exceptClause[1], env)
		}

		// Handle except with type
		if len(exceptClause) >= 3 {
			exceptionType, ok := exceptClause[1].(core.LispSymbol)
			if !ok {
				continue
			}

			// Check if exception type matches
			if isException && exception.Type == string(exceptionType) {
				// Simple except with type
				if len(exceptClause) == 3 {
					return e.Eval(exceptClause[2], env)
				}
				
				// Try to handle 'as' variable binding: (except ExceptionType as e ...)
				if len(exceptClause) >= 5 && string(exceptClause[2].(core.LispSymbol)) == "as" {
					varName := exceptClause[3].(core.LispSymbol)
					exceptBody := exceptClause[4]
					
					// Create a new environment with the exception bound to the variable
					exceptEnv := env.NewEnvironment(env)
					exceptEnv.Define(varName, exception)
					
					return e.Eval(exceptBody, exceptEnv)
				}
			}
		}
	}

	// No matching except found, re-raise the exception
	return nil, tryErr
}

func EvalBreak(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, fmt.Errorf("break encountered")
}

func EvalContinue(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, fmt.Errorf("continue encountered")
}

func EvalPass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, nil
}

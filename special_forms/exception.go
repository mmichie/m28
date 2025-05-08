package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Helper function to unwrap LocatedValue if present
func unwrapValue(val core.LispValue) core.LispValue {
	if located, ok := val.(core.LocatedValue); ok {
		return located.Value
	}
	return val
}

// Helper function to unwrap a list of values
func unwrapList(list core.LispList) core.LispList {
	result := make(core.LispList, len(list))
	for i, val := range list {
		result[i] = unwrapValue(val)
	}
	return result
}

// identifyBlockType examines a block and determines if it's an except or finally block
func identifyBlockType(block core.LispValue) (string, core.LispList, bool) {
	// Unwrap if it's a LocatedValue
	block = unwrapValue(block)

	// Ensure it's a list
	blockList, ok := block.(core.LispList)
	if !ok || len(blockList) < 1 {
		return "", nil, false
	}

	// Unwrap the first element (should be the block type symbol)
	blockTypeVal := unwrapValue(blockList[0])

	// Ensure first element is a symbol
	blockType, ok := blockTypeVal.(core.LispSymbol)
	if !ok {
		return "", blockList, false
	}

	return string(blockType), blockList, true
}

// EvalTry implements the 'try' special form for exception handling with
// except and finally blocks.
func EvalTry(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("try requires at least one argument")
	}

	// Extract the try body - all statements before the first except or finally block
	var tryBodyStatements []core.LispValue

	// Find indices of except/finally blocks to separate try body from handlers
	exceptStartIdx := -1
	tryBodyStatements = []core.LispValue{}

	// All expressions before the first except/finally are part of the try body
	for i := 0; i < len(args); i++ {
		blockType, _, isBlock := identifyBlockType(args[i])
		if isBlock && (blockType == "except" || blockType == "finally") {
			exceptStartIdx = i
			break
		}
		// If not a special block, it's part of the try body
		tryBodyStatements = append(tryBodyStatements, args[i])
	}

	// If we didn't find any except/finally blocks, all args are part of the try body
	if exceptStartIdx == -1 {
		exceptStartIdx = len(args)
	}

	// Find the except and finally blocks starting at exceptStartIdx
	exceptBlocks := []core.LispList{}
	var finallyBlock []core.LispValue // Store as a slice of statements

	// Look for except and finally blocks starting from exceptStartIdx
	for i := exceptStartIdx; i < len(args); i++ {
		blockType, blockContents, ok := identifyBlockType(args[i])
		if !ok {
			// If not a valid block, treat it as part of the try body
			continue
		}

		if blockType == "finally" {
			if finallyBlock != nil {
				return nil, fmt.Errorf("multiple finally blocks not allowed")
			}
			if len(blockContents) > 1 {
				// Store all statements after the "finally" symbol
				finallyBlock = blockContents[1:]
			}
		} else if blockType == "except" {
			exceptBlocks = append(exceptBlocks, blockContents)
		}
	}

	// Execute try body statements in sequence and capture any errors
	var tryResult core.LispValue = core.PythonicNone{}
	var tryError error

	for _, stmt := range tryBodyStatements {
		tryResult, tryError = e.Eval(stmt, env)
		if tryError != nil {
			break
		}
	}

	// If we have a finally block, ensure it gets executed
	if finallyBlock != nil {
		var finallyError error

		// Execute each statement in the finally block
		for _, stmt := range finallyBlock {
			_, finallyError = e.Eval(stmt, env)
			if finallyError != nil {
				break
			}
		}

		// If try succeeded but finally failed, propagate the finally error
		if tryError == nil && finallyError != nil {
			return nil, finallyError
		}

		// If both try and finally succeeded, return try's result
		if tryError == nil {
			return tryResult, nil
		}
	}

	// If no error in try block, return its result
	if tryError == nil {
		return tryResult, nil
	}

	// Convert the error to an Exception if it's not already one
	var exception *core.Exception
	if ex, ok := tryError.(*core.Exception); ok {
		exception = ex
	} else {
		// Special handling for ReturnSignal and other special error types
		if _, isReturnSignal := tryError.(ReturnSignal); isReturnSignal {
			return nil, tryError // Pass through without converting
		}

		// Otherwise convert to a RuntimeError
		exception = core.NewException("RuntimeError", tryError.Error())
	}

	// Process except blocks to find a matching handler
	for _, exceptBlock := range exceptBlocks {
		// Handle bare except: (except body)
		if len(exceptBlock) == 2 {
			exceptBody := exceptBlock[1]

			// Unwrap the except body
			exceptBody = unwrapValue(exceptBody)

			// The body might be a list of statements
			if bodyList, isList := exceptBody.(core.LispList); isList {
				var result core.LispValue = core.PythonicNone{}
				var err error

				// Evaluate each statement in the list
				for _, stmt := range bodyList {
					result, err = e.Eval(stmt, env)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}

			// If not a list, just evaluate the single statement
			return e.Eval(exceptBody, env)
		}

		// Handle typed except: (except ExceptionType body)
		if len(exceptBlock) == 3 {
			exceptionTypeVal := unwrapValue(exceptBlock[1])
			exceptionType, ok := exceptionTypeVal.(core.LispSymbol)
			if !ok {
				continue // Skip this handler if not a valid type
			}

			// Check if exception matches the type
			if exception.IsSubclassOf(string(exceptionType)) {
				exceptBody := exceptBlock[2]

				// Unwrap the except body
				exceptBody = unwrapValue(exceptBody)

				// The body might be a list of statements
				if bodyList, isList := exceptBody.(core.LispList); isList {
					var result core.LispValue = core.PythonicNone{}
					var err error

					// Evaluate each statement in the list
					for _, stmt := range bodyList {
						result, err = e.Eval(stmt, env)
						if err != nil {
							return nil, err
						}
					}
					return result, nil
				}

				// If not a list, just evaluate the single statement
				return e.Eval(exceptBody, env)
			}
		}

		// Handle except with binding: (except ExceptionType as var body)
		if len(exceptBlock) >= 5 {
			exceptionTypeVal := unwrapValue(exceptBlock[1])
			exceptionType, ok := exceptionTypeVal.(core.LispSymbol)
			if !ok {
				continue
			}

			// Check for "as" keyword
			asKeywordVal := unwrapValue(exceptBlock[2])
			asKeyword, ok := asKeywordVal.(core.LispSymbol)
			if !ok || string(asKeyword) != "as" {
				continue
			}

			// Get variable name
			varNameVal := unwrapValue(exceptBlock[3])
			varName, ok := varNameVal.(core.LispSymbol)
			if !ok {
				continue
			}

			// Check if exception matches the type
			if exception.IsSubclassOf(string(exceptionType)) {
				// Create a new environment with the exception bound to the variable
				exceptEnv := env.NewEnvironment(env)
				exceptEnv.Define(varName, exception)

				// Get the body
				exceptBody := exceptBlock[4]

				// Unwrap the except body
				exceptBody = unwrapValue(exceptBody)

				// The body might be a list of statements
				if bodyList, isList := exceptBody.(core.LispList); isList {
					var result core.LispValue = core.PythonicNone{}
					var err error

					// Evaluate each statement in the list
					for _, stmt := range bodyList {
						result, err = e.Eval(stmt, exceptEnv)
						if err != nil {
							return nil, err
						}
					}
					return result, nil
				}

				// If not a list, just evaluate the single statement
				return e.Eval(exceptBody, exceptEnv)
			}
		}
	}

	// No matching except block found, propagate the exception
	return nil, exception
}

func EvalRaise(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 3 {
		return nil, fmt.Errorf("raise requires 1-3 arguments")
	}

	// Case 1: (raise "Error message")
	if len(args) == 1 {
		message, err := e.Eval(args[0], env)
		if err != nil {
			return nil, err
		}

		// Create a generic exception with the message
		exception := core.NewException("Exception", fmt.Sprintf("%v", message))
		return nil, exception
	}

	// Unwrap exception type if it's a LocatedValue
	exTypeVal := args[0]
	if located, ok := exTypeVal.(core.LocatedValue); ok {
		exTypeVal = located.Value
	}

	// Case 2: (raise ExceptionType "Error message")
	exceptionType, ok := exTypeVal.(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to raise must be a symbol")
	}

	message, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	typeName := string(exceptionType)
	exception := core.NewException(typeName, fmt.Sprintf("%v", message))

	// Case 3: (raise ExceptionType "Error message" cause)
	// This allows for chained exceptions
	if len(args) == 3 {
		cause, err := e.Eval(args[2], env)
		if err != nil {
			return nil, err
		}

		// If cause is an exception, set it as the cause of this exception
		if causeException, ok := cause.(*core.Exception); ok {
			exception.Cause = causeException
		}
	}

	return nil, exception
}

// EvalDefException implements the 'defexception' special form to define new exception types
// Syntax: (defexception ExceptionName ParentException)
func EvalDefException(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("defexception requires 1 or 2 arguments")
	}

	// Unwrap exception name if it's a LocatedValue
	exNameVal := args[0]
	if located, ok := exNameVal.(core.LocatedValue); ok {
		exNameVal = located.Value
	}

	// Get exception name
	exceptionName, ok := exNameVal.(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("exception name must be a symbol")
	}

	// Default parent is Exception
	parentName := "Exception"

	// If parent is specified, get it
	if len(args) == 2 {
		// Unwrap parent type if it's a LocatedValue
		parentVal := args[1]
		if located, ok := parentVal.(core.LocatedValue); ok {
			parentVal = located.Value
		}

		parentType, ok := parentVal.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("parent exception name must be a symbol")
		}
		parentName = string(parentType)
	}

	// Define the new exception type
	exception := core.DefineCustomException(string(exceptionName), parentName)

	// Register the exception type in the environment
	env.Define(exceptionName, exception)

	return exception, nil
}

func EvalAssert(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("assert takes 1 or 2 arguments")
	}

	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if !core.IsTruthy(condition) {
		message := "Assertion failed"
		if len(args) == 2 {
			messageVal, err := e.Eval(args[1], env)
			if err != nil {
				return nil, err
			}
			message = fmt.Sprintf("%v", messageVal)
		}

		// Create an AssertionError exception
		exception := core.NewException("AssertionError", message)
		return nil, exception
	}

	return core.PythonicNone{}, nil
}

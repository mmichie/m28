package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

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

	// Case 2: (raise ExceptionType "Error message")
	exceptionType, ok := args[0].(core.LispSymbol)
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
	
	// Get exception name
	exceptionName, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("exception name must be a symbol")
	}
	
	// Default parent is Exception
	parentName := "Exception"
	
	// If parent is specified, use it
	if len(args) == 2 {
		parentType, ok := args[1].(core.LispSymbol)
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

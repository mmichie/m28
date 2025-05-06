package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalRaise(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("raise requires 1 or 2 arguments")
	}

	// Case 1: (raise "Error message")
	if len(args) == 1 {
		message, err := e.Eval(args[0], env)
		if err != nil {
			return nil, err
		}

		// Create a generic exception with the message
		exception := &core.Exception{
			Type:    "Exception",
			Message: fmt.Sprintf("%v", message),
		}
		return nil, exception
	}

	// Case 2: (raise ExceptionType "Error message")
	exceptionType, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to raise must be a symbol or string")
	}

	message, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	// Check if it's a standard exception type
	typeName := string(exceptionType)
	if _, ok := core.StandardExceptions[typeName]; ok {
		// Create a new exception of this type
		exception := &core.Exception{
			Type:    typeName,
			Message: fmt.Sprintf("%v", message),
		}
		return nil, exception
	}

	// If not a standard type, create a custom exception
	exception := &core.Exception{
		Type:    typeName,
		Message: fmt.Sprintf("%v", message),
	}
	return nil, exception
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
		exception := &core.Exception{
			Type:    "AssertionError",
			Message: message,
		}
		return nil, exception
	}

	return core.PythonicNone{}, nil
}

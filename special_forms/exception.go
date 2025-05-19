// Package special_forms provides special form implementations for the M28 language.
package special_forms

import (
	"fmt"
	
	"m28/core"
	"m28/eval"
)

// RegisterExceptionForms registers exception-handling special forms
func RegisterExceptionForms() {
	registerSpecialForm("try", TryForm)
	registerSpecialForm("raise", RaiseForm)
}

// TryForm implements the try-except-finally special form
// Syntax: (try body... (except [type var] handler...)... (finally cleanup...))
func TryForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("try requires at least a body")
	}
	
	// Find except and finally clauses
	bodyEnd := len(args)
	var exceptClauses []core.Value
	var finallyClause core.Value
	
	for i := 0; i < len(args); i++ {
		if list, ok := args[i].(core.ListValue); ok && len(list) > 0 {
			if sym, ok := list[0].(core.SymbolValue); ok {
				if sym == "except" {
					bodyEnd = i
					exceptClauses = append(exceptClauses, args[i])
				} else if sym == "finally" {
					if bodyEnd == len(args) {
						bodyEnd = i
					}
					finallyClause = args[i]
				}
			}
		}
	}
	
	// Execute the body in a try block
	body := args[:bodyEnd]
	var result core.Value = core.Nil
	var err error
	
	// Try to execute the body
	for _, expr := range body {
		result, err = eval.Eval(expr, ctx)
		if err != nil {
			break
		}
	}
	
	// If an error occurred, try to handle it
	if err != nil {
		// Check if it's an exception we can handle
		var exceptionHandled bool
		
		if ex, ok := err.(*core.ExceptionValue); ok {
			// Try to match with an except clause
			for _, clause := range exceptClauses {
				exceptList := clause.(core.ListValue)
				
				// Check except clause format: (except [type var] handler...)
				if len(exceptList) < 2 {
					continue
				}
				
				binding, ok := exceptList[1].(core.ListValue)
				if !ok || len(binding) != 2 {
					continue
				}
				
				// Match exception type
				exType, ok := binding[0].(core.SymbolValue)
				if !ok {
					continue
				}
				
				// Skip if the exception type doesn't match
				if string(exType) != "exception" && ex.Type().Name() != string(exType) {
					continue
				}
				
				// Get exception variable name
				exVar, ok := binding[1].(core.SymbolValue)
				if !ok {
					continue
				}
				
				// Create a new environment for the handler
				handlerEnv := core.NewContext(ctx)
				handlerEnv.Define(string(exVar), ex)
				
				// Execute the handler
				handlers := exceptList[2:]
				result = core.Nil
				
				for _, handler := range handlers {
					result, err = eval.Eval(handler, handlerEnv)
					if err != nil {
						break
					}
				}
				
				exceptionHandled = true
				break
			}
		}
		
		// If the exception wasn't handled, we'll re-throw it after finally
		if !exceptionHandled {
			// Execute finally clause if present
			if finallyClause != nil {
				finallyList := finallyClause.(core.ListValue)
				for _, cleanup := range finallyList[1:] {
					_, finallyErr := eval.Eval(cleanup, ctx)
					if finallyErr != nil {
						// If finally has an error, it takes precedence
						return nil, finallyErr
					}
				}
			}
			
			// Re-throw the original exception
			return nil, err
		}
	}
	
	// Execute finally clause if present
	if finallyClause != nil {
		finallyList := finallyClause.(core.ListValue)
		for _, cleanup := range finallyList[1:] {
			finallyResult, finallyErr := eval.Eval(cleanup, ctx)
			if finallyErr != nil {
				// If finally has an error, it takes precedence
				return nil, finallyErr
			}
			// Last value from finally becomes the result if there was no exception
			result = finallyResult
		}
	}
	
	return result, nil
}

// RaiseForm implements the raise special form
// Syntax: (raise exception)
func RaiseForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("raise requires exactly 1 argument")
	}
	
	exception, err := eval.Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}
	
	// If it's already an exception, raise it
	if ex, ok := exception.(*core.ExceptionValue); ok {
		return nil, ex
	}
	
	// Otherwise, create a new exception
	var message string
	if str, ok := exception.(core.StringValue); ok {
		message = string(str)
	} else {
		message = exception.String()
	}
	
	return nil, core.NewException(core.ExceptionType, message, exception, ctx)
}
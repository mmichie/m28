// Package eval provides control flow special forms (try, raise, import)
package eval

import (
	"errors"
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

func importForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, fmt.Errorf("import requires 1 argument")
	}

	// Get the module name
	var moduleName string
	switch name := args.Items()[0].(type) {
	case core.StringValue:
		moduleName = string(name)
	case core.SymbolValue:
		moduleName = string(name)
	default:
		return nil, fmt.Errorf("import: argument must be a string or symbol")
	}

	// Get the module loader
	loader := core.GetModuleLoader()
	if loader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	// Load the module
	module, err := loader.LoadModule(moduleName, ctx)
	if err != nil {
		// If it's already an ImportError, return it directly so it can be caught by try/except
		if _, ok := err.(*core.ImportError); ok {
			return nil, err
		}
		// Otherwise wrap with context
		return nil, fmt.Errorf("failed to import module %s: %v", moduleName, err)
	}

	// Store module in the context
	ctx.Define(moduleName, module)

	return module, nil
}

// ReturnValue represents a return value from a function

type Exception struct {
	Type    string
	Message string
	Value   core.Value
	Cause   *Exception // Explicit cause (from "raise ... from ...")
	Context *Exception // Implicit context (exception during handling)
}

func (e *Exception) Error() string {
	if e.Message != "" {
		return fmt.Sprintf("%s: %s", e.Type, e.Message)
	}
	return e.Type
}

// ErrorWithChain returns the error string with the full exception chain
func (e *Exception) ErrorWithChain() string {
	var parts []string
	parts = append(parts, e.Error())

	// Show explicit cause first
	if e.Cause != nil {
		parts = append(parts, "\nThe above exception was the direct cause of the following exception:\n")
		parts = append(parts, e.Cause.ErrorWithChain())
	} else if e.Context != nil {
		// Show implicit context if no explicit cause
		parts = append(parts, "\nDuring handling of the above exception, another exception occurred:\n")
		parts = append(parts, e.Context.ErrorWithChain())
	}

	return strings.Join(parts, "")
}

// isExceptionType checks if a string is a known exception type
func isExceptionType(name string) bool {
	knownTypes := map[string]bool{
		"Exception":         true,
		"Error":             true,
		"NameError":         true,
		"TypeError":         true,
		"ValueError":        true,
		"ZeroDivisionError": true,
		"KeyError":          true,
		"IndexError":        true,
		"AttributeError":    true,
		"RuntimeError":      true,
		"FileNotFoundError": true,
		"PermissionError":   true,
		"AssertionError":    true,
	}
	return knownTypes[name]
}

// isLikelyExceptionType returns true if the name looks like an exception type
// based on naming conventions: starts with capital letter, or underscore+capital
func isLikelyExceptionType(name string) bool {
	if len(name) == 0 {
		return false
	}
	// Starts with capital letter
	if name[0] >= 'A' && name[0] <= 'Z' {
		return true
	}
	// Starts with underscore followed by capital letter (e.g. _Stop)
	if len(name) > 1 && name[0] == '_' && name[1] >= 'A' && name[1] <= 'Z' {
		return true
	}
	return false
}

// errorToExceptionInstance converts any error into a Python exception instance
// This handles both custom Exception types and Go error types (OSError, TypeError, etc.)
func errorToExceptionInstance(err error, ctx *core.Context) core.Value {
	if err == nil {
		return core.Nil
	}

	// Check if it's already a custom Exception with a Value
	if exc, ok := err.(*Exception); ok {
		if exc.Value != nil {
			return exc.Value
		}
		// Create instance from Exception type
		// Use exc.Message directly instead of exc.Error() to avoid duplicating the type name
		// exc.Error() returns "Type: Message", but we just want "Message" here
		message := exc.Message
		if message == "" {
			message = exc.Type
		}
		return createPythonExceptionInstance(ctx, exc.Type, message)
	}

	// Unwrap EvalError if needed
	var baseErr error = err
	var errMsg string

	if evalErr, ok := err.(*core.EvalError); ok && evalErr.Wrapped != nil {
		baseErr = evalErr.Wrapped
		errMsg = evalErr.Wrapped.Error()
	} else if evalErr, ok := err.(*core.EvalError); ok {
		errMsg = evalErr.Message
	} else {
		errMsg = err.Error()
	}

	// Also unwrap any standard wrapped errors (fmt.Errorf with %w)
	for {
		unwrapped := errors.Unwrap(baseErr)
		if unwrapped == nil {
			break
		}
		baseErr = unwrapped
		// Keep the original error message for display
	}

	// Check if error message starts with a Python exception type name
	// Format: "ExceptionType: message"
	if idx := strings.Index(errMsg, ": "); idx > 0 {
		possibleType := errMsg[:idx]
		// List of known exception types that might be in error messages
		knownTypes := []string{
			"SyntaxError", "TypeError", "ValueError", "NameError",
			"AttributeError", "KeyError", "IndexError", "ZeroDivisionError",
			"ImportError", "ModuleNotFoundError", "OSError", "FileNotFoundError",
			"RuntimeError", "NotImplementedError", "StopIteration", "AssertionError",
		}
		for _, knownType := range knownTypes {
			if possibleType == knownType {
				// Extract message part (after ": "), keeping any suggestions
				msgOnly := errMsg[idx+2:]
				// For NameError and AttributeError, check if baseErr has a suggestion
				// and append it if not already in the message
				if knownType == "NameError" || knownType == "AttributeError" {
					var suggestion string
					if nameErr, ok := baseErr.(*core.NameError); ok {
						suggestion = nameErr.Suggestion
					} else if attrErr, ok := baseErr.(*core.AttributeError); ok {
						suggestion = attrErr.Suggestion
					}
					if suggestion != "" && !strings.Contains(msgOnly, suggestion) {
						msgOnly = fmt.Sprintf("%s. %s", msgOnly, suggestion)
					}
				}
				return createPythonExceptionInstance(ctx, knownType, msgOnly)
			}
		}
	}

	// Map Go error types to Python exception classes
	switch baseErr.(type) {
	case *core.StopIteration:
		// StopIteration from generators
		return createPythonExceptionInstance(ctx, "StopIteration", errMsg)
	case *protocols.StopIteration:
		// StopIteration from iterators
		return createPythonExceptionInstance(ctx, "StopIteration", errMsg)
	case *core.FileNotFoundError:
		return createPythonExceptionInstance(ctx, "FileNotFoundError", errMsg)
	case *core.OSError:
		return createPythonExceptionInstance(ctx, "OSError", errMsg)
	case *core.NameError:
		// Include suggestion in error message if present
		msg := errMsg
		if nameErr, ok := err.(*core.NameError); ok && nameErr.Suggestion != "" {
			msg = fmt.Sprintf("%s. %s", msg, nameErr.Suggestion)
		}
		return createPythonExceptionInstance(ctx, "NameError", msg)
	case *core.TypeError:
		return createPythonExceptionInstance(ctx, "TypeError", errMsg)
	case *core.ZeroDivisionError:
		return createPythonExceptionInstance(ctx, "ZeroDivisionError", errMsg)
	case *core.KeyError:
		return createPythonExceptionInstance(ctx, "KeyError", errMsg)
	case *core.IndexError:
		return createPythonExceptionInstance(ctx, "IndexError", errMsg)
	case *core.ModuleNotFoundError:
		// Must check ModuleNotFoundError before ImportError since it embeds ImportError
		return createPythonExceptionInstance(ctx, "ModuleNotFoundError", errMsg)
	case *core.ImportError:
		return createPythonExceptionInstance(ctx, "ImportError", errMsg)
	case *core.AttributeError:
		// Include suggestion in error message if present
		msg := errMsg
		if attrErr, ok := err.(*core.AttributeError); ok && attrErr.Suggestion != "" {
			msg = fmt.Sprintf("%s. %s", msg, attrErr.Suggestion)
		}
		return createPythonExceptionInstance(ctx, "AttributeError", msg)
	case *core.ValueError:
		return createPythonExceptionInstance(ctx, "ValueError", errMsg)
	case *core.AssertionError:
		return createPythonExceptionInstance(ctx, "AssertionError", errMsg)
	case *core.SystemExit:
		// SystemExit should be converted to a Python SystemExit instance
		// Extract the exit code if available
		sysExit := baseErr.(*core.SystemExit)
		return createPythonExceptionInstance(ctx, "SystemExit", fmt.Sprintf("%d", sysExit.Code))
	default:
		// Generic exception for unknown error types
		return createPythonExceptionInstance(ctx, "Exception", errMsg)
	}
}

// tryForm implements the try/except/finally special form
// createPythonExceptionInstance creates a proper Python exception instance
// instead of a string, so that caught exceptions have the correct type and attributes
func createPythonExceptionInstance(ctx *core.Context, exceptionType string, message string) core.Value {
	// Try to get the exception class from context
	exceptionClass, err := ctx.Lookup(exceptionType)
	if err != nil {
		// Exception class not found - create a minimal exception instance manually
		// We can't use the class system, so create a basic Instance with required attributes
		inst := &core.Instance{
			Class: &core.Class{Name: exceptionType},
			Attributes: map[string]core.Value{
				"args": core.TupleValue{core.StringValue(message)},
			},
		}
		return inst
	}

	// Check if it's a class
	class, ok := exceptionClass.(*core.Class)
	if !ok {
		// Not a class - create a minimal exception instance manually
		inst := &core.Instance{
			Class: &core.Class{Name: exceptionType},
			Attributes: map[string]core.Value{
				"args": core.TupleValue{core.StringValue(message)},
			},
		}
		return inst
	}

	// Instantiate the exception class with the message
	instance, err := class.Call([]core.Value{core.StringValue(message)}, ctx)
	if err != nil {
		// Instantiation failed - log the error and create a minimal instance manually
		// This preserves the original error message instead of losing it
		core.Log.Debug(core.SubsystemEval, "Exception instantiation failed, creating minimal instance",
			"exception_type", exceptionType,
			"message", message,
			"instantiation_error", err.Error())

		// Create a minimal exception instance that preserves the message
		inst := &core.Instance{
			Class: class,
			Attributes: map[string]core.Value{
				"args": core.TupleValue{core.StringValue(message)},
			},
		}
		return inst
	}

	return instance
}

// Forms:
//
//	(try body)
//	(try body (except handler)) ; catch all
//	(try body (except var handler)) ; catch all with variable
//	(try body (except Type handler)) ; catch specific type
//	(try body (except Type var handler)) ; catch specific type with variable
//	(try body ... (finally cleanup))
func tryForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return nil, fmt.Errorf("try requires at least a body")
	}

	var tryBody []core.Value
	var exceptClauses []*core.ListValue
	var elseClause *core.ListValue
	var finallyClause *core.ListValue

	// Parse the try form
	for i, arg := range args.Items() {
		unwrappedArg := unwrapLocated(arg)
		if list, ok := unwrappedArg.(*core.ListValue); ok && list.Len() > 0 {
			firstElem := unwrapLocated(list.Items()[0])
			if sym, ok := firstElem.(core.SymbolValue); ok {
				switch string(sym) {
				case "except":
					if i == 0 {
						return nil, fmt.Errorf("try must have a body before except")
					}
					exceptClauses = append(exceptClauses, list)
					continue
				case "else":
					if i == 0 {
						return nil, fmt.Errorf("try must have a body before else")
					}
					elseClause = list
					continue
				case "finally":
					if i == 0 {
						return nil, fmt.Errorf("try must have a body before finally")
					}
					finallyClause = list
					continue
				}
			}
		}

		// If we haven't seen except, else, or finally yet, it's part of the try body
		if len(exceptClauses) == 0 && elseClause == nil && finallyClause == nil {
			tryBody = append(tryBody, arg)
		}
	}

	// Helper to run finally clause
	// Returns (finallyResult, finallyErr)
	// If finally block contains return/break/continue, finallyResult will be non-nil
	runFinally := func() (core.Value, error) {
		if finallyClause != nil && finallyClause.Len() > 1 {
			var finallyResult core.Value
			for _, expr := range finallyClause.Items()[1:] {
				var err error
				finallyResult, err = Eval(expr, ctx)
				if err != nil {
					return nil, err
				}
				// If finally block has a return/break/continue, stop executing
				if _, ok := finallyResult.(*ReturnValue); ok {
					return finallyResult, nil
				}
				if _, ok := finallyResult.(*BreakValue); ok {
					return finallyResult, nil
				}
				if _, ok := finallyResult.(*ContinueValue); ok {
					return finallyResult, nil
				}
			}
			return finallyResult, nil
		}
		return nil, nil
	}

	// Execute try body
	var result core.Value = core.Nil
	var tryErr error

	for _, expr := range tryBody {
		result, tryErr = Eval(expr, ctx)
		if tryErr != nil {
			break
		}
	}

	// If no error, run else clause (if present), then finally, and return
	if tryErr == nil {
		// Execute else clause if present (only runs when no exception occurred)
		if elseClause != nil && elseClause.Len() > 1 {
			for _, expr := range elseClause.Items()[1:] {
				var elseErr error
				result, elseErr = Eval(expr, ctx)
				if elseErr != nil {
					// Error in else clause becomes the new error
					finallyResult, finallyErr := runFinally()
					if finallyErr != nil {
						return nil, finallyErr
					}
					// If finally has return/break/continue, it overrides the error
					if finallyResult != nil {
						if _, ok := finallyResult.(*ReturnValue); ok {
							return finallyResult, nil
						}
						if _, ok := finallyResult.(*BreakValue); ok {
							return finallyResult, nil
						}
						if _, ok := finallyResult.(*ContinueValue); ok {
							return finallyResult, nil
						}
					}
					return nil, elseErr
				}
			}
		}

		finallyResult, finallyErr := runFinally()
		if finallyErr != nil {
			return nil, finallyErr
		}
		// If finally has return/break/continue, it overrides the try block result
		if finallyResult != nil {
			if _, ok := finallyResult.(*ReturnValue); ok {
				return finallyResult, nil
			}
			if _, ok := finallyResult.(*BreakValue); ok {
				return finallyResult, nil
			}
			if _, ok := finallyResult.(*ContinueValue); ok {
				return finallyResult, nil
			}
		}
		return result, nil
	}

	// Handle the exception
	for _, exceptClause := range exceptClauses {
		if exceptClause.Len() < 2 {
			continue
		}

		// Parse except clause
		// Forms:
		// (except handler...) - catch all
		// (except var handler...) - catch all with variable binding (legacy)
		// (except Type handler...) - catch specific type
		// (except Type var handler...) - catch specific type with variable (legacy)
		// (except Type as var handler...) - catch specific type with variable (Python style)
		// (except as var handler...) - catch all with variable (Python style)
		var excType string
		var excTypes []string // For tuple exception types like (ValueError, TypeError)
		var excVar string
		var handlerStart int = 1

		if exceptClause.Len() > 1 {
			// Use smart accessor to get unwrapped second element
			secondElem, _ := exceptClause.GetItemUnwrapped(1)

			// Check if it's a tuple of exception types
			if tupleList, ok := secondElem.(*core.ListValue); ok && tupleList.Len() > 0 {
				// Check for tuple-literal marker using smart accessor
				if sym, ok := tupleList.GetItemAsSymbol(0); ok && string(sym) == "tuple-literal" {
					// It's a tuple like (tuple-literal ValueError TypeError)
					for i := 1; i < tupleList.Len(); i++ {
						if typeSym, ok := tupleList.GetItemAsSymbol(i); ok {
							excTypes = append(excTypes, string(typeSym))
						}
					}
					handlerStart = 2

					// Check for "as" syntax
					if exceptClause.Len() > 3 {
						if asSym, ok := exceptClause.GetItemAsSymbol(2); ok && string(asSym) == "as" {
							if varSym, ok := exceptClause.GetItemAsSymbol(3); ok {
								excVar = string(varSym)
								handlerStart = 4
							}
						}
					}
				}
			} else if sym, ok := secondElem.(core.SymbolValue); ok {
				// Check if second element is a symbol
				symStr := string(sym)

				// Check for "as" syntax for catch-all
				if symStr == "as" && exceptClause.Len() > 2 {
					// (except as var handler...)
					if varSym, ok := exceptClause.GetItemAsSymbol(2); ok {
						excVar = string(varSym)
						handlerStart = 3
					}
				} else if isExceptionType(symStr) || isLikelyExceptionType(symStr) {
					// It's an exception type
					excType = symStr
					handlerStart = 2

					// Check for "as" syntax
					if exceptClause.Len() > 3 && handlerStart == 2 {
						if asSym, ok := exceptClause.GetItemAsSymbol(2); ok && string(asSym) == "as" {
							// (except Type as var handler...)
							if varSym, ok := exceptClause.GetItemAsSymbol(3); ok {
								excVar = string(varSym)
								handlerStart = 4
							}
						}
					}

					// Legacy: Check if next element is a variable name (lowercase)
					if excVar == "" && exceptClause.Len() > 2 {
						if varSym, ok := exceptClause.GetItemAsSymbol(2); ok {
							varStr := string(varSym)
							if len(varStr) > 0 && varStr[0] >= 'a' && varStr[0] <= 'z' {
								excVar = varStr
								handlerStart = 3
							}
						}
					}
				} else {
					// Legacy: It's a variable name for catch-all
					excVar = symStr
					handlerStart = 2
				}
			}
		}

		// Check if this except matches
		matches := false
		if excType == "" && len(excTypes) == 0 {
			// Catch-all
			matches = true
		} else if len(excTypes) > 0 {
			// Tuple of exception types - match if ANY match
			excInstance := errorToExceptionInstance(tryErr, ctx)

			for _, exType := range excTypes {
				// Look up the target exception class
				targetClassVal, err := ctx.Lookup(exType)
				if err != nil {
					// Exception class not found - only match "Exception" or "Error"
					if exType == "Exception" || exType == "Error" {
						matches = true
						break
					}
				} else if targetClass, ok := targetClassVal.(*core.Class); ok {
					// Use isinstance semantics with proper inheritance
					if inst, ok := excInstance.(*core.Instance); ok {
						if core.IsInstanceOf(inst, targetClass) {
							matches = true
							break
						}
					} else if exType == "Exception" {
						// Not an instance - only match "Exception"
						matches = true
						break
					}
				} else {
					// Target is not a class - fallback to string matching
					if exc, ok := tryErr.(*Exception); ok {
						if exc.Type == exType || exType == "Exception" {
							matches = true
							break
						}
					} else if exType == "Exception" || exType == "Error" {
						matches = true
						break
					}
				}
			}
		} else {
			// Single exception type
			// Convert error to Python exception instance
			excInstance := errorToExceptionInstance(tryErr, ctx)

			// Look up the target exception class
			targetClassVal, err := ctx.Lookup(excType)
			if err != nil {
				// Exception class not found - only match "Exception" or "Error"
				matches = excType == "Exception" || excType == "Error"
			} else if targetClass, ok := targetClassVal.(*core.Class); ok {
				// Use isinstance semantics with proper inheritance
				if inst, ok := excInstance.(*core.Instance); ok {
					matches = core.IsInstanceOf(inst, targetClass)
				} else {
					// Not an instance - only match "Exception"
					matches = excType == "Exception"
				}
			} else {
				// Target is not a class - fallback to string matching
				if exc, ok := tryErr.(*Exception); ok {
					matches = exc.Type == excType || excType == "Exception"
				} else {
					matches = excType == "Exception" || excType == "Error"
				}
			}
		}

		if matches {
			// Use parent context for handler to preserve variable assignments
			handlerCtx := ctx

			// Convert error to exception instance
			excValue := errorToExceptionInstance(tryErr, ctx)

			// Bind exception variable if specified (in parent context to preserve assignments)
			if excVar != "" {
				ctx.Define(excVar, excValue)
			}

			// Store exception info in context for sys.exc_info()
			// Get the exception class
			var excTypeVal core.Value = core.None
			if inst, ok := excValue.(*core.Instance); ok {
				if classVal, hasClass := inst.GetAttr("__class__"); hasClass {
					excTypeVal = classVal
				}
			}

			// Save previous exception info to restore later
			prevExcType := handlerCtx.ExcType
			prevExcValue := handlerCtx.ExcValue
			prevExcTb := handlerCtx.ExcTb
			prevExcError, _ := handlerCtx.Lookup("__current_exception__")

			// Set current exception info
			handlerCtx.ExcType = excTypeVal
			handlerCtx.ExcValue = excValue
			handlerCtx.ExcTb = core.None // Traceback object not yet implemented
			// Store the exception instance for bare raise
			handlerCtx.Define("__current_exception__", excValue)

			// Execute handler
			handlerErr := error(nil)
			for i := handlerStart; i < exceptClause.Len(); i++ {
				result, handlerErr = Eval(exceptClause.Items()[i], handlerCtx)
				if handlerErr != nil {
					// Exception occurred in handler - this becomes the new error
					tryErr = handlerErr
					break
				}
			}

			// Restore previous exception info
			handlerCtx.ExcType = prevExcType
			handlerCtx.ExcValue = prevExcValue
			handlerCtx.ExcTb = prevExcTb
			// Restore previous exception error
			if prevExcError != nil {
				handlerCtx.Define("__current_exception__", prevExcError)
			}

			// Delete exception variable after handler (Python PEP 3110 semantics)
			// This prevents reference cycles and matches CPython behavior
			if excVar != "" {
				ctx.Delete(excVar)
			}

			// If handler completed without error, the exception is handled
			if handlerErr == nil {
				tryErr = nil // Clear the original exception
			}
			break
		}
	}

	// Run finally clause
	finallyResult, finallyErr := runFinally()
	if finallyErr != nil {
		return nil, finallyErr
	}

	// If finally has return/break/continue, it overrides everything
	if finallyResult != nil {
		if _, ok := finallyResult.(*ReturnValue); ok {
			return finallyResult, nil
		}
		if _, ok := finallyResult.(*BreakValue); ok {
			return finallyResult, nil
		}
		if _, ok := finallyResult.(*ContinueValue); ok {
			return finallyResult, nil
		}
	}

	// If exception wasn't handled or a new exception occurred, raise it
	if tryErr != nil {
		return nil, tryErr
	}

	return result, nil
}

// raiseForm implements the raise special form
// Forms:
//
//	(raise "message")
//	(raise ExceptionType)
//	(raise ExceptionType "message")
//	(raise (ExceptionType args...))
//	(raise) - re-raise current exception
func raiseForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Bare raise - re-raise current exception
	if args.Len() == 0 {
		// Try to get the current exception from context
		if excVal, err := ctx.Lookup("__current_exception__"); err == nil {
			// Convert exception instance back to error
			if inst, ok := excVal.(*core.Instance); ok {
				// Get exception type name
				var excType string
				if classVal, hasClass := inst.GetAttr("__class__"); hasClass {
					if class, ok := classVal.(*core.Class); ok {
						excType = class.Name
					}
				}
				// Get exception message
				var message string
				if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
					if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
						if msgStr, ok := argsTuple[0].(core.StringValue); ok {
							message = string(msgStr)
						}
					}
				}
				return nil, &Exception{Type: excType, Message: message}
			}
		}
		// No current exception to re-raise
		return nil, &Exception{Type: "RuntimeError", Message: "No active exception to re-raise"}
	}

	// Single argument
	if args.Len() == 1 {
		// Check if it's a list (exception constructor call)
		if list, ok := args.Items()[0].(*core.ListValue); ok && list.Len() > 0 {
			// Evaluate the exception constructor
			excObj, err := Eval(list, ctx)
			if err != nil {
				return nil, err
			}

			// Extract exception information from the object
			if sym, ok := list.Items()[0].(core.SymbolValue); ok {
				excType := string(sym)

				// Try to get message from the object
				var message string
				// Check if it's an exception instance
				if inst, ok := excObj.(*core.Instance); ok {
					// Try args attribute first (Python standard)
					if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
						if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
							if msgStr, ok := argsTuple[0].(core.StringValue); ok {
								message = string(msgStr)
							}
						}
					}
					// Fallback to message attribute
					if message == "" {
						if msgVal, found := inst.GetAttr("message"); found {
							message = core.PrintValueWithoutQuotes(msgVal)
						}
					}
				} else if obj, ok := excObj.(*core.DictValue); ok {
					// Fallback for dict-based exceptions
					if msgVal, exists := obj.Get("message"); exists {
						message = core.PrintValueWithoutQuotes(msgVal)
					}
				}

				return nil, &Exception{
					Type:    excType,
					Message: message,
					Value:   excObj, // Store the actual exception object
				}
			}
		}

		// Evaluate the argument
		val, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}

		// Check if it's an exception class
		if class, ok := val.(*core.Class); ok {
			// Instantiate the exception class
			instance, err := class.Call([]core.Value{}, ctx)
			if err != nil {
				return nil, err
			}

			// Add __traceback__ attribute to the instance
			if inst, ok := instance.(*core.Instance); ok {
				inst.SetAttr("__traceback__", core.NewTraceback())
			}

			// Return exception with class name as type
			return nil, &Exception{
				Type:    class.Name,
				Message: "",
				Value:   instance, // Store the instance
			}
		}

		// Check if it's an exception instance
		if inst, ok := val.(*core.Instance); ok {
			// Get message from instance's args or message attribute
			var message string
			// Try args attribute first (Python standard)
			if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
				if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
					if msgStr, ok := argsTuple[0].(core.StringValue); ok {
						message = string(msgStr)
					}
				}
			}
			// Fallback to message attribute
			if message == "" {
				if msgVal, found := inst.GetAttr("message"); found {
					message = core.PrintValueWithoutQuotes(msgVal)
				}
			}

			return nil, &Exception{
				Type:    inst.Class.Name,
				Message: message,
				Value:   inst,
			}
		}

		// Single string argument - generic exception with message
		if msg, ok := val.(core.StringValue); ok {
			return nil, &Exception{
				Type:    "Exception",
				Message: string(msg),
			}
		}

		// If it's not a string, convert to string
		return nil, &Exception{
			Type:    "Exception",
			Message: core.PrintValueWithoutQuotes(val),
		}
	}

	// Two arguments - type and message
	if args.Len() == 2 {
		var excType string
		var excMsg string

		// Get exception type
		switch t := args.Items()[0].(type) {
		case core.SymbolValue:
			excType = string(t)
		case core.StringValue:
			excType = string(t)
		default:
			return nil, fmt.Errorf("raise: exception type must be a symbol or string")
		}

		// Evaluate and get message
		msgVal, err := Eval(args.Items()[1], ctx)
		if err != nil {
			return nil, err
		}

		if msg, ok := msgVal.(core.StringValue); ok {
			excMsg = string(msg)
		} else {
			excMsg = core.PrintValueWithoutQuotes(msgVal)
		}

		return nil, &Exception{
			Type:    excType,
			Message: excMsg,
		}
	}

	// Three or more arguments - check for "raise X from Y"
	if args.Len() >= 3 {
		// Check if second argument is the "from" keyword
		if fromSym, ok := args.Items()[1].(core.SymbolValue); ok && string(fromSym) == "from" {
			// Evaluate the exception
			excVal, err := Eval(args.Items()[0], ctx)
			if err != nil {
				return nil, err
			}

			// Evaluate the cause
			causeVal, err := Eval(args.Items()[2], ctx)
			if err != nil {
				return nil, err
			}

			// Create the main exception
			mainExc := &Exception{
				Type:    "Exception",
				Message: "",
			}

			// Extract exception info from excVal
			if inst, ok := excVal.(*core.Instance); ok {
				mainExc.Type = inst.Class.Name
				// Try to get message
				if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
					if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
						if msgStr, ok := argsTuple[0].(core.StringValue); ok {
							mainExc.Message = string(msgStr)
						}
					}
				}
				if mainExc.Message == "" {
					if msgVal, found := inst.GetAttr("message"); found {
						mainExc.Message = core.PrintValueWithoutQuotes(msgVal)
					}
				}
				mainExc.Value = inst
			} else if class, ok := excVal.(*core.Class); ok {
				// Exception class - instantiate it
				instance, err := class.Call([]core.Value{}, ctx)
				if err != nil {
					return nil, err
				}
				mainExc.Type = class.Name
				mainExc.Value = instance
			} else if msg, ok := excVal.(core.StringValue); ok {
				mainExc.Message = string(msg)
			}

			// Extract cause info
			cause := &Exception{
				Type:    "Exception",
				Message: "",
			}
			if inst, ok := causeVal.(*core.Instance); ok {
				cause.Type = inst.Class.Name
				// Try to get message
				if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
					if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
						if msgStr, ok := argsTuple[0].(core.StringValue); ok {
							cause.Message = string(msgStr)
						}
					}
				}
				if cause.Message == "" {
					if msgVal, found := inst.GetAttr("message"); found {
						cause.Message = core.PrintValueWithoutQuotes(msgVal)
					}
				}
				cause.Value = inst
			} else if class, ok := causeVal.(*core.Class); ok {
				// Exception class - instantiate it
				instance, err := class.Call([]core.Value{}, ctx)
				if err != nil {
					return nil, err
				}
				cause.Type = class.Name
				cause.Value = instance
			} else if msg, ok := causeVal.(core.StringValue); ok {
				cause.Message = string(msg)
			}

			// Set __cause__ attribute on main exception's value if it's an instance
			if mainInst, ok := mainExc.Value.(*core.Instance); ok {
				if causeInst, ok := cause.Value.(*core.Instance); ok {
					mainInst.SetAttr("__cause__", causeInst)
				}
			}

			// Link the cause
			mainExc.Cause = cause

			return nil, mainExc
		}
	}

	return nil, fmt.Errorf("raise: too many arguments")
}

// convertIterableToSlice converts an iterable value to a slice of values
// This supports lists, tuples, strings, and any type implementing the Iterable interface

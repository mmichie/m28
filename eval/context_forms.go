package eval

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
)

// withForm implements the with statement
// (with expr body...)              - simple form
// (with expr as var body...)       - with variable binding
// (with [expr1 as var1 expr2 as var2] body...) - multiple context managers
func withForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("with requires at least 2 arguments")
	}

	// Debug: print all args
	// for i, arg := range args.Items() {
	// 	fmt.Printf("  arg[%d]: %v (type: %T)\n", i, arg, arg)
	// }

	// Parse context managers
	var managers []withManager

	// Check if first arg is a list (multiple managers)
	// We need to distinguish between:
	// 1. (with [mgr1 as var1 mgr2 as var2] ...) - multiple managers
	// 2. (with (open file mode) as var ...) - single manager that's a function call
	if list, ok := args.Items()[0].(*core.ListValue); ok {
		// Check if this looks like a multiple manager list by looking for 'as' keywords
		// or if it's too short to be a manager list
		if list.Len() == 0 || !looksLikeManagerList(list) {
			// It's a function call or expression, treat as single manager
			mgr := withManager{expr: args.Items()[0]}
			bodyStart := 1

			// Check if args[1] is a variable name (symbol), tuple pattern (list), or None
			if args.Len() >= 3 {
				// Unwrap LocatedValue to get actual type
				target := unwrapLocated(args.Items()[1])
				if sym, ok := target.(core.SymbolValue); ok {
					symStr := string(sym)
					if symStr != "None" {
						mgr.varName = symStr
						bodyStart = 2
					} else {
						bodyStart = 2
					}
				} else if list, ok := target.(*core.ListValue); ok {
					// Tuple unpacking pattern: with ctx() as (a, b): ...
					mgr.target = list
					bodyStart = 2
				} else if target == core.None {
					bodyStart = 2
				}
			}

			managers = []withManager{mgr}

			// Get the body
			body := args.Items()[bodyStart:]

			// Execute with proper context management
			return executeWith(managers, body, ctx)
		} else {
			// Multiple managers: (with [mgr1 as var1 mgr2 as var2] ...)
			managers = parseManagerList(list)
		}
	} else {
		// Single manager: (with mgr ...) or (with mgr var body)
		// Format from ToIR is: (with context-expr variable-name body)
		// Or: (with context-expr None body) if no variable
		mgr := withManager{expr: args.Items()[0]}
		bodyStart := 1

		// Check if args[1] is a variable name (symbol), tuple pattern (list), or None
		if args.Len() >= 3 {
			// Unwrap LocatedValue to get actual type
			target := unwrapLocated(args.Items()[1])
			if sym, ok := target.(core.SymbolValue); ok {
				// Check if it's not "None" - if it's a real symbol, it's the variable name
				symStr := string(sym)
				if symStr != "None" {
					mgr.varName = symStr
					bodyStart = 2 // Skip "mgr var"
				} else {
					// It's None, no variable binding
					bodyStart = 2
				}
			} else if list, ok := target.(*core.ListValue); ok {
				// Tuple unpacking pattern: with ctx() as (a, b): ...
				mgr.target = list
				bodyStart = 2
			} else if target == core.None {
				// Explicitly None
				bodyStart = 2
			}
		}

		managers = []withManager{mgr}

		// Get the body
		body := args.Items()[bodyStart:]

		// Execute with proper context management
		return executeWith(managers, body, ctx)
	}

	// For multiple managers case, body is everything after the manager list
	body := args.Items()[1:]

	// Execute with proper context management
	return executeWith(managers, body, ctx)
}

// withManager represents a single context manager in a with statement
type withManager struct {
	expr    core.Value
	varName string     // For simple variable: as x
	target  core.Value // For tuple unpacking: as (a, b, c)
}

// looksLikeManagerList checks if a list looks like a multiple manager list
// by checking for 'as' keywords in the expected positions
func looksLikeManagerList(list *core.ListValue) bool {
	// A manager list should have 'as' keywords at positions 1, 4, 7, etc.
	// or could be just a list of expressions without 'as'
	// The heuristic: if we see an 'as' symbol in position 1 or 2, it's likely a manager list
	for i := 1; i < list.Len() && i <= 2; i++ {
		if sym, ok := list.Items()[i].(core.SymbolValue); ok && string(sym) == "as" {
			return true
		}
	}
	return false
}

// parseManagerList parses a list of managers with optional 'as' clauses
func parseManagerList(list *core.ListValue) []withManager {
	var managers []withManager

	i := 0
	for i < list.Len() {
		mgr := withManager{expr: list.Items()[i]}

		// Check for 'as var' after the expression
		if i+2 < list.Len() {
			if sym, ok := list.Items()[i+1].(core.SymbolValue); ok && string(sym) == "as" {
				if varSym, ok := list.Items()[i+2].(core.SymbolValue); ok {
					mgr.varName = string(varSym)
					i += 3
				} else {
					i++
				}
			} else {
				i++
			}
		} else {
			i++
		}

		managers = append(managers, mgr)
	}

	return managers
}

// executeWith executes a with statement with proper enter/exit handling
func executeWith(managers []withManager, body []core.Value, ctx *core.Context) (core.Value, error) {
	if len(managers) == 0 {
		// No managers, just execute body
		return executeBody(body, ctx)
	}

	// Take the first manager
	mgr := managers[0]
	rest := managers[1:]

	// Evaluate the manager expression
	mgrValue, err := Eval(mgr.expr, ctx)
	if err != nil {
		return nil, err
	}

	// Debug output

	// Check if it's a context manager
	cm, ok := core.IsContextManager(mgrValue)
	if !ok {
		return nil, fmt.Errorf("'%s' object does not support the context manager protocol", mgrValue.Type())
	}

	// Call __enter__
	enterValue, err := cm.Enter()
	if err != nil {
		return nil, err
	}

	// Bind the value if there's an 'as' clause
	if mgr.varName != "" {
		// Simple variable binding: with ctx() as x:
		ctx.Define(mgr.varName, enterValue)
	} else if mgr.target != nil {
		// Tuple unpacking: with ctx() as (a, b, c):
		if err := UnpackPattern(mgr.target, enterValue, ctx); err != nil {
			// Call __exit__ before returning error
			cm.Exit(core.Nil, core.Nil, core.Nil)
			return nil, err
		}
	}

	// Execute the rest in a try-finally to ensure __exit__ is called
	var result core.Value
	var bodyErr error

	// If there are more managers, recurse
	if len(rest) > 0 {
		result, bodyErr = executeWith(rest, body, ctx)
	} else {
		result, bodyErr = executeBody(body, ctx)
	}

	// CRITICAL: After executing the body, variables defined in body should still be in ctx
	// because we passed the SAME ctx to executeBody. This is correct Python behavior.
	// The with statement does NOT create a new scope.

	// Call __exit__ with exception info
	var excType, excValue, excTraceback core.Value = core.Nil, core.Nil, core.Nil

	if bodyErr != nil {
		// Extract exception information
		// Convert error to exception instance to get the proper type
		excInstance := errorToExceptionInstance(bodyErr, ctx)

		// Get the exception class
		if inst, ok := excInstance.(*core.Instance); ok {
			if classVal, hasClass := inst.GetAttr("__class__"); hasClass {
				excType = classVal
			} else {
				excType = core.StringValue("Exception")
			}
		} else {
			// Fallback: try to determine type from error
			switch bodyErr.(type) {
			case *core.TypeError:
				if typeErrorClass, err := ctx.Lookup("TypeError"); err == nil {
					excType = typeErrorClass
				} else {
					excType = core.StringValue("TypeError")
				}
			case *core.ValueError:
				if valueErrorClass, err := ctx.Lookup("ValueError"); err == nil {
					excType = valueErrorClass
				} else {
					excType = core.StringValue("ValueError")
				}
			default:
				excType = core.StringValue("Exception")
			}
		}

		excValue = excInstance
		// excTraceback would be set if we had proper traceback objects
	}

	suppress, exitErr := cm.Exit(excType, excValue, excTraceback)
	if exitErr != nil {
		// Exit raised an exception
		return nil, exitErr
	}

	// If __exit__ returned true, suppress the exception
	if suppress && bodyErr != nil {
		return result, nil
	}

	// Re-raise the original exception if not suppressed
	if bodyErr != nil {
		return nil, bodyErr
	}

	return result, nil
}

// executeBody executes a sequence of expressions
func executeBody(body []core.Value, ctx *core.Context) (core.Value, error) {
	var result core.Value = core.Nil

	for _, expr := range body {
		var err error
		result, err = Eval(expr, ctx)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

// asyncWithForm implements the async with statement
// (async-with expr body...)              - simple form
// (async-with expr as var body...)       - with variable binding
func asyncWithForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, fmt.Errorf("async with requires at least 2 arguments")
	}

	// Parse async context managers - same structure as sync with
	var managers []asyncWithManager

	// Handle single manager case (the common case from ToIR)
	mgr := asyncWithManager{expr: args.Items()[0]}
	bodyStart := 1

	// Check if args[1] is a variable name (symbol), tuple pattern (list), or None
	if args.Len() >= 3 {
		target := unwrapLocated(args.Items()[1])
		if sym, ok := target.(core.SymbolValue); ok {
			symStr := string(sym)
			if symStr != "None" {
				mgr.varName = symStr
				bodyStart = 2
			} else {
				bodyStart = 2
			}
		} else if list, ok := target.(*core.ListValue); ok {
			mgr.target = list
			bodyStart = 2
		} else if target == core.None {
			bodyStart = 2
		}
	}

	managers = []asyncWithManager{mgr}
	body := args.Items()[bodyStart:]

	return executeAsyncWith(managers, body, ctx)
}

// asyncWithManager represents a single async context manager in an async with statement
type asyncWithManager struct {
	expr    core.Value
	varName string     // For simple variable: as x
	target  core.Value // For tuple unpacking: as (a, b, c)
}

// executeAsyncWith executes an async with statement with proper async enter/exit handling
func executeAsyncWith(managers []asyncWithManager, body []core.Value, ctx *core.Context) (core.Value, error) {
	if len(managers) == 0 {
		return executeBody(body, ctx)
	}

	// Take the first manager
	mgr := managers[0]
	rest := managers[1:]

	// Evaluate the manager expression
	mgrValue, err := Eval(mgr.expr, ctx)
	if err != nil {
		return nil, err
	}

	// Check if it's an async context manager (has __aenter__ and __aexit__)
	if !types.HasAenter(mgrValue) || !types.HasAexit(mgrValue) {
		return nil, fmt.Errorf("'%s' object does not support the async context manager protocol", mgrValue.Type())
	}

	// Call __aenter__
	aenterResult, found, err := types.CallAenter(mgrValue, ctx)
	if err != nil {
		return nil, err
	}
	if !found {
		return nil, fmt.Errorf("'%s' object has no __aenter__ method", mgrValue.Type())
	}

	// If __aenter__ returns a coroutine, execute it to get the actual value
	enterValue := aenterResult
	if coro, ok := aenterResult.(*core.Coroutine); ok {
		if callable, callOk := coro.Function.(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		}); callOk {
			enterValue, err = callable.Call(coro.Args, ctx)
			if err != nil {
				return nil, err
			}
		}
	}

	// Bind the value if there's an 'as' clause
	if mgr.varName != "" {
		ctx.Define(mgr.varName, enterValue)
	} else if mgr.target != nil {
		if err := UnpackPattern(mgr.target, enterValue, ctx); err != nil {
			// Call __aexit__ before returning error
			callAsyncExit(mgrValue, core.Nil, core.Nil, core.Nil, ctx)
			return nil, err
		}
	}

	// Execute the body
	var result core.Value
	var bodyErr error

	if len(rest) > 0 {
		result, bodyErr = executeAsyncWith(rest, body, ctx)
	} else {
		result, bodyErr = executeBody(body, ctx)
	}

	// Call __aexit__ with exception info
	var excType, excValue, excTraceback core.Value = core.Nil, core.Nil, core.Nil

	if bodyErr != nil {
		excInstance := errorToExceptionInstance(bodyErr, ctx)
		if inst, ok := excInstance.(*core.Instance); ok {
			if classVal, hasClass := inst.GetAttr("__class__"); hasClass {
				excType = classVal
			} else {
				excType = core.StringValue("Exception")
			}
		} else {
			excType = core.StringValue("Exception")
		}
		excValue = excInstance
	}

	suppress, exitErr := callAsyncExit(mgrValue, excType, excValue, excTraceback, ctx)
	if exitErr != nil {
		return nil, exitErr
	}

	// If __aexit__ returned true, suppress the exception
	if suppress && bodyErr != nil {
		return result, nil
	}

	if bodyErr != nil {
		return nil, bodyErr
	}

	return result, nil
}

// callAsyncExit calls __aexit__ on an async context manager, handling coroutine return
func callAsyncExit(mgrValue, excType, excValue, excTb core.Value, ctx *core.Context) (bool, error) {
	aexitResult, found, err := types.CallAexit(mgrValue, excType, excValue, excTb, ctx)
	if err != nil {
		return false, err
	}
	if !found {
		return false, fmt.Errorf("'%s' object has no __aexit__ method", mgrValue.Type())
	}

	// If __aexit__ returns a coroutine, execute it to get the actual return value
	exitValue := aexitResult
	if coro, ok := aexitResult.(*core.Coroutine); ok {
		if callable, callOk := coro.Function.(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		}); callOk {
			exitValue, err = callable.Call(coro.Args, ctx)
			if err != nil {
				return false, err
			}
		}
	}

	// Check if __aexit__ returned truthy value to suppress exception
	return core.IsTruthy(exitValue), nil
}

// RegisterContextForms registers context manager related forms
func RegisterContextForms() {
	RegisterSpecialForm("with", withForm)
	RegisterSpecialForm("async-with", asyncWithForm)
	// open is now a builtin function, not a special form
}

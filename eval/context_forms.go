package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// withForm implements the with statement
// (with expr body...)              - simple form
// (with expr as var body...)       - with variable binding
// (with [expr1 as var1 expr2 as var2] body...) - multiple context managers
func withForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("with requires at least 2 arguments")
	}

	// Parse context managers
	var managers []withManager
	
	// Check if first arg is a list (multiple managers)
	if list, ok := args[0].(core.ListValue); ok {
		// Multiple managers: (with [mgr1 as var1 mgr2 as var2] ...)
		managers = parseManagerList(list)
	} else {
		// Single manager: (with mgr ...) or (with mgr as var ...)
		mgr := withManager{expr: args[0]}
		
		// Check for 'as' clause
		if len(args) >= 3 {
			if sym, ok := args[1].(core.SymbolValue); ok && string(sym) == "as" {
				if varSym, ok := args[2].(core.SymbolValue); ok {
					mgr.varName = string(varSym)
					args = args[2:] // Skip "as var"
				} else {
					return nil, fmt.Errorf("with: variable name must be a symbol")
				}
			}
		}
		
		managers = []withManager{mgr}
	}

	// Get the body
	body := args[1:]
	
	// Execute with proper context management
	return executeWith(managers, body, ctx)
}

// withManager represents a single context manager in a with statement
type withManager struct {
	expr    core.Value
	varName string
}

// parseManagerList parses a list of managers with optional 'as' clauses
func parseManagerList(list core.ListValue) []withManager {
	var managers []withManager
	
	i := 0
	for i < len(list) {
		mgr := withManager{expr: list[i]}
		
		// Check for 'as var' after the expression
		if i+2 < len(list) {
			if sym, ok := list[i+1].(core.SymbolValue); ok && string(sym) == "as" {
				if varSym, ok := list[i+2].(core.SymbolValue); ok {
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
		ctx.Define(mgr.varName, enterValue)
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

	// Call __exit__ with exception info
	var excType, excValue, excTraceback core.Value = core.Nil, core.Nil, core.Nil
	
	if bodyErr != nil {
		// Extract exception information
		excType = core.StringValue("Exception")
		excValue = core.StringValue(bodyErr.Error())
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

// openForm creates a file context manager
func openForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("open() takes 1 or 2 arguments")
	}

	// Get filename
	filename, err := Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}
	
	filenameStr, ok := filename.(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("filename must be a string")
	}

	// Get mode (default to "r")
	mode := "r"
	if len(args) == 2 {
		modeVal, err := Eval(args[1], ctx)
		if err != nil {
			return nil, err
		}
		
		if modeStr, ok := modeVal.(core.StringValue); ok {
			mode = string(modeStr)
		} else {
			return nil, fmt.Errorf("mode must be a string")
		}
	}

	// Create file context manager
	return core.NewFileContextManager(string(filenameStr), mode), nil
}

// RegisterContextForms registers context manager related forms
func RegisterContextForms() {
	RegisterSpecialForm("with", withForm)
	RegisterSpecialForm("open", openForm)
}
package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalWith(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("with requires at least a context manager expression and a body")
	}

	// Check if using 'as' form: (with context-expr as var-name body...)
	var contextManager core.LispValue
	var varName core.LispSymbol
	var bodyStart int

	// Parse the with statement
	contextExpr := args[0]
	if len(args) >= 3 && args[1] == core.LispSymbol("as") {
		// (with context-expr as var-name body...)
		if symName, ok := args[2].(core.LispSymbol); ok {
			varName = symName
			bodyStart = 3
		} else {
			return nil, fmt.Errorf("variable name in 'with as' must be a symbol")
		}
	} else {
		// (with context-expr body...)
		bodyStart = 1
	}

	// Evaluate the context expression to get the context manager
	cm, err := e.Eval(contextExpr, env)
	if err != nil {
		return nil, err
	}
	contextManager = cm

	// Check if it's a function call that returns a context manager
	if contextList, ok := contextExpr.(core.LispList); ok && len(contextList) > 0 {
		if funcName, ok := contextList[0].(core.LispSymbol); ok {
			if funcName == "open" {
				// It's a call to open() - create a FileContextManager
				if len(contextList) < 2 {
					return nil, fmt.Errorf("open requires at least a filename")
				}

				// Evaluate the filename
				filenameVal, err := e.Eval(contextList[1], env)
				if err != nil {
					return nil, err
				}
				filename := fmt.Sprintf("%v", filenameVal)

				// Determine the mode (default to "r")
				mode := "r"
				if len(contextList) >= 3 {
					modeVal, err := e.Eval(contextList[2], env)
					if err != nil {
						return nil, err
					}
					mode = fmt.Sprintf("%v", modeVal)
				}

				contextManager = core.NewFileContextManager(filename, mode)
			}
		}
	}

	// Check if object implements the ContextManager interface
	cmInterface, ok := contextManager.(core.ContextManager)
	if !ok {
		return nil, fmt.Errorf("object does not implement the context manager protocol")
	}

	// Call __enter__ method
	contextValue, err := cmInterface.Enter()
	if err != nil {
		return nil, err
	}

	// Create a new environment for the with block
	withEnv := env.NewEnvironment(env)

	// If using 'as', bind the context value to the variable
	if varName != "" {
		withEnv.Define(varName, contextValue)
	}

	// Execute the body
	var result core.LispValue = core.PythonicNone{}
	var bodyErr error

	for i := bodyStart; i < len(args); i++ {
		result, bodyErr = e.Eval(args[i], withEnv)
		if bodyErr != nil {
			break
		}
	}

	// Always call __exit__ method, even if body raised an exception
	var exitException core.LispValue = core.PythonicNone{}
	if bodyErr != nil {
		// If there was an exception in the body, pass it to __exit__
		if exc, ok := bodyErr.(*core.Exception); ok {
			exitException = exc
		} else {
			// Convert Go error to Exception
			exitException = &core.Exception{
				Type:    "Exception",
				Message: bodyErr.Error(),
			}
		}
	}

	exitErr := cmInterface.Exit(exitException)

	// If body had an error and __exit__ didn't suppress it, propagate the body error
	if bodyErr != nil && exitErr == nil {
		return nil, bodyErr
	}

	// If __exit__ raised an exception, propagate it
	if exitErr != nil {
		return nil, exitErr
	}

	return result, nil
}

func EvalBegin(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// 'begin' evaluates multiple expressions and returns the result of the last one
	if len(args) == 0 {
		return core.PythonicNone{}, nil
	}

	var result core.LispValue
	var err error
	for _, expr := range args {
		result, err = e.Eval(expr, env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

// ReturnSignal is a custom error type that signals a return with a value
type ReturnSignal struct {
	Value core.LispValue
}

func (r ReturnSignal) Error() string {
	return "return with value"
}

// YieldSignal is a custom error type that signals a generator yield
type YieldSignal struct {
	Value core.LispValue
}

func (y YieldSignal) Error() string {
	return "yield with value"
}

func EvalReturn(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("return takes at most one argument")
	}

	// If no argument is provided, return None
	if len(args) == 0 {
		return nil, ReturnSignal{Value: core.PythonicNone{}}
	}

	// Evaluate the return value
	val, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Return a special signal with the return value
	return nil, ReturnSignal{Value: val}
}

func EvalYield(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("yield requires exactly one argument")
	}

	// Evaluate the value to yield
	value, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Signal generator suspension with the yielded value
	return nil, YieldSignal{Value: value}
}

func EvalDel(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without true deletion
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("del arguments must be symbols")
		}
		env.Set(symbol, nil)
	}
	return nil, nil
}

// Package special_forms provides special form implementations for the M28 language.
package special_forms

import (
	"fmt"
	
	"m28/core"
	"m28/eval"
)

// RegisterLoopForms registers loop-related special forms
func RegisterLoopForms() {
	// Basic loop forms
	registerSpecialForm("for", ForForm)
	registerSpecialForm("while", WhileForm)
	
	// Flow control forms
	registerSpecialForm("break", BreakForm)
	registerSpecialForm("continue", ContinueForm)
}

// BreakSignal is used to signal a break from a loop
type BreakSignal struct{}

func (b BreakSignal) Error() string {
	return "break"
}

// ContinueSignal is used to signal a continue in a loop
type ContinueSignal struct{}

func (c ContinueSignal) Error() string {
	return "continue"
}

// BreakForm implements the 'break' special form
func BreakForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("break does not take any arguments")
	}
	
	return core.Nil, BreakSignal{}
}

// ContinueForm implements the 'continue' special form
func ContinueForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("continue does not take any arguments")
	}
	
	return core.Nil, ContinueSignal{}
}

// ForForm implements the 'for' special form
// Syntax: (for [item items] body...)
func ForForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("for requires at least 2 arguments")
	}
	
	// Get the loop binding
	binding, ok := args[0].(core.ListValue)
	if !ok {
		return nil, fmt.Errorf("for requires a binding list as its first argument")
	}
	
	// Check binding format: [var iterable]
	if len(binding) != 2 {
		return nil, fmt.Errorf("for binding must have exactly 2 elements")
	}
	
	// Get the variable name
	varName, ok := binding[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("for binding variable must be a symbol")
	}
	
	// Evaluate the iterable
	iterable, err := eval.Eval(binding[1], ctx)
	if err != nil {
		return nil, err
	}
	
	// Loop body (implicit do)
	body := args[1:]
	
	// Create a slice to store loop items
	var items []core.Value
	
	// Extract items from iterable based on type
	switch it := iterable.(type) {
	case core.ListValue:
		items = it
	case core.TupleValue:
		items = []core.Value(it)
	case core.StringValue:
		// Convert string characters to string values
		items = make([]core.Value, len(it))
		for i, ch := range it {
			items[i] = core.StringValue(string(ch))
		}
	case *core.DictValue:
		// Get keys from dictionary
		keysValue, err := it.CallMethod("keys", nil, ctx)
		if err != nil {
			return nil, err
		}
		if keys, ok := keysValue.(core.ListValue); ok {
			items = keys
		} else {
			return nil, fmt.Errorf("cannot iterate over %s", it.Type().Name())
		}
	default:
		return nil, fmt.Errorf("cannot iterate over %s", iterable.Type().Name())
	}
	
	// Create a new environment for the loop
	loopEnv := core.NewContext(ctx)
	
	// Execute the loop
	var result core.Value = core.Nil
	
	for _, item := range items {
		// Bind the loop variable
		loopEnv.Define(string(varName), item)
		
		// Execute the body
		for _, expr := range body {
			result, err = eval.Eval(expr, loopEnv)
			if err != nil {
				// Handle break and continue
				if _, ok := err.(BreakSignal); ok {
					return core.Nil, nil // Exit the loop
				}
				if _, ok := err.(ContinueSignal); ok {
					break // Skip to next iteration
				}
				return nil, err // Pass other errors up
			}
		}
	}
	
	return result, nil
}

// WhileForm implements the 'while' special form
// Syntax: (while condition body...)
func WhileForm(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("while requires at least 1 argument")
	}
	
	// Get condition and body
	condition := args[0]
	body := args[1:]
	
	// Execute the loop
	var result core.Value = core.Nil
	var err error
	
	for {
		// Evaluate the condition
		condResult, err := eval.Eval(condition, ctx)
		if err != nil {
			return nil, err
		}
		
		// Check truthiness to continue loop
		var isTruthy bool
		switch v := condResult.(type) {
		case core.BoolValue:
			isTruthy = bool(v)
		case core.NilValue:
			isTruthy = false
		case core.NumberValue:
			isTruthy = float64(v) != 0
		case core.StringValue:
			isTruthy = string(v) != ""
		case core.ListValue:
			isTruthy = len(v) > 0
		case core.TupleValue:
			isTruthy = len(v) > 0
		default:
			isTruthy = true
		}
		
		if !isTruthy {
			break
		}
		
		// Execute the body
		for _, expr := range body {
			result, err = eval.Eval(expr, ctx)
			if err != nil {
				// Handle break and continue
				if _, ok := err.(BreakSignal); ok {
					return core.Nil, nil // Exit the loop
				}
				if _, ok := err.(ContinueSignal); ok {
					break // Skip to next iteration
				}
				return nil, err // Pass other errors up
			}
		}
	}
	
	return result, nil
}
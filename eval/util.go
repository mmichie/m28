// Package eval provides the evaluation system for M28 expressions.
package eval

import (
	"fmt"
	"os"
	
	"m28/core"
)

// ReadFile reads a file and returns its contents as a string
func ReadFile(filename string) (string, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

// EvalString parses and evaluates a string as M28 code
func EvalString(code string, ctx *core.Context) (core.Value, error) {
	// For now, we need to use a placeholder function
	// In a real implementation, this would parse the string into expressions
	// and evaluate them
	return core.Nil, fmt.Errorf("EvalString not implemented yet")
}

// EvalDo implements the do special form for evaluating a block of expressions
func EvalDo(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.Nil, nil
	}
	
	var result core.Value = core.Nil
	var err error
	
	for _, expr := range args {
		result, err = Eval(expr, ctx)
		if err != nil {
			return nil, err
		}
	}
	
	return result, nil
}

// EvalIf implements the if special form
func EvalIf(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("if requires 2 or 3 arguments")
	}
	
	// Evaluate the condition
	condition, err := Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}
	
	// Check if condition is truthy
	var isTruthy bool
	switch v := condition.(type) {
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
	
	// Choose which branch to evaluate
	if isTruthy {
		return Eval(args[1], ctx)
	} else if len(args) > 2 {
		return Eval(args[2], ctx)
	}
	
	// No else clause, return nil
	return core.Nil, nil
}

// EvalDef implements the def special form for function/variable definition
func EvalDef(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least 2 arguments")
	}
	
	// Get the name
	name, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("def: first argument must be a symbol")
	}
	
	// Check if it's a function definition
	if list, ok := args[1].(core.ListValue); ok {
		// It's a function definition: (def name (args...) body...)
		if len(args) < 3 {
			return nil, fmt.Errorf("function definition requires a body")
		}
		
		// Parse parameter list
		params := make([]core.SymbolValue, 0, len(list))
		for _, param := range list {
			if sym, ok := param.(core.SymbolValue); ok {
				params = append(params, sym)
			} else {
				return nil, fmt.Errorf("parameters must be symbols")
			}
		}
		
		// Create function body (implicit do)
		body := args[2:]
		
		function := &UserFunction{
			BaseObject: *core.NewBaseObject(core.FunctionType),
			params:     params,
			body:       body,
			env:        ctx,
		}
		
		ctx.Define(string(name), function)
		return function, nil
	}
	
	// Otherwise, it's a variable definition
	value, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}
	
	ctx.Define(string(name), value)
	return value, nil
}

// EvalAssign implements the = special form for assignment
func EvalAssign(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("= requires 2 arguments")
	}
	
	// Get the target
	target := args[0]
	
	// Evaluate the value
	value, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}
	
	// Handle different assignment targets
	switch t := target.(type) {
	case core.SymbolValue:
		// Variable assignment
		err := ctx.Set(string(t), value)
		if err != nil {
			return nil, err
		}
		return value, nil
		
	default:
		return nil, fmt.Errorf("invalid assignment target: %v", target)
	}
}

// EvalQuote implements the quote special form
func EvalQuote(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quote requires 1 argument")
	}
	
	// Return the argument unevaluated
	return args[0], nil
}

// EvalLambda implements the lambda special form for anonymous functions
func EvalLambda(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("lambda requires at least 2 arguments")
	}
	
	// Get parameter list
	paramList, ok := args[0].(core.ListValue)
	if !ok {
		return nil, fmt.Errorf("lambda: first argument must be a parameter list")
	}
	
	// Parse parameters
	params := make([]core.SymbolValue, 0, len(paramList))
	for _, param := range paramList {
		if sym, ok := param.(core.SymbolValue); ok {
			params = append(params, sym)
		} else {
			return nil, fmt.Errorf("parameters must be symbols")
		}
	}
	
	// Create function body (implicit do)
	body := args[1:]
	
	function := &UserFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		params:     params,
		body:       body,
		env:        ctx,
	}
	
	return function, nil
}

// UserFunction represents a user-defined function
type UserFunction struct {
	core.BaseObject
	params []core.SymbolValue
	body   []core.Value
	env    *core.Context
}

// Type implements Value.Type
func (f *UserFunction) Type() core.Type {
	return core.FunctionType
}

// String implements Value.String
func (f *UserFunction) String() string {
	if len(f.params) == 0 {
		return "<function ()>"
	}
	
	return fmt.Sprintf("<function (%s)>", f.params[0])
}

// Call implements Callable.Call
func (f *UserFunction) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Create a new environment with the function's environment as parent
	funcEnv := core.NewContext(f.env)
	
	// Bind arguments to parameters
	if len(args) != len(f.params) {
		return nil, fmt.Errorf("expected %d arguments, got %d", len(f.params), len(args))
	}
	
	for i, param := range f.params {
		funcEnv.Define(string(param), args[i])
	}
	
	// Evaluate the body in the new environment
	if len(f.body) == 1 {
		return Eval(f.body[0], funcEnv)
	}
	
	// Implicit do for multiple expressions
	return EvalDo(f.body, funcEnv)
}
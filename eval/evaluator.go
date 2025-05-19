// Package eval provides the evaluation system for M28 expressions.
package eval

import (
	"fmt"
	
	"m28/core"
)

// Eval evaluates an expression in a context
func Eval(expr core.Value, ctx *core.Context) (core.Value, error) {
	switch v := expr.(type) {
	case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue:
		// Self-evaluating primitives
		return v, nil
		
	case core.SymbolValue:
		// Variable lookup
		return ctx.Lookup(string(v))
		
	case core.ListValue:
		// Empty list evaluates to itself
		if len(v) == 0 {
			return core.EmptyList, nil
		}
		
		// Check if it's a special form first (if, def, etc.)
		if sym, ok := v[0].(core.SymbolValue); ok {
			if handler, ok := specialForms[string(sym)]; ok {
				return handler(v[1:], ctx)
			}
		}
		
		// Otherwise it's a function call
		return evalFunctionCall(v, ctx)
		
	default:
		// Other values evaluate to themselves
		return expr, nil
	}
}

// evalFunctionCall evaluates a function call expression
func evalFunctionCall(expr core.ListValue, ctx *core.Context) (core.Value, error) {
	// Evaluate the function
	fn, err := Eval(expr[0], ctx)
	if err != nil {
		return nil, err
	}
	
	// Evaluate the arguments
	args := make([]core.Value, 0, len(expr)-1)
	for _, arg := range expr[1:] {
		evalArg, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		args = append(args, evalArg)
	}
	
	// Call the function
	callable, ok := fn.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("not callable: %v", fn)
	}
	
	return callable.Call(args, ctx)
}

// SpecialFormHandler handles special forms like if, def, etc.
type SpecialFormHandler func(args core.ListValue, ctx *core.Context) (core.Value, error)

// specialForms maps special form names to their handlers
var specialForms = map[string]SpecialFormHandler{
	"if":    ifForm,
	"def":   defForm,
	"=":     assignForm,
	"quote": quoteForm,
	"do":    doForm,
	// Other special forms will be added here
}

// ifForm implements the if special form
func ifForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("if requires 2 or 3 arguments")
	}
	
	// Evaluate the condition
	condition, err := Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}
	
	// Check if condition is truthy
	isTruthy := true
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

// defForm implements the def special form to define functions and variables
func defForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
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
		if len(body) == 1 {
			// Single expression body
			function := &UserFunction{
				params: params,
				body:   body[0],
				env:    ctx,
			}
			
			ctx.Define(string(name), function)
			return function, nil
		} else {
			// Multi-expression body, wrap in do
			function := &UserFunction{
				params: params,
				body:   core.ListValue(append([]core.Value{core.SymbolValue("do")}, body...)),
				env:    ctx,
			}
			
			ctx.Define(string(name), function)
			return function, nil
		}
	}
	
	// Otherwise, it's a variable definition
	value, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}
	
	ctx.Define(string(name), value)
	return value, nil
}

// assignForm implements the = special form for assignment
func assignForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
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

// quoteForm implements the quote special form
func quoteForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quote requires 1 argument")
	}
	
	// Return the argument unevaluated
	return args[0], nil
}

// doForm implements the do special form for grouping expressions
func doForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.Nil, nil
	}
	
	// Evaluate all expressions in sequence
	var result core.Value = core.Nil
	var err error
	
	for _, expr := range args {
		result, err = Eval(expr, ctx)
		if err != nil {
			return nil, err
		}
	}
	
	// Return the value of the last expression
	return result, nil
}

// UserFunction represents a user-defined function
type UserFunction struct {
	core.BaseObject
	params []core.SymbolValue
	body   core.Value
	env    *core.Context
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
	return Eval(f.body, funcEnv)
}

// Type implements Value.Type
func (f *UserFunction) Type() core.Type {
	return core.FunctionType
}

// String implements Value.String
func (f *UserFunction) String() string {
	params := make([]string, len(f.params))
	for i, param := range f.params {
		params[i] = string(param)
	}
	return fmt.Sprintf("<function (%s)>", params)
}
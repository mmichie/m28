// Package eval provides the evaluation system for M28 expressions.
package eval

import (
	"fmt"
	
	"github.com/mmichie/m28/core"
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
var specialForms map[string]SpecialFormHandler

func init() {
	specialForms = map[string]SpecialFormHandler{
		// Control flow
		"if":      ifForm,
		"do":      doForm,
		"return":  returnForm,
		
		// Definitions
		"def":     defForm,
		"=":       assignForm,
		"quote":   quoteForm,
		
		// Module system
		"import":  importForm,
		
		// Other special forms will be added through RegisterSpecialForm
	}
}

// RegisterSpecialForm registers a special form
func RegisterSpecialForm(name string, handler SpecialFormHandler) {
	specialForms[name] = handler
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
	if core.IsTruthy(condition) {
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
	
	// Check for alternative function definition form: (def (name args...) body...)
	if fnDef, ok := args[0].(core.ListValue); ok && len(fnDef) > 0 {
		// Get function name from the first element of the list
		name, ok := fnDef[0].(core.SymbolValue)
		if !ok {
			return nil, fmt.Errorf("def: function name must be a symbol")
		}
		
		// Get parameter list (the rest of the elements after the name)
		params := make([]core.SymbolValue, 0, len(fnDef)-1)
		for _, param := range fnDef[1:] {
			if sym, ok := param.(core.SymbolValue); ok {
				params = append(params, sym)
			} else {
				return nil, fmt.Errorf("parameters must be symbols")
			}
		}
		
		// Create function body (implicit do)
		body := args[1:]
		var functionBody core.Value
		
		if len(body) == 1 {
			// Single expression body
			functionBody = body[0]
		} else {
			// Multi-expression body, wrap in do
			functionBody = core.ListValue(append([]core.Value{core.SymbolValue("do")}, body...))
		}
		
		function := &UserFunction{
			BaseObject: *core.NewBaseObject(core.FunctionType),
			params:     params,
			body:       functionBody,
			env:        ctx,
		}
		
		ctx.Define(string(name), function)
		return function, nil
	}
	
	// Standard form: (def name ...)
	name, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("def: first argument must be a symbol")
	}
	
	// Check different definition forms
	if len(args) >= 3 {
		// Check for function definition: (def name (params) body...)
		if paramList, ok := args[1].(core.ListValue); ok {
			// It's a function definition with parameter list
			
			// Parse parameter list
			params := make([]core.SymbolValue, 0, len(paramList))
			for _, param := range paramList {
				if sym, ok := param.(core.SymbolValue); ok {
					params = append(params, sym)
				} else {
					return nil, fmt.Errorf("parameters must be symbols")
				}
			}
			
			// Create function body (implicit do)
			body := args[2:]
			var functionBody core.Value
			
			if len(body) == 1 {
				// Single expression body
				functionBody = body[0]
			} else {
				// Multi-expression body, wrap in do
				functionBody = core.ListValue(append([]core.Value{core.SymbolValue("do")}, body...))
			}
			
			function := &UserFunction{
				BaseObject: *core.NewBaseObject(core.FunctionType),
				params:     params,
				body:       functionBody,
				env:        ctx,
			}
			
			ctx.Define(string(name), function)
			return function, nil
		}
	}
	
	// Variable definition: (def name value)
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
		// Variable assignment - first try setting an existing variable
		symName := string(t)
		err := ctx.Set(symName, value)
		if err != nil {
			// If the variable doesn't exist, define it in the current scope
			ctx.Define(symName, value)
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
		
		// Check for return value
		if ret, ok := result.(*ReturnValue); ok {
			return ret.Value, nil
		}
	}
	
	// Return the value of the last expression
	return result, nil
}

// returnForm implements the return special form
func returnForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("return takes at most 1 argument")
	}
	
	var value core.Value = core.Nil
	var err error
	
	if len(args) == 1 {
		value, err = Eval(args[0], ctx)
		if err != nil {
			return nil, err
		}
	}
	
	return &ReturnValue{Value: value}, nil
}

// importForm implements the import special form
func importForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("import requires 1 argument")
	}
	
	// Get the module name
	var moduleName string
	switch name := args[0].(type) {
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
		return nil, fmt.Errorf("failed to import module %s: %v", moduleName, err)
	}
	
	// Store module in the context
	ctx.Define(moduleName, module)
	
	return module, nil
}

// ReturnValue represents a return value from a function
type ReturnValue struct {
	Value core.Value
}

// Type implements Value.Type
func (r *ReturnValue) Type() core.Type {
	return "return"
}

// String implements Value.String
func (r *ReturnValue) String() string {
	return fmt.Sprintf("<return %v>", r.Value)
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
	result, err := Eval(f.body, funcEnv)
	if err != nil {
		return nil, err
	}
	
	// Handle return values
	if ret, ok := result.(*ReturnValue); ok {
		return ret.Value, nil
	}
	
	return result, nil
}

// String implements Value.String
func (f *UserFunction) String() string {
	params := make([]string, len(f.params))
	for i, param := range f.params {
		params[i] = string(param)
	}
	return fmt.Sprintf("<function (%s)>", params)
}
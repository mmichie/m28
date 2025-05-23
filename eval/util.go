package eval

import (
	"github.com/mmichie/m28/core"
)

// IfForm provides the implementation of the if special form
func IfForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, ErrArgCount("if requires 2 or 3 arguments")
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

// DoForm provides the implementation of the do special form
func DoForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
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

// DefForm provides the implementation of the def special form
func DefForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("def requires at least 2 arguments")
	}

	// Get the name
	name, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, TypeError{Expected: "symbol", Got: args[0].Type()}
	}

	// Check different definition forms
	if len(args) >= 3 {
		// Check for function definition: (def name (params) body...)
		// First form: (def name (arg1 arg2...) body...)
		if paramList, ok := args[1].(core.ListValue); ok {
			// It's a function definition with parameter list

			// Parse parameter list
			params := make([]core.SymbolValue, 0, len(paramList))
			for _, param := range paramList {
				if sym, ok := param.(core.SymbolValue); ok {
					params = append(params, sym)
				} else {
					return nil, TypeError{Expected: "symbol", Got: param.Type()}
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

	// Second form: (def name value)
	// It's a variable definition
	value, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}

	ctx.Define(string(name), value)
	return value, nil
}

// AssignForm provides the implementation of the = special form
func AssignForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, ErrArgCount("= requires 2 arguments")
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
		// Variable assignment - define in current scope
		ctx.Define(string(t), value)
		return value, nil

	default:
		return nil, TypeError{Expected: "symbol", Got: target.Type()}
	}
}

// QuoteForm provides the implementation of the quote special form
func QuoteForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, ErrArgCount("quote requires 1 argument")
	}

	// Return the argument unevaluated
	return args[0], nil
}

// ReturnForm provides the implementation of the return special form
func ReturnForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) > 1 {
		return nil, ErrArgCount("return takes at most 1 argument")
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

// ImportForm provides the implementation of the import special form
func ImportForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, ErrArgCount("import requires 1 argument")
	}

	// Get the module name
	var moduleName string
	switch name := args[0].(type) {
	case core.StringValue:
		moduleName = string(name)
	case core.SymbolValue:
		moduleName = string(name)
	default:
		return nil, TypeError{Expected: "string or symbol", Got: args[0].Type()}
	}

	// Get the module loader
	loader := core.GetModuleLoader()
	if loader == nil {
		return nil, ArgumentError{"no module loader registered"}
	}

	// Load the module
	module, err := loader.LoadModule(moduleName, ctx)
	if err != nil {
		return nil, err
	}

	// Store module in the context
	ctx.Define(moduleName, module)

	return module, nil
}

package eval

import (
	"fmt"
	
	"github.com/mmichie/m28/core"
)

// IfForm provides the implementation of the if special form with elif support
func IfForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("if requires at least 2 arguments")
	}

	// Handle simple if or if-else (2 or 3 args)
	if len(args) == 2 || len(args) == 3 {
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

	// Handle if-elif-else chain
	// Must have pairs of condition-expr, optionally with final else
	i := 0
	for i < len(args)-1 {
		// Evaluate condition
		condition, err := Eval(args[i], ctx)
		if err != nil {
			return nil, err
		}

		// If truthy, evaluate and return the corresponding expression
		if core.IsTruthy(condition) {
			return Eval(args[i+1], ctx)
		}

		i += 2
	}

	// If we have one remaining arg, it's the else clause
	if i < len(args) {
		return Eval(args[i], ctx)
	}

	// No condition matched and no else clause
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
				name:       string(name),
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

// DictLiteralForm provides the implementation of the dict-literal special form
func DictLiteralForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	// Arguments should be key-value pairs
	if len(args)%2 != 0 {
		return nil, fmt.Errorf("dict-literal requires an even number of arguments (key-value pairs)")
	}
	
	// Create a new dictionary
	dict := core.NewDict()
	
	// Process key-value pairs
	for i := 0; i < len(args); i += 2 {
		// Evaluate the key
		key, err := Eval(args[i], ctx)
		if err != nil {
			return nil, fmt.Errorf("error evaluating dict key: %v", err)
		}
		
		// Convert key to string
		var keyStr string
		switch k := key.(type) {
		case core.StringValue:
			keyStr = string(k)
		case core.SymbolValue:
			keyStr = string(k)
		default:
			return nil, fmt.Errorf("dict keys must be strings or symbols, got %v", key.Type())
		}
		
		// Evaluate the value
		value, err := Eval(args[i+1], ctx)
		if err != nil {
			return nil, fmt.Errorf("error evaluating dict value for key %q: %v", keyStr, err)
		}
		
		// Set the key-value pair
		dict.Set(keyStr, value)
	}
	
	return dict, nil
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
		
	case core.ListValue:
		// Check if it's a dot notation expression
		if len(t) >= 3 {
			if dotSym, ok := t[0].(core.SymbolValue); ok && string(dotSym) == "." {
				// Dot notation assignment: (. obj prop) = value
				// Evaluate the object
				obj, err := Eval(t[1], ctx)
				if err != nil {
					return nil, err
				}
				
				// Get the property name
				propName, ok := t[2].(core.StringValue)
				if !ok {
					return nil, TypeError{Expected: "string property name", Got: t[2].Type()}
				}
				
				// Set the attribute
				if objWithAttrs, ok := obj.(interface {
					SetAttr(string, core.Value) error
				}); ok {
					err := objWithAttrs.SetAttr(string(propName), value)
					if err != nil {
						return nil, err
					}
					return value, nil
				}
				
				// Special handling for dicts
				if dict, ok := obj.(*core.DictValue); ok {
					dict.Set(string(propName), value)
					return value, nil
				}
				
				return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
			}
		}
		// Fall through to error

	default:
		return nil, TypeError{Expected: "symbol or dot notation", Got: target.Type()}
	}
	
	return nil, TypeError{Expected: "symbol or dot notation", Got: target.Type()}
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

package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// handleElifOrElse is a helper to process nested elif or else clauses
func handleElifOrElse(clause core.Value, ctx *core.Context) (core.Value, error) {
	// Check if it's another elif
	if list, ok := clause.(core.ListValue); ok && len(list) > 0 {
		if sym, ok := list[0].(core.SymbolValue); ok && string(sym) == "elif" {
			// Recursive elif
			if len(list) < 3 {
				return nil, ErrArgCount("elif requires condition and expression")
			}

			// Evaluate condition
			cond, err := Eval(list[1], ctx)
			if err != nil {
				return nil, err
			}

			if core.IsTruthy(cond) {
				return Eval(list[2], ctx)
			}

			// Check for more elif/else
			if len(list) > 3 {
				return handleElifOrElse(list[3], ctx)
			}

			return core.Nil, nil
		}
	}

	// It's an else expression
	return Eval(clause, ctx)
}

// IfForm provides the implementation of the if special form with elif support
func IfForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, ErrArgCount("if requires at least 2 arguments")
	}

	// Evaluate the first condition
	condition, err := Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}

	// If truthy, evaluate and return the then-branch
	if core.IsTruthy(condition) {
		return Eval(args[1], ctx)
	}

	// Check for elif or else
	if len(args) <= 2 {
		// No else clause, return nil
		return core.Nil, nil
	}

	// Check if the third argument is a list starting with elif
	if list, ok := args[2].(core.ListValue); ok && len(list) > 0 {
		if sym, ok := list[0].(core.SymbolValue); ok && string(sym) == "elif" {
			// Nested elif structure: (elif condition expr else-clause)
			if len(list) < 3 {
				return nil, ErrArgCount("elif requires condition and expression")
			}

			// Evaluate elif condition
			elifCond, err := Eval(list[1], ctx)
			if err != nil {
				return nil, err
			}

			if core.IsTruthy(elifCond) {
				return Eval(list[2], ctx)
			}

			// Check if there's an else clause (4th element) or another elif
			if len(list) > 3 {
				// The 4th element could be another elif or an else expression
				return handleElifOrElse(list[3], ctx)
			}

			// No else clause
			return core.Nil, nil
		}
	}

	// Check if the third argument is elif symbol (flat structure)
	if sym, ok := args[2].(core.SymbolValue); ok && string(sym) == "elif" {
		// Handle flat elif chain
		// Structure: (if cond1 expr1 elif cond2 expr2 elif cond3 expr3 else expr4)
		elifArgs := args[2:] // Skip "if" condition and expression

		for len(elifArgs) > 0 {
			// Check if current element is elif
			if sym, ok := elifArgs[0].(core.SymbolValue); ok && string(sym) == "elif" {
				if len(elifArgs) < 3 {
					return nil, ErrArgCount("elif requires condition and expression")
				}

				// Evaluate elif condition
				elifCond, err := Eval(elifArgs[1], ctx)
				if err != nil {
					return nil, err
				}

				if core.IsTruthy(elifCond) {
					return Eval(elifArgs[2], ctx)
				}

				// Move to next elif or else
				elifArgs = elifArgs[3:]
			} else if sym, ok := elifArgs[0].(core.SymbolValue); ok && string(sym) == "else" {
				// Handle else clause
				if len(elifArgs) < 2 {
					return nil, ErrArgCount("else requires an expression")
				}
				return Eval(elifArgs[1], ctx)
			} else {
				// This is the else expression (old style)
				return Eval(elifArgs[0], ctx)
			}
		}

		// No condition matched and no else clause
		return core.Nil, nil
	} else {
		// Traditional if-else (third argument is the else expression)
		return Eval(args[2], ctx)
	}
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

			// Try to parse as new-style parameter list with defaults
			signature, err := ParseParameterList(paramList)
			if err != nil {
				// Fall back to legacy simple parameter parsing
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

			// New-style function with signature
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

			// Build legacy params list for backward compatibility
			var params []core.SymbolValue
			for _, p := range signature.RequiredParams {
				params = append(params, p.Name)
			}
			for _, p := range signature.OptionalParams {
				params = append(params, p.Name)
			}

			function := &UserFunction{
				BaseObject: *core.NewBaseObject(core.FunctionType),
				params:     params,
				signature:  signature,
				body:       functionBody,
				env:        ctx,
				name:       string(name),
			}

			ctx.Define(string(name), function)
			return function, nil
		}
	}

	// Second form: (def name value)
	// Check if it's a lambda/function value
	value, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}

	// Only allow def for functions
	switch value.(type) {
	case *UserFunction:
		// It's a function, allow it
		ctx.Define(string(name), value)
	case core.Callable:
		// It's some other callable (builtin function, etc), allow it
		ctx.Define(string(name), value)
	default:
		// Not a function, error
		return nil, fmt.Errorf("def can only be used to define functions, not %s values. Use = for variable assignment", value.Type())
	}
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

		// Check if key is hashable
		if !core.IsHashable(key) {
			return nil, fmt.Errorf("unhashable type: '%s'", key.Type())
		}

		// Convert key to string representation
		keyStr := core.ValueToKey(key)

		// Evaluate the value
		value, err := Eval(args[i+1], ctx)
		if err != nil {
			return nil, fmt.Errorf("error evaluating dict value for key %v: %v", key, err)
		}

		// Set the key-value pair
		dict.SetWithKey(keyStr, key, value)
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
		// Check if it's a special form first (get-item or dot notation)
		if len(t) >= 3 {
			// Check if it's a dot notation expression
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

			// Check if it's an index expression
			if len(t) == 3 {
				if getItemSym, ok := t[0].(core.SymbolValue); ok && string(getItemSym) == "get-item" {
					// Index assignment: (get-item obj index) = value
					// Convert to (set-item obj index value)
					setItemArgs := core.ListValue{t[1], t[2], value}
					return SetItemForm(setItemArgs, ctx)
				}
			}
		}

		// Check if it's tuple unpacking
		isTupleUnpacking := true
		for _, elem := range t {
			if _, ok := elem.(core.SymbolValue); !ok {
				isTupleUnpacking = false
				break
			}
		}

		if isTupleUnpacking && len(t) > 0 {
			// Tuple unpacking: (= (x, y) [10, 20])
			// The value must be a tuple or list
			var values []core.Value
			switch v := value.(type) {
			case core.TupleValue:
				values = []core.Value(v)
			case core.ListValue:
				values = []core.Value(v)
			default:
				return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
			}

			// Check that the number of targets matches the number of values
			if len(t) != len(values) {
				return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", len(t), len(values))
			}

			// Assign each value to its corresponding target
			for i, target := range t {
				sym, ok := target.(core.SymbolValue)
				if !ok {
					return nil, fmt.Errorf("assignment target must be a symbol, got %v", target.Type())
				}
				ctx.Define(string(sym), values[i])
			}

			return value, nil
		}

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

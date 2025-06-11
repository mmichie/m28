package eval

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// parseKeywordArguments extracts keyword arguments from a function call
// It looks for patterns like: symbol = value in the argument list
func parseKeywordArguments(args core.ListValue) ([]core.Value, map[string]core.Value, error) {
	positional := []core.Value{}
	keywords := make(map[string]core.Value)

	i := 0
	for i < len(args) {
		// Check if this is a keyword argument pattern: symbol = value
		if i+2 < len(args) {
			if sym, ok := args[i].(core.SymbolValue); ok {
				if eq, ok := args[i+1].(core.SymbolValue); ok && string(eq) == "=" {
					// This is a keyword argument
					paramName := string(sym)
					paramValue := args[i+2]

					// Check for duplicate keyword arguments
					if _, exists := keywords[paramName]; exists {
						return nil, nil, fmt.Errorf("duplicate keyword argument: %s", paramName)
					}

					keywords[paramName] = paramValue
					i += 3 // Skip symbol, =, and value
					continue
				}
			}
		}

		// This is a positional argument
		// But we can't have positional args after keyword args
		if len(keywords) > 0 {
			return nil, nil, fmt.Errorf("positional argument follows keyword argument")
		}

		positional = append(positional, args[i])
		i++
	}

	return positional, keywords, nil
}

// evalFunctionCallWithKeywords evaluates a function call with keyword argument support
func evalFunctionCallWithKeywords(expr core.ListValue, ctx *core.Context) (core.Value, error) {
	// Evaluate the function
	fn, err := Eval(expr[0], ctx)
	if err != nil {
		return nil, err
	}

	// Parse arguments to separate positional and keyword arguments
	positionalExprs, keywordExprs, err := parseKeywordArguments(expr[1:])
	if err != nil {
		return nil, err
	}

	// Evaluate positional arguments
	positionalArgs := make([]core.Value, 0, len(positionalExprs))
	for _, arg := range positionalExprs {
		evalArg, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}
		positionalArgs = append(positionalArgs, evalArg)
	}

	// Evaluate keyword arguments
	keywordArgs := make(map[string]core.Value)
	for name, expr := range keywordExprs {
		evalArg, err := Eval(expr, ctx)
		if err != nil {
			return nil, err
		}
		keywordArgs[name] = evalArg
	}

	// Check if it's a UserFunction that supports keyword arguments
	if userFunc, ok := fn.(*UserFunction); ok && userFunc.signature != nil {
		// Create a new context for the function
		funcEnv := core.NewContext(userFunc.env)

		// Bind arguments using the signature
		err := userFunc.signature.BindArguments(positionalArgs, keywordArgs, userFunc.env, funcEnv)
		if err != nil {
			return nil, err
		}

		// Evaluate the body in the new environment
		result, err := Eval(userFunc.body, funcEnv)
		if err != nil {
			return nil, err
		}

		// Handle return values
		if ret, ok := result.(*ReturnValue); ok {
			return ret.Value, nil
		}

		return result, nil
	}

	// Check if it's a KwargsBuiltinFunction that supports keyword arguments
	if kwargsFunc, ok := fn.(interface {
		CallWithKeywords([]core.Value, map[string]core.Value, *core.Context) (core.Value, error)
	}); ok {
		result, err := kwargsFunc.CallWithKeywords(positionalArgs, keywordArgs, ctx)
		if err != nil {
			return nil, err
		}
		return result, nil
	}

	// For functions that don't support keyword arguments,
	// only allow calls with no keyword arguments
	if len(keywordArgs) > 0 {
		return nil, fmt.Errorf("function does not support keyword arguments")
	}

	// Call as normal with just positional arguments
	callable, ok := fn.(core.Callable)
	if !ok {
		return nil, core.NewTypeError("callable", fn, "function call")
	}

	// Add function name to call stack if available
	var funcName string
	switch f := fn.(type) {
	case core.SymbolValue:
		funcName = string(f)
	case *core.BuiltinFunction:
		funcName = "<builtin>"
	case *core.BoundMethod:
		funcName = fmt.Sprintf("%s.%s", f.TypeDesc.PythonName, f.Method.Name)
	case *UserFunction:
		if f.name != "" {
			funcName = f.name
		} else {
			funcName = "<anonymous>"
		}
	default:
		funcName = "<anonymous>"
	}

	ctx.PushStack(funcName, "", 0, 0)
	defer ctx.PopStack()

	result, err := callable.Call(positionalArgs, ctx)
	if err != nil {
		return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s", funcName), ctx)
	}

	return result, nil
}

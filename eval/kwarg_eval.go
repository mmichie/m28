package eval

import (
	"fmt"
	"github.com/mmichie/m28/core"
	"strings"
)

// ArgumentElement represents a single argument element (regular or unpacking)
type ArgumentElement struct {
	IsUnpack   bool       // true if this is *expr
	IsKwUnpack bool       // true if this is **expr
	Expr       core.Value // The expression (without * or **)
}

// ArgumentInfo holds information about parsed arguments including unpacking
type ArgumentInfo struct {
	Elements     []ArgumentElement     // All positional elements in order
	KeywordExprs map[string]core.Value // Regular keyword arguments
}

// parseKeywordArguments extracts keyword arguments from a function call
// It looks for patterns like: symbol = value in the argument list
// Also handles *args and **kwargs unpacking
func parseKeywordArguments(args core.ListValue) ([]core.Value, map[string]core.Value, error) {
	info, err := parseArgumentsWithUnpacking(args)
	if err != nil {
		return nil, nil, err
	}

	// For backward compatibility, extract simple positional args
	positional := []core.Value{}
	for _, elem := range info.Elements {
		if elem.IsUnpack || elem.IsKwUnpack {
			// Use the new evaluation function instead
			return nil, nil, fmt.Errorf("argument unpacking requires evalFunctionCallWithKeywords")
		}
		positional = append(positional, elem.Expr)
	}

	return positional, info.KeywordExprs, nil
}

// parseArgumentsWithUnpacking extracts all argument information including unpacking
func parseArgumentsWithUnpacking(args core.ListValue) (*ArgumentInfo, error) {
	info := &ArgumentInfo{
		Elements:     []ArgumentElement{},
		KeywordExprs: make(map[string]core.Value),
	}

	i := 0
	seenKeyword := false

	for i < len(args) {
		// Check for unpacking operators
		if sym, ok := args[i].(core.SymbolValue); ok {
			symStr := string(sym)

			// Check for **unpack marker (parser creates: ["**unpack", <expr>])
			if symStr == "**unpack" {
				if i+1 >= len(args) {
					return nil, fmt.Errorf("** unpacking requires an expression")
				}
				info.Elements = append(info.Elements, ArgumentElement{
					IsKwUnpack: true,
					Expr:       args[i+1],
				})
				i += 2 // Skip marker and expression
				continue
			}

			// Check for *unpack marker (parser creates: ["*unpack", <expr>])
			if symStr == "*unpack" {
				if i+1 >= len(args) {
					return nil, fmt.Errorf("* unpacking requires an expression")
				}
				if seenKeyword {
					return nil, fmt.Errorf("* unpacking cannot follow keyword arguments")
				}
				info.Elements = append(info.Elements, ArgumentElement{
					IsUnpack: true,
					Expr:     args[i+1],
				})
				i += 2 // Skip marker and expression
				continue
			}

			// Legacy support: Check for **kwargs unpacking (old style where expr is in the symbol)
			// Also handles parser-generated **kwargs markers followed by dict-literal
			if strings.HasPrefix(symStr, "**") && symStr != "**unpack" {
				exprStr := strings.TrimPrefix(symStr, "**")
				if exprStr == "" {
					return nil, fmt.Errorf("** unpacking requires an expression")
				}

				// Check if next argument is the expression to unpack (parser style)
				// Parser generates: ["**kwargs", (dict-literal ...)]
				if i+1 < len(args) {
					// Use the next argument as the dict expression
					info.Elements = append(info.Elements, ArgumentElement{
						IsKwUnpack: true,
						Expr:       args[i+1],
					})
					i += 2 // Skip marker and dict expression
					continue
				}

				// Old legacy style where variable name is embedded
				info.Elements = append(info.Elements, ArgumentElement{
					IsKwUnpack: true,
					Expr:       core.SymbolValue(exprStr),
				})
				i++
				continue
			}

			// Legacy support: Check for *args unpacking (old style)
			if strings.HasPrefix(symStr, "*") && !strings.HasPrefix(symStr, "**") && symStr != "*unpack" {
				exprStr := strings.TrimPrefix(symStr, "*")
				if exprStr == "" {
					return nil, fmt.Errorf("* unpacking requires an expression")
				}
				if seenKeyword {
					return nil, fmt.Errorf("* unpacking cannot follow keyword arguments")
				}
				info.Elements = append(info.Elements, ArgumentElement{
					IsUnpack: true,
					Expr:     core.SymbolValue(exprStr),
				})
				i++
				continue
			}
		}

		// Check if this is a keyword argument pattern: symbol = value
		if i+2 < len(args) {
			if sym, ok := args[i].(core.SymbolValue); ok {
				if eq, ok := args[i+1].(core.SymbolValue); ok && string(eq) == "=" {
					// This is a keyword argument
					paramName := string(sym)
					paramValue := args[i+2]
					seenKeyword = true

					// Check for duplicate keyword arguments
					if _, exists := info.KeywordExprs[paramName]; exists {
						return nil, fmt.Errorf("duplicate keyword argument: %s", paramName)
					}

					info.KeywordExprs[paramName] = paramValue
					i += 3 // Skip symbol, =, and value
					continue
				}
			}
		}

		// This is a positional argument
		// But we can't have positional args after keyword args
		if seenKeyword {
			return nil, fmt.Errorf("positional argument follows keyword argument")
		}

		info.Elements = append(info.Elements, ArgumentElement{
			IsUnpack:   false,
			IsKwUnpack: false,
			Expr:       args[i],
		})
		i++
	}

	return info, nil
}

// evalFunctionCallWithKeywords evaluates a function call with keyword argument support
func evalFunctionCallWithKeywords(expr core.ListValue, ctx *core.Context) (core.Value, error) {
	// Check if the function is referenced by a symbol (for better error messages)
	var symbolName string
	if sym, ok := expr[0].(core.SymbolValue); ok {
		symbolName = string(sym)
	}

	// Evaluate the function
	fn, err := Eval(expr[0], ctx)
	if err != nil {
		return nil, err
	}

	// Parse arguments including unpacking
	argInfo, err := parseArgumentsWithUnpacking(expr[1:])
	if err != nil {
		return nil, err
	}

	// Process all positional elements in order
	positionalArgs := make([]core.Value, 0)
	for _, elem := range argInfo.Elements {
		if elem.IsKwUnpack {
			// Handle **kwargs unpacking later
			continue
		}

		if elem.IsUnpack {
			// Evaluate the expression to unpack
			val, err := Eval(elem.Expr, ctx)
			if err != nil {
				return nil, err
			}

			// Unpack based on type
			switch v := val.(type) {
			case core.ListValue:
				positionalArgs = append(positionalArgs, []core.Value(v)...)
			case core.TupleValue:
				positionalArgs = append(positionalArgs, []core.Value(v)...)
			case *core.SetValue:
				// Sets are not directly iterable for unpacking in Python either
				return nil, fmt.Errorf("* unpacking does not support sets, convert to list first")
			case core.StringValue:
				// String unpacking: each character becomes an argument
				for _, ch := range string(v) {
					positionalArgs = append(positionalArgs, core.StringValue(string(ch)))
				}
			default:
				// Try to iterate
				if iterable, ok := val.(core.Iterable); ok {
					iter := iterable.Iterator()
					for {
						item, ok := iter.Next()
						if !ok {
							break
						}
						positionalArgs = append(positionalArgs, item)
					}
				} else {
					return nil, fmt.Errorf("* unpacking requires an iterable, got %s", val.Type())
				}
			}
		} else {
			// Regular positional argument
			evalArg, err := Eval(elem.Expr, ctx)
			if err != nil {
				return nil, err
			}
			positionalArgs = append(positionalArgs, evalArg)
		}
	}

	// Evaluate keyword arguments
	keywordArgs := make(map[string]core.Value)
	for name, expr := range argInfo.KeywordExprs {
		evalArg, err := Eval(expr, ctx)
		if err != nil {
			return nil, err
		}
		keywordArgs[name] = evalArg
	}

	// Handle **kwargs unpacking
	for _, elem := range argInfo.Elements {
		if !elem.IsKwUnpack {
			continue
		}

		// Evaluate the expression to unpack
		val, err := Eval(elem.Expr, ctx)
		if err != nil {
			return nil, err
		}

		// Must be a dict
		dict, ok := val.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("** unpacking requires a dict, got %s", val.Type())
		}

		// Unpack the dict into keyword arguments
		for _, keyRepr := range dict.Keys() {
			val, _ := dict.Get(keyRepr)

			// For keyword arguments, we need the original key which should be a string
			origKeys := dict.OriginalKeys()
			var paramName string
			found := false

			// Find the original key that corresponds to this internal key
			for _, origKey := range origKeys {
				if core.ValueToKey(origKey) == keyRepr {
					// Original key must be a string for keyword arguments
					keyStr, ok := origKey.(core.StringValue)
					if !ok {
						return nil, fmt.Errorf("** unpacking requires string keys, got %s", origKey.Type())
					}
					paramName = string(keyStr)
					found = true
					break
				}
			}

			if !found {
				return nil, fmt.Errorf("internal error: could not find original key for %s", keyRepr)
			}

			// Check for duplicates
			if _, exists := keywordArgs[paramName]; exists {
				return nil, fmt.Errorf("duplicate keyword argument: %s", paramName)
			}

			keywordArgs[paramName] = val
		}
	}

	// Check if it's a UserFunction that supports keyword arguments
	if userFunc, ok := fn.(*UserFunction); ok && userFunc.signature != nil {
		// Create a new context for the function
		funcEnv := core.NewContext(userFunc.env)

		// Debug: Print what we're passing
		// fmt.Printf("DEBUG: Passing %d positional args and %d keyword args\n", len(positionalArgs), len(keywordArgs))
		// for k, v := range keywordArgs {
		//     fmt.Printf("  kwarg %s = %v\n", k, v)
		// }

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

	// Prefer the symbol name if we have it (for better error messages)
	if symbolName != "" {
		funcName = symbolName
	} else {
		// Fall back to introspecting the function object
		switch f := fn.(type) {
		case core.SymbolValue:
			funcName = string(f)
		case *core.BuiltinFunction:
			if nameVal, ok := f.GetAttr("__name__"); ok {
				if nameStr, ok := nameVal.(core.StringValue); ok {
					funcName = string(nameStr)
				} else {
					funcName = "<builtin>"
				}
			} else {
				funcName = "<builtin>"
			}
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
	}

	ctx.PushStack(funcName, "", 0, 0)
	defer ctx.PopStack()

	result, err := callable.Call(positionalArgs, ctx)
	if err != nil {
		// Check if this is already a well-formatted error (e.g., from assert)
		// Don't wrap errors that are already EvalError or have specific messages
		if _, isEvalError := err.(*core.EvalError); isEvalError {
			return nil, err
		}

		// Special handling for assert - preserve its custom error messages
		if funcName == "assert" {
			return nil, core.WrapEvalError(err, err.Error(), ctx)
		}

		// Include the underlying error message for better debugging
		return nil, core.WrapEvalError(err, fmt.Sprintf("error in %s: %v", funcName, err), ctx)
	}

	return result, nil
}

package eval

import (
	"fmt"
	"github.com/mmichie/m28/core"
	"strconv"
	"strings"
)

// ParameterInfo holds information about a function parameter
type ParameterInfo struct {
	Name         core.SymbolValue
	DefaultValue core.Value // nil if no default
	HasDefault   bool
	KeywordOnly  bool // true if parameter can only be passed by keyword (after *)
}

// FunctionSignature holds the full signature of a function
type FunctionSignature struct {
	RequiredParams []ParameterInfo   // Parameters without defaults
	OptionalParams []ParameterInfo   // Parameters with defaults
	RestParam      *core.SymbolValue // *args parameter (nil if none)
	KeywordParam   *core.SymbolValue // **kwargs parameter (nil if none)
}

// MinArgs returns the minimum number of arguments required
func (sig *FunctionSignature) MinArgs() int {
	return len(sig.RequiredParams)
}

// MaxArgs returns the maximum number of positional arguments (-1 for unlimited)
func (sig *FunctionSignature) MaxArgs() int {
	if sig.RestParam != nil {
		return -1
	}
	return len(sig.RequiredParams) + len(sig.OptionalParams)
}

// ParseParameterList parses a parameter list into a FunctionSignature
// Supports: (a b c=10 d=20 *args **kwargs)
func ParseParameterList(paramList []core.Value) (*FunctionSignature, error) {

	// First, transform Python-style parameters (name=value) to M28 style ((name value))
	// Also handle (* args) and (** kwargs) parsed as separate tokens
	transformedParams := make([]core.Value, 0, len(paramList))

	i := 0
	for i < len(paramList) {
		param := paramList[i]

		// Check if this is a varargs marker (* or **)
		if sym, ok := param.(core.SymbolValue); ok {
			symStr := string(sym)

			// Handle (* args) - varargs parameter split into two tokens
			// vs bare * (keyword-only separator)
			if symStr == "*" && i+1 < len(paramList) {
				if nextSym, ok := paramList[i+1].(core.SymbolValue); ok {
					// Check if this is bare * (keyword-only separator) or *args (varargs)
					// Heuristic: if there are more parameters after the next symbol,
					// or if the next-next token is **, then * is a separator
					hasMoreParams := i+2 < len(paramList)
					if hasMoreParams {
						// Check if next-next is ** (would make this: *, param, **kwargs)
						if i+2 < len(paramList) {
							if nextNext, ok := paramList[i+2].(core.SymbolValue); ok && string(nextNext) == "**" {
								// Bare * separator: *, param, **kwargs
								transformedParams = append(transformedParams, sym) // Keep bare *
								i++                                                // Skip just the *
								continue
							}
						}
						// Bare * separator: *, param1, param2, ...
						transformedParams = append(transformedParams, sym) // Keep bare *
						i++                                                // Skip just the *
						continue
					}
					// This is *args style (last parameter or before **)
					transformedParams = append(transformedParams, core.SymbolValue("*"+string(nextSym)))
					i += 2 // Skip both * and args
					continue
				}
			}

			// Handle (** kwargs) - keyword args parameter split into two tokens
			if symStr == "**" && i+1 < len(paramList) {
				if nextSym, ok := paramList[i+1].(core.SymbolValue); ok {
					// Combine into single **kwargs symbol
					transformedParams = append(transformedParams, core.SymbolValue("**"+string(nextSym)))
					i += 2 // Skip both ** and kwargs
					continue
				}
			}
		}

		// Check if this is a symbol ending with = (parser creates "name=" as one symbol)
		if sym, ok := param.(core.SymbolValue); ok {
			symStr := string(sym)

			// Check if this symbol contains = (e.g., "c=3")
			if idx := strings.Index(symStr, "="); idx > 0 && idx < len(symStr)-1 {
				// This is Python-style combined: name=value in one symbol
				// Split it and transform to M28 style: (name value)
				paramName := core.SymbolValue(symStr[:idx])
				valueStr := symStr[idx+1:]

				// Parse the value - could be a number or other literal
				var defaultValue core.Value
				if num, err := strconv.ParseFloat(valueStr, 64); err == nil {
					defaultValue = core.NumberValue(num)
				} else if valueStr == "true" || valueStr == "false" {
					defaultValue = core.BoolValue(valueStr == "true")
				} else if valueStr == "nil" || valueStr == "None" {
					defaultValue = core.Nil
				} else if len(valueStr) >= 2 && valueStr[0] == '"' && valueStr[len(valueStr)-1] == '"' {
					// String literal
					defaultValue = core.StringValue(valueStr[1 : len(valueStr)-1])
				} else {
					// Treat as symbol
					defaultValue = core.SymbolValue(valueStr)
				}

				defaultParam := core.NewList(paramName, defaultValue)
				transformedParams = append(transformedParams, defaultParam)
				i++
				continue
			}

			if len(symStr) > 1 && symStr[len(symStr)-1] == '=' && i+1 < len(paramList) {
				// This is Python-style: name= value
				// Transform to M28 style: (name value)
				paramName := core.SymbolValue(symStr[:len(symStr)-1]) // Remove the =
				defaultParam := core.NewList(paramName, paramList[i+1])
				transformedParams = append(transformedParams, defaultParam)
				i += 2 // Skip name= and value
				continue
			}
		}

		// Also check for separated form: name = value
		if sym, ok := param.(core.SymbolValue); ok && i+2 < len(paramList) {
			if eq, ok := paramList[i+1].(core.SymbolValue); ok && string(eq) == "=" {
				// This is Python-style: name = value
				// Transform to M28 style: (name value)
				defaultParam := core.NewList(sym, paramList[i+2])
				transformedParams = append(transformedParams, defaultParam)
				i += 3 // Skip name, =, and value
				continue
			}
		}

		// Not a default parameter, keep as-is
		transformedParams = append(transformedParams, param)
		i++
	}

	sig := &FunctionSignature{
		RequiredParams: []ParameterInfo{},
		OptionalParams: []ParameterInfo{},
	}

	seenDefault := false
	seenRest := false
	seenKeyword := false
	keywordOnly := false // Track if we're after * (keyword-only separator)

	for _, param := range transformedParams {
		switch p := param.(type) {
		case core.SymbolValue:
			name := string(p)

			// Check for bare * (keyword-only separator)
			if name == "*" {
				// Following parameters are keyword-only
				keywordOnly = true
				continue
			}

			// Check for special parameters
			if name == "*args" || (len(name) > 1 && name[0] == '*' && name[1] != '*') {
				if seenRest {
					return nil, fmt.Errorf("multiple *args parameters not allowed")
				}
				if seenKeyword {
					return nil, fmt.Errorf("*args must come before **kwargs")
				}
				seenRest = true
				keywordOnly = true                      // Parameters after *args are also keyword-only
				paramName := core.SymbolValue(name[1:]) // Remove *
				if paramName == "" {
					paramName = "args"
				}
				sig.RestParam = &paramName
				continue
			}

			if name == "**kwargs" || (len(name) > 2 && name[0] == '*' && name[1] == '*') {
				if seenKeyword {
					return nil, fmt.Errorf("multiple **kwargs parameters not allowed")
				}
				seenKeyword = true
				paramName := core.SymbolValue(name[2:]) // Remove **
				if paramName == "" {
					paramName = "kwargs"
				}
				sig.KeywordParam = &paramName
				continue
			}

			// Regular parameter without default
			// In Python, you can have required parameters after optional ones if they're keyword-only (after *)
			if seenDefault && !seenRest && !keywordOnly {
				return nil, fmt.Errorf("non-default parameter %s after default parameter", name)
			}

			sig.RequiredParams = append(sig.RequiredParams, ParameterInfo{
				Name:        p,
				HasDefault:  false,
				KeywordOnly: keywordOnly,
			})

		case *core.ListValue:
			// Check if this is a varargs parameter parsed as (* args) or (** kwargs)
			if p.Len() == 2 {
				if star, ok := p.Items()[0].(core.SymbolValue); ok && string(star) == "*" {
					// This is (*args) - varargs parameter
					if argSym, ok := p.Items()[1].(core.SymbolValue); ok {
						if seenRest {
							return nil, fmt.Errorf("multiple *args parameters not allowed")
						}
						if seenKeyword {
							return nil, fmt.Errorf("*args must come before **kwargs")
						}
						seenRest = true
						sig.RestParam = &argSym
						continue
					}
				}
				if dstar, ok := p.Items()[0].(core.SymbolValue); ok && string(dstar) == "**" {
					// This is (**kwargs) - keyword args parameter
					if kwargSym, ok := p.Items()[1].(core.SymbolValue); ok {
						if seenKeyword {
							return nil, fmt.Errorf("multiple **kwargs parameters not allowed")
						}
						seenKeyword = true
						sig.KeywordParam = &kwargSym
						continue
					}
				}
			}

			// Parameter with default: (name default-value)
			if p.Len() != 2 {
				return nil, fmt.Errorf("invalid parameter with default: expected (name value)")
			}

			sym, ok := p.Items()[0].(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("parameter name must be a symbol, got %T", p.Items()[0])
			}

			seenDefault = true
			sig.OptionalParams = append(sig.OptionalParams, ParameterInfo{
				Name:         sym,
				DefaultValue: p.Items()[1], // Store the unevaluated default expression
				HasDefault:   true,
				KeywordOnly:  keywordOnly,
			})

		default:
			return nil, fmt.Errorf("invalid parameter: expected symbol or (symbol default), got %T", param)
		}
	}

	// fmt.Printf("DEBUG ParseParameterList result: %d required, %d optional, RestParam=%v, KeywordParam=%v\n",
	// 	len(sig.RequiredParams), len(sig.OptionalParams), sig.RestParam, sig.KeywordParam)
	return sig, nil
}

// BindArguments binds arguments to parameters according to the signature
func (sig *FunctionSignature) BindArguments(args []core.Value, kwargs map[string]core.Value, evalCtx *core.Context, bindCtx *core.Context) error {
	// Track which parameters have been bound
	boundParams := make(map[string]bool)
	argIndex := 0

	// Debug: print signature info
	// fmt.Printf("DEBUG BindArguments: %d args, %d kwargs, %d required, %d optional params\n",
	// 	len(args), len(kwargs), len(sig.RequiredParams), len(sig.OptionalParams))
	// for i, arg := range args {
	// 	fmt.Printf("  arg[%d] = %T: %v\n", i, arg, arg)
	// }
	// for k, v := range kwargs {
	// 	fmt.Printf("  kwarg %s = %T: %v\n", k, v, v)
	// }
	// for _, p := range sig.RequiredParams {
	// 	fmt.Printf("  Required: %s\n", p.Name)
	// }
	// for _, p := range sig.OptionalParams {
	// 	fmt.Printf("  Optional: %s (default=%v)\n", p.Name, p.DefaultValue)
	// }
	// if sig.RestParam != nil {
	// 	fmt.Printf("  *args: %s\n", *sig.RestParam)
	// }
	// if sig.KeywordParam != nil {
	// 	fmt.Printf("  **kwargs: %s\n", *sig.KeywordParam)
	// }

	// 1. Bind required parameters
	for _, param := range sig.RequiredParams {
		paramName := string(param.Name)

		// Check if provided as keyword argument
		if kwValue, ok := kwargs[paramName]; ok {
			bindCtx.Define(paramName, kwValue)
			boundParams[paramName] = true
			delete(kwargs, paramName)
		} else if param.KeywordOnly {
			// Keyword-only parameters cannot be bound from positional arguments
			return fmt.Errorf("missing required keyword-only argument: %s", paramName)
		} else if argIndex < len(args) {
			// Bind from positional arguments (only for non-keyword-only params)
			bindCtx.Define(paramName, args[argIndex])
			boundParams[paramName] = true
			argIndex++
		} else {
			// Debug for missing argument issues - print stack trace
			fmt.Printf("[DEBUG BindArguments] Missing required argument: %s\n", paramName)
			fmt.Printf("[DEBUG BindArguments] args=%d, argIndex=%d\n", len(args), argIndex)
			fmt.Printf("[DEBUG BindArguments] RequiredParams=%v\n", func() []string {
				names := make([]string, len(sig.RequiredParams))
				for i, p := range sig.RequiredParams {
					names[i] = string(p.Name)
				}
				return names
			}())
			fmt.Printf("[DEBUG BindArguments] Provided args:\n")
			for i, arg := range args {
				fmt.Printf("  args[%d]: %T = %v\n", i, arg, arg)
			}
			return fmt.Errorf("missing required argument: %s", paramName)
		}
	}

	// 2. Bind optional parameters with defaults
	for _, param := range sig.OptionalParams {
		paramName := string(param.Name)

		// Check if provided as keyword argument
		if kwValue, ok := kwargs[paramName]; ok {
			bindCtx.Define(paramName, kwValue)
			boundParams[paramName] = true
			delete(kwargs, paramName)
		} else if !param.KeywordOnly && argIndex < len(args) {
			// Bind from positional arguments (only for non-keyword-only params)
			bindCtx.Define(paramName, args[argIndex])
			boundParams[paramName] = true
			argIndex++
		} else {
			// Use default value - evaluate it in the evaluation context
			// This ensures access to builtins and outer scope
			defaultVal, err := Eval(param.DefaultValue, evalCtx)
			if err != nil {
				return fmt.Errorf("error evaluating default value for %s: %v", paramName, err)
			}
			bindCtx.Define(paramName, defaultVal)
			boundParams[paramName] = true
		}
	}

	// 3. Collect remaining positional arguments into *args if present
	if sig.RestParam != nil {
		restArgs := make([]core.Value, 0)
		for ; argIndex < len(args); argIndex++ {
			restArgs = append(restArgs, args[argIndex])
		}
		bindCtx.Define(string(*sig.RestParam), core.NewList(restArgs...))
	} else if argIndex < len(args) {
		// Too many positional arguments
		return fmt.Errorf("too many positional arguments: expected at most %d, got %d",
			len(sig.RequiredParams)+len(sig.OptionalParams), len(args))
	}

	// 4. Collect remaining keyword arguments into **kwargs if present
	if sig.KeywordParam != nil {
		kwargsDict := core.NewDict()
		for k, v := range kwargs {
			kwargsDict.Set(k, v)
		}
		bindCtx.Define(string(*sig.KeywordParam), kwargsDict)
	} else if len(kwargs) > 0 {
		// Unexpected keyword arguments
		for k := range kwargs {
			return fmt.Errorf("unexpected keyword argument: %s", k)
		}
	}

	return nil
}

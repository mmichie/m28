package eval

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// ParameterInfo holds information about a function parameter
type ParameterInfo struct {
	Name         core.SymbolValue
	DefaultValue core.Value // nil if no default
	HasDefault   bool
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
func ParseParameterList(paramList core.ListValue) (*FunctionSignature, error) {
	// First, transform Python-style parameters (name=value) to M28 style ((name value))
	transformedParams := make(core.ListValue, 0, len(paramList))
	
	i := 0
	for i < len(paramList) {
		param := paramList[i]
		
		// Check if this is a symbol ending with = (parser creates "name=" as one symbol)
		if sym, ok := param.(core.SymbolValue); ok {
			symStr := string(sym)
			if len(symStr) > 1 && symStr[len(symStr)-1] == '=' && i+1 < len(paramList) {
				// This is Python-style: name= value
				// Transform to M28 style: (name value)
				paramName := core.SymbolValue(symStr[:len(symStr)-1]) // Remove the =
				defaultParam := core.ListValue{paramName, paramList[i+1]}
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
				defaultParam := core.ListValue{sym, paramList[i+2]}
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

	for _, param := range transformedParams {
		switch p := param.(type) {
		case core.SymbolValue:
			name := string(p)

			// Check for special parameters
			if name == "*args" || (len(name) > 1 && name[0] == '*' && name[1] != '*') {
				if seenRest {
					return nil, fmt.Errorf("multiple *args parameters not allowed")
				}
				if seenKeyword {
					return nil, fmt.Errorf("*args must come before **kwargs")
				}
				seenRest = true
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
			if seenDefault && !seenRest {
				return nil, fmt.Errorf("non-default parameter %s after default parameter", name)
			}

			sig.RequiredParams = append(sig.RequiredParams, ParameterInfo{
				Name:       p,
				HasDefault: false,
			})

		case core.ListValue:
			// Parameter with default: (name default-value)
			if len(p) != 2 {
				return nil, fmt.Errorf("invalid parameter with default: expected (name value)")
			}

			sym, ok := p[0].(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("parameter name must be a symbol, got %T", p[0])
			}

			seenDefault = true
			sig.OptionalParams = append(sig.OptionalParams, ParameterInfo{
				Name:         sym,
				DefaultValue: p[1], // Store the unevaluated default expression
				HasDefault:   true,
			})

		default:
			return nil, fmt.Errorf("invalid parameter: expected symbol or (symbol default), got %T", param)
		}
	}

	return sig, nil
}

// BindArguments binds arguments to parameters according to the signature
func (sig *FunctionSignature) BindArguments(args []core.Value, kwargs map[string]core.Value, evalCtx *core.Context, bindCtx *core.Context) error {
	// Track which parameters have been bound
	boundParams := make(map[string]bool)
	argIndex := 0

	// 1. Bind required positional parameters
	for _, param := range sig.RequiredParams {
		paramName := string(param.Name)

		// Check if provided as keyword argument
		if kwValue, ok := kwargs[paramName]; ok {
			bindCtx.Define(paramName, kwValue)
			boundParams[paramName] = true
			delete(kwargs, paramName)
		} else if argIndex < len(args) {
			// Bind from positional arguments
			bindCtx.Define(paramName, args[argIndex])
			boundParams[paramName] = true
			argIndex++
		} else {
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
		} else if argIndex < len(args) {
			// Bind from positional arguments
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
		restArgs := core.ListValue{}
		for ; argIndex < len(args); argIndex++ {
			restArgs = append(restArgs, args[argIndex])
		}
		bindCtx.Define(string(*sig.RestParam), restArgs)
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

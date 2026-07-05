package eval

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

// ParameterInfo holds information about a function parameter
type ParameterInfo struct {
	Name         core.SymbolValue
	DefaultValue core.Value // nil if no default
	HasDefault   bool
	// DefaultEvaluated marks DefaultValue as the def-time evaluated value
	// (Python semantics: defaults are computed once in the defining scope).
	// When false, legacy paths still hold the unevaluated expression.
	DefaultEvaluated bool
	KeywordOnly      bool // true if parameter can only be passed by keyword (after *)
	PositionalOnly   bool // true if parameter can only be passed positionally (before /, PEP 570)
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
// EvaluateSignatureDefaults evaluates default-parameter expressions once in
// the defining context — Python semantics: `def f(x=[])` computes the default
// at definition time and shares it across calls (the previous per-call
// re-evaluation broke identity-sensitive defaults like functools' kwd_mark
// sentinel, silently defeating every lru_cache keyword hit).
func EvaluateSignatureDefaults(sig *FunctionSignature, ctx *core.Context) error {
	for i := range sig.OptionalParams {
		p := &sig.OptionalParams[i]
		if !p.HasDefault || p.DefaultValue == nil || p.DefaultEvaluated {
			continue
		}
		expr := p.DefaultValue
		if located, ok := expr.(core.LocatedValue); ok {
			expr = located.Unwrap()
		}
		v, err := Eval(expr, ctx)
		if err != nil {
			// Defer to call-time evaluation (legacy behavior) instead of
			// failing the definition. CPython would raise here, but M28's
			// module system resolves `from . import sub` lazily, so during
			// a circular package init a default like `_parser.TYPE_FLAGS`
			// (re/_compiler.py) is not yet resolvable at def time even
			// though it will be by first call. Tracked as the eager-
			// submodule-import gap; when that lands this fallback can
			// become a hard error.
			continue
		}
		p.DefaultValue = v
		p.DefaultEvaluated = true
	}
	return nil
}

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
					nextStr := string(nextSym)

					// If the next symbol looks like a special marker (starts with * or **),
					// then this is just a bare * separator
					if strings.HasPrefix(nextStr, "*") {
						transformedParams = append(transformedParams, sym) // Keep bare *
						i++                                                // Skip just the *
						continue
					}

					// Check if this is bare * (keyword-only separator) or *args (varargs)
					// NEW HEURISTIC: If the next symbol looks like a regular parameter name
					// (doesn't contain =, isn't a known special form), treat * as separator
					// The only way to have *args is to write it as a single token "*args"
					// or to have it as the conventional name "args" after *

					// Check for varargs pattern vs keyword-only separator
					// Key distinction:
					// - varargs: def f(*args) or def f(*args, **kwargs) - no regular params after *
					// - separator: def f(a, *, b) or def f(a, *, b, c) - has regular params after *
					//
					// Heuristic:
					// 1. If there are regular (non-special) params BEFORE *, treat as separator
					// 2. Otherwise (*, x) is first/only param → treat as varargs

					hasRegularParamsBefore := false
					for j := 0; j < i; j++ {
						if p, ok := paramList[j].(core.SymbolValue); ok {
							ps := string(p)
							// Check if it's a regular param (not * or **)
							if !strings.HasPrefix(ps, "*") {
								hasRegularParamsBefore = true
								break
							}
						} else {
							// List parameter (param with default) counts as regular
							hasRegularParamsBefore = true
							break
						}
					}

					// Count regular params after * (up to a **kwargs). A keyword-only
					// separator can be followed by several params (def f(*, k1, k2));
					// *args varargs is a single collector. So 2+ following params
					// means this is unambiguously a separator, even when * is first
					// (def f(*, k1, k2) — no params before *).
					regularParamsAfter := 0
					for j := i + 1; j < len(paramList); j++ {
						if p, ok := paramList[j].(core.SymbolValue); ok && strings.HasPrefix(string(p), "**") {
							break
						}
						regularParamsAfter++
					}

					isArgsPattern := false
					if !hasRegularParamsBefore && regularParamsAfter < 2 {
						// No regular params before * and a single following name →
						// varargs like def f(*args).
						isArgsPattern = true
					}

					if isArgsPattern {
						// This is *args style
						transformedParams = append(transformedParams, core.SymbolValue("*"+nextStr))
						i += 2 // Skip both * and args
						continue
					}

					// Otherwise, treat as bare * separator (keyword-only marker)
					transformedParams = append(transformedParams, sym) // Keep bare *
					i++                                                // Skip just the *
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
				} else if len(valueStr) >= 2 && valueStr[0] == '"' && valueStr[len(valueStr)-1] == '"' {
					// String literal
					defaultValue = core.StringValue(valueStr[1 : len(valueStr)-1])
				} else {
					// Treat as symbol (including "nil" and "None")
					// This ensures None evaluates to the singleton core.None, not a new NilValue instance
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

			// Check for / (positional-only separator, PEP 570)
			if name == "/" {
				// All previous parameters are now positional-only
				// Mark them by updating the already-parsed params
				for i := range sig.RequiredParams {
					sig.RequiredParams[i].PositionalOnly = true
				}
				for i := range sig.OptionalParams {
					sig.OptionalParams[i].PositionalOnly = true
				}
				continue
			}

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
				// Use smart accessor to auto-unwrap LocatedValue
				if star, ok := p.GetItemAsSymbol(0); ok && string(star) == "*" {
					// This is (*args) - varargs parameter
					if argSym, ok := p.GetItemAsSymbol(1); ok {
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
				// Use smart accessor to auto-unwrap LocatedValue
				if dstar, ok := p.GetItemAsSymbol(0); ok && string(dstar) == "**" {
					// This is (**kwargs) - keyword args parameter
					if kwargSym, ok := p.GetItemAsSymbol(1); ok {
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

			// Use smart accessor to auto-unwrap LocatedValue
			sym, ok := p.GetItemAsSymbol(0)
			if !ok {
				item, _ := p.GetItemUnwrapped(0)
				return nil, fmt.Errorf("parameter name must be a symbol, got %T", item)
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

	// 	len(sig.RequiredParams), len(sig.OptionalParams), sig.RestParam, sig.KeywordParam)
	return sig, nil
}

// BindArguments binds arguments to parameters according to the signature
// tooManyPositionalError builds CPython's TypeError for passing too many
// positional arguments, e.g. "f() takes from 1 to 2 positional arguments but 3
// were given" (or "f() takes 2 positional arguments but 3 were given" when there
// are no optional positional params).
func tooManyPositionalError(funcName string, sig *FunctionSignature, got int) *core.TypeError {
	if funcName == "" {
		funcName = "<lambda>"
	}
	minPos := 0
	for _, p := range sig.RequiredParams {
		if !p.KeywordOnly {
			minPos++
		}
	}
	maxPos := minPos
	for _, p := range sig.OptionalParams {
		if !p.KeywordOnly {
			maxPos++
		}
	}

	var takes string
	if minPos == maxPos {
		plural := "s"
		if maxPos == 1 {
			plural = ""
		}
		takes = fmt.Sprintf("%d positional argument%s", maxPos, plural)
	} else {
		takes = fmt.Sprintf("from %d to %d positional arguments", minPos, maxPos)
	}
	were := "were"
	if got == 1 {
		were = "was"
	}
	return &core.TypeError{Message: fmt.Sprintf("%s() takes %s but %d %s given", funcName, takes, got, were)}
}

// callableName returns funcName or a placeholder for anonymous functions.
func callableName(funcName string) string {
	if funcName == "" {
		return "<lambda>"
	}
	return funcName
}

// missingRequiredError builds CPython's "<name>() missing N required positional
// argument(s): 'a' and 'b'" TypeError.
func missingRequiredError(funcName string, missing []string) *core.TypeError {
	n := len(missing)
	quoted := make([]string, n)
	for i, m := range missing {
		quoted[i] = "'" + m + "'"
	}
	var names string
	switch n {
	case 1:
		names = quoted[0]
	case 2:
		names = quoted[0] + " and " + quoted[1]
	default:
		names = strings.Join(quoted[:n-1], ", ") + ", and " + quoted[n-1]
	}
	plural := "s"
	if n == 1 {
		plural = ""
	}
	return &core.TypeError{Message: fmt.Sprintf("%s() missing %d required positional argument%s: %s", callableName(funcName), n, plural, names)}
}

func (sig *FunctionSignature) BindArguments(funcName string, args []core.Value, kwargs *core.Kwargs, evalCtx *core.Context, bindCtx *core.Context) error {
	// Track which parameters have been bound
	boundParams := make(map[string]bool)
	argIndex := 0

	// Names of keyword arguments consumed by named parameters. Binding must not
	// mutate the caller's kwargs: the same collection may be passed to both
	// __new__ and __init__, and partial() reuses its stored keywords per call.
	var consumedKw map[string]bool
	if kwargs.Len() > 0 {
		consumedKw = make(map[string]bool, kwargs.Len())
	}

	// Calculate max positional arguments (excluding keyword-only params)
	maxPositional := 0
	for _, p := range sig.RequiredParams {
		if !p.KeywordOnly {
			maxPositional++
		}
	}
	for _, p := range sig.OptionalParams {
		if !p.KeywordOnly {
			maxPositional++
		}
	}

	// Check for too many positional arguments early (unless we have *args)
	if sig.RestParam == nil && len(args) > maxPositional {
		return tooManyPositionalError(funcName, sig, len(args))
	}

	// Debug: print signature info
	// NOTE: Disabled to avoid infinite recursion when printing Instance arguments
	// The recursion: printf -> Instance.String -> GetAttr -> NewBuiltinFunction -> String...
	debugBind := os.Getenv("M28_DEBUG_BIND") != ""
	if debugBind {
		core.Log.Debug(core.SubsystemEval, "Binding arguments to function signature",
			"args_count", len(args), "kwargs_count", kwargs.Len(),
			"required_params", len(sig.RequiredParams), "optional_params", len(sig.OptionalParams))
		// Print only types, not values, to avoid recursion
		for i, arg := range args {
			core.Log.Debug(core.SubsystemEval, "Positional argument", "index", i, "type", fmt.Sprintf("%T", arg))
		}
		for _, e := range kwargs.Entries() {
			core.Log.Debug(core.SubsystemEval, "Keyword argument", "name", e.Name)
		}
		for _, p := range sig.RequiredParams {
			core.Log.Debug(core.SubsystemEval, "Required parameter", "name", p.Name)
		}
		for _, p := range sig.OptionalParams {
			core.Log.Debug(core.SubsystemEval, "Optional parameter", "name", p.Name)
		}
		if sig.RestParam != nil {
			core.Log.Debug(core.SubsystemEval, "Variadic parameter (*args)", "name", *sig.RestParam)
		}
		if sig.KeywordParam != nil {
			core.Log.Debug(core.SubsystemEval, "Keyword parameter (**kwargs)", "name", *sig.KeywordParam)
		}
	}

	// 1. Bind required parameters
	var missingRequired []string
	for _, param := range sig.RequiredParams {
		paramName := string(param.Name)

		// Check if provided as keyword argument
		kwValue, hasKw := kwargs.Get(paramName)
		if hasKw && param.PositionalOnly {
			// PEP 570: a keyword that matches a positional-only parameter name
			// is only an error if there is no **kwargs to absorb it. Otherwise
			// the keyword is left in kwargs (collected by **kwargs) and the
			// parameter is bound positionally / from its default.
			if sig.KeywordParam == nil {
				return &core.TypeError{
					Message: fmt.Sprintf("'%s' is a positional-only parameter", paramName),
				}
			}
			hasKw = false
		}
		// A non-keyword-only parameter whose positional slot is also filled and
		// that is given by keyword gets multiple values (CPython TypeError).
		if hasKw && !param.KeywordOnly && argIndex < len(args) {
			return &core.TypeError{Message: fmt.Sprintf("%s() got multiple values for argument '%s'", callableName(funcName), paramName)}
		}
		if hasKw {
			bindCtx.Define(paramName, kwValue)
			boundParams[paramName] = true
			consumedKw[paramName] = true
		} else if param.KeywordOnly {
			// Keyword-only parameters cannot be bound from positional arguments
			return &core.TypeError{
				Message: fmt.Sprintf("missing required keyword-only argument: %s", paramName),
			}
		} else if argIndex < len(args) {
			// Bind from positional arguments (only for non-keyword-only params)
			bindCtx.Define(paramName, args[argIndex])
			boundParams[paramName] = true
			argIndex++
		} else {
			// Collect all missing required positional args for one CPython-style
			// "missing N required positional arguments" error.
			missingRequired = append(missingRequired, paramName)
		}
	}
	if len(missingRequired) > 0 {
		return missingRequiredError(funcName, missingRequired)
	}

	// 2. Bind optional parameters with defaults
	for _, param := range sig.OptionalParams {
		paramName := string(param.Name)

		// Check if provided as keyword argument
		kwValue, hasKw := kwargs.Get(paramName)
		if hasKw && param.PositionalOnly {
			// PEP 570: see note above — leave the keyword for **kwargs unless
			// there is no **kwargs, in which case it is an error.
			if sig.KeywordParam == nil {
				return &core.TypeError{
					Message: fmt.Sprintf("'%s' is a positional-only parameter", paramName),
				}
			}
			hasKw = false
		}
		if hasKw && !param.KeywordOnly && argIndex < len(args) {
			return &core.TypeError{Message: fmt.Sprintf("%s() got multiple values for argument '%s'", callableName(funcName), paramName)}
		}
		if hasKw {
			bindCtx.Define(paramName, kwValue)
			boundParams[paramName] = true
			consumedKw[paramName] = true
		} else if !param.KeywordOnly && argIndex < len(args) {
			// Bind from positional arguments (only for non-keyword-only params)
			bindCtx.Define(paramName, args[argIndex])
			boundParams[paramName] = true
			argIndex++
		} else {
			// Use default value
			// If it's still unevaluated (symbol/expression), evaluate it now in function's env
			// If it was already evaluated at definition time, use it directly
			defaultVal := param.DefaultValue
			if param.DefaultEvaluated {
				bindCtx.Define(paramName, defaultVal)
				boundParams[paramName] = true
				continue
			}

			// Unwrap LocatedValue if present (Python AST wraps values with source location)
			if located, ok := defaultVal.(core.LocatedValue); ok {
				defaultVal = located.Unwrap()
			}

			// Check if it's an unevaluated symbol or expression
			if sym, isSymbol := defaultVal.(core.SymbolValue); isSymbol {
				// Unevaluated symbol - evaluate in function's env context
				var err error
				defaultVal, err = Eval(sym, evalCtx)
				if err != nil {
					return fmt.Errorf("error evaluating default value for %s: %v", paramName, err)
				}
			} else if list, isList := defaultVal.(*core.ListValue); isList {
				// Check if it's an unevaluated expression (list starting with an operator/function)
				// For now, treat all lists as potentially unevaluated expressions
				// This is conservative but safe
				var err error
				// 				fmt.Printf("[DEBUG BindArguments] Evaluating list default for %s: %v\n", paramName, core.PrintValue(list))
				defaultVal, err = Eval(list, evalCtx)
				if err != nil {
					// 					fmt.Printf("[DEBUG BindArguments] Error evaluating list default for %s: %v\n", paramName, err)
					// If evaluation fails, use the list as-is (it's probably a literal)
					defaultVal = list
				}
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
		// Python requires *args to be a tuple, not a list
		bindCtx.Define(string(*sig.RestParam), core.TupleValue(restArgs))
	} else if argIndex < len(args) {
		// Too many positional arguments (CPython raises TypeError).
		return tooManyPositionalError(funcName, sig, len(args))
	}

	// 4. Collect unconsumed keyword arguments into **kwargs if present. The
	// dict is always fresh (callee mutations must not alias the call site) and
	// preserves call-site keyword order (PEP 468).
	if sig.KeywordParam != nil {
		kwargsDict := core.NewDict()
		for _, e := range kwargs.Entries() {
			if consumedKw[e.Name] {
				continue
			}
			kwargsDict.SetStr(e.Name, e.Value)
		}
		bindCtx.Define(string(*sig.KeywordParam), kwargsDict)
	} else if kwargs.Len() > len(consumedKw) {
		// Unexpected keyword arguments (CPython raises TypeError). Report the
		// first unconsumed keyword in call order.
		for _, e := range kwargs.Entries() {
			if !consumedKw[e.Name] {
				return &core.TypeError{Message: fmt.Sprintf("%s() got an unexpected keyword argument '%s'", callableName(funcName), e.Name)}
			}
		}
	}

	return nil
}

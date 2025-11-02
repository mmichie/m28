package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// assignVariable handles variable assignment with global/nonlocal support
// It checks if the variable is declared as global or nonlocal and assigns accordingly
func assignVariable(ctx *core.Context, name string, value core.Value) error {
	// Check if the variable is declared as global in this scope
	if ctx.IsGlobal(name) {
		// Assign to the global context
		ctx.Global.Define(name, value)
		return nil
	}

	// Check if the variable is declared as nonlocal in this scope
	if ctx.IsNonlocal(name) {
		// Find the nearest enclosing scope that has this variable
		// We need to use Set() which searches up the scope chain
		if err := ctx.Outer.Set(name, value); err != nil {
			// If Set fails, it means the variable doesn't exist in any outer scope
			// This shouldn't happen because DeclareNonlocal validates this
			return fmt.Errorf("no binding for nonlocal '%s' found", name)
		}
		return nil
	}

	// Normal assignment: define in the current scope
	ctx.Define(name, value)
	return nil
}

// handleElifOrElse is a helper to process nested elif or else clauses
func handleElifOrElse(clause core.Value, ctx *core.Context) (core.Value, error) {
	// Check if it's another elif
	if list, ok := clause.(*core.ListValue); ok && list.Len() > 0 {
		if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "elif" {
			// Recursive elif
			if list.Len() < 3 {
				return nil, ErrArgCount("elif requires condition and expression")
			}

			// Evaluate condition
			cond, err := Eval(list.Items()[1], ctx)
			if err != nil {
				return nil, err
			}

			if core.IsTruthy(cond) {
				return Eval(list.Items()[2], ctx)
			}

			// Check for more elif/else
			if list.Len() > 3 {
				return handleElifOrElse(list.Items()[3], ctx)
			}

			return core.Nil, nil
		}
	}

	// It's an else expression
	return Eval(clause, ctx)
}

// IfForm provides the implementation of the if special form with elif support
func IfForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, ErrArgCount("if requires at least 2 arguments")
	}

	// Evaluate the first condition
	condition, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	// If truthy, evaluate and return the then-branch
	if core.IsTruthy(condition) {
		return Eval(args.Items()[1], ctx)
	}

	// Check for elif or else
	if args.Len() <= 2 {
		// No else clause, return nil
		return core.Nil, nil
	}

	// Check if the third argument is a list starting with elif
	if list, ok := args.Items()[2].(*core.ListValue); ok && list.Len() > 0 {
		if sym, ok := list.Items()[0].(core.SymbolValue); ok && string(sym) == "elif" {
			// Nested elif structure: (elif condition expr else-clause)
			if list.Len() < 3 {
				return nil, ErrArgCount("elif requires condition and expression")
			}

			// Evaluate elif condition
			elifCond, err := Eval(list.Items()[1], ctx)
			if err != nil {
				return nil, err
			}

			if core.IsTruthy(elifCond) {
				return Eval(list.Items()[2], ctx)
			}

			// Check if there's an else clause (4th element) or another elif
			if list.Len() > 3 {
				// The 4th element could be another elif or an else expression
				return handleElifOrElse(list.Items()[3], ctx)
			}

			// No else clause
			return core.Nil, nil
		}
	}

	// Check if the third argument is elif symbol (flat structure)
	if sym, ok := args.Items()[2].(core.SymbolValue); ok && string(sym) == "elif" {
		// Handle flat elif chain
		// Structure: (if cond1 expr1 elif cond2 expr2 elif cond3 expr3 else expr4)
		elifArgs := args.Items()[2:] // Skip "if" condition and expression

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
		return Eval(args.Items()[2], ctx)
	}
}

// DoForm provides the implementation of the do special form
func DoForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return core.Nil, nil
	}

	// Evaluate all expressions in sequence
	var result core.Value = core.Nil
	var err error

	for _, expr := range args.Items() {
		result, err = Eval(expr, ctx)
		if err != nil {
			return nil, err
		}

		// Check for return value - propagate it, don't unwrap!
		// The unwrapping should only happen at function boundaries
		if _, ok := result.(*ReturnValue); ok {
			return result, nil
		}
	}

	// Return the value of the last expression
	return result, nil
}

// evaluateDefaultValues evaluates all default values in a function signature
// This should be called at function definition time to match Python semantics
func evaluateDefaultValues(signature *FunctionSignature, ctx *core.Context) error {
	for i := range signature.OptionalParams {
		if signature.OptionalParams[i].DefaultValue != nil {
			// Skip evaluation for simple literals (numbers, strings, None, lists, dicts)
			// These don't need evaluation
			defaultVal := signature.OptionalParams[i].DefaultValue
			switch defaultVal.(type) {
			case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue,
				*core.ListValue, *core.DictValue, core.TupleValue:
				// Already evaluated, skip
				continue
			}

			evaluatedDefault, err := Eval(signature.OptionalParams[i].DefaultValue, ctx)
			if err != nil {
				// If evaluation fails, keep the unevaluated default
				// It will be evaluated at call time (lazy evaluation)
				// This handles forward references and circular dependencies
				continue
			}
			signature.OptionalParams[i].DefaultValue = evaluatedDefault
		}
	}
	return nil
}

// DefForm provides the implementation of the def special form
func DefForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, ErrArgCount("def requires at least 2 arguments")
	}

	// Check for alternative function definition form: (def (name args...) body...)
	if fnDef, ok := args.Items()[0].(*core.ListValue); ok && fnDef.Len() > 0 {
		// Get function name from the first element of the list
		name, ok := fnDef.Items()[0].(core.SymbolValue)
		if !ok {
			return nil, TypeError{Expected: "symbol for function name", Got: fnDef.Items()[0].Type()}
		}

		// Get parameter list (the rest of the elements after the name)
		paramListSlice := fnDef.Items()[1:]

		// Try to parse as new-style parameter list with defaults
		signature, err := ParseParameterList(paramListSlice)
		if err != nil {
			// Fall back to legacy simple parameter parsing
			params := make([]core.SymbolValue, 0, fnDef.Len()-1)
			for _, param := range fnDef.Items()[1:] {
				if sym, ok := param.(core.SymbolValue); ok {
					params = append(params, sym)
				} else {
					return nil, TypeError{Expected: "symbol", Got: param.Type()}
				}
			}

			// Create function body (implicit do)
			body := args.Items()[1:]
			var functionBody core.Value

			if len(body) == 1 {
				// Single expression body
				functionBody = body[0]
			} else {
				// Multi-expression body, wrap in do
				functionBody = core.NewList(append([]core.Value{core.SymbolValue("do")}, body...)...)
			}

			function := &UserFunction{
				BaseObject: *core.NewBaseObject(core.FunctionType),
				params:     params,
				body:       functionBody,
				env:        ctx,
				name:       string(name),
			}

			// Check if this is a generator function
			finalFunc := makeGeneratorFunction(function)

			ctx.Define(string(name), finalFunc)
			return finalFunc, nil
		}

		// Evaluate default values at definition time
		if err := evaluateDefaultValues(signature, ctx); err != nil {
			return nil, err
		}

		// Create function body (implicit do) for new-style function
		body := args.Items()[1:]
		var functionBody core.Value

		if len(body) == 1 {
			// Single expression body
			functionBody = body[0]
		} else {
			// Multi-expression body, wrap in do
			functionBody = core.NewList(append([]core.Value{core.SymbolValue("do")}, body...)...)
		}

		// Create the function with signature
		function := &UserFunction{
			BaseObject: *core.NewBaseObject(core.FunctionType),
			signature:  signature,
			body:       functionBody,
			env:        ctx,
			name:       string(name),
		}

		// Check if this is a generator function
		finalFunc := makeGeneratorFunction(function)

		ctx.Define(string(name), finalFunc)
		return finalFunc, nil
	}

	// Standard form: (def name ...)
	name, ok := args.Items()[0].(core.SymbolValue)
	if !ok {
		return nil, TypeError{Expected: "symbol", Got: args.Items()[0].Type()}
	}

	// Check different definition forms
	if args.Len() >= 3 {
		// Check for function definition: (def name (params) body...)
		// First form: (def name (arg1 arg2...) body...)
		if paramList, ok := args.Items()[1].(*core.ListValue); ok {
			// It's a function definition with parameter list

			// Try to parse as new-style parameter list with defaults
			signature, err := ParseParameterList(paramList.Items())
			if err != nil {
				// Fall back to legacy simple parameter parsing
				params := make([]core.SymbolValue, 0, paramList.Len())
				for _, param := range paramList.Items() {
					if sym, ok := param.(core.SymbolValue); ok {
						params = append(params, sym)
					} else {
						return nil, TypeError{Expected: "symbol", Got: param.Type()}
					}
				}

				// Create function body (implicit do)
				body := args.Items()[2:]
				var functionBody core.Value

				if len(body) == 1 {
					// Single expression body
					functionBody = body[0]
				} else {
					// Multi-expression body, wrap in do
					functionBody = core.NewList(append([]core.Value{core.SymbolValue("do")}, body...)...)
				}

				function := &UserFunction{
					BaseObject: *core.NewBaseObject(core.FunctionType),
					params:     params,
					body:       functionBody,
					env:        ctx,
					name:       string(name),
				}

				// Check if this is a generator function
				finalFunc := makeGeneratorFunction(function)

				ctx.Define(string(name), finalFunc)
				return finalFunc, nil
			}

			// New-style function with signature
			// Evaluate default values at definition time
			if err := evaluateDefaultValues(signature, ctx); err != nil {
				return nil, err
			}

			// Create function body (implicit do)
			body := args.Items()[2:]
			var functionBody core.Value

			if len(body) == 1 {
				// Single expression body
				functionBody = body[0]
			} else {
				// Multi-expression body, wrap in do
				functionBody = core.NewList(append([]core.Value{core.SymbolValue("do")}, body...)...)
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

			// Check if this is a generator function
			finalFunc := makeGeneratorFunction(function)

			ctx.Define(string(name), finalFunc)
			return finalFunc, nil
		}
	}

	// Second form: (def name value)
	// Check if it's a lambda/function value
	value, err := Eval(args.Items()[1], ctx)
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
func DictLiteralForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Create a new dictionary
	dict := core.NewDict()

	// Check if arguments are wrapped pairs or flat
	// Wrapped: (dict-literal (key1 val1) (key2 val2))
	// Flat: (dict-literal key1 val1 key2 val2)

	// If first arg is a ListValue with 2 elements, assume wrapped pairs
	if args.Len() > 0 {
		if pair, ok := args.Items()[0].(*core.ListValue); ok && pair.Len() == 2 {
			// Wrapped pairs format - used by Python keyword arguments
			// But check if ALL args are actually wrapped pairs - if not, fall through to flat format
			allWrapped := true
			for i := 0; i < args.Len(); i++ {
				arg := args.Items()[i]
				// Allow **unpack markers
				if sym, ok := arg.(core.SymbolValue); ok && string(sym) == "**unpack" {
					if i+1 < args.Len() {
						i++ // Skip the dict expression after **unpack
					}
					continue
				}
				// Check if it's a wrapped pair
				if pairList, ok := arg.(*core.ListValue); !ok || pairList.Len() != 2 {
					allWrapped = false
					break
				}
			}

			// If not all wrapped, treat as flat format
			if !allWrapped {
				// Fall through to flat format handling below
			} else {
				// Process as wrapped pairs
				for i := 0; i < args.Len(); i++ {
					arg := args.Items()[i]

					// Check for **unpack marker
					if sym, ok := arg.(core.SymbolValue); ok && string(sym) == "**unpack" {
						if i+1 >= args.Len() {
							return nil, fmt.Errorf("**unpack requires a dict expression")
						}

						// Evaluate the dict to unpack
						unpackVal, err := Eval(args.Items()[i+1], ctx)
						if err != nil {
							return nil, fmt.Errorf("error evaluating **unpack dict: %v", err)
						}

						// Must be a dict
						unpackDict, ok := unpackVal.(*core.DictValue)
						if !ok {
							return nil, fmt.Errorf("**unpack requires a dict, got %s", unpackVal.Type())
						}

						// Merge all key-value pairs from unpacked dict
						for _, keyRepr := range unpackDict.Keys() {
							val, _ := unpackDict.Get(keyRepr)
							origKeys := unpackDict.OriginalKeys()
							for _, origKey := range origKeys {
								if core.ValueToKey(origKey) == keyRepr {
									dict.SetWithKey(keyRepr, origKey, val)
									break
								}
							}
						}

						i++ // Skip the next item (the unpacked dict)
						continue
					}

					// Regular wrapped pair
					pairList, ok := arg.(*core.ListValue)
					if !ok || pairList.Len() != 2 {
						return nil, fmt.Errorf("dict-literal with wrapped pairs: expected (key value) pairs or **unpack")
					}

					// Evaluate the key
					key, err := Eval(pairList.Items()[0], ctx)
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
					value, err := Eval(pairList.Items()[1], ctx)
					if err != nil {
						return nil, fmt.Errorf("error evaluating dict value for key %v: %v", key, err)
					}

					// Set the key-value pair
					dict.SetWithKey(keyStr, key, value)
				}
				return dict, nil
			}
		}
	}

	// Flat format - can contain regular key-value pairs and **unpack markers
	// Process entries sequentially
	i := 0
	for i < args.Len() {
		// Check for **unpack marker
		if sym, ok := args.Items()[i].(core.SymbolValue); ok && string(sym) == "**unpack" {
			if i+1 >= args.Len() {
				return nil, fmt.Errorf("**unpack requires a dict expression")
			}

			// Evaluate the dict to unpack
			unpackVal, err := Eval(args.Items()[i+1], ctx)
			if err != nil {
				return nil, fmt.Errorf("error evaluating **unpack dict: %v", err)
			}

			// Must be a dict
			unpackDict, ok := unpackVal.(*core.DictValue)
			if !ok {
				return nil, fmt.Errorf("**unpack requires a dict, got %s", unpackVal.Type())
			}

			// Merge all key-value pairs from unpacked dict
			for _, keyRepr := range unpackDict.Keys() {
				val, _ := unpackDict.Get(keyRepr)

				// Find the original key
				origKeys := unpackDict.OriginalKeys()
				for _, origKey := range origKeys {
					if core.ValueToKey(origKey) == keyRepr {
						dict.SetWithKey(keyRepr, origKey, val)
						break
					}
				}
			}

			i += 2 // Skip marker and dict expression
			continue
		}

		// Regular key-value pair
		if i+1 >= args.Len() {
			return nil, fmt.Errorf("dict-literal requires key-value pairs (odd number of arguments after unpacking)")
		}

		// Evaluate the key
		key, err := Eval(args.Items()[i], ctx)
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
		value, err := Eval(args.Items()[i+1], ctx)
		if err != nil {
			return nil, fmt.Errorf("error evaluating dict value for key %v: %v", key, err)
		}

		// Set the key-value pair
		dict.SetWithKey(keyStr, key, value)

		i += 2
	}

	return dict, nil
}

// assignFormInternal is the internal implementation that returns the assigned value
func assignFormInternal(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 2 {
		return nil, ErrArgCount("= requires at least 2 arguments")
	}

	// Check for multiple assignment (= a b expr) or (= a b c d ...)
	if args.Len() > 2 {
		// Multiple assignment
		// Two cases:
		// 1. (= a b expr) where expr returns multiple values
		// 2. (= a b c d) where we assign a=b, c=d

		// First, check if we have multiple symbols followed by values
		// Count leading symbols
		symbolCount := 0
		for i := 0; i < args.Len(); i++ {
			if _, ok := args.Items()[i].(core.SymbolValue); ok {
				symbolCount++
			} else {
				break
			}
		}

		// Check for star unpacking first: (= (a (*unpack b) c) expr)
		// This happens when we have something like: a, *b, c = values
		if symbolCount == 0 && args.Len() == 2 {
			if targetList, ok := args.Items()[0].(*core.ListValue); ok {
				// Check if any element is (*unpack ...)
				starIndex := -1
				for i, t := range targetList.Items() {
					if tList, ok := t.(*core.ListValue); ok && tList.Len() == 2 {
						if sym, ok := tList.Items()[0].(core.SymbolValue); ok && string(sym) == "*unpack" {
							starIndex = i
							break
						}
					}
				}

				if starIndex >= 0 {
					// Star unpacking assignment
					expr := args.Items()[1]
					value, err := Eval(expr, ctx)
					if err != nil {
						return nil, err
					}

					// Convert value to a sequence
					var values []core.Value
					switch v := value.(type) {
					case core.TupleValue:
						values = []core.Value(v)
					case *core.ListValue:
						values = v.Items()
					default:
						return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
					}

					// Calculate minimum required length
					minLen := targetList.Len() - 1 // All targets except the star
					if len(values) < minLen {
						return nil, fmt.Errorf("not enough values to unpack (expected at least %d, got %d)", minLen, len(values))
					}

					// Assign values
					valIdx := 0
					for i, target := range targetList.Items() {
						if i == starIndex {
							// Star unpacking: collect remaining values
							starTarget := target.(*core.ListValue)
							starVar := starTarget.Items()[1].(core.SymbolValue)
							remaining := len(values) - valIdx - (targetList.Len() - i - 1)
							starValues := values[valIdx : valIdx+remaining]
							if err := assignVariable(ctx, string(starVar), core.NewList(starValues...)); err != nil {
								return nil, err
							}
							valIdx += remaining
						} else {
							// Regular assignment
							if sym, ok := target.(core.SymbolValue); ok {
								if err := assignVariable(ctx, string(sym), values[valIdx]); err != nil {
									return nil, err
								}
								valIdx++
							} else {
								return nil, fmt.Errorf("unpacking target must be a symbol")
							}
						}
					}

					return value, nil
				}
			}
		}

		// If we have multiple symbols followed by values, it's multiple assignment
		if symbolCount > 1 && symbolCount < args.Len() {
			// Multiple assignment: (= a b c ... val1 val2 val3 ...)
			targets := args.Items()[:symbolCount]
			valueExprs := args.Items()[symbolCount:]

			// Evaluate all value expressions
			values := make([]core.Value, 0, len(valueExprs))
			for _, expr := range valueExprs {
				val, err := Eval(expr, ctx)
				if err != nil {
					return nil, err
				}
				values = append(values, val)
			}

			// If there's only one value expression but it evaluates to a sequence, unpack it
			if len(valueExprs) == 1 && len(values) == 1 {
				switch v := values[0].(type) {
				case core.TupleValue:
					values = []core.Value(v)
				case *core.ListValue:
					values = v.Items()
				}
			}

			// Check length match
			if len(targets) != len(values) {
				if len(values) < len(targets) {
					return nil, fmt.Errorf("not enough values to unpack (expected %d, got %d)", len(targets), len(values))
				}
				return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", len(targets), len(values))
			}

			// Assign each value using generic unpacking
			for i, target := range targets {
				if err := UnpackPattern(target, values[i], ctx); err != nil {
					return nil, err
				}
			}

			// Return a tuple of the assigned values
			return core.TupleValue(values), nil
		}

		// Check if all but last are symbols (old case 1)
		allSymbols := true
		for i := 0; i < args.Len()-1; i++ {
			if _, ok := args.Items()[i].(core.SymbolValue); !ok {
				allSymbols = false
				break
			}
		}

		if allSymbols && symbolCount > 1 {
			// Case 1: (= a b expr) - special handling for tuple unpacking
			targets := args.Items()[:args.Len()-1]
			expr := args.Items()[args.Len()-1]

			// Special case: (= a b (+ a b)) for Fibonacci-style assignment
			// We need to build a tuple of the current values first
			if len(targets) == 2 {
				// Get current values of targets
				var currentValues []core.Value
				for _, target := range targets {
					if sym, ok := target.(core.SymbolValue); ok {
						val, err := ctx.Lookup(string(sym))
						if err != nil {
							// Variable doesn't exist yet, use nil
							currentValues = append(currentValues, core.Nil)
						} else {
							currentValues = append(currentValues, val)
						}
					}
				}

				// Create a temporary context with the old values available
				tempCtx := core.NewContext(ctx)

				// Evaluate the expression
				value, err := Eval(expr, tempCtx)
				if err != nil {
					return nil, err
				}

				// Now handle the assignment
				// If expr is a single value, assign it to the last target
				// If expr is a tuple/list, unpack it
				var values []core.Value
				switch v := value.(type) {
				case core.TupleValue:
					values = v
				case *core.ListValue:
					values = v.Items()
				default:
					// Single value case: (= a b expr) means a=b, b=expr
					// This matches Python's a, b = b, expr
					if len(targets) == 2 {
						values = []core.Value{currentValues[1], value}
					} else {
						values = []core.Value{value}
					}
				}

				// Check length match
				if len(targets) != len(values) {
					return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", len(targets), len(values))
				}

				// Assign each value
				for i, target := range targets {
					if sym, ok := target.(core.SymbolValue); ok {
						if err := assignVariable(ctx, string(sym), values[i]); err != nil {
							return nil, err
						}
					}
				}

				return value, nil
			}

			// General case for more than 2 targets
			// Evaluate the expression
			value, err := Eval(expr, ctx)
			if err != nil {
				return nil, err
			}

			// Check if it's a tuple or list
			var values []core.Value
			switch v := value.(type) {
			case core.TupleValue:
				values = v
			case *core.ListValue:
				values = v.Items()
			default:
				// If not a sequence, error
				return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
			}

			// Check length match
			if len(targets) != len(values) {
				if len(values) < len(targets) {
					return nil, fmt.Errorf("not enough values to unpack (expected %d, got %d)", len(targets), len(values))
				}
				return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", len(targets), len(values))
			}

			// Assign each value using generic unpacking
			for i, target := range targets {
				if err := UnpackPattern(target, values[i], ctx); err != nil {
					return nil, err
				}
			}

			return value, nil
		}

		// Case 2: (= a b c d) - pairwise assignment
		if args.Len()%2 != 0 {
			return nil, fmt.Errorf("= with multiple arguments requires an even number of arguments")
		}

		var lastValue core.Value
		for i := 0; i < args.Len(); i += 2 {
			target := args.Items()[i]
			value, err := Eval(args.Items()[i+1], ctx)
			if err != nil {
				return nil, err
			}

			if sym, ok := target.(core.SymbolValue); ok {
				if err := assignVariable(ctx, string(sym), value); err != nil {
					return nil, err
				}
				lastValue = value
			} else {
				return nil, fmt.Errorf("assignment target must be a symbol")
			}
		}

		return lastValue, nil
	}

	// Single assignment (original logic)
	target := args.Items()[0]
	value, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, err
	}

	// Handle different assignment targets
	switch t := target.(type) {
	case core.SymbolValue:
		// Variable assignment - handle global/nonlocal if declared
		if err := assignVariable(ctx, string(t), value); err != nil {
			return nil, err
		}
		return value, nil

	case *core.ListValue:
		// Check if it's a special form first (get-item or dot notation)
		if t.Len() >= 3 {
			// Check if it's a dot notation expression
			if dotSym, ok := t.Items()[0].(core.SymbolValue); ok && string(dotSym) == "." {
				// Dot notation assignment: (. obj prop) = value
				// Evaluate the object
				obj, err := Eval(t.Items()[1], ctx)
				if err != nil {
					return nil, err
				}

				// Get the property name (can be StringValue or SymbolValue)
				var propName string
				if strName, ok := t.Items()[2].(core.StringValue); ok {
					propName = string(strName)
				} else if symName, ok := t.Items()[2].(core.SymbolValue); ok {
					propName = string(symName)
				} else {
					return nil, TypeError{Expected: "string or symbol property name", Got: t.Items()[2].Type()}
				}

				// Set the attribute
				if objWithAttrs, ok := obj.(interface {
					SetAttr(string, core.Value) error
				}); ok {
					err := objWithAttrs.SetAttr(propName, value)
					if err != nil {
						return nil, err
					}
					return value, nil
				}

				// Special handling for dicts
				if dict, ok := obj.(*core.DictValue); ok {
					dict.Set(propName, value)
					return value, nil
				}

				return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
			}

			// Check if it's an index expression
			if t.Len() == 3 {
				if getItemSym, ok := t.Items()[0].(core.SymbolValue); ok && string(getItemSym) == "get-item" {
					// Index assignment: (get-item obj index) = value
					// Convert to (set-item obj index value)
					setItemArgs := core.NewList(t.Items()[1], t.Items()[2], value)
					// fmt.Printf("DEBUG: Converting index assignment to set-item with args: %v\n", setItemArgs)
					return SetItemForm(setItemArgs, ctx)
				}
			}
		}

		// Check if it's tuple unpacking (with possible star unpacking)
		isTupleUnpacking := true
		hasStarUnpack := false
		starIndex := -1
		for i, elem := range t.Items() {
			if _, ok := elem.(core.SymbolValue); ok {
				continue
			}
			// Check if it's (*unpack var)
			if elemList, ok := elem.(*core.ListValue); ok {
				if elemList.Len() == 2 {
					if sym, ok := elemList.Items()[0].(core.SymbolValue); ok && string(sym) == "*unpack" {
						if _, ok := elemList.Items()[1].(core.SymbolValue); ok {
							hasStarUnpack = true
							starIndex = i
							continue
						}
					}
				}
				// Check if it's dot notation (. obj attr)
				if elemList.Len() == 3 {
					if sym, ok := elemList.Items()[0].(core.SymbolValue); ok && string(sym) == "." {
						// This is a dot notation target like self.a
						continue
					}
				}
				// Otherwise, it's a nested unpacking pattern like (b, c)
				// This is valid for tuple unpacking
				continue
			}
			isTupleUnpacking = false
			break
		}

		if isTupleUnpacking && t.Len() > 0 {
			// Check if this is a tuple-literal form: (tuple-literal elem1 elem2 ...)
			// If so, skip the tuple-literal marker and use the rest as the pattern
			actualPattern := t
			if t.Len() > 0 {
				if sym, ok := t.Items()[0].(core.SymbolValue); ok && string(sym) == "tuple-literal" {
					// Skip the tuple-literal marker
					actualPattern = core.NewList(t.Items()[1:]...)
					// Re-check for star unpacking in the actual pattern
					hasStarUnpack = false
					starIndex = -1
					for i, elem := range actualPattern.Items() {
						if elemList, ok := elem.(*core.ListValue); ok {
							if elemList.Len() == 2 {
								if sym, ok := elemList.Items()[0].(core.SymbolValue); ok && string(sym) == "*unpack" {
									hasStarUnpack = true
									starIndex = i
									break
								}
							}
						}
					}
				}
			}
			t = actualPattern

			// Tuple unpacking: (= (x, y) [10, 20]) or (= (a (*unpack b) c) [1, 2, 3, 4])
			// The value must be a tuple or list
			var values []core.Value
			switch v := value.(type) {
			case core.TupleValue:
				values = []core.Value(v)
			case *core.ListValue:
				values = v.Items()
			default:
				return nil, fmt.Errorf("cannot unpack non-sequence %v", value.Type())
			}

			if hasStarUnpack {
				// Star unpacking
				minLen := t.Len() - 1 // All targets except the star
				if len(values) < minLen {
					return nil, fmt.Errorf("not enough values to unpack (expected at least %d, got %d)", minLen, len(values))
				}

				// Assign values
				valIdx := 0
				for i, target := range t.Items() {
					if i == starIndex {
						// Star unpacking: collect remaining values
						starTarget := target.(*core.ListValue)
						starVar := starTarget.Items()[1].(core.SymbolValue)
						remaining := len(values) - valIdx - (t.Len() - i - 1)
						starValues := values[valIdx : valIdx+remaining]
						if err := assignVariable(ctx, string(starVar), core.NewList(starValues...)); err != nil {
							return nil, err
						}
						valIdx += remaining
					} else {
						// Regular assignment
						if sym, ok := target.(core.SymbolValue); ok {
							if err := assignVariable(ctx, string(sym), values[valIdx]); err != nil {
								return nil, err
							}
							valIdx++
							continue
						}

						// Check if it's dot notation
						if targetList, ok := target.(*core.ListValue); ok && targetList.Len() == 3 {
							if sym, ok := targetList.Items()[0].(core.SymbolValue); ok && string(sym) == "." {
								// Evaluate the object
								obj, err := Eval(targetList.Items()[1], ctx)
								if err != nil {
									return nil, err
								}

								// Get the attribute name
								var attrName string
								if strName, ok := targetList.Items()[2].(core.StringValue); ok {
									attrName = string(strName)
								} else if symName, ok := targetList.Items()[2].(core.SymbolValue); ok {
									attrName = string(symName)
								} else {
									return nil, fmt.Errorf("attribute name must be a string or symbol")
								}

								// Set the attribute
								if objWithAttrs, ok := obj.(interface {
									SetAttr(string, core.Value) error
								}); ok {
									if err := objWithAttrs.SetAttr(attrName, values[valIdx]); err != nil {
										return nil, err
									}
									valIdx++
									continue
								}

								// Special handling for dicts
								if dict, ok := obj.(*core.DictValue); ok {
									dict.Set(attrName, values[valIdx])
									valIdx++
									continue
								}

								return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
							}
						}

						return nil, fmt.Errorf("unpacking target must be a symbol or dot notation")
					}
				}
			} else {
				// Regular tuple unpacking (no star)
				// Check that the number of targets matches the number of values
				if t.Len() != len(values) {
					if len(values) < t.Len() {
						return nil, fmt.Errorf("not enough values to unpack (expected %d, got %d)", t.Len(), len(values))
					}
					return nil, fmt.Errorf("too many values to unpack (expected %d, got %d)", t.Len(), len(values))
				}

				// Assign each value to its corresponding target
				for i, target := range t.Items() {
					// Check if target is a simple symbol
					if sym, ok := target.(core.SymbolValue); ok {
						if err := assignVariable(ctx, string(sym), values[i]); err != nil {
							return nil, err
						}
						continue
					}

					// Check if target is dot notation (attribute access)
					if targetList, ok := target.(*core.ListValue); ok && targetList.Len() > 0 {
						if sym, ok := targetList.Items()[0].(core.SymbolValue); ok && string(sym) == "." {
							// This is attribute assignment like self.a = value
							if targetList.Len() != 3 {
								return nil, fmt.Errorf("invalid dot notation in assignment target")
							}

							// Evaluate the object
							obj, err := Eval(targetList.Items()[1], ctx)
							if err != nil {
								return nil, err
							}

							// Get the attribute name (can be StringValue or SymbolValue)
							var attrName string
							if strName, ok := targetList.Items()[2].(core.StringValue); ok {
								attrName = string(strName)
							} else if symName, ok := targetList.Items()[2].(core.SymbolValue); ok {
								attrName = string(symName)
							} else {
								return nil, fmt.Errorf("attribute name must be a string or symbol")
							}

							// Set the attribute
							if objWithAttrs, ok := obj.(interface {
								SetAttr(string, core.Value) error
							}); ok {
								if err := objWithAttrs.SetAttr(attrName, values[i]); err != nil {
									return nil, err
								}
								continue
							}

							// Special handling for dicts
							if dict, ok := obj.(*core.DictValue); ok {
								dict.Set(attrName, values[i])
								continue
							}

							return nil, fmt.Errorf("%s does not support attribute assignment", obj.Type())
						}

						// Not dot notation - must be nested unpacking pattern like (a, b)
						// Use generic unpacking
						if err := UnpackPattern(targetList, values[i], ctx); err != nil {
							return nil, err
						}
						continue
					}

					return nil, fmt.Errorf("assignment target must be a symbol, dot notation, or tuple pattern, got %v", target.Type())
				}
			}

			return value, nil
		}

	default:
		return nil, TypeError{Expected: "symbol or dot notation", Got: target.Type()}
	}

	return nil, TypeError{Expected: "symbol or dot notation", Got: target.Type()}
}

// AssignForm provides the implementation of the = special form
// Python assignments are statements and always return None
func AssignForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Call the internal implementation
	_, err := assignFormInternal(args, ctx)
	if err != nil {
		return nil, err
	}
	// Python assignments are statements and return None
	return core.None, nil
}

// WalrusForm provides the implementation of the walrus operator (:=)
// The walrus operator is an assignment expression that returns the assigned value
// Usage: (if (:= n (len data)) (> n 10))
func WalrusForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, ErrArgCount(":= requires exactly 2 arguments (variable and value)")
	}

	// First argument must be a symbol
	target, ok := args.Items()[0].(core.SymbolValue)
	if !ok {
		return nil, TypeError{Expected: "symbol", Got: args.Items()[0].Type()}
	}

	// Evaluate the expression
	value, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, err
	}

	// Assign the value to the variable - handle global/nonlocal if declared
	if err := assignVariable(ctx, string(target), value); err != nil {
		return nil, err
	}

	// Return the value (this is what makes it an expression)
	return value, nil
}

// QuoteForm provides the implementation of the quote special form
func QuoteForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, ErrArgCount("quote requires 1 argument")
	}

	// Return the argument unevaluated
	return args.Items()[0], nil
}

// ReturnForm provides the implementation of the return special form
func ReturnForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() > 1 {
		return nil, ErrArgCount("return takes at most 1 argument")
	}

	var value core.Value = core.Nil
	var err error

	if args.Len() == 1 {
		value, err = Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
	}

	return &ReturnValue{Value: value}, nil
}

// ImportForm provides the implementation of the import special form
func ImportForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 1 {
		return nil, ErrArgCount("import requires 1 argument")
	}

	// Get the module name
	var moduleName string
	switch name := args.Items()[0].(type) {
	case core.StringValue:
		moduleName = string(name)
	case core.SymbolValue:
		moduleName = string(name)
	default:
		return nil, TypeError{Expected: "string or symbol", Got: args.Items()[0].Type()}
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

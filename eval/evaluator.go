package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/special_forms"
)

type Evaluator struct {
	specialForms map[core.LispSymbol]special_forms.SpecialFormFunc
	callStack    []core.TraceEntry // Track the current call stack
	currentFunc  string            // Track the current function name
}

func NewEvaluator() core.Evaluator {
	// Use the exported SpecialForms map
	forms := make(map[core.LispSymbol]special_forms.SpecialFormFunc)
	for name, form := range special_forms.SpecialForms {
		forms[name] = form
	}

	return &Evaluator{
		specialForms: forms,
		callStack:    make([]core.TraceEntry, 0),
		currentFunc:  "<toplevel>",
	}
}

// pushCallFrame adds a new call frame to the call stack
func (e *Evaluator) pushCallFrame(funcName string, location core.Location, statement string) {
	// Enhance the statement by extracting more code context if available
	enhancedStatement := statement

	// If this is an expression, try to get more context from source code
	if sourceCode, ok := core.GetSourceCode(location); ok && sourceCode != "" {
		// Use the source code from the expression to provide better context
		enhancedStatement = sourceCode
	}

	entry := core.TraceEntry{
		Function:  funcName,
		Location:  location,
		Statement: enhancedStatement,
	}
	e.callStack = append(e.callStack, entry)
	e.currentFunc = funcName
}

// popCallFrame removes the last call frame from the call stack
func (e *Evaluator) popCallFrame() {
	if len(e.callStack) > 0 {
		e.callStack = e.callStack[:len(e.callStack)-1]
		if len(e.callStack) > 0 {
			e.currentFunc = e.callStack[len(e.callStack)-1].Function
		} else {
			e.currentFunc = "<toplevel>"
		}
	}
}

// getCurrentTraceback returns the current traceback
func (e *Evaluator) getCurrentTraceback() core.Traceback {
	// Create a copy of the call stack
	traceback := make(core.Traceback, len(e.callStack))
	copy(traceback, e.callStack)
	return traceback
}

// getLocationFromExpr extracts location information from an expression
func (e *Evaluator) getLocationFromExpr(expr core.LispValue) core.Location {
	if located, ok := expr.(core.LocatedValue); ok {
		return located.Location
	}
	// Default location if no source information is available
	return core.Location{
		Filename: "<unknown>",
		Line:     0,
		Column:   0,
	}
}

// unwrapLocatedValue extracts the value from a LocatedValue
func (e *Evaluator) unwrapLocatedValue(expr core.LispValue) core.LispValue {
	if located, ok := expr.(core.LocatedValue); ok {
		return located.Value
	}
	return expr
}

func (e *Evaluator) Eval(expr core.LispValue, env core.Environment) (core.LispValue, error) {
	// Debug output for evaluator
	//fmt.Printf("DEBUG Eval: Type %T, Value: %v\n", expr, expr)
	// Extract location information if available
	location := e.getLocationFromExpr(expr)

	// Track current expression evaluation in call stack for top-level expressions
	// Use a generic name for expressions, we'll update it if it's a function call
	exprStr := "expression"
	if list, ok := expr.(core.LispList); ok && len(list) > 0 {
		// Unwrap first element if it's a LocatedValue to check for symbol
		firstElem := e.unwrapLocatedValue(list[0])
		if sym, ok := firstElem.(core.LispSymbol); ok {
			exprStr = string(sym)
		}
	} else if locatedList, ok := expr.(core.LocatedValue); ok {
		if list, ok := locatedList.Value.(core.LispList); ok && len(list) > 0 {
			firstElem := e.unwrapLocatedValue(list[0])
			if sym, ok := firstElem.(core.LispSymbol); ok {
				exprStr = string(sym)
			}
		}
	}
	e.pushCallFrame("<eval>", location, exprStr)
	defer e.popCallFrame()

	// Unwrap the LocatedValue if present
	unwrappedExpr := e.unwrapLocatedValue(expr)

	// We need to switch on the unwrapped expression
	switch v := unwrappedExpr.(type) {
	case core.LispSymbol:
		// Check for dot notation: module.attribute, dict.method, or object.attribute
		if strings.Contains(string(v), ".") {
			// Use our dedicated dot notation handler
			if core.Debug {
				fmt.Printf("DEBUG: Dot notation detected in evaluator: %s\n", string(v))
			}
			return e.HandleDotNotation(v, env)
		}

		// Regular symbol lookup
		value, ok := env.Get(v)
		if !ok {
			// Check if this is likely a special form that wasn't properly registered
			if core.IsBuiltinSpecialForm(v) {
				err := fmt.Errorf("special form '%s' not properly registered in environment - ensure special_forms.RegisterSpecialForms() is called during initialization", v)
				return nil, e.enrichErrorWithTraceback(err)
			}

			// Check for typos in common symbols
			suggestions := suggestSymbol(v, env)
			if len(suggestions) > 0 {
				err := fmt.Errorf("undefined symbol: %s (did you mean: %s?)", v, strings.Join(suggestions, ", "))
				return nil, e.enrichErrorWithTraceback(err)
			}

			err := fmt.Errorf("undefined symbol: %s", v)
			return nil, e.enrichErrorWithTraceback(err)
		}
		return value, nil
	case float64, int, string, core.PythonicBool, core.PythonicNone:
		return v, nil
	case *core.PythonicDict:
		// Evaluate dictionary literals
		result, err := e.evalDict(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	case *core.PythonicSet:
		// Evaluate set literals
		result, err := e.evalSet(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	case core.LispListLiteral:
		// Evaluate list literal elements
		result := make(core.LispList, len(v))
		for i, elem := range v {
			evalElem, err := e.Eval(elem, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			result[i] = evalElem
		}
		return result, nil
	case core.LispTuple:
		// Evaluate tuple elements (similar to list literals)
		result := make(core.LispTuple, len(v))
		for i, elem := range v {
			evalElem, err := e.Eval(elem, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			result[i] = evalElem
		}
		return result, nil
	case core.LispComprehension:
		result, err := e.evalComprehension(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	case core.LispList:
		if len(v) == 0 {
			return core.LispList{}, nil
		}

		// Ensure we unwrap the first element for special form and function lookup
		first := e.unwrapLocatedValue(v[0])
		rest := v[1:]

		switch f := first.(type) {
		case core.LispSymbol:
			// Check for dot notation in function call: module.function(args) or object.method(args)
			if strings.Contains(string(f), ".") {
				// Use our dedicated dot notation handler for function calls
				dotSymbol, dotArgs := e.prepareDotNotationCall(f, rest, env)
				return e.HandleDotNotationCall(dotSymbol, dotArgs, env)
			} else if f == core.LispSymbol("=") {
				// Special handling for assignment
				if len(rest) != 2 {
					err := fmt.Errorf("= requires exactly two arguments")
					return nil, e.enrichErrorWithTraceback(err)
				}

				// Get the target of the assignment
				lhs := rest[0]
				rhs := rest[1]

				// Check if this is a dot notation assignment: obj.attr = value
				unwrappedLhs := e.unwrapLocatedValue(lhs)
				if lhsSymbol, ok := unwrappedLhs.(core.LispSymbol); ok && strings.Contains(string(lhsSymbol), ".") {
					// Handle dot notation assignment via set-attr
					return e.evalDotAssignment(lhsSymbol, rhs, env)
				}

				// Evaluate the right-hand side
				value, err := e.Eval(rhs, env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}

				// Ensure evaluator is set on evaluator-aware values
				if evalAware, ok := value.(core.EvaluatorAware); ok {
					if core.Debug {
						fmt.Println("DEBUG: Setting evaluator on", value)
					}
					evalAware.SetEvaluator(e)
				}

				// Special handling for PythonicDict to ensure it has the evaluator
				if dict, ok := value.(*core.PythonicDict); ok {
					if core.Debug {
						fmt.Println("DEBUG: Ensuring dictionary has evaluator:", dict)
					}
					dict.SetEvaluator(e)
				}

				// Ensure left-hand side is a symbol for assignment
				if lhsSymbol, ok := unwrappedLhs.(core.LispSymbol); ok {
					// Set the value in the environment
					if strings.HasPrefix(string(lhsSymbol), "global.") {
						// Handle assignment to global variables
						globalSymbol := core.LispSymbol(strings.TrimPrefix(string(lhsSymbol), "global."))
						env.Set(globalSymbol, value)

						// Additional debug for dictionary assignment
						if dict, ok := value.(*core.PythonicDict); ok {
							if core.Debug {
								fmt.Println("DEBUG: Assigned dictionary to global variable", globalSymbol, ":", dict)
							}
						}
					} else {
						// Normal assignment
						env.Set(lhsSymbol, value)

						// Additional debug for dictionary assignment
						if dict, ok := value.(*core.PythonicDict); ok {
							if core.Debug {
								fmt.Println("DEBUG: Assigned dictionary to variable", lhsSymbol, ":", dict)

								// Verify assignment immediately
								if val, found := env.Get(lhsSymbol); found {
									fmt.Println("DEBUG: Verified assignment of", lhsSymbol, ":", val)
								} else {
									fmt.Println("DEBUG: CRITICAL - Assignment verification failed for", lhsSymbol)
								}
							}
						}
					}
					return value, nil
				} else {
					err := fmt.Errorf("invalid assignment target: %v", lhs)
					return nil, e.enrichErrorWithTraceback(err)
				}
			} else if specialForm, exists := e.specialForms[f]; exists {
				// Evaluate using special form
				result, err := specialForm(e, rest, env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}
				return result, nil
			} else {
				// Regular function call
				// Evaluate the function value
				function, err := e.Eval(f, env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}

				// Evaluate arguments and call the function
				evaledArgs, err := e.evalArgs(rest, env)
				if err != nil {
					return nil, err
				}

				// Apply the function with the evaluated arguments
				result, err := e.Apply(function, evaledArgs, env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}
				return result, nil
			}
		default:
			// If the first item is not a symbol, it could be a function expression
			// Evaluate the first item to get the function
			function, err := e.Eval(first, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}

			// Evaluate the arguments
			evaledArgs, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, err
			}

			// Apply the function with the evaluated arguments
			result, err := e.Apply(function, evaledArgs, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			return result, nil
		}
	case core.Evaluable:
		// If the expression implements Evaluable, use its Eval method
		result, err := v.Eval(e, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	default:
		// Unknown expression type
		err := fmt.Errorf("unknown expression type: %T", expr)
		return nil, e.enrichErrorWithTraceback(err)
	}
}

func (e *Evaluator) evalArgs(args []core.LispValue, env core.Environment) ([]core.LispValue, error) {
	evaluated := make([]core.LispValue, len(args))
	for i, arg := range args {
		// First, unwrap LocatedValue if present to check for keyword arguments
		unwrappedArg := e.unwrapLocatedValue(arg)

		// Check if arg is a symbol that represents a keyword argument
		if symbol, ok := unwrappedArg.(core.LispSymbol); ok {
			symbolStr := string(symbol)
			if len(symbolStr) > 0 && strings.Contains(symbolStr, "=") {
				// This is a keyword argument in the form name=value
				evaluated[i] = symbolStr // Pass it as a string for the lambda to process
				continue
			} else if len(symbol) > 1 && symbol[0] == ':' {
				// This is a keyword argument, don't evaluate it
				evaluated[i] = unwrappedArg
				continue
			}
		}

		// Otherwise evaluate normally, preserving location information where appropriate
		value, err := e.Eval(arg, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}

		// Check if the evaluated value contains a keyword format
		unwrappedValue := e.unwrapLocatedValue(value)
		if str, ok := unwrappedValue.(string); ok && strings.Contains(str, "=") {
			// Preserve the string format for keyword detection in ApplyLambda
			evaluated[i] = str
		} else {
			evaluated[i] = value
		}
	}
	return evaluated, nil
}

// enrichErrorWithTraceback adds traceback information to errors
func (e *Evaluator) enrichErrorWithTraceback(err error) error {
	if err == nil {
		return nil
	}

	// Skip traceback for special control flow signals
	switch err.(type) {
	case special_forms.ReturnSignal, special_forms.YieldSignal,
		special_forms.BreakSignal, special_forms.ContinueSignal:
		return err // Control flow signals should pass through unchanged
	}

	// Handle exceptions with existing traceback
	if ex, ok := err.(*core.Exception); ok {
		// If the exception already has a traceback, we need to merge it with the current traceback
		// This ensures we don't lose context when exceptions propagate through nested calls
		if ex.Traceback != nil && len(ex.Traceback) > 0 {
			// We need to prepend the current traceback to the existing one
			// but avoid duplicating entries from the same function call
			currentTraceback := e.getCurrentTraceback()

			// We'll only prepend entries that aren't already at the start of the exception traceback
			// This avoids duplicate entries when an exception propagates through multiple evaluations
			if len(currentTraceback) > 0 && (len(ex.Traceback) == 0 ||
				!isSameTraceEntry(currentTraceback[len(currentTraceback)-1], ex.Traceback[0])) {
				// Merge tracebacks
				mergedTraceback := make(core.Traceback, 0, len(currentTraceback)+len(ex.Traceback))
				mergedTraceback = append(mergedTraceback, currentTraceback...)
				mergedTraceback = append(mergedTraceback, ex.Traceback...)
				ex.Traceback = mergedTraceback
			}
			return ex
		}

		// If the exception doesn't have a traceback yet, add the current one
		ex.Traceback = e.getCurrentTraceback()
		return ex
	}

	// Create a new exception from a regular error
	// Convert regular error to Exception and improve error message
	errorMsg := err.Error()

	// Enhance common error messages with more descriptive information
	if strings.Contains(errorMsg, "undefined symbol") {
		// Add suggestion to check variable names or import modules
		symbol := strings.TrimPrefix(errorMsg, "undefined symbol: ")
		errorMsg = fmt.Sprintf("undefined symbol: %s (did you forget to define the variable or import a module?)", symbol)
	} else if strings.Contains(errorMsg, "cannot call non-function") {
		// Add suggestion for calling non-callable values
		errorMsg = fmt.Sprintf("%s (make sure you're calling a function, not a value)", errorMsg)
	} else if strings.Contains(errorMsg, "index out of range") {
		// Add suggestion for list index errors
		errorMsg = fmt.Sprintf("%s (check your list indices and list length)", errorMsg)
	} else if strings.Contains(errorMsg, "divide by zero") {
		// Improve division by zero errors
		errorMsg = "division by zero is not allowed"
	}

	// Create the enhanced exception with the current traceback
	ex := core.NewException("RuntimeError", errorMsg)
	ex.Traceback = e.getCurrentTraceback()
	return ex
}

// isSameTraceEntry checks if two trace entries point to the same location/function
func isSameTraceEntry(a, b core.TraceEntry) bool {
	// Check if the entries refer to the same location and function
	return a.Location.Filename == b.Location.Filename &&
		a.Location.Line == b.Location.Line &&
		a.Function == b.Function
}

// ApplyLambda applies a Lambda function to arguments with proper environment handling
// This delegates to the Lambda's Apply method for most of the logic
func (e *Evaluator) ApplyLambda(lambda *core.Lambda, args []core.LispValue, callEnv core.Environment) (core.LispValue, error) {
	// Call the Lambda.Apply method to handle argument binding and evaluation
	result, err := lambda.Apply(e, args, callEnv)

	// Special handling for return statements (ReturnSignal)
	if err != nil {
		if ret, ok := err.(special_forms.ReturnSignal); ok {
			// Return the value from the return statement
			return ret.Value, nil
		}
		// Pass through other errors
		return nil, err
	}

	return result, nil
}

// processLambdaArgs binds function arguments to parameters in the lambda environment
// DEPRECATED: This function is kept for backward compatibility and will be removed in a future version.
// Use core.processLambdaArgs instead, which has the same functionality.
// Returns true if successful, false if there's a parameter mismatch
func (e *Evaluator) processLambdaArgs(lambda *core.Lambda, args []core.LispValue, lambdaEnv core.Environment) bool {
	// Extract keyword arguments and count positional arguments
	keywordArgs := make(map[string]core.LispValue)
	positionalArgs := make([]core.LispValue, 0, len(args))

	for _, arg := range args {
		// Check if it's a keyword argument (string in the form "name=value")
		if strArg, ok := arg.(string); ok && strings.Contains(strArg, "=") {
			// Parse the keyword argument
			parts := strings.SplitN(strArg, "=", 2)
			if len(parts) == 2 {
				keywordName := parts[0]
				keywordValue := parts[1]
				// Store as string for now, may need more complex parsing later
				keywordArgs[keywordName] = keywordValue
			}
		} else {
			// It's a positional argument
			positionalArgs = append(positionalArgs, arg)
		}
	}

	// Bind positional arguments to parameters
	paramCount := len(lambda.Params)
	argCount := len(positionalArgs)

	// Check if we have too many arguments
	if argCount > paramCount && !hasVarArgs(lambda.Params) {
		return false
	}

	// Bind positional parameters
	for i, param := range lambda.Params {
		if i < argCount {
			// Bind argument to parameter
			lambdaEnv.Define(param, positionalArgs[i])
		} else if defaultVal, hasDefault := lambda.DefaultValues[param]; hasDefault {
			// Use default value
			lambdaEnv.Define(param, defaultVal)
		} else {
			// Parameter without argument or default value
			// For now, we'll set it to None
			lambdaEnv.Define(param, core.PythonicNone{})
		}
	}

	// Apply keyword arguments (overriding positional bindings if applicable)
	for name, value := range keywordArgs {
		paramName := core.LispSymbol(name)
		// Check if this is a valid parameter name
		found := false
		for _, param := range lambda.Params {
			if param == paramName {
				found = true
				lambdaEnv.Define(paramName, value)
				break
			}
		}
		if !found {
			// Keyword argument doesn't match any parameter
			return false
		}
	}

	return true
}

// evalDotAssignment handles assignments to dot notation expressions (obj.attr = value)
func (e *Evaluator) evalDotAssignment(lhs core.LispSymbol, rhs core.LispValue, env core.Environment) (core.LispValue, error) {
	// Parse the dot notation (obj.attr)
	parts := strings.Split(string(lhs), ".")
	if len(parts) < 2 {
		err := fmt.Errorf("invalid dot notation: %s", lhs)
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Get the base object
	objSymbol := core.LispSymbol(parts[0])
	obj, err := e.Eval(objSymbol, env)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Get the attribute name
	attrName := parts[len(parts)-1]

	// For intermediate parts (if any), navigate through the object hierarchy
	var currentObj = obj
	for i := 1; i < len(parts)-1; i++ {
		// Get the next level object
		currentAttr := parts[i]
		args := []core.LispValue{currentObj, currentAttr}
		nextObj, err := e.specialForms[core.LispSymbol("dot")](e, args, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		currentObj = nextObj
	}

	// Evaluate the right-hand side to get the value to assign
	value, err := e.Eval(rhs, env)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Use the dot-assign special form to set the attribute
	args := []core.LispValue{currentObj, attrName, value}
	_, ok := e.specialForms[core.LispSymbol("dot-assign")]
	if ok {
		result, err := e.specialForms[core.LispSymbol("dot-assign")](e, args, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		return result, nil
	}

	// Fall back to set-prop if dot-assign is not available
	result, err := e.specialForms[core.LispSymbol("set-prop")](e, args, env)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	return result, nil
}

// hasVarArgs checks if the parameter list contains a variadic parameter
func hasVarArgs(params []core.LispSymbol) bool {
	for _, param := range params {
		if strings.HasPrefix(string(param), "*") {
			return true
		}
	}
	return false
}

func suggestSymbol(symbol core.LispSymbol, env core.Environment) []string {
	// Simple suggestion algorithm - look for symbols that are close in edit distance
	// or that have a similar prefix
	suggestions := []string{}
	symbolStr := string(symbol)

	// Check if it's a typo in a commonly used name
	commonSymbols := []string{
		"print", "str", "int", "float", "dict", "list", "set",
		"if", "for", "while", "def", "lambda", "import",
		"return", "and", "or", "not", "in", "True", "False", "None",
	}

	for _, common := range commonSymbols {
		if levenshteinDistance(symbolStr, common) <= 2 {
			suggestions = append(suggestions, common)
		}
	}

	// We would check the environment for similar names here, but Environment doesn't have a Visit method

	// Limit the number of suggestions
	if len(suggestions) > 3 {
		suggestions = suggestions[:3]
	}

	return suggestions
}

// levenshteinDistance calculates the edit distance between two strings
func levenshteinDistance(s1, s2 string) int {
	// Simple implementation of Levenshtein distance
	// For performance, we can replace this with a more efficient algorithm if needed

	if len(s1) == 0 {
		return len(s2)
	}
	if len(s2) == 0 {
		return len(s1)
	}

	// Create a matrix for dynamic programming
	matrix := make([][]int, len(s1)+1)
	for i := range matrix {
		matrix[i] = make([]int, len(s2)+1)
		matrix[i][0] = i
	}
	for j := range matrix[0] {
		matrix[0][j] = j
	}

	// Fill the matrix
	for i := 1; i <= len(s1); i++ {
		for j := 1; j <= len(s2); j++ {
			cost := 1
			if s1[i-1] == s2[j-1] {
				cost = 0
			}

			matrix[i][j] = min(
				matrix[i-1][j]+1,      // deletion
				matrix[i][j-1]+1,      // insertion
				matrix[i-1][j-1]+cost, // substitution
			)
		}
	}

	return matrix[len(s1)][len(s2)]
}

func min(a, b, c int) int {
	if a < b && a < c {
		return a
	} else if b < c {
		return b
	}
	return c
}

// Apply applies a function to arguments
func (e *Evaluator) Apply(fn core.LispValue, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	switch fn := fn.(type) {
	case core.Applicable:
		// Use the Apply method defined on the function
		return fn.Apply(e, args, env)
	case core.BuiltinFunc:
		// Call the builtin function directly
		return fn(args, env)
	case *core.Lambda:
		// Apply lambda function via lambda.Apply, but keeping return handling in ApplyLambda
		// This allows us to properly handle return signals while using Lambda.Apply
		return e.ApplyLambda(fn, args, env)
	case *core.BoundMethod:
		// Apply method with instance
		return fn.Apply(e, args, env)
	case core.Invokable:
		// Use the Invoke method
		return fn.Invoke(args, e, env)
	default:
		// Not a callable value
		err := fmt.Errorf("cannot call non-function value: %v", fn)
		return nil, e.enrichErrorWithTraceback(err)
	}
}

// evalDict evaluates dictionary literals by evaluating all keys and values
func (e *Evaluator) evalDict(dict *core.PythonicDict, env core.Environment) (*core.PythonicDict, error) {
	// Debug output
	if core.Debug {
		fmt.Println("DEBUG evalDict: Processing dictionary literal")
	}

	result := core.NewPythonicDict()

	// Ensure the dictionary has a reference to the evaluator
	result.SetEvaluator(e)

	// Ensure the environment has the evaluator reference
	env.SetEvaluator(e)

	// Also store evaluator as a symbol for direct access
	env.Define(core.LispSymbol("EVALUATOR"), e)

	// Iterate over all entries and evaluate them
	err := dict.Iterate(func(k, v core.LispValue) error {
		// If key is already a string, we can use it as is
		// This handles the common case of {"key": value} where the key is a string literal
		var evalKey core.LispValue
		var evalErr error

		if strKey, ok := k.(string); ok {
			evalKey = strKey
		} else {
			// Otherwise, evaluate the key
			evalKey, evalErr = e.Eval(k, env)
			if evalErr != nil {
				return evalErr
			}
		}

		// Evaluate the value
		evalVal, err := e.Eval(v, env)
		if err != nil {
			return err
		}

		// Add to the dictionary
		result.Set(evalKey, evalVal)
		return nil
	})

	if err != nil {
		return nil, err
	}

	if core.Debug {
		fmt.Println("DEBUG evalDict: Completed dictionary literal evaluation, result:", result)
	}
	return result, nil
}

// evalSet evaluates set literals by evaluating all elements
func (e *Evaluator) evalSet(set *core.PythonicSet, env core.Environment) (*core.PythonicSet, error) {
	result := core.NewPythonicSet()

	// Evaluate each set element
	for elem := range set.Data() {
		evalElem, err := e.Eval(elem, env)
		if err != nil {
			return nil, err
		}

		result.Add(evalElem)
	}

	return result, nil
}

// evalComprehension evaluates list comprehensions
func (e *Evaluator) evalComprehension(comp core.LispComprehension, env core.Environment) (core.LispValue, error) {
	// First evaluate the iterable
	iterVal, err := e.Eval(comp.Iterable, env)
	if err != nil {
		return nil, err
	}

	// Convert to list if possible
	var iterItems []core.LispValue
	switch it := iterVal.(type) {
	case core.LispList:
		iterItems = it
	case core.LispListLiteral:
		iterItems = core.LispList(it)
	case core.LispTuple:
		iterItems = core.LispList(it)
	case string:
		// Convert string to list of characters
		chars := []core.LispValue{}
		for _, ch := range it {
			chars = append(chars, string(ch))
		}
		iterItems = chars
	case *core.PythonicDict:
		// Iterate over dictionary keys
		keys := it.SortedKeys()
		iterItems = keys
	case *core.PythonicSet:
		// Iterate over set elements
		elements := make([]core.LispValue, 0, it.Size())
		for k := range it.Data() {
			elements = append(elements, k)
		}
		iterItems = elements
	default:
		return nil, fmt.Errorf("cannot iterate over %T", iterVal)
	}

	// Create a new environment for the comprehension
	compEnv := env.NewEnvironment(env)

	// Prepare the result list
	result := make(core.LispList, 0, len(iterItems))

	// Iterate over the items
	for _, item := range iterItems {
		// Bind the iteration variable
		compEnv.Define(comp.Variable, item)

		// Check the condition if present
		if comp.Condition != nil {
			condResult, err := e.Eval(comp.Condition, compEnv)
			if err != nil {
				return nil, err
			}

			// Skip this item if the condition is not truthy
			if !core.IsTruthy(condResult) {
				continue
			}
		}

		// Evaluate the expression for this item
		exprResult, err := e.Eval(comp.Expression, compEnv)
		if err != nil {
			return nil, err
		}

		// Add to the result list
		result = append(result, exprResult)
	}

	return result, nil
}

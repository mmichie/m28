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
	return &Evaluator{
		specialForms: special_forms.GetSpecialForms(),
		callStack:    make([]core.TraceEntry, 0),
		currentFunc:  "<toplevel>",
	}
}

// pushCallFrame adds a new call frame to the call stack
func (e *Evaluator) pushCallFrame(funcName string, location core.Location, statement string) {
	entry := core.TraceEntry{
		Function:  funcName,
		Location:  location,
		Statement: statement,
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
	// Extract location information if available
	location := e.getLocationFromExpr(expr)

	// Track current expression evaluation in call stack for top-level expressions
	// Use a generic name for expressions, we'll update it if it's a function call
	exprStr := "expression"
	if list, ok := expr.(core.LispList); ok && len(list) > 0 {
		if sym, ok := list[0].(core.LispSymbol); ok {
			exprStr = string(sym)
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
			parts := strings.Split(string(v), ".")

			// Special case for dict methods and set methods - look up directly in the environment
			if parts[0] == "dict" || parts[0] == "set" {
				// Functions like dict.keys, dict.update, set.intersection, etc.
				// should be looked up directly in the environment
				val, ok := env.Get(v)
				if ok {
					return val, nil
				}
				// Fall through to the module lookup if not found
			}

			// Handle multiple levels of attributes (e.g., module.submodule.attribute)
			current := parts[0]

			// Start with the first part
			currentObj, ok := env.Get(core.LispSymbol(current))
			if !ok {
				err := fmt.Errorf("undefined object: %s", current)
				return nil, e.enrichErrorWithTraceback(err)
			}

			// Traverse the chain of attributes
			for i := 1; i < len(parts); i++ {
				attrName := core.LispSymbol(parts[i])

				// Check if it's a dictionary
				if dict, ok := currentObj.(*core.PythonicDict); ok {
					// Get the attribute
					var found bool
					currentObj, found = dict.Get(attrName)
					if !found {
						err := fmt.Errorf("attribute %s not found in %s", attrName, current)
						return nil, e.enrichErrorWithTraceback(err)
					}
					current = current + "." + parts[i]
				} else {
					err := fmt.Errorf("%s is not an object with attributes", current)
					return nil, e.enrichErrorWithTraceback(err)
				}
			}

			return currentObj, nil
		}

		// Regular symbol lookup
		value, ok := env.Get(v)
		if !ok {
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

		first := v[0]
		rest := v[1:]

		switch f := first.(type) {
		case core.LispSymbol:
			// Check for dot notation in function call: module.function(args) or object.method(args)
			if strings.Contains(string(f), ".") {
				// Handle multiple levels of attributes (e.g., module.submodule.function)
				parts := strings.Split(string(f), ".")
				current := parts[0]

				// Start with the first part
				currentObj, ok := env.Get(core.LispSymbol(current))
				if !ok {
					err := fmt.Errorf("undefined object: %s", current)
					return nil, e.enrichErrorWithTraceback(err)
				}

				// Traverse all but the last part (which is the function name)
				for i := 1; i < len(parts)-1; i++ {
					attrName := core.LispSymbol(parts[i])

					// Check if it's a dictionary
					if dict, ok := currentObj.(*core.PythonicDict); ok {
						// Get the attribute
						var found bool
						currentObj, found = dict.Get(attrName)
						if !found {
							err := fmt.Errorf("attribute %s not found in %s", attrName, current)
							return nil, e.enrichErrorWithTraceback(err)
						}
						current = current + "." + parts[i]
					} else {
						err := fmt.Errorf("%s is not an object with attributes", current)
						return nil, e.enrichErrorWithTraceback(err)
					}
				}

				// Get the function name (last part)
				funcName := core.LispSymbol(parts[len(parts)-1])

				// Check if the object is a dictionary to get the function
				if dict, ok := currentObj.(*core.PythonicDict); ok {
					// Get the function
					fnVal, found := dict.Get(funcName)
					if !found {
						err := fmt.Errorf("function %s not found in %s", funcName, current)
						return nil, e.enrichErrorWithTraceback(err)
					}

					// Evaluate the arguments
					args, err := e.evalArgs(rest, env)
					if err != nil {
						return nil, e.enrichErrorWithTraceback(err)
					}

					// Apply the function
					return e.Apply(fnVal, args, env)
				} else {
					err := fmt.Errorf("%s is not an object with methods", current)
					return nil, e.enrichErrorWithTraceback(err)
				}
			} else if f == core.LispSymbol("=") {
				// Special handling for assignment
				if len(rest) != 2 {
					err := fmt.Errorf("= requires exactly two arguments")
					return nil, e.enrichErrorWithTraceback(err)
				}
				symbol, ok := rest[0].(core.LispSymbol)
				if !ok {
					err := fmt.Errorf("first argument to = must be a symbol")
					return nil, e.enrichErrorWithTraceback(err)
				}
				value, err := e.Eval(rest[1], env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}
				env.Define(symbol, value)
				return value, nil
			}
			if specialForm, ok := e.specialForms[f]; ok {
				result, err := specialForm(e, rest, env)
				if err != nil {
					return nil, e.enrichErrorWithTraceback(err)
				}
				return result, nil
			}

			// Check for special form marker in the environment
			if value, ok := env.Get(f); ok {
				if marker, ok := value.(core.SpecialFormMarker); ok {
					if specialForm, ok := e.specialForms[marker.Name]; ok {
						result, err := specialForm(e, rest, env)
						if err != nil {
							return nil, e.enrichErrorWithTraceback(err)
						}
						return result, nil
					}
				}
			}
			fn, err := e.Eval(f, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			args, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			return e.Apply(fn, args, env)
		default:
			fn, err := e.Eval(first, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			args, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			return e.Apply(fn, args, env)
		}
	default:
		err := fmt.Errorf("unknown expression type: %T", expr)
		return nil, e.enrichErrorWithTraceback(err)
	}
}

func (e *Evaluator) evalArgs(args []core.LispValue, env core.Environment) ([]core.LispValue, error) {
	evaluated := make([]core.LispValue, len(args))
	for i, arg := range args {
		// Check if arg is a symbol that represents a keyword argument
		if symbol, ok := arg.(core.LispSymbol); ok {
			symbolStr := string(symbol)
			if len(symbolStr) > 0 && strings.Contains(symbolStr, "=") {
				// This is a keyword argument in the form name=value
				evaluated[i] = symbolStr // Pass it as a string for the lambda to process
				continue
			} else if len(symbol) > 1 && symbol[0] == ':' {
				// This is a keyword argument, don't evaluate it
				evaluated[i] = arg
				continue
			}
		}

		// Otherwise evaluate normally
		value, err := e.Eval(arg, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}

		// Check if the evaluated value contains a keyword format
		if str, ok := value.(string); ok && strings.Contains(str, "=") {
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

	// If it's already an exception with a traceback, return it as is
	if ex, ok := err.(*core.Exception); ok && ex.Traceback != nil && len(ex.Traceback) > 0 {
		return ex
	}

	// Create a new exception or enrich an existing one
	var ex *core.Exception
	if exErr, ok := err.(*core.Exception); ok {
		ex = exErr
	} else {
		// Convert regular error to Exception
		ex = core.NewException("RuntimeError", err.Error())
	}

	// Add current traceback if not present
	if ex.Traceback == nil || len(ex.Traceback) == 0 {
		ex.Traceback = e.getCurrentTraceback()
	}

	return ex
}

func (e *Evaluator) Apply(fn core.LispValue, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Unwrap the function if it's a LocatedValue
	unwrappedFn := e.unwrapLocatedValue(fn)

	// Extract location information if available
	location := e.getLocationFromExpr(fn)

	// Convert args to string for traceback
	argsStr := "("
	for i, arg := range args {
		if i > 0 {
			argsStr += ", "
		}
		// Limit the string representation for brevity
		argStr := core.PrintValue(arg)
		if len(argStr) > 20 {
			argStr = argStr[:17] + "..."
		}
		argsStr += argStr
	}
	argsStr += ")"

	// Function name for call stack
	var funcName string
	switch f := unwrappedFn.(type) {
	case core.BuiltinFunc:
		funcName = "<builtin>"
	case *core.Lambda:
		funcName = "<lambda>"
	case core.LispSymbol:
		funcName = string(f)
	default:
		funcName = "<anonymous>"
	}

	// Create a statement representation for the traceback
	statement := funcName + argsStr

	// Push a new call frame
	e.pushCallFrame(funcName, location, statement)

	// Defer popping the call frame to ensure it happens even on panic
	defer e.popCallFrame()

	var result core.LispValue
	var err error

	// Apply the function based on its type
	switch f := unwrappedFn.(type) {
	case core.BuiltinFunc:
		result, err = f(args, env)
	case *core.Lambda:
		// Use the lambda directly - the instance ID mechanism ensures proper state isolation
		result, err = special_forms.ApplyLambda(e, f, args, env)
	case core.LispList:
		if len(f) > 0 && f[0] == core.LispSymbol("lambda") {
			lambda, lambdaErr := special_forms.EvalLambdaPython(e, f[1:], env)
			if lambdaErr != nil {
				err = lambdaErr
				break
			}
			result, err = special_forms.ApplyLambda(e, lambda.(*core.Lambda), args, env)
		} else {
			err = fmt.Errorf("not a function: %v", fn)
		}
	default:
		err = fmt.Errorf("not a function: %v", fn)
	}

	// Enrich error with traceback if needed
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	return result, nil
}

func evalSymbol(symbol core.LispSymbol, env core.Environment) (core.LispValue, error) {
	switch symbol {
	case "None":
		return core.PythonicNone{}, nil
	case "True":
		return core.PythonicBool(true), nil
	case "False":
		return core.PythonicBool(false), nil
	default:
		value, ok := env.Get(symbol)
		if !ok {
			return nil, fmt.Errorf("undefined symbol: %s", symbol)
		}
		return value, nil
	}
}

func (e *Evaluator) evalList(list core.LispList, env core.Environment) (core.LispValue, error) {
	if len(list) == 0 {
		return core.LispList{}, nil
	}

	first := list[0]
	rest := list[1:]

	switch v := first.(type) {
	case core.LispSymbol:
		if specialForm, ok := e.specialForms[v]; ok {
			result, err := specialForm(e, rest, env)
			if err != nil {
				return nil, e.enrichErrorWithTraceback(err)
			}
			return result, nil
		}

		// Check for special form marker in the environment
		if value, ok := env.Get(v); ok {
			if marker, ok := value.(core.SpecialFormMarker); ok {
				if specialForm, ok := e.specialForms[marker.Name]; ok {
					result, err := specialForm(e, rest, env)
					if err != nil {
						return nil, e.enrichErrorWithTraceback(err)
					}
					return result, nil
				}
			}
		}
		fn, err := e.Eval(v, env)
		if err != nil {
			err = fmt.Errorf("error evaluating symbol %s: %v", v, err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			err = fmt.Errorf("error evaluating arguments: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		return e.Apply(fn, args, env)
	default:
		fn, err := e.Eval(first, env)
		if err != nil {
			err = fmt.Errorf("error evaluating function: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			err = fmt.Errorf("error evaluating arguments: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}
		return e.Apply(fn, args, env)
	}
}

func (e *Evaluator) evalDict(dict *core.PythonicDict, env core.Environment) (core.LispValue, error) {
	result := core.NewPythonicDict()

	// Iterate over the original dict and evaluate each key-value pair
	keyFunc := func(key, value core.LispValue) error {
		evaluatedKey, err := e.Eval(key, env)
		if err != nil {
			return e.enrichErrorWithTraceback(err)
		}
		evaluatedValue, err := e.Eval(value, env)
		if err != nil {
			return e.enrichErrorWithTraceback(err)
		}
		result.Set(evaluatedKey, evaluatedValue)
		return nil
	}

	err := dict.Iterate(keyFunc)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	return result, nil
}

func (e *Evaluator) evalSet(set *core.PythonicSet, env core.Environment) (core.LispValue, error) {
	newSet := core.NewPythonicSet()
	for v := range set.Data() {
		evalValue, err := e.Eval(v, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}
		newSet.Add(evalValue)
	}
	return newSet, nil
}

func (e *Evaluator) applyLambda(lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	result, err := special_forms.ApplyLambda(e, lambda, args, env)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}
	return result, nil
}

// evalComprehension evaluates a list comprehension: [expr for var in iterable if condition]
func (e *Evaluator) evalComprehension(comp core.LispComprehension, env core.Environment) (core.LispValue, error) {
	// Evaluate the iterable expression
	iterableVal, err := e.Eval(comp.Iterable, env)
	if err != nil {
		err = fmt.Errorf("error evaluating iterable in list comprehension: %v", err)
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Convert the iterable to a list
	var iterList core.LispList
	switch v := iterableVal.(type) {
	case core.LispList:
		iterList = v
	case core.LispListLiteral:
		iterList = core.LispList(v)
	case string:
		// Handle strings by converting them to a list of characters
		iterList = make(core.LispList, len(v))
		for i, ch := range v {
			iterList[i] = string(ch)
		}
	default:
		err = fmt.Errorf("iterable in list comprehension must be a list or string, got %T", iterableVal)
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Create a result list
	var result core.LispList

	// Create a new environment for the loop
	loopEnv := env.NewEnvironment(env)

	// Iterate over the iterable
	for _, item := range iterList {
		// Bind the variable
		loopEnv.Define(comp.Variable, item)

		// Check the condition if there is one
		if comp.Condition != nil {
			condVal, err := e.Eval(comp.Condition, loopEnv)
			if err != nil {
				err = fmt.Errorf("error evaluating condition in list comprehension: %v", err)
				return nil, e.enrichErrorWithTraceback(err)
			}
			// Skip this iteration if the condition is false
			if !core.IsTruthy(condVal) {
				continue
			}
		}

		// Evaluate the expression
		exprVal, err := e.Eval(comp.Expression, loopEnv)
		if err != nil {
			err = fmt.Errorf("error evaluating expression in list comprehension: %v", err)
			return nil, e.enrichErrorWithTraceback(err)
		}

		// Add the result to our list
		result = append(result, exprVal)
	}

	return result, nil
}

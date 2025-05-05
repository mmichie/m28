package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/special_forms"
)

type Evaluator struct {
	specialForms map[core.LispSymbol]special_forms.SpecialFormFunc
}

func NewEvaluator() core.Evaluator {
	return &Evaluator{
		specialForms: special_forms.GetSpecialForms(),
	}
}

func (e *Evaluator) Eval(expr core.LispValue, env core.Environment) (core.LispValue, error) {
	switch v := expr.(type) {
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
				return nil, fmt.Errorf("undefined object: %s", current)
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
						return nil, fmt.Errorf("attribute %s not found in %s", attrName, current)
					}
					current = current + "." + parts[i]
				} else {
					return nil, fmt.Errorf("%s is not an object with attributes", current)
				}
			}

			return currentObj, nil
		}

		// Regular symbol lookup
		value, ok := env.Get(v)
		if !ok {
			return nil, fmt.Errorf("undefined symbol: %s", v)
		}
		return value, nil
	case float64, int, string, core.PythonicBool, core.PythonicNone:
		return v, nil
	case *core.PythonicDict:
		// Evaluate dictionary literals
		return e.evalDict(v, env)
	case *core.PythonicSet:
		// Evaluate set literals
		return e.evalSet(v, env)
	case core.LispListLiteral:
		// Evaluate list literal elements
		result := make(core.LispList, len(v))
		for i, elem := range v {
			evalElem, err := e.Eval(elem, env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating list element: %v", err)
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
				return nil, fmt.Errorf("error evaluating tuple element: %v", err)
			}
			result[i] = evalElem
		}
		return result, nil
	case core.LispComprehension:
		return e.evalComprehension(v, env)
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
					return nil, fmt.Errorf("undefined object: %s", current)
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
							return nil, fmt.Errorf("attribute %s not found in %s", attrName, current)
						}
						current = current + "." + parts[i]
					} else {
						return nil, fmt.Errorf("%s is not an object with attributes", current)
					}
				}

				// Get the function name (last part)
				funcName := core.LispSymbol(parts[len(parts)-1])

				// Check if the object is a dictionary to get the function
				if dict, ok := currentObj.(*core.PythonicDict); ok {
					// Get the function
					fnVal, found := dict.Get(funcName)
					if !found {
						return nil, fmt.Errorf("function %s not found in %s", funcName, current)
					}

					// Evaluate the arguments
					args, err := e.evalArgs(rest, env)
					if err != nil {
						return nil, fmt.Errorf("error evaluating arguments: %v", err)
					}

					// Apply the function
					return e.Apply(fnVal, args, env)
				} else {
					return nil, fmt.Errorf("%s is not an object with methods", current)
				}
			} else if f == core.LispSymbol("=") {
				// Special handling for assignment
				if len(rest) != 2 {
					return nil, fmt.Errorf("= requires exactly two arguments")
				}
				symbol, ok := rest[0].(core.LispSymbol)
				if !ok {
					return nil, fmt.Errorf("first argument to = must be a symbol")
				}
				value, err := e.Eval(rest[1], env)
				if err != nil {
					return nil, err
				}
				env.Define(symbol, value)
				return value, nil
			}
			if specialForm, ok := e.specialForms[f]; ok {
				return specialForm(e, rest, env)
			}
			
			// Check for special form marker in the environment
			if value, ok := env.Get(f); ok {
				if marker, ok := value.(core.SpecialFormMarker); ok {
					if specialForm, ok := e.specialForms[marker.Name]; ok {
						return specialForm(e, rest, env)
					}
				}
			}
			fn, err := e.Eval(f, env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating symbol %s: %v", f, err)
			}
			args, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating arguments: %v", err)
			}
			return e.Apply(fn, args, env)
		default:
			fn, err := e.Eval(first, env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating function: %v", err)
			}
			args, err := e.evalArgs(rest, env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating arguments: %v", err)
			}
			return e.Apply(fn, args, env)
		}
	default:
		return nil, fmt.Errorf("unknown expression type: %T", expr)
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
			return nil, err
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

func (e *Evaluator) Apply(fn core.LispValue, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	switch f := fn.(type) {
	case core.BuiltinFunc:
		return f(args, env)
	case *core.Lambda:
		// Use the lambda directly - the instance ID mechanism ensures proper state isolation
		return special_forms.ApplyLambda(e, f, args, env)
	case core.LispList:
		if len(f) > 0 && f[0] == core.LispSymbol("lambda") {
			lambda, err := special_forms.EvalLambdaPython(e, f[1:], env)
			if err != nil {
				return nil, err
			}
			return special_forms.ApplyLambda(e, lambda.(*core.Lambda), args, env)
		}
	}
	return nil, fmt.Errorf("not a function: %v", fn)
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
			return specialForm(e, rest, env)
		}
		
		// Check for special form marker in the environment
		if value, ok := env.Get(v); ok {
			if marker, ok := value.(core.SpecialFormMarker); ok {
				if specialForm, ok := e.specialForms[marker.Name]; ok {
					return specialForm(e, rest, env)
				}
			}
		}
		fn, err := e.Eval(v, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating symbol %s: %v", v, err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating arguments: %v", err)
		}
		return e.Apply(fn, args, env)
	default:
		fn, err := e.Eval(first, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating function: %v", err)
		}
		args, err := e.evalArgs(rest, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating arguments: %v", err)
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
			return err
		}
		evaluatedValue, err := e.Eval(value, env)
		if err != nil {
			return err
		}
		result.Set(evaluatedKey, evaluatedValue)
		return nil
	}

	err := dict.Iterate(keyFunc)
	if err != nil {
		return nil, err
	}

	return result, nil
}

func (e *Evaluator) evalSet(set *core.PythonicSet, env core.Environment) (core.LispValue, error) {
	newSet := core.NewPythonicSet()
	for v := range set.Data() {
		evalValue, err := e.Eval(v, env)
		if err != nil {
			return nil, err
		}
		newSet.Add(evalValue)
	}
	return newSet, nil
}

func (e *Evaluator) applyLambda(lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return special_forms.ApplyLambda(e, lambda, args, env)
}

// evalComprehension evaluates a list comprehension: [expr for var in iterable if condition]
func (e *Evaluator) evalComprehension(comp core.LispComprehension, env core.Environment) (core.LispValue, error) {
	// Evaluate the iterable expression
	iterableVal, err := e.Eval(comp.Iterable, env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating iterable in list comprehension: %v", err)
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
		return nil, fmt.Errorf("iterable in list comprehension must be a list or string, got %T", iterableVal)
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
				return nil, fmt.Errorf("error evaluating condition in list comprehension: %v", err)
			}
			// Skip this iteration if the condition is false
			if !core.IsTruthy(condVal) {
				continue
			}
		}

		// Evaluate the expression
		exprVal, err := e.Eval(comp.Expression, loopEnv)
		if err != nil {
			return nil, fmt.Errorf("error evaluating expression in list comprehension: %v", err)
		}

		// Add the result to our list
		result = append(result, exprVal)
	}

	return result, nil
}

package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterDotForms registers the dot notation special forms
func RegisterDotForms(forms map[core.LispSymbol]SpecialFormFunc) {
	forms["dot"] = EvalDot
	forms["."] = EvalDot
}

// EvalDot implements the dot notation for method access and property access
// It supports objects that implement the DotAccessible interface and nested property access
func EvalDot(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Validate arguments
	if len(args) < 2 {
		return nil, fmt.Errorf(core.ErrDotMissingArgs)
	}

	// Get the object
	object, err := e.Eval(args[0], env)
	if err != nil {
		return nil, fmt.Errorf(core.ErrDotObjectEval, err)
	}

	// Get the property/method name
	var propertyName string
	switch prop := args[1].(type) {
	case string:
		propertyName = prop
	case core.LispSymbol:
		propertyName = string(prop)
	default:
		// Evaluate it if needed
		evalProp, err := e.Eval(args[1], env)
		if err != nil {
			return nil, fmt.Errorf(core.ErrDotPropertyEval, err)
		}

		// Convert to string
		switch ep := evalProp.(type) {
		case string:
			propertyName = ep
		case core.LispSymbol:
			propertyName = string(ep)
		default:
			propertyName = fmt.Sprintf("%v", evalProp)
		}
	}

	// If there are more arguments, it's a method call or nested property access
	if len(args) > 2 {
		// Check if this is a method call with arguments
		if isMethodCall(args, 2) {
			// Evaluate all method arguments
			methodArgs := make([]core.LispValue, 0, len(args)-2)
			for _, arg := range args[2:] {
				evalArg, err := e.Eval(arg, env)
				if err != nil {
					return nil, fmt.Errorf("error evaluating method argument: %v", err)
				}
				methodArgs = append(methodArgs, evalArg)
			}

			// First get the member using the unified AccessObjectMember helper
			member, err := core.AccessObjectMember(object, propertyName, e, env)
			if err != nil {
				return nil, err
			}

			// Call the method/function
			switch fn := member.(type) {
			case core.Applicable:
				// If it's directly applicable, apply it
				return fn.Apply(e, methodArgs, env)
			case core.BuiltinFunc:
				// If it's a builtin function, call it
				return fn(methodArgs, env)
			default:
				// Otherwise, it's not callable
				return nil, fmt.Errorf("'%s' is not a callable method", propertyName)
			}
		} else {
			// This is nested property access (multiple property names)
			current := object
			currentPath := fmt.Sprintf("%v.%s", args[0], propertyName)

			// Get the first property using the unified accessor
			current, err = core.AccessObjectMember(current, propertyName, e, env)
			if err != nil {
				return nil, err
			}

			// Process each subsequent property in sequence
			for i := 2; i < len(args); i++ {
				// Get the next property name
				var nextProp string
				switch prop := args[i].(type) {
				case string:
					nextProp = prop
				case core.LispSymbol:
					nextProp = string(prop)
				default:
					// Evaluate it if needed
					evalProp, err := e.Eval(args[i], env)
					if err != nil {
						return nil, fmt.Errorf(core.ErrDotPropertyEval, err)
					}

					// Convert to string
					switch ep := evalProp.(type) {
					case string:
						nextProp = ep
					case core.LispSymbol:
						nextProp = string(ep)
					default:
						nextProp = fmt.Sprintf("%v", evalProp)
					}
				}

				// Access the next property using the unified accessor
				current, err = core.AccessObjectMember(current, nextProp, e, env)
				if err != nil {
					return nil, core.ErrDotNestedAccessf(currentPath, nextProp, err)
				}

				// Update current path for error reporting
				currentPath += "." + nextProp
			}

			return current, nil
		}
	} else {
		// Simple property access using the unified accessor
		return core.AccessObjectMember(object, propertyName, e, env)
	}
}

// Helper function to check if arguments represent a method call
// Method calls are identified by having a callable object at the specified index
func isMethodCall(args []core.LispValue, idx int) bool {
	// For now, we'll use a simple heuristic based on the number of args
	// and their types. In the future, this could be more sophisticated.
	if idx >= len(args) {
		return false
	}

	// If the next argument is a list or another complex structure,
	// it's more likely to be method arguments than nested property access
	switch args[idx].(type) {
	case core.LispList, core.LispListLiteral, core.LispTuple:
		return true
	}

	// Default to assuming it's a method call if there are additional arguments
	return true
}

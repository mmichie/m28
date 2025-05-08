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
		eval_prop, err := e.Eval(args[1], env)
		if err != nil {
			return nil, fmt.Errorf(core.ErrDotPropertyEval, err)
		}

		// Convert to string
		switch ep := eval_prop.(type) {
		case string:
			propertyName = ep
		case core.LispSymbol:
			propertyName = string(ep)
		default:
			propertyName = fmt.Sprintf("%v", eval_prop)
		}
	}

	// Check if this is a nested property access (multiple property names)
	if len(args) > 2 && !isMethodCallWithArgs(args, 2) {
		current := object
		currentPath := fmt.Sprintf("%v.%s", args[0], propertyName)
		
		// Get the first property
		current, err = accessProperty(current, propertyName)
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
			
			// Access the next property
			current, err = accessProperty(current, nextProp)
			if err != nil {
				return nil, core.ErrDotNestedAccessf(currentPath, nextProp, err)
			}
			
			// Update current path for error reporting
			currentPath += "." + nextProp
		}
		
		return current, nil
	}

	// First check if the object implements the DotAccessible interface
	if dotObj, ok := object.(core.DotAccessible); ok {
		// Check if this is a method call (with additional arguments)
		if len(args) > 2 {
			// Extract the method arguments (skip the property name)
			methodArgs := []core.LispValue{}
			for _, arg := range args[2:] {
				evalArg, err := e.Eval(arg, env)
				if err != nil {
					return nil, err
				}
				methodArgs = append(methodArgs, evalArg)
			}

			// Check if the method exists
			if dotObj.HasMethod(propertyName) {
				// Use the CallMethod function to invoke the method
				result, err := dotObj.CallMethod(propertyName, methodArgs)
				if err != nil {
					return nil, core.ErrDotMethodCallf(propertyName, err)
				}
				return result, nil
			} else {
				return nil, core.ErrDotNoMethodf(propertyName)
			}
		}

		// Property access - first check if it's a registered method without arguments
		if dotObj.HasMethod(propertyName) {
			// If it's a method without arguments, call it with an empty args slice
			result, err := dotObj.CallMethod(propertyName, []core.LispValue{})
			if err != nil {
				return nil, core.ErrDotMethodCallf(propertyName, err)
			}
			return result, nil
		}

		// Regular property access
		if dotObj.HasProperty(propertyName) {
			prop, ok := dotObj.GetProperty(propertyName)
			if !ok {
				return nil, fmt.Errorf(core.ErrDotPropertyNotFound, propertyName)
			}
			return prop, nil
		} else {
			return nil, core.ErrDotNoPropertyf(propertyName)
		}
	}

	// Fall back to type-specific handling for objects that don't implement DotAccessible
	switch obj := object.(type) {
	case *core.Generator:
		// Handle generator methods
		switch propertyName {
		case "next":
			return core.PythonicNone{}, fmt.Errorf("generator.next requires an evaluator, use the 'next' built-in function instead")
		default:
			return nil, core.ErrDotNoMethodf(propertyName)
		}

	case *core.Lambda:
		// Handle lambda objects (functions) with methods
		if propertyName == "call" {
			// Evaluate all arguments
			callArgs := []core.LispValue{}
			for _, arg := range args[2:] {
				evalArg, err := e.Eval(arg, env)
				if err != nil {
					return nil, err
				}
				callArgs = append(callArgs, evalArg)
			}

			// Apply the function with the arguments
			return e.Apply(obj, callArgs, env)
		}
		return nil, core.ErrDotNoMethodf(propertyName)

	case *core.PythonicDict:
		// This should rarely happen since PythonicDict implements DotAccessible
		value, ok := obj.Get(propertyName)
		if !ok {
			return nil, core.ErrDotNoPropertyf(propertyName)
		}
		return value, nil

	default:
		return nil, core.ErrDotMissingInterfacef(object)
	}
}

// Helper function to check if arguments represent a method call
// Method calls are identified by having a callable object at the specified index
func isMethodCallWithArgs(args []core.LispValue, idx int) bool {
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
	
	// Default to not being a method call
	return false
}

// Helper function to access a property on an object
func accessProperty(obj core.LispValue, propName string) (core.LispValue, error) {
	// First check if the object implements DotAccessible
	if dotObj, ok := obj.(core.DotAccessible); ok {
		// Check if it's a property
		if dotObj.HasProperty(propName) {
			value, _ := dotObj.GetProperty(propName)
			return value, nil
		}
		
		// Check if it's a method (return a method reference)
		if dotObj.HasMethod(propName) {
			// Return a method reference (builtin function)
			return core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
				return dotObj.CallMethod(propName, args)
			}), nil
		}
		
		return nil, core.ErrDotNoPropertyf(propName)
	}
	
	// Handle special cases for non-DotAccessible objects
	switch typedObj := obj.(type) {
	case *core.PythonicDict:
		// This should rarely happen since PythonicDict should implement DotAccessible
		value, ok := typedObj.Get(propName)
		if !ok {
			return nil, core.ErrDotNoPropertyf(propName)
		}
		return value, nil
		
	case *core.Generator:
		if propName == "next" {
			return nil, fmt.Errorf(core.ErrDotEvaluatorMissing, "next")
		}
		return nil, core.ErrDotNoPropertyf(propName)
		
	case *core.Lambda:
		if propName == "call" {
			return typedObj, nil // Return the lambda itself for later application
		}
		return nil, core.ErrDotNoPropertyf(propName)
		
	default:
		return nil, core.ErrDotMissingInterfacef(obj)
	}
}
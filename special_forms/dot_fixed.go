package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterDotForms registers the dot notation special forms
func RegisterDotForms(forms map[core.LispSymbol]SpecialFormFunc) {
	forms["dot"] = EvalDotFixed
	forms["."] = EvalDotFixed
}

// EvalDotFixed implements the dot notation for method access and property access
// This is a fixed version that handles dictionaries and other objects properly
func EvalDotFixed(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("dot notation requires at least an object and property/method")
	}

	// Get the object first
	object, err := e.Eval(args[0], env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Get the property/method name (second argument)
	var propertyName string

	// Use the property directly as a string if possible
	switch prop := args[1].(type) {
	case string:
		propertyName = prop
	case core.LispSymbol:
		propertyName = string(prop)
	default:
		// Evaluate it
		eval_prop, err := e.Eval(args[1], env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating property name: %v", err)
		}

		// Try to convert to string
		switch ep := eval_prop.(type) {
		case string:
			propertyName = ep
		case core.LispSymbol:
			propertyName = string(ep)
		default:
			propertyName = fmt.Sprintf("%v", eval_prop)
		}
	}

	// Type-specific handling based on the object type
	switch obj := object.(type) {
	case *core.PythonicDict:
		// Check if this is a method call (with additional arguments)
		if len(args) > 2 {
			// Evaluate all arguments for the method call
			methodArgs := []core.LispValue{}

			for _, arg := range args[2:] {
				evalArg, err := e.Eval(arg, env)
				if err != nil {
					return nil, err
				}
				methodArgs = append(methodArgs, evalArg)
			}

			// Use the CallMethod function to invoke the method
			return obj.CallMethod(propertyName, methodArgs)
		}

		// Property access - first check if it's a registered method without arguments
		if obj.HasMethod(propertyName) {
			// If it's a method without arguments, call it with an empty args slice
			return obj.CallMethod(propertyName, []core.LispValue{})
		}

		// Regular property access
		prop, ok := obj.Get(propertyName)
		if !ok {
			return nil, fmt.Errorf("object has no property '%s'", propertyName)
		}

		return prop, nil

	case *core.Generator:
		// Handle generator methods
		switch propertyName {
		case "next":
			// For now, return a placeholder and error since we can't call Next without an evaluator
			return core.PythonicNone{}, fmt.Errorf("generator.next requires an evaluator, use the 'next' built-in function instead")
		default:
			return nil, fmt.Errorf("generator has no method '%s'", propertyName)
		}

	case *core.Lambda:
		// Handle lambda objects (functions) with methods
		// This allows for function objects with attached methods
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
		return nil, fmt.Errorf("function has no method '%s'", propertyName)

	default:
		return nil, fmt.Errorf("object type %T does not support dot notation", object)
	}
}

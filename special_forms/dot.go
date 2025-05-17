package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterDotForms registers the dot notation special forms
func RegisterDotForms(forms map[core.LispSymbol]SpecialFormFunc) {
	forms["dot"] = EvalDot
	forms["."] = EvalDot
	forms["set-prop"] = EvalSetProperty // Direct property setter
	forms["get-prop"] = EvalGetProperty // Direct property getter
}

// EvalDot implements the dot notation for method access and property access
// It supports objects that implement various object access interfaces for property and method access
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

	if object == nil {
		return nil, fmt.Errorf("cannot access properties on nil")
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

			// Use FastGetPropFrom to get the member
			member, exists := core.FastGetPropFrom(object, propertyName)
			if !exists {
				return nil, fmt.Errorf("object has no property '%s'", propertyName)
			}

			// Call the method/function
			switch fn := member.(type) {
			case core.Applicable:
				// If it's directly applicable, apply it
				return fn.Apply(e, methodArgs, env)
			case core.BuiltinFunc:
				// If it's a builtin function, call it
				return fn(methodArgs, env)
			case core.ObjMethodImpl:
				// If it's an object method implementation, call it
				return fn.CallMethod(methodArgs, e, env)
			default:
				// Try to call method using CallMethodPOn
				if core.FastHasMethodPOn(object, propertyName) {
					return core.FastCallMethodPOn(object, propertyName, methodArgs, e, env)
				}
				// Otherwise, it's not callable
				return nil, fmt.Errorf("'%s' is not a callable method (type: %T)", propertyName, member)
			}
		} else {
			// This is nested property access (multiple property names)
			current := object
			currentPath := fmt.Sprintf("%v.%s", args[0], propertyName)

			// Get the first property using FastGetPropFrom
			val, exists := core.FastGetPropFrom(current, propertyName)
			if !exists {
				return nil, fmt.Errorf("object has no property '%s'", propertyName)
			}
			current = val

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

				// Access the next property using FastGetPropFrom
				val, exists := core.FastGetPropFrom(current, nextProp)
				if !exists {
					return nil, core.ErrDotNestedAccessf(currentPath, nextProp, fmt.Errorf("object has no property '%s'", nextProp))
				}
				current = val

				// Update current path for error reporting
				currentPath += "." + nextProp
			}

			return current, nil
		}
	} else {
		// Simple property access using FastGetPropFrom
		val, exists := core.FastGetPropFrom(object, propertyName)
		if !exists {
			return nil, fmt.Errorf("object has no property '%s'", propertyName)
		}
		return val, nil
	}
}

// Helper function to check if arguments represent a method call
// Method calls are identified by having arguments after the method name
func isMethodCall(args []core.LispValue, idx int) bool {
	// If there's no room for arguments, can't be a method call
	if idx >= len(args) {
		return false
	}

	// Check for empty argument list (no args but still a call)
	if len(args) == idx {
		return true
	}

	// If the next argument is a list or another complex structure,
	// it's more likely to be method arguments than nested property access
	switch args[idx].(type) {
	case core.LispList, core.LispListLiteral, core.LispTuple:
		return true
	case core.LispSymbol:
		// If this is a symbol that looks like a property name,
		// it's likely a nested property access instead of a method call
		// But we need some heuristic to decide...
		return true
	}

	// Default to assuming it's a method call if there are additional arguments
	return true
}

// EvalGetProperty implements direct property access for objects
// Usage: (get-prop object "property-name")
func EvalGetProperty(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Validate arguments
	if len(args) != 2 {
		return nil, fmt.Errorf("get-prop requires exactly 2 arguments: object and property name")
	}

	// Get the object
	object, err := e.Eval(args[0], env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object expression: %v", err)
	}

	// Get the property name
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
			return nil, fmt.Errorf("error evaluating property name: %v", err)
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

	// Use FastGetPropFrom directly
	value, exists := core.FastGetPropFrom(object, propertyName)
	if !exists {
		return nil, fmt.Errorf("object has no property '%s'", propertyName)
	}

	return value, nil
}

// EvalSetProperty implements direct property setting for objects
// Usage: (set-prop object "property-name" value)
func EvalSetProperty(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Validate arguments
	if len(args) != 3 {
		return nil, fmt.Errorf("set-prop requires exactly 3 arguments: object, property name, and value")
	}

	// Get the object
	object, err := e.Eval(args[0], env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object expression: %v", err)
	}

	// Get the property name
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
			return nil, fmt.Errorf("error evaluating property name: %v", err)
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

	// Evaluate the value to set
	value, err := e.Eval(args[2], env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating value: %v", err)
	}

	// Use FastSetPropOn directly
	if err := core.FastSetPropOn(object, propertyName, value); err != nil {
		return nil, err
	}

	// Return the set value
	return value, nil
}

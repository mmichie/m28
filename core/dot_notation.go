package core

import (
	"fmt"
)

// DotNotationHelper contains utility functions for dot notation access
// This provides a unified implementation for member access across different interfaces

// AccessObjectMember is a helper function to access a member via dot notation
// This provides a common implementation that can be used by both the evaluator and special forms
func AccessObjectMember(obj LispValue, name string, eval Evaluator, env Environment) (LispValue, error) {
	// Try the new Object Protocol first
	if val, exists := GetPropFrom(obj, name); exists {
		return val, nil
	}

	// Try direct property access for PythonicObject instances
	if val, exists := DirectGetProp(obj, name); exists {
		return val, nil
	}

	// Fall back to legacy interfaces
	// First, check if the object implements the EvaluatorAware interface
	if evalAware, ok := obj.(EvaluatorAware); ok {
		// Use the newer interface which includes evaluator context
		return evalAware.GetMember(name, eval, env)
	}

	// If the object implements DotAccessible (legacy interface)
	if dotObj, ok := obj.(DotAccessible); ok {
		// Access via DotAccessible interface
		// First check property
		if dotObj.HasProperty(name) {
			if val, ok := dotObj.GetProperty(name); ok {
				return val, nil
			}
		}

		// Then check method
		if dotObj.HasMethod(name) {
			// For method access without call, just return the method directly
			// It can be called later via dotObj.CallMethod
			// We wrap it in a BuiltinFunc for ease of use
			return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
				return dotObj.CallMethod(name, args)
			}), nil
		}

		return nil, fmt.Errorf("object has no attribute '%s'", name)
	}

	// Legacy special handling for PythonicDict
	if dict, ok := obj.(*PythonicDict); ok {
		// Check for method
		if dict.HasMethod(name) {
			method, _ := dict.methods[name]
			// We wrap it in a BuiltinFunc for later calling
			return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
				return method(dict, args)
			}), nil
		}

		// Check for property/attribute
		if value, exists := dict.Get(name); exists {
			return value, nil
		}

		return nil, fmt.Errorf("dict has no attribute '%s'", name)
	}

	// Legacy special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		// Use the GetMember method which should be implemented by PythonicObject
		return pyObj.GetMember(name, eval, env)
	}

	// Handle other types with basic type-specific logic
	switch typedObj := obj.(type) {
	case *Generator:
		if name == "next" {
			return nil, fmt.Errorf("generator.next requires an evaluator reference")
		}
		return nil, fmt.Errorf("generator has no attribute '%s'", name)

	case *Lambda:
		if name == "call" {
			return typedObj, nil // Return the lambda itself for later application
		}
		return nil, fmt.Errorf("function has no attribute '%s'", name)

	default:
		return nil, fmt.Errorf("object does not support dot notation: %T", obj)
	}
}

// SetObjectMember is a helper function to set a member via dot notation
func SetObjectMember(obj LispValue, name string, value LispValue, eval Evaluator, env Environment) error {
	// Try the new Object Protocol first
	err := SetPropOn(obj, name, value)
	if err == nil {
		return nil
	}

	// Try direct property setting for better performance
	err = DirectSetProp(obj, name, value)
	if err == nil {
		return nil
	}

	// Fall back to legacy interfaces
	// First, check if the object implements the newer EvaluatorAware interface
	if evalAware, ok := obj.(EvaluatorAware); ok {
		return evalAware.SetMember(name, value, eval, env)
	}

	// If the object implements DotAccessible (legacy interface)
	if dotObj, ok := obj.(DotAccessible); ok {
		return dotObj.SetProperty(name, value)
	}

	// Legacy special handling for PythonicDict
	if dict, ok := obj.(*PythonicDict); ok {
		dict.Set(name, value)
		return nil
	}

	// Legacy special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		return pyObj.SetMember(name, value, eval, env)
	}

	return fmt.Errorf("object does not support dot notation: %T", obj)
}

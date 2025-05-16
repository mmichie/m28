package core

import (
	"fmt"
)

// DotNotationHelper contains utility functions for dot notation access
// This provides a unified implementation for member access across different interfaces

// AccessObjectMember is a helper function to access a member via dot notation
// This provides a common implementation that can be used by both the evaluator and special forms
func AccessObjectMember(obj LispValue, name string, eval Evaluator, env Environment) (LispValue, error) {
	if obj == nil {
		return nil, fmt.Errorf("cannot access member '%s' on nil", name)
	}

	// Try the new Object Protocol first
	if val, exists := GetPropFrom(obj, name); exists {
		return val, nil
	}

	// Try direct property access for PythonicObject instances
	if val, exists := DirectGetProp(obj, name); exists {
		return val, nil
	}

	// Check if the object implements ObjProtocol
	if objProto, ok := obj.(ObjProtocol); ok {
		if val, exists := objProto.GetProp(name); exists {
			return val, nil
		}
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
	}

	// Try map-like access for dictionaries
	// We check different dict types directly instead of an interface

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
	}

	// Legacy special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		// Use the GetMember method which should be implemented by PythonicObject
		return pyObj.GetMember(name, eval, env)
	}

	// Handle module-like objects (no central interface)

	// Handle other types with basic type-specific logic
	switch typedObj := obj.(type) {
	case *Generator:
		if name == "next" {
			// Return a method that can be called later
			return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
				return typedObj.NextWithEval(eval)
			}), nil
		}

	case *Lambda:
		if name == "call" {
			return typedObj, nil // Return the lambda itself for later application
		}

	case LispList:
		// Add basic list methods
		if name == "length" || name == "len" {
			return float64(len(typedObj)), nil
		}
		if name == "first" && len(typedObj) > 0 {
			return typedObj[0], nil
		}
		if name == "last" && len(typedObj) > 0 {
			return typedObj[len(typedObj)-1], nil
		}
	}

	// For unhandled types, provide a descriptive error
	return nil, fmt.Errorf("object %T has no attribute '%s'", obj, name)
}

// SetObjectMember is a helper function to set a member via dot notation
func SetObjectMember(obj LispValue, name string, value LispValue, eval Evaluator, env Environment) error {
	if obj == nil {
		return fmt.Errorf("cannot set member '%s' on nil", name)
	}

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

	// Check if the object implements ObjProtocol
	if objProto, ok := obj.(ObjProtocol); ok {
		return objProto.SetProp(name, value)
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

	// Try map-like access for dictionaries
	// We check specific types directly

	// Legacy special handling for PythonicDict
	if dict, ok := obj.(*PythonicDict); ok {
		dict.Set(name, value)
		return nil
	}

	// Legacy special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		return pyObj.SetMember(name, value, eval, env)
	}

	// Special handling for type-specific cases
	switch obj.(type) {
	case LispList:
		return fmt.Errorf("lists are immutable, cannot set %s", name)
	}

	return fmt.Errorf("object type %T does not support property setting", obj)
}

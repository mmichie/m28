package core

import "fmt"

// ObjProtocol defines the unified interface for all object-like values
// This provides a common protocol for property access and method calls
// without conflicting with existing interfaces
type ObjProtocol interface {
	// GetProp retrieves a property value
	GetProp(name string) (LispValue, bool)
	
	// SetProp sets a property value
	SetProp(name string, value LispValue) error
	
	// HasMethodP checks if a method exists (P suffix to avoid name conflicts)
	HasMethodP(name string) bool
	
	// CallMethodP calls a method with arguments (P suffix to avoid name conflicts)
	CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error)
}

// AdaptableLispValue is an interface for values that can provide an ObjProtocol adapter
type AdaptableLispValue interface {
	// AsObject returns an ObjProtocol implementation for this value
	AsObject() ObjProtocol
}

// ObjMethodImpl represents a callable method on an object
type ObjMethodImpl interface {
	// CallMethod invokes the method with arguments
	CallMethod(args []LispValue, eval Evaluator, env Environment) (LispValue, error)
}

// Helper functions for the ObjProtocol interface

// GetPropFrom is a helper function to get a property from any value
// It handles both values implementing ObjProtocol and other special types
func GetPropFrom(obj LispValue, name string) (LispValue, bool) {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.GetProp(name)
	}
	
	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().GetProp(name)
	}
	
	// Legacy handling for DotAccessible
	if dotObj, ok := obj.(DotAccessible); ok {
		return dotObj.GetProperty(name)
	}
	
	// Special handling for PythonicDict
	if dict, ok := obj.(*PythonicDict); ok {
		// Check for method
		if dict.HasMethod(name) {
			method, _ := dict.methods[name]
			// We wrap it in a BuiltinFunc for later calling
			return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
				return method(dict, args)
			}), true
		}
		
		// Check for property/attribute
		if value, exists := dict.Get(name); exists {
			return value, true
		}
	}
	
	// Special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		// First check instance attributes
		if pyObj.Attributes != nil {
			if val, exists := pyObj.Attributes.Get(name); exists {
				return val, true
			}
		}

		// Then try class methods
		if pyObj.Class != nil && pyObj.HasMethod(name) {
			method, exists := pyObj.Class.GetMethod(name)
			if exists {
				// Create a bound method that includes the object instance
				boundMethod := NewBoundMethod(method, pyObj, pyObj.evaluator)
				return boundMethod, true
			}
		}

		// Finally check class attributes
		if pyObj.Class != nil {
			if attr, exists := pyObj.Class.GetAttribute(name); exists {
				return attr, true
			}
		}

		// Fall back to the standard method if nothing found
		return pyObj.GetAttribute(name)
	}
	
	// Handle generators
	if gen, ok := obj.(*Generator); ok {
		if name == "next" {
			return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
				// We need an evaluator for generators
				if evalValue, exists := callEnv.Get("EVALUATOR"); exists {
					if eval, ok := evalValue.(Evaluator); ok {
						return gen.NextWithEval(eval)
					}
				}
				return gen.Next() // Will return an error about needing evaluator
			}), true
		}
	}
	
	// Handle lambdas
	if lambda, ok := obj.(*Lambda); ok {
		if name == "call" {
			return lambda, true // Return the lambda itself for later application
		}
	}
	
	return nil, false
}

// SetPropOn is a helper function to set a property on any value
// It handles both values implementing ObjProtocol and other special types
func SetPropOn(obj LispValue, name string, value LispValue) error {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.SetProp(name, value)
	}
	
	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().SetProp(name, value)
	}
	
	// Legacy handling for DotAccessible
	if dotObj, ok := obj.(DotAccessible); ok {
		return dotObj.SetProperty(name, value)
	}
	
	// Special handling for PythonicDict
	if dict, ok := obj.(*PythonicDict); ok {
		dict.Set(name, value)
		return nil
	}
	
	// Special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		return pyObj.SetProperty(name, value)
	}
	
	return ErrDotMissingInterfacef(obj)
}

// HasMethodPOn is a helper function to check if a value has a method
// It handles both values implementing ObjProtocol and other special types
func HasMethodPOn(obj LispValue, name string) bool {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.HasMethodP(name)
	}
	
	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().HasMethodP(name)
	}
	
	// Legacy handling for DotAccessible
	if dotObj, ok := obj.(DotAccessible); ok {
		return dotObj.HasMethod(name)
	}
	
	// Special handling for PythonicDict
	if dict, ok := obj.(*PythonicDict); ok {
		return dict.HasMethod(name)
	}
	
	// Special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		return pyObj.HasMethod(name)
	}
	
	// Special handling for special types
	switch obj.(type) {
	case *Generator:
		return name == "next"
	case *Lambda:
		return name == "call"
	}
	
	return false
}

// CallMethodPOn is a helper function to call a method on any value
// It handles both values implementing ObjProtocol and other special types
func CallMethodPOn(obj LispValue, name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.CallMethodP(name, args, eval, env)
	}
	
	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().CallMethodP(name, args, eval, env)
	}
	
	// Legacy handling for DotAccessible
	if dotObj, ok := obj.(DotAccessible); ok {
		return dotObj.CallMethod(name, args)
	}
	
	// Special handling for PythonicDict
	if dict, ok := obj.(*PythonicDict); ok {
		return dict.CallMethod(name, args)
	}
	
	// Special handling for PythonicObject
	if pyObj, ok := obj.(*PythonicObject); ok {
		// Make sure evaluator is set
		pyObj.SetEvaluator(eval)
		return pyObj.CallMethod(name, args)
	}
	
	// Special handling for special types
	switch typedObj := obj.(type) {
	case *Generator:
		if name == "next" {
			return typedObj.NextWithEval(eval)
		}
	case *Lambda:
		if name == "call" {
			return eval.Apply(typedObj, args, env)
		}
	}
	
	return nil, ErrDotNoMethodf(name)
}

// ErrDotEvaluatorMissingf formats an error message for missing evaluator
func ErrDotEvaluatorMissingf(methodName string) error {
	return fmt.Errorf(ErrDotEvaluatorMissing, methodName)
}
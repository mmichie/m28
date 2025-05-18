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
// It uses the adapter system when needed for consistent property access
// Prefer FastGetPropFrom for better performance in hot paths
func GetPropFrom(obj LispValue, name string) (LispValue, bool) {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.GetProp(name)
	}

	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().GetProp(name)
	}

	// For all other types, get an adapter from the cache
	// This handles all special cases in a unified way
	adapter := globalAdapterCache.GetAdapter(obj)
	result, exists := adapter.GetProp(name)

	// Make sure to recycle the adapter if it came from a pool
	globalAdapterCache.RecycleAdapter(adapter)

	return result, exists
}

// SetPropOn is a helper function to set a property on any value
// It uses the adapter system when needed for consistent property setting
// Prefer FastSetPropOn for better performance in hot paths
func SetPropOn(obj LispValue, name string, value LispValue) error {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.SetProp(name, value)
	}

	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().SetProp(name, value)
	}

	// For all other types, get an adapter from the cache
	// This handles all special cases in a unified way
	adapter := globalAdapterCache.GetAdapter(obj)
	err := adapter.SetProp(name, value)

	// Make sure to recycle the adapter if it came from a pool
	globalAdapterCache.RecycleAdapter(adapter)

	return err
}

// HasMethodPOn is a helper function to check if a value has a method
// It uses the adapter system when needed for consistent method checking
// Prefer FastHasMethodPOn for better performance in hot paths
func HasMethodPOn(obj LispValue, name string) bool {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.HasMethodP(name)
	}

	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().HasMethodP(name)
	}

	// For all other types, get an adapter from the cache
	// This handles all special cases in a unified way
	adapter := globalAdapterCache.GetAdapter(obj)
	result := adapter.HasMethodP(name)

	// Make sure to recycle the adapter if it came from a pool
	globalAdapterCache.RecycleAdapter(adapter)

	return result
}

// CallMethodPOn is a helper function to call a method on any value
// It uses the adapter system when needed for consistent method calling
// Prefer FastCallMethodPOn for better performance in hot paths
func CallMethodPOn(obj LispValue, name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// First try direct ObjProtocol implementation
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj.CallMethodP(name, args, eval, env)
	}

	// Then try adaptable values
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().CallMethodP(name, args, eval, env)
	}

	// For all other types, get an adapter from the cache
	// This handles all special cases in a unified way
	adapter := globalAdapterCache.GetAdapter(obj)
	result, err := adapter.CallMethodP(name, args, eval, env)

	// Make sure to recycle the adapter if it came from a pool
	globalAdapterCache.RecycleAdapter(adapter)

	return result, err
}

// ErrDotEvaluatorMissingf formats an error message for missing evaluator
func ErrDotEvaluatorMissingf(methodName string) error {
	return fmt.Errorf(ErrDotEvaluatorMissing, methodName)
}

// DirectGetProp is a helper function for direct property access on instances
// It skips intermediate steps and directly accesses the object's attributes
func DirectGetProp(obj LispValue, name string) (LispValue, bool) {
	// Handle PythonicObject directly for better performance
	if pyObj, ok := obj.(*PythonicObject); ok {
		if pyObj.Attributes != nil {
			if val, exists := pyObj.Attributes.Get(name); exists {
				return val, true
			}
		}
		return nil, false
	}

	// For all other types, fall back to the standard GetPropFrom
	return GetPropFrom(obj, name)
}

// DirectSetProp is a helper function for direct property setting on instances
// It skips intermediate steps and directly sets the object's attributes
func DirectSetProp(obj LispValue, name string, value LispValue) error {
	// Handle PythonicObject directly for better performance
	if pyObj, ok := obj.(*PythonicObject); ok {
		if pyObj.Attributes == nil {
			pyObj.Attributes = NewPythonicDict()
		}
		pyObj.Attributes.Set(name, value)
		return nil
	}

	// Handle PythonicDict directly
	if dict, ok := obj.(*PythonicDict); ok {
		dict.Set(name, value)
		return nil
	}

	// For all other types, fall back to the standard SetPropOn
	return SetPropOn(obj, name, value)
}

package core

import (
	"reflect"
	"sync"
)

// This file implements an object cache system to improve the performance of
// property access and method calls by reducing repeated type checks and lookups.

// TypeAdapterCache caches information about how to adapt various types to ObjProtocol
// This helps avoid repeated type checks and interface assertions
type TypeAdapterCache struct {
	// Cache maps type information to adapter factory functions
	cache map[reflect.Type]AdapterFactory
	mu    sync.RWMutex
}

// AdapterFactory is a function that creates an ObjProtocol adapter for a given value
type AdapterFactory func(obj LispValue) ObjProtocol

// NewTypeAdapterCache creates a new type adapter cache
func NewTypeAdapterCache() *TypeAdapterCache {
	return &TypeAdapterCache{
		cache: make(map[reflect.Type]AdapterFactory),
	}
}

// Global instance of the type adapter cache
var globalAdapterCache = NewTypeAdapterCache()

// GetAdapter retrieves or creates an adapter for the given value
func (c *TypeAdapterCache) GetAdapter(obj LispValue) ObjProtocol {
	if obj == nil {
		return &ObjAdapter{} // Default adapter for nil
	}

	// First, check if the object already implements ObjProtocol
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj
	}

	// Then check if it implements AdaptableLispValue
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject()
	}

	// Look up the type in the cache
	objType := reflect.TypeOf(obj)

	// Try to get a cached adapter factory
	c.mu.RLock()
	factory, ok := c.cache[objType]
	c.mu.RUnlock()

	if ok {
		// Use the cached factory to create an adapter
		return factory(obj)
	}

	// No cached factory, determine how to adapt this type
	var adapterFactory AdapterFactory

	switch obj.(type) {
	case *PythonicDict:
		adapterFactory = func(o LispValue) ObjProtocol {
			return dictAdapter(o.(*PythonicDict))
		}
	case *PythonicObject:
		adapterFactory = func(o LispValue) ObjProtocol {
			return objectAdapter(o.(*PythonicObject))
		}
	case *SuperObject:
		adapterFactory = func(o LispValue) ObjProtocol {
			return superObjectAdapter(o.(*SuperObject))
		}
	case LispList:
		adapterFactory = func(o LispValue) ObjProtocol {
			return &LispListAdapter{list: o.(LispList)}
		}
	case LispListLiteral:
		adapterFactory = func(o LispValue) ObjProtocol {
			return &LispListAdapter{list: LispList(o.(LispListLiteral))}
		}
	case LispTuple:
		adapterFactory = func(o LispValue) ObjProtocol {
			return &LispTupleAdapter{tuple: o.(LispTuple)}
		}
	case string:
		adapterFactory = func(o LispValue) ObjProtocol {
			return &StringAdapter{str: o.(string)}
		}
	case *PythonicSet:
		adapterFactory = func(o LispValue) ObjProtocol {
			return &PythonicSetAdapter{set: o.(*PythonicSet)}
		}
	case DotAccessible:
		adapterFactory = func(o LispValue) ObjProtocol {
			return dotAccessibleAdapter(o.(DotAccessible))
		}
	default:
		// Default adapter factory for unknown types
		adapterFactory = func(o LispValue) ObjProtocol {
			return &ObjAdapter{}
		}
	}

	// Cache the adapter factory for future use
	c.mu.Lock()
	c.cache[objType] = adapterFactory
	c.mu.Unlock()

	// Use the factory to create an adapter
	return adapterFactory(obj)
}

// FastGetPropFrom is an optimized version of GetPropFrom that uses the adapter cache
func FastGetPropFrom(obj LispValue, name string) (LispValue, bool) {
	// Use special fast paths for common types
	switch typedObj := obj.(type) {
	case *PythonicDict:
		// Fast path for dictionaries
		if typedObj.HasMethod(name) {
			method, _ := typedObj.methods[name]
			// We wrap it in a BuiltinFunc for later calling
			return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
				return method(typedObj, args)
			}), true
		}

		// Check for property/attribute
		return typedObj.Get(name)

	case *PythonicObject:
		// Fast path for objects
		if typedObj.Attributes != nil {
			if val, exists := typedObj.Attributes.Get(name); exists {
				return val, true
			}
		}

		// Then try class methods
		if typedObj.Class != nil && typedObj.HasMethod(name) {
			method, exists := typedObj.Class.GetMethod(name)
			if exists {
				// Create a bound method that includes the object instance
				boundMethod := NewBoundMethod(method, typedObj, typedObj.evaluator)
				return boundMethod, true
			}
		}

		// Finally check class attributes
		if typedObj.Class != nil {
			if attr, exists := typedObj.Class.GetAttribute(name); exists {
				return attr, true
			}
		}

		// Fall back to the standard method
		return typedObj.GetAttribute(name)

	case LispList:
		// Fast path for lists
		if idx, err := ParseIndex(name); err == nil && idx >= 0 && idx < len(typedObj) {
			return typedObj[idx], true
		}

		// Check for common methods
		switch name {
		case "length", "len":
			return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
				return float64(len(typedObj)), nil
			}), true
		}

	case string:
		// Fast path for strings
		if idx, err := ParseIndex(name); err == nil && idx >= 0 && idx < len(typedObj) {
			return string(typedObj[idx]), true
		}

		// Check for common methods
		switch name {
		case "length", "len":
			return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
				return float64(len(typedObj)), nil
			}), true
		}
	}

	// Use the cached adapter for the general case
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.GetProp(name)
}

// FastSetPropOn is an optimized version of SetPropOn that uses the adapter cache
func FastSetPropOn(obj LispValue, name string, value LispValue) error {
	// Use special fast paths for common types
	switch typedObj := obj.(type) {
	case *PythonicDict:
		// Fast path for dictionaries
		typedObj.Set(name, value)
		return nil

	case *PythonicObject:
		// Fast path for objects
		if typedObj.Attributes == nil {
			typedObj.Attributes = NewPythonicDict()
		}
		typedObj.Attributes.Set(name, value)
		return nil

	case LispList:
		// Fast path for lists
		if idx, err := ParseIndex(name); err == nil && idx >= 0 && idx < len(typedObj) {
			typedObj[idx] = value
			return nil
		}
	}

	// Use the cached adapter for the general case
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.SetProp(name, value)
}

// FastHasMethodPOn is an optimized version of HasMethodPOn that uses the adapter cache
func FastHasMethodPOn(obj LispValue, name string) bool {
	// Use special fast paths for common types
	switch typedObj := obj.(type) {
	case *PythonicDict:
		// Fast path for dictionaries
		return typedObj.HasMethod(name)

	case *PythonicObject:
		// Fast path for objects
		return typedObj.HasMethod(name)
	}

	// Use the cached adapter for the general case
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.HasMethodP(name)
}

// FastCallMethodPOn is an optimized version of CallMethodPOn that uses the adapter cache
func FastCallMethodPOn(obj LispValue, name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// Use special fast paths for common types
	switch typedObj := obj.(type) {
	case *PythonicDict:
		// Fast path for dictionaries
		return typedObj.CallMethod(name, args)

	case *PythonicObject:
		// Fast path for objects
		typedObj.SetEvaluator(eval)
		return typedObj.CallMethod(name, args)
	}

	// Use the cached adapter for the general case
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.CallMethodP(name, args, eval, env)
}

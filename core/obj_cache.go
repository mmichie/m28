package core

import (
	"fmt"
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
	
	// Object pools for common adapter types to reduce GC pressure
	listAdapterPool    sync.Pool
	stringAdapterPool  sync.Pool
	dictAdapterPool    sync.Pool
	objectAdapterPool  sync.Pool
	tupleAdapterPool   sync.Pool
	setAdapterPool     sync.Pool
	genericAdapterPool sync.Pool
}

// AdapterFactory is a function that creates an ObjProtocol adapter for a given value
type AdapterFactory func(obj LispValue) ObjProtocol

// NewTypeAdapterCache creates a new type adapter cache with pooled adapters
func NewTypeAdapterCache() *TypeAdapterCache {
	cache := &TypeAdapterCache{
		cache: make(map[reflect.Type]AdapterFactory),
		
		// Initialize object pools with appropriate constructor functions
		listAdapterPool: sync.Pool{
			New: func() interface{} {
				return &LispListAdapter{}
			},
		},
		stringAdapterPool: sync.Pool{
			New: func() interface{} {
				return &StringAdapter{}
			},
		},
		dictAdapterPool: sync.Pool{
			New: func() interface{} {
				return &ObjAdapter{}
			},
		},
		objectAdapterPool: sync.Pool{
			New: func() interface{} {
				return &ObjAdapter{}
			},
		},
		tupleAdapterPool: sync.Pool{
			New: func() interface{} {
				return &LispTupleAdapter{}
			},
		},
		setAdapterPool: sync.Pool{
			New: func() interface{} {
				return &PythonicSetAdapter{}
			},
		},
		genericAdapterPool: sync.Pool{
			New: func() interface{} {
				return &ObjAdapter{}
			},
		},
	}
	
	// Pre-warm the pools with a few instances
	for i := 0; i < 10; i++ {
		cache.genericAdapterPool.Put(&ObjAdapter{})
		cache.listAdapterPool.Put(&LispListAdapter{})
		cache.stringAdapterPool.Put(&StringAdapter{})
	}
	
	return cache
}

// RecycleAdapter returns an adapter to its pool for reuse
func (c *TypeAdapterCache) RecycleAdapter(adapter ObjProtocol) {
	switch a := adapter.(type) {
	case *LispListAdapter:
		// Clear fields to prevent memory leaks
		a.list = nil
		c.listAdapterPool.Put(a)
	case *StringAdapter:
		// Clear fields to prevent memory leaks
		a.str = ""
		c.stringAdapterPool.Put(a)
	case *LispTupleAdapter:
		// Clear fields to prevent memory leaks
		a.tuple = nil
		c.tupleAdapterPool.Put(a)
	case *PythonicSetAdapter:
		// Clear fields to prevent memory leaks
		a.set = nil
		c.setAdapterPool.Put(a)
	case *ObjAdapter:
		// Reset adapter functions
		a.GetPropFn = nil
		a.SetPropFn = nil
		a.HasMethodPFn = nil
		a.CallMethodPFn = nil
		c.genericAdapterPool.Put(a)
	}
}

// Global instance of the type adapter cache
var globalAdapterCache = NewTypeAdapterCache()

// GetAdapter retrieves or creates an adapter for the given value
func (c *TypeAdapterCache) GetAdapter(obj LispValue) ObjProtocol {
	if obj == nil {
		// Get a pooled generic adapter for nil
		adapter := c.genericAdapterPool.Get().(*ObjAdapter)
		return adapter
	}

	// Fast path: Direct implementation checks avoid reflection
	// First, check if the object already implements ObjProtocol
	if asObj, ok := obj.(ObjProtocol); ok {
		return asObj
	}

	// Then check if it implements AdaptableLispValue
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject()
	}

	// Fast path: Direct type checks for common types
	// These avoid both reflection and factory lookups for performance-critical types
	switch typedObj := obj.(type) {
	case LispList:
		adapter := c.listAdapterPool.Get().(*LispListAdapter)
		adapter.list = typedObj
		return adapter
	case string:
		adapter := c.stringAdapterPool.Get().(*StringAdapter)
		adapter.str = typedObj
		return adapter
	case *PythonicDict:
		// Get a dict adapter from the pool and configure it
		adapter := c.dictAdapterPool.Get().(*ObjAdapter)
		configureDictAdapter(adapter, typedObj)
		return adapter
	case *PythonicObject:
		// Get an object adapter from the pool and configure it
		adapter := c.objectAdapterPool.Get().(*ObjAdapter)
		configureObjectAdapter(adapter, typedObj)
		return adapter
	case LispTuple:
		adapter := c.tupleAdapterPool.Get().(*LispTupleAdapter)
		adapter.tuple = typedObj
		return adapter
	case *PythonicSet:
		adapter := c.setAdapterPool.Get().(*PythonicSetAdapter)
		adapter.set = typedObj
		return adapter
	case LispListLiteral:
		adapter := c.listAdapterPool.Get().(*LispListAdapter)
		adapter.list = LispList(typedObj)
		return adapter
	}

	// General case: Use the adapter cache for less common types
	objType := reflect.TypeOf(obj)

	// Try to get a cached adapter factory
	c.mu.RLock()
	factory, ok := c.cache[objType]
	c.mu.RUnlock()

	if ok {
		// Use the cached factory to create an adapter
		return factory(obj)
	}

	// No cached factory, create one for this type
	var adapterFactory AdapterFactory

	switch obj.(type) {
	case *SuperObject:
		adapterFactory = func(o LispValue) ObjProtocol {
			adapter := c.genericAdapterPool.Get().(*ObjAdapter)
			configureSuperObjectAdapter(adapter, o.(*SuperObject))
			return adapter
		}
	default:
		// Default adapter factory for unknown types
		adapterFactory = func(o LispValue) ObjProtocol {
			adapter := c.genericAdapterPool.Get().(*ObjAdapter)
			return adapter
		}
	}

	// Cache the adapter factory for future use
	c.mu.Lock()
	c.cache[objType] = adapterFactory
	c.mu.Unlock()

	// Use the factory to create an adapter
	return adapterFactory(obj)
}

// Helper functions to configure pooled adapters

// configureDictAdapter sets up a pooled ObjAdapter for PythonicDict
func configureDictAdapter(adapter *ObjAdapter, dict *PythonicDict) {
	adapter.GetPropFn = func(name string) (LispValue, bool) {
		// First, check for methods
		if dict.HasMethod(name) {
			method, _ := dict.methods[name]
			// We wrap it in a BuiltinFunc for later calling
			return BuiltinFunc(func(args []LispValue, callEnv Environment) (LispValue, error) {
				return method(dict, args)
			}), true
		}

		// For dictionaries, we also implement some pseudo-properties
		switch name {
		case "length", "len", "size", "count":
			return float64(dict.Size()), true
		}

		// Check for property/attribute
		return dict.Get(name)
	}

	adapter.SetPropFn = func(name string, value LispValue) error {
		dict.Set(name, value)
		return nil
	}

	adapter.HasMethodPFn = func(name string) bool {
		// Dictionary methods
		if dict.HasMethod(name) {
			return true
		}

		// Length is also available as a method
		if name == "length" || name == "len" || name == "size" || name == "count" {
			return true
		}

		return false
	}

	adapter.CallMethodPFn = func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
		// For convenience, we also treat length/size as a method
		if name == "length" || name == "len" || name == "size" || name == "count" {
			if len(args) > 0 {
				return nil, ErrTooManyArgumentsf("len", 0, len(args))
			}
			return float64(dict.Size()), nil
		}

		// Normal method call
		return dict.CallMethod(name, args)
	}
}

// configureObjectAdapter sets up a pooled ObjAdapter for PythonicObject
func configureObjectAdapter(adapter *ObjAdapter, obj *PythonicObject) {
	adapter.GetPropFn = func(name string) (LispValue, bool) {
		return obj.GetAttribute(name)
	}

	adapter.SetPropFn = func(name string, value LispValue) error {
		return obj.SetProperty(name, value)
	}

	adapter.HasMethodPFn = func(name string) bool {
		return obj.HasMethod(name)
	}

	adapter.CallMethodPFn = func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
		obj.SetEvaluator(eval)
		return obj.CallMethod(name, args)
	}
}

// configureSuperObjectAdapter sets up a pooled ObjAdapter for SuperObject
func configureSuperObjectAdapter(adapter *ObjAdapter, obj *SuperObject) {
	adapter.GetPropFn = func(name string) (LispValue, bool) {
		// Check parent class methods specifically to create bound methods with proper 'self'
		for _, parent := range obj.Object.Class.Parents {
			if method, exists := parent.GetMethod(name); exists {
				// Create a bound method with the original instance
				boundMethod := NewBoundMethod(method, obj.Object, obj.Object.evaluator)
				return boundMethod, true
			}
		}

		// Check parent class attributes
		for _, parent := range obj.Object.Class.Parents {
			if attr, exists := parent.GetAttribute(name); exists {
				return attr, true
			}
		}

		return nil, false
	}

	adapter.SetPropFn = func(name string, value LispValue) error {
		return ErrDotNoPropertyf(name)
	}

	adapter.HasMethodPFn = func(name string) bool {
		// Check if any parent class has the method
		for _, parent := range obj.Object.Class.Parents {
			if _, exists := parent.GetMethod(name); exists {
				return true
			}
		}
		return false
	}

	adapter.CallMethodPFn = func(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
		// Store evaluator in the super object
		obj.SetEvaluator(eval)

		// Check parent class methods specifically to create bound methods with proper 'self'
		for _, parent := range obj.Object.Class.Parents {
			if method, exists := parent.GetMethod(name); exists {
				// Create a bound method with the original instance
				boundMethod := NewBoundMethod(method, obj.Object, eval)
				// Apply with the evaluator passed to ensure context is maintained
				return boundMethod.Apply(eval, args, env)
			}
		}

		return nil, ErrDotNoMethodf(name)
	}
}

// FastGetPropFrom is an optimized version of GetPropFrom that uses the adapter cache
// and includes fast paths for common property names
func FastGetPropFrom(obj LispValue, name string) (LispValue, bool) {
	// Fast path for common property names
	if isLengthProperty(name) {
		// Handle length/size properties directly for common types to avoid adapter creation
		switch typedObj := obj.(type) {
		case LispList:
			return float64(len(typedObj)), true
		case string:
			return float64(len(typedObj)), true
		case *PythonicDict:
			return float64(typedObj.Size()), true
		case LispTuple:
			return float64(len(typedObj)), true
		case *PythonicSet:
			return float64(typedObj.Size()), true
		case LispListLiteral:
			return float64(len(typedObj)), true
		}
	}

	// Fast path for indexed access (numerical properties)
	if idx, err := ParseIndex(name); err == nil {
		switch typedObj := obj.(type) {
		case LispList:
			if idx >= 0 && idx < len(typedObj) {
				return typedObj[idx], true
			}
		case string:
			if idx >= 0 && idx < len(typedObj) {
				return string(typedObj[idx]), true
			}
		case LispTuple:
			if idx >= 0 && idx < len(typedObj) {
				return typedObj[idx], true
			}
		}
	}

	// Standard interface checks
	// First, check if the object directly implements ObjProtocol
	if objProto, ok := obj.(ObjProtocol); ok {
		return objProto.GetProp(name)
	}

	// Then, check if the object implements AdaptableLispValue
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().GetProp(name)
	}

	// Use the cached adapter for all other cases
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.GetProp(name)
}

// isLengthProperty checks if the property name is a common length/size property
func isLengthProperty(name string) bool {
	return name == "length" || name == "len" || name == "size" || name == "count"
}

// FastSetPropOn is an optimized version of SetPropOn that uses the adapter cache
func FastSetPropOn(obj LispValue, name string, value LispValue) error {
	// Fast path for indexed access (numerical properties)
	if idx, err := ParseIndex(name); err == nil {
		switch typedObj := obj.(type) {
		case LispList:
			if idx >= 0 && idx < len(typedObj) {
				typedObj[idx] = value
				return nil
			} else {
				return ErrIndexOutOfRange(idx, len(typedObj))
			}
		case LispTuple:
			return fmt.Errorf("tuples are immutable")
		}
	}

	// Fast path for direct PythonicDict/PythonicObject access
	switch typedObj := obj.(type) {
	case *PythonicDict:
		typedObj.Set(name, value)
		return nil
	case *PythonicObject:
		if typedObj.Attributes == nil {
			typedObj.Attributes = NewPythonicDict()
		}
		typedObj.Attributes.Set(name, value)
		return nil
	}

	// First, check if the object directly implements ObjProtocol
	if objProto, ok := obj.(ObjProtocol); ok {
		return objProto.SetProp(name, value)
	}

	// Then, check if the object implements AdaptableLispValue
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().SetProp(name, value)
	}

	// Use the cached adapter for all other cases
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.SetProp(name, value)
}

// FastHasMethodPOn is an optimized version of HasMethodPOn that uses the adapter cache
func FastHasMethodPOn(obj LispValue, name string) bool {
	// Fast path for common length methods
	if isLengthProperty(name) {
		switch obj.(type) {
		case LispList, string, *PythonicDict, LispTuple, *PythonicSet, LispListLiteral:
			return true
		}
	}

	// Fast path for common list methods
	if isListMethod(name) {
		switch obj.(type) {
		case LispList, LispListLiteral:
			return true
		}
	}

	// Fast path for common string methods
	if isStringMethod(name) {
		switch obj.(type) {
		case string:
			return true
		}
	}

	// Fast path for direct method checks on core types
	switch typedObj := obj.(type) {
	case *PythonicDict:
		if typedObj.HasMethod(name) {
			return true
		}
	case *PythonicObject:
		if typedObj.HasMethod(name) {
			return true
		}
	}

	// First, check if the object directly implements ObjProtocol
	if objProto, ok := obj.(ObjProtocol); ok {
		return objProto.HasMethodP(name)
	}

	// Then, check if the object implements AdaptableLispValue
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().HasMethodP(name)
	}

	// Use the cached adapter for all other cases
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.HasMethodP(name)
}

// isListMethod checks if the method name is a common list method
func isListMethod(name string) bool {
	switch name {
	case "append", "extend", "pop", "remove", "get", "set", "contains", 
		"map", "filter", "reduce", "sort", "reverse", "join":
		return true
	}
	return false
}

// isStringMethod checks if the method name is a common string method
func isStringMethod(name string) bool {
	switch name {
	case "upper", "lower", "strip", "split", "join", "replace", 
		"contains", "startswith", "endswith", "find", "format":
		return true
	}
	return false
}

// FastCallMethodPOn is an optimized version of CallMethodPOn that uses the adapter cache
func FastCallMethodPOn(obj LispValue, name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// First, check if the object directly implements ObjProtocol
	if objProto, ok := obj.(ObjProtocol); ok {
		return objProto.CallMethodP(name, args, eval, env)
	}

	// Then, check if the object implements AdaptableLispValue
	if adaptable, ok := obj.(AdaptableLispValue); ok {
		return adaptable.AsObject().CallMethodP(name, args, eval, env)
	}

	// Use the cached adapter for all other cases
	adapter := globalAdapterCache.GetAdapter(obj)
	return adapter.CallMethodP(name, args, eval, env)
}

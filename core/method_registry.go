package core

import (
	"fmt"
)

// MethodHandler is already defined in type_descriptor.go

// PropertyGetter is the function signature for property getters
type PropertyGetter func(receiver Value) (Value, error)

// MethodEntry describes a method that can be registered
type MethodEntry struct {
	Name    string
	Arity   int
	Doc     string
	Handler MethodHandler
}

// PropertyEntry describes a property that can be registered
type PropertyEntry struct {
	Name   string
	Doc    string
	Getter PropertyGetter
}

// MethodRegistry provides a registry for methods and properties
type MethodRegistry struct {
	methods    map[string]*MethodEntry
	properties map[string]*PropertyEntry
}

// NewMethodRegistry creates a new method registry
func NewMethodRegistry() *MethodRegistry {
	return &MethodRegistry{
		methods:    make(map[string]*MethodEntry),
		properties: make(map[string]*PropertyEntry),
	}
}

// RegisterMethod adds a method to the registry
func (r *MethodRegistry) RegisterMethod(entry *MethodEntry) {
	r.methods[entry.Name] = entry
}

// RegisterProperty adds a property to the registry
func (r *MethodRegistry) RegisterProperty(entry *PropertyEntry) {
	r.properties[entry.Name] = entry
}

// RegisterMethods adds multiple methods at once
func (r *MethodRegistry) RegisterMethods(entries ...*MethodEntry) {
	for _, entry := range entries {
		r.RegisterMethod(entry)
	}
}

// RegisterProperties adds multiple properties at once
func (r *MethodRegistry) RegisterProperties(entries ...*PropertyEntry) {
	for _, entry := range entries {
		r.RegisterProperty(entry)
	}
}

// GetAttr looks up an attribute in the registry
func (r *MethodRegistry) GetAttr(receiver Value, name string) (Value, bool) {
	// Check methods first
	if method, ok := r.methods[name]; ok {
		// Return a builtin function that captures the receiver and calls the handler
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return method.Handler(receiver, args, ctx)
		}), true
	}

	// Check properties
	if prop, ok := r.properties[name]; ok {
		value, err := prop.Getter(receiver)
		if err != nil {
			// Properties shouldn't fail, but if they do, return not found
			return nil, false
		}
		return value, true
	}

	return nil, false
}

// AttributeProvider is an interface for types that provide attributes
type AttributeProvider interface {
	Value
	GetRegistry() *MethodRegistry
	GetBaseObject() *BaseObject
}

// GetAttrWithRegistry provides common GetAttr implementation using a registry
func GetAttrWithRegistry(provider AttributeProvider, name string) (Value, bool) {
	// First check the registry
	if registry := provider.GetRegistry(); registry != nil {
		if value, ok := registry.GetAttr(provider, name); ok {
			return value, true
		}
	}

	// Fall back to base object
	if base := provider.GetBaseObject(); base != nil {
		return base.GetAttr(name)
	}

	return nil, false
}

// Helper functions for common patterns

// MakeMethod creates a MethodEntry with common validation
func MakeMethod(name string, arity int, doc string, handler MethodHandler) *MethodEntry {
	return &MethodEntry{
		Name:    name,
		Arity:   arity,
		Doc:     doc,
		Handler: handler,
	}
}

// MakeProperty creates a PropertyEntry
func MakeProperty(name, doc string, getter PropertyGetter) *PropertyEntry {
	return &PropertyEntry{
		Name:   name,
		Doc:    doc,
		Getter: getter,
	}
}

// ValidateArity checks argument count and returns an error if invalid
func ValidateArity(name string, args []Value, expected int) error {
	if len(args) != expected {
		return fmt.Errorf("%s() takes exactly %d argument(s), got %d", name, expected, len(args))
	}
	return nil
}

// ValidateArityRange checks argument count is within range
func ValidateArityRange(name string, args []Value, min, max int) error {
	count := len(args)
	if count < min || count > max {
		if min == max {
			return ValidateArity(name, args, min)
		}
		return fmt.Errorf("%s() takes %d to %d arguments, got %d", name, min, max, count)
	}
	return nil
}

// Common protocol implementations

// MakeContextEnterMethod creates a standard __enter__ method
func MakeContextEnterMethod() *MethodEntry {
	return MakeMethod("__enter__", 0, "Enter the context", func(receiver Value, args []Value, ctx *Context) (Value, error) {
		return receiver, nil
	})
}

// MakeContextExitMethod creates a standard __exit__ method with custom cleanup
func MakeContextExitMethod(cleanup func(receiver Value) error) *MethodEntry {
	return MakeMethod("__exit__", 3, "Exit the context", func(receiver Value, args []Value, ctx *Context) (Value, error) {
		if cleanup != nil {
			if err := cleanup(receiver); err != nil {
				return nil, err
			}
		}
		return BoolValue(false), nil
	})
}

// MakeIterMethod creates a standard __iter__ method
func MakeIterMethod() *MethodEntry {
	return MakeMethod("__iter__", 0, "Return an iterator", func(receiver Value, args []Value, ctx *Context) (Value, error) {
		return receiver, nil
	})
}

// TypedReceiver extracts and type-asserts the receiver
func TypedReceiver[T Value](receiver Value, methodName string) (T, error) {
	typed, ok := receiver.(T)
	if !ok {
		var zero T
		return zero, fmt.Errorf("%s() called on wrong type: expected %T, got %T", methodName, zero, receiver)
	}
	return typed, nil
}

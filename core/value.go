// Package core provides the fundamental types and interfaces for the M28 language.
package core

import (
	"fmt"
	"sync"
)

// Type represents a type in the language
type Type interface {
	// Name returns the type's name
	Name() string
	
	// IsSubtypeOf checks if this type is a subtype of another
	IsSubtypeOf(other Type) bool
}

// Value is the base interface for all values in the language
type Value interface {
	// Type returns the value's type
	Type() Type
	
	// String returns a string representation
	String() string
}

// TypeDescriptor holds metadata about a type
type TypeDescriptor struct {
	name       string
	methods    map[string]Method
	properties map[string]Property
	bases      []Type
}

// NewTypeDescriptor creates a new type descriptor
func NewTypeDescriptor(name string, bases ...Type) *TypeDescriptor {
	return &TypeDescriptor{
		name:       name,
		methods:    make(map[string]Method),
		properties: make(map[string]Property),
		bases:      bases,
	}
}

// Name returns the type's name
func (t *TypeDescriptor) Name() string {
	return t.name
}

// IsSubtypeOf checks if this type is a subtype of another
func (t *TypeDescriptor) IsSubtypeOf(other Type) bool {
	if t == other {
		return true
	}
	
	for _, base := range t.bases {
		if base.IsSubtypeOf(other) {
			return true
		}
	}
	
	return false
}

// Common predefined types
var (
	// Core types
	TypeType      = NewTypeDescriptor("type")
	ObjectType    = NewTypeDescriptor("object")
	NumberType    = NewTypeDescriptor("number", ObjectType)
	StringType    = NewTypeDescriptor("string", ObjectType)
	BoolType      = NewTypeDescriptor("bool", ObjectType)
	NilType       = NewTypeDescriptor("nil", ObjectType)
	SymbolType    = NewTypeDescriptor("symbol", ObjectType)
	
	// Container types
	ListType      = NewTypeDescriptor("list", ObjectType)
	DictType      = NewTypeDescriptor("dict", ObjectType)
	TupleType     = NewTypeDescriptor("tuple", ObjectType)
	SetType       = NewTypeDescriptor("set", ObjectType)
	
	// Function types
	CallableType  = NewTypeDescriptor("callable", ObjectType)
	FunctionType  = NewTypeDescriptor("function", CallableType)
	MethodType    = NewTypeDescriptor("method", CallableType)
	
	// Other types
	ModuleType    = NewTypeDescriptor("module", ObjectType)
	ClassType     = NewTypeDescriptor("class", ObjectType)
	ExceptionType = NewTypeDescriptor("exception", ObjectType)
)

// Method represents a callable method
type Method interface {
	// Call the method with arguments in a context
	Call(receiver Value, args []Value, ctx *Context) (Value, error)
	
	// Bind creates a bound method for a specific receiver
	Bind(receiver Value) Value
}

// Property represents a property with custom getter/setter
type Property interface {
	// Get the property value
	Get(receiver Value) (Value, bool)
	
	// Set the property value
	Set(receiver Value, value Value) error
}

// Object represents any value that can have attributes and methods
type Object interface {
	Value
	
	// GetAttr retrieves an attribute by name
	GetAttr(name string) (Value, bool)
	
	// SetAttr sets an attribute value
	SetAttr(name string, value Value) error
	
	// CallMethod calls a method with arguments in a context
	CallMethod(name string, args []Value, ctx *Context) (Value, error)
}

// Callable represents any value that can be called as a function
type Callable interface {
	Value
	
	// Call invokes the callable with arguments in a context
	Call(args []Value, ctx *Context) (Value, error)
}

// BaseObject provides a standard implementation of Object
type BaseObject struct {
	attrs    map[string]Value
	typeDesc *TypeDescriptor
	mu       sync.RWMutex // For thread safety
}

// NewBaseObject creates a new base object
func NewBaseObject(typeDesc *TypeDescriptor) *BaseObject {
	return &BaseObject{
		attrs:    make(map[string]Value),
		typeDesc: typeDesc,
	}
}

// Type implements Value.Type
func (o *BaseObject) Type() Type {
	return o.typeDesc
}

// String implements Value.String
func (o *BaseObject) String() string {
	return fmt.Sprintf("<%s object>", o.typeDesc.Name())
}

// GetAttr implements Object.GetAttr
func (o *BaseObject) GetAttr(name string) (Value, bool) {
	o.mu.RLock()
	defer o.mu.RUnlock()
	
	// Check local attributes
	if val, ok := o.attrs[name]; ok {
		return val, true
	}
	
	// Check type for methods
	if method, ok := o.typeDesc.methods[name]; ok {
		return method.Bind(o), true
	}
	
	// Check type for properties
	if prop, ok := o.typeDesc.properties[name]; ok {
		return prop.Get(o)
	}
	
	return nil, false
}

// SetAttr implements Object.SetAttr
func (o *BaseObject) SetAttr(name string, value Value) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	
	// Check if there's a property with a setter
	if prop, ok := o.typeDesc.properties[name]; ok {
		return prop.Set(o, value)
	}
	
	// Normal attribute
	o.attrs[name] = value
	return nil
}

// CallMethod implements Object.CallMethod
func (o *BaseObject) CallMethod(name string, args []Value, ctx *Context) (Value, error) {
	method, ok := o.GetAttr(name)
	if !ok {
		return nil, fmt.Errorf("no method named %s", name)
	}
	
	callable, ok := method.(Callable)
	if !ok {
		return nil, fmt.Errorf("%s is not callable", name)
	}
	
	return callable.Call(args, ctx)
}
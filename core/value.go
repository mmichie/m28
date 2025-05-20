// Package core provides the fundamental types and interfaces for the M28 language.
package core

import (
	"fmt"
	"sync"
)

// Type represents a type in the language
type Type string

// Predefined type constants
const (
	NumberType    Type = "number"
	StringType    Type = "string"
	BoolType      Type = "bool"
	NilType       Type = "nil"
	SymbolType    Type = "symbol"
	ListType      Type = "list"
	DictType      Type = "dict"
	TupleType     Type = "tuple"
	SetType       Type = "set"
	FunctionType  Type = "function"
	MethodType    Type = "method"
	ModuleType    Type = "module"
	ClassType     Type = "class"
	ExceptionType Type = "exception"
)

// Value is the base interface for all values in the language
type Value interface {
	// Type returns the value's type
	Type() Type
	
	// String returns a string representation
	String() string
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

// Method represents a method that can be bound to an instance
type Method interface {
	Callable
	
	// Bind binds the method to a receiver object
	Bind(receiver Value) Value
}

// Iterable represents any value that can be iterated over
type Iterable interface {
	Value
	
	// Iterator returns an iterator over the elements
	Iterator() Iterator
}

// Iterator represents an iterator over a collection
type Iterator interface {
	// Next advances the iterator and returns the next value
	Next() (Value, bool)
	
	// Reset resets the iterator to the beginning
	Reset()
}

// BaseObject provides a standard implementation of Object
type BaseObject struct {
	attrs map[string]Value
	typ   Type
	mu    sync.RWMutex // For thread safety
}

// NewBaseObject creates a new base object
func NewBaseObject(typ Type) *BaseObject {
	return &BaseObject{
		attrs: make(map[string]Value),
		typ:   typ,
	}
}

// Type implements Value.Type
func (o *BaseObject) Type() Type {
	return o.typ
}

// String implements Value.String
func (o *BaseObject) String() string {
	return fmt.Sprintf("<%s object>", o.typ)
}

// GetAttr implements Object.GetAttr
func (o *BaseObject) GetAttr(name string) (Value, bool) {
	o.mu.RLock()
	defer o.mu.RUnlock()
	
	// Check local attributes
	if val, ok := o.attrs[name]; ok {
		return val, true
	}
	
	return nil, false
}

// SetAttr implements Object.SetAttr
func (o *BaseObject) SetAttr(name string, value Value) error {
	o.mu.Lock()
	defer o.mu.Unlock()
	
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
package core

import (
	"fmt"
	"sort"
)

// MethodHandler is a function that implements a method
type MethodHandler func(receiver Value, args []Value, ctx *Context) (Value, error)

// ConstructorFunc creates a new instance of a type
type ConstructorFunc func(args []Value, ctx *Context) (Value, error)

// TypeDescriptor holds all metadata about a type
type TypeDescriptor struct {
	Name        string                         // M28 type name
	PythonName  string                         // Python-compatible name
	BaseType    Type                           // Core type constant
	Parent      *TypeDescriptor                // For inheritance (future)
	Methods     map[string]*MethodDescriptor   // Available methods
	Properties  map[string]*PropertyDescriptor // Properties/attributes
	Constructor ConstructorFunc                // How to create instances
	Repr        func(Value) string             // String representation
	Str         func(Value) string             // Human-readable string
	Doc         string                         // Type documentation
}

// MethodDescriptor describes a method
type MethodDescriptor struct {
	Name    string
	Arity   int // -1 for variadic
	Doc     string
	Builtin bool // Is it implemented in Go?
	Handler MethodHandler
}

// PropertyDescriptor describes an attribute/property
type PropertyDescriptor struct {
	Name     string
	ReadOnly bool
	Doc      string
	Getter   func(Value) (Value, error)
	Setter   func(Value, Value) error
}

// Global type registry
var typeRegistry = make(map[Type]*TypeDescriptor)

// RegisterType registers a type descriptor
func RegisterType(desc *TypeDescriptor) {
	typeRegistry[desc.BaseType] = desc
}

// GetTypeDescriptor returns the descriptor for a type
func GetTypeDescriptor(t Type) *TypeDescriptor {
	return typeRegistry[t]
}

// GetTypeDescriptorForValue returns the descriptor for a value's type
func GetTypeDescriptorForValue(v Value) *TypeDescriptor {
	return GetTypeDescriptor(v.Type())
}

// GetMethod looks up a method by name
func (td *TypeDescriptor) GetMethod(name string) (*MethodDescriptor, bool) {
	method, ok := td.Methods[name]
	if ok {
		return method, true
	}

	// Check parent type (for future inheritance)
	if td.Parent != nil {
		return td.Parent.GetMethod(name)
	}

	return nil, false
}

// GetProperty looks up a property by name
func (td *TypeDescriptor) GetProperty(name string) (*PropertyDescriptor, bool) {
	prop, ok := td.Properties[name]
	if ok {
		return prop, true
	}

	// Check parent type (for future inheritance)
	if td.Parent != nil {
		return td.Parent.GetProperty(name)
	}

	return nil, false
}

// GetAttributeNames returns all attribute names (methods and properties)
func (td *TypeDescriptor) GetAttributeNames() []string {
	names := make([]string, 0, len(td.Methods)+len(td.Properties))

	for name := range td.Methods {
		names = append(names, name)
	}

	for name := range td.Properties {
		names = append(names, name)
	}

	// Add parent attributes
	if td.Parent != nil {
		parentNames := td.Parent.GetAttributeNames()
		for _, name := range parentNames {
			// Check if not already added (override)
			found := false
			for _, existing := range names {
				if existing == name {
					found = true
					break
				}
			}
			if !found {
				names = append(names, name)
			}
		}
	}

	sort.Strings(names)
	return names
}

// CallMethod calls a method on a value
func (td *TypeDescriptor) CallMethod(receiver Value, methodName string, args []Value, ctx *Context) (Value, error) {
	method, ok := td.GetMethod(methodName)
	if !ok {
		return nil, fmt.Errorf("%s object has no method '%s'", td.PythonName, methodName)
	}

	// Check arity (-1 means variadic)
	if method.Arity >= 0 && len(args) != method.Arity {
		return nil, fmt.Errorf("%s.%s() takes %d arguments, got %d",
			td.PythonName, methodName, method.Arity, len(args))
	}

	return method.Handler(receiver, args, ctx)
}

// GetAttribute gets an attribute value (method or property)
func (td *TypeDescriptor) GetAttribute(receiver Value, name string) (Value, error) {
	// Check methods first
	if method, ok := td.GetMethod(name); ok {
		// Return a bound method
		return &BoundMethod{
			Receiver: receiver,
			Method:   method,
			TypeDesc: td,
		}, nil
	}

	// Check properties
	if prop, ok := td.GetProperty(name); ok {
		if prop.Getter != nil {
			return prop.Getter(receiver)
		}
		return nil, fmt.Errorf("property '%s' has no getter", name)
	}

	return nil, fmt.Errorf("'%s' object has no attribute '%s'", td.PythonName, name)
}

// SetAttribute sets an attribute value (only for writable properties)
func (td *TypeDescriptor) SetAttribute(receiver Value, name string, value Value) error {
	// Check if it's a property
	if prop, ok := td.GetProperty(name); ok {
		if prop.ReadOnly {
			return fmt.Errorf("can't set attribute '%s' of '%s' object", name, td.PythonName)
		}
		if prop.Setter != nil {
			return prop.Setter(receiver, value)
		}
		return fmt.Errorf("property '%s' has no setter", name)
	}

	// Methods are read-only
	if _, ok := td.GetMethod(name); ok {
		return fmt.Errorf("can't set method '%s' of '%s' object", name, td.PythonName)
	}

	return fmt.Errorf("'%s' object has no attribute '%s'", td.PythonName, name)
}

// BoundMethod represents a method bound to an instance
type BoundMethod struct {
	Receiver Value
	Method   *MethodDescriptor
	TypeDesc *TypeDescriptor
}

// Type implements Value.Type
func (bm *BoundMethod) Type() Type {
	return MethodType
}

// String implements Value.String
func (bm *BoundMethod) String() string {
	return fmt.Sprintf("<bound method %s.%s of %s>",
		bm.TypeDesc.PythonName, bm.Method.Name, PrintValueWithoutQuotes(bm.Receiver))
}

// Call implements Callable
func (bm *BoundMethod) Call(args []Value, ctx *Context) (Value, error) {
	if bm.Method == nil {
		return nil, fmt.Errorf("bound method has nil method descriptor")
	}
	if bm.Method.Handler == nil {
		return nil, fmt.Errorf("method %s has nil handler", bm.Method.Name)
	}
	return bm.Method.Handler(bm.Receiver, args, ctx)
}

// Standard type representations
func defaultRepr(v Value) string {
	return v.String()
}

func defaultStr(v Value) string {
	// For most types, str() removes quotes
	s := v.String()
	if len(s) >= 2 && s[0] == '"' && s[len(s)-1] == '"' {
		return s[1 : len(s)-1]
	}
	return s
}

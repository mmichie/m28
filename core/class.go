package core

import (
	"fmt"
)

// Class represents a class definition
type Class struct {
	BaseObject
	Name        string            // Class name
	Parent      *Class            // Parent class (for inheritance) - deprecated, use Parents
	Parents     []*Class          // Parent classes (for multiple inheritance)
	Methods     map[string]Value  // Class methods
	Attributes  map[string]Value  // Class attributes
	Constructor *MethodDescriptor // __init__ method
}

// NewClass creates a new class
func NewClass(name string, parent *Class) *Class {
	var parents []*Class
	if parent != nil {
		parents = []*Class{parent}
	}
	return &Class{
		BaseObject: *NewBaseObject(Type("class")),
		Name:       name,
		Parent:     parent, // Keep for backward compatibility
		Parents:    parents,
		Methods:    make(map[string]Value),
		Attributes: make(map[string]Value),
	}
}

// NewClassWithParents creates a new class with multiple parents
func NewClassWithParents(name string, parents []*Class) *Class {
	var parent *Class
	if len(parents) > 0 {
		parent = parents[0] // First parent for backward compatibility
	}
	return &Class{
		BaseObject: *NewBaseObject(Type("class")),
		Name:       name,
		Parent:     parent,
		Parents:    parents,
		Methods:    make(map[string]Value),
		Attributes: make(map[string]Value),
	}
}

// Type returns the class type
func (c *Class) Type() Type {
	return Type("class")
}

// String returns the string representation of the class
func (c *Class) String() string {
	return fmt.Sprintf("<class '%s'>", c.Name)
}

// GetMethod looks up a method in the class hierarchy
func (c *Class) GetMethod(name string) (Value, bool) {
	// Check this class first
	if method, ok := c.Methods[name]; ok {
		return method, true
	}

	// Check parent classes using MRO (Method Resolution Order)
	// Simple left-to-right depth-first search
	if len(c.Parents) > 0 {
		for _, parent := range c.Parents {
			if method, ok := parent.GetMethod(name); ok {
				return method, true
			}
		}
	} else if c.Parent != nil {
		// Fallback to single parent for backward compatibility
		return c.Parent.GetMethod(name)
	}

	return nil, false
}

// GetMethodWithClass looks up a method and returns the class where it was defined
func (c *Class) GetMethodWithClass(name string) (Value, *Class, bool) {
	// Check this class first
	if method, ok := c.Methods[name]; ok {
		return method, c, true
	}

	// Check parent classes using MRO (Method Resolution Order)
	// Simple left-to-right depth-first search
	if len(c.Parents) > 0 {
		for _, parent := range c.Parents {
			if method, defClass, ok := parent.GetMethodWithClass(name); ok {
				return method, defClass, true
			}
		}
	} else if c.Parent != nil {
		// Fallback to single parent for backward compatibility
		return c.Parent.GetMethodWithClass(name)
	}

	return nil, nil, false
}

// SetMethod adds a method to the class
func (c *Class) SetMethod(name string, method Value) {
	c.Methods[name] = method

	// Special handling for __init__
	// Constructor is set when creating instances
}

// GetClassAttr gets a class attribute
func (c *Class) GetClassAttr(name string) (Value, bool) {
	// Check this class first
	if attr, ok := c.Attributes[name]; ok {
		return attr, true
	}

	// Check parent classes using MRO
	if len(c.Parents) > 0 {
		for _, parent := range c.Parents {
			if attr, ok := parent.GetClassAttr(name); ok {
				return attr, true
			}
		}
	} else if c.Parent != nil {
		// Fallback to single parent for backward compatibility
		return c.Parent.GetClassAttr(name)
	}

	return nil, false
}

// SetClassAttr sets a class attribute
func (c *Class) SetClassAttr(name string, value Value) {
	c.Attributes[name] = value
}

// GetAttr implements Object interface for classes
func (c *Class) GetAttr(name string) (Value, bool) {
	// Special handling for __dict__
	if name == "__dict__" {
		dict := NewDict()
		// Add all methods with proper key conversion
		for methodName, method := range c.Methods {
			// Use SetWithKey to ensure proper key storage with original key tracking
			keyStr := StringValue(methodName)
			dict.SetWithKey(ValueToKey(keyStr), keyStr, method)
		}
		// Add all attributes with proper key conversion
		for attrName, attr := range c.Attributes {
			keyStr := StringValue(attrName)
			dict.SetWithKey(ValueToKey(keyStr), keyStr, attr)
		}
		return dict, true
	}

	// First check methods
	if method, ok := c.GetMethod(name); ok {
		return method, true
	}

	// Then check attributes
	if attr, ok := c.GetClassAttr(name); ok {
		return attr, true
	}

	// Finally check base object
	return c.BaseObject.GetAttr(name)
}

// SetAttr implements Object interface for classes
func (c *Class) SetAttr(name string, value Value) error {
	// Set as class attribute
	c.SetClassAttr(name, value)
	return nil
}

// Call implements Callable interface for classes (instantiation)
func (c *Class) Call(args []Value, ctx *Context) (Value, error) {
	// Create new instance
	instance := NewInstance(c)

	// Call __init__ if it exists
	if initMethod, ok := c.GetMethod("__init__"); ok {
		// Create bound method for __init__
		if callable, ok := initMethod.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			// Prepend instance as first argument (self)
			initArgs := append([]Value{instance}, args...)
			_, err := callable.Call(initArgs, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in %s.__init__: %v", c.Name, err)
			}
		}
	}

	return instance, nil
}

// Instance represents an instance of a class
type Instance struct {
	BaseObject
	Class      *Class           // The class this is an instance of
	Attributes map[string]Value // Instance attributes
}

// NewInstance creates a new instance of a class
func NewInstance(class *Class) *Instance {
	return &Instance{
		BaseObject: *NewBaseObject(Type("instance")),
		Class:      class,
		Attributes: make(map[string]Value),
	}
}

// Type returns the instance type
func (i *Instance) Type() Type {
	return Type(i.Class.Name)
}

// String returns the string representation of the instance
func (i *Instance) String() string {
	// Check for __str__ method
	if strMethod, ok := i.GetAttr("__str__"); ok {
		if callable, ok := strMethod.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			result, err := callable.Call([]Value{}, nil)
			if err == nil {
				if str, ok := result.(StringValue); ok {
					return string(str)
				}
			}
		}
	}

	return fmt.Sprintf("<%s instance at %p>", i.Class.Name, i)
}

// GetAttr implements Object interface for instances
func (i *Instance) GetAttr(name string) (Value, bool) {
	// First check instance attributes
	if attr, ok := i.Attributes[name]; ok {
		return attr, true
	}

	// Then check class methods
	if method, defClass, ok := i.Class.GetMethodWithClass(name); ok {
		// Bind method to instance if it's callable
		if callable, ok := method.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			// Create bound method
			boundMethod := &BoundInstanceMethod{
				Instance:      i,
				Method:        callable,
				DefiningClass: defClass,
			}
			return boundMethod, true
		}
		return method, true
	}

	// Then check class attributes
	if attr, ok := i.Class.GetClassAttr(name); ok {
		return attr, true
	}

	// Check special attributes
	if name == "__class__" {
		return i.Class, true
	}

	return nil, false
}

// SetAttr implements Object interface for instances
func (i *Instance) SetAttr(name string, value Value) error {
	i.Attributes[name] = value
	return nil
}

// BoundInstanceMethod represents a method bound to an instance
type BoundInstanceMethod struct {
	Instance *Instance
	Method   interface {
		Call([]Value, *Context) (Value, error)
	}
	DefiningClass *Class // The class where this method was defined
}

// Type returns the bound method type
func (bm *BoundInstanceMethod) Type() Type {
	return MethodType
}

// String returns the string representation
func (bm *BoundInstanceMethod) String() string {
	methodStr := "?"
	if stringer, ok := bm.Method.(interface{ String() string }); ok {
		methodStr = stringer.String()
	}
	return fmt.Sprintf("<bound method %s of %s>", methodStr, bm.Instance.String())
}

// Call implements Callable interface
func (bm *BoundInstanceMethod) Call(args []Value, ctx *Context) (Value, error) {
	// Create a new context with __class__ set for super() support
	methodCtx := NewContext(ctx)

	// Determine the class to use for super
	classForSuper := bm.DefiningClass
	if classForSuper == nil {
		// If DefiningClass is not set, use the instance's class
		classForSuper = bm.Instance.Class
	}

	methodCtx.Define("__class__", classForSuper)
	// Always define super as a value for bare super access
	methodCtx.Define("super", NewSuper(classForSuper, bm.Instance))

	// Prepend instance as first argument (self)
	callArgs := append([]Value{bm.Instance}, args...)
	return bm.Method.Call(callArgs, methodCtx)
}

// Super represents access to parent class methods
type Super struct {
	BaseObject
	Class    *Class
	Instance *Instance
}

// NewSuper creates a new super object
func NewSuper(class *Class, instance *Instance) *Super {
	return &Super{
		BaseObject: *NewBaseObject(Type("super")),
		Class:      class,
		Instance:   instance,
	}
}

// Type returns the super type
func (s *Super) Type() Type {
	return Type("super")
}

// String returns the string representation
func (s *Super) String() string {
	return fmt.Sprintf("<super: %s, <%s object>>", s.Class.Name, s.Instance.Class.Name)
}

// GetAttr gets an attribute from the parent class
func (s *Super) GetAttr(name string) (Value, bool) {
	// Check if there are parent classes
	if len(s.Class.Parents) == 0 && s.Class.Parent == nil {
		return nil, false
	}

	// Look up in parent classes using MRO
	if len(s.Class.Parents) > 0 {
		for _, parent := range s.Class.Parents {
			if method, _, ok := parent.GetMethodWithClass(name); ok {
				// Return the method unbound for backward compatibility
				// The existing tests expect to pass self explicitly
				return method, true
			}
		}
	} else if s.Class.Parent != nil {
		// Fallback to single parent for backward compatibility
		if method, _, ok := s.Class.Parent.GetMethodWithClass(name); ok {
			// Return the method unbound for backward compatibility
			return method, true
		}
	}

	return nil, false
}

// Helper function to check if a value is a class
func IsClass(v Value) bool {
	_, ok := v.(*Class)
	return ok
}

// Helper function to check if a value is an instance
func IsInstance(v Value) bool {
	_, ok := v.(*Instance)
	return ok
}

// Helper function to check instance of class
func IsInstanceOf(instance *Instance, class *Class) bool {
	current := instance.Class
	for current != nil {
		if current == class {
			return true
		}
		current = current.Parent
	}
	return false
}

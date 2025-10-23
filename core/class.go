package core

import (
	"fmt"
	"strings"
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
	// Return just the class name for backwards compatibility with tests
	// that compare type() to strings like "number", "string", etc.
	return c.Name
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
	// Special handling for __mro__ (Method Resolution Order)
	if name == "__mro__" {
		// Build the MRO tuple: (CurrentClass, Parent, Grandparent, ..., object)
		mro := []Value{c}

		// Follow the parent chain
		current := c.Parent
		for current != nil {
			mro = append(mro, current)
			current = current.Parent
		}

		// Also include parents from multiple inheritance
		// For simplicity, we'll add them after the main parent chain
		if len(c.Parents) > 0 {
			// Already have the main parent, now add any additional parents
			for i, parent := range c.Parents {
				if i == 0 && c.Parent != nil {
					// Skip first if it's the same as c.Parent
					continue
				}
				// Add this parent and its ancestors
				p := parent
				for p != nil {
					// Check if already in MRO to avoid duplicates
					found := false
					for _, existing := range mro {
						if existingClass, ok := existing.(*Class); ok {
							if existingClass == p {
								found = true
								break
							}
						}
					}
					if !found {
						mro = append(mro, p)
					}
					p = p.Parent
				}
			}
		}

		return TupleValue(mro), true
	}

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

	// Special handling for __getitem__ to support generic type syntax (list[int], dict[str, int], etc.)
	if name == "__getitem__" {
		// Return a function that creates generic alias objects
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// For now, return a simple GenericAlias instance
			// args contains the type parameters (e.g., int for list[int])
			return NewGenericAlias(c, args), nil
		}), true
	}

	// Special handling for __or__ to support union type syntax (int | str, etc.)
	if name == "__or__" {
		// Return a function that creates union type objects
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// args[0] is the right operand
			if len(args) != 1 {
				return nil, fmt.Errorf("__or__ takes exactly 1 argument")
			}
			return NewUnionType([]Value{c, args[0]}), nil
		}), true
	}

	// First check methods
	if method, ok := c.GetMethod(name); ok {
		return method, true
	}

	// Then check attributes
	if attr, ok := c.GetClassAttr(name); ok {
		return attr, true
	}

	// Provide default magic methods that Python classes automatically have
	switch name {
	case "__eq__":
		// Default __eq__ for classes
		// Note: Called via protocol, so only receives 'other' argument (self is implicit)
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__eq__ requires exactly 1 argument")
			}
			other := args[0]

			// Compare classes by name if both are classes
			if otherClass, ok := other.(*Class); ok {
				return BoolValue(c.Name == otherClass.Name), nil
			}

			// Check if other has GetClass() method (for type wrappers like StrType, TypeType, etc.)
			if classGetter, ok := other.(interface{ GetClass() *Class }); ok {
				otherClass := classGetter.GetClass()
				return BoolValue(c.Name == otherClass.Name), nil
			}

			// For other types, use pointer equality
			return BoolValue(Value(c) == other), nil
		}), true
	case "__ne__":
		// Default __ne__ is negation of __eq__
		// Note: Called via protocol, so only receives 'other' argument (self is implicit)
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__ne__ requires exactly 1 argument")
			}
			// Get __eq__ method
			eqMethod, ok := c.GetAttr("__eq__")
			if !ok {
				// Fallback to identity comparison
				return BoolValue(Value(c) != args[0]), nil
			}
			// Call __eq__ and negate the result
			if callable, ok := eqMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				result, err := callable.Call(args, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(!IsTruthy(result)), nil
			}
			return BoolValue(Value(c) != args[0]), nil
		}), true
	case "__repr__":
		// Default __repr__ returns a placeholder function
		// Classes use this as a type marker (e.g., dict.__repr__, list.__repr__)
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__repr__ requires exactly 1 argument")
			}
			// Return a representation string
			return StringValue(fmt.Sprintf("<%s object>", c.Name)), nil
		}), true
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
	// Python instance creation flow:
	// 1. Call __new__ to create the instance
	// 2. If __new__ returns an instance of the class, call __init__ on it
	// 3. Return the instance

	var instance Value

	// Check for __new__ method
	if newMethod, ok := c.GetMethod("__new__"); ok {
		// __new__ is a static method that takes the class as first argument
		if callable, ok := newMethod.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			// Prepend class as first argument
			newArgs := append([]Value{c}, args...)
			result, err := callable.Call(newArgs, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in %s.__new__: %v", c.Name, err)
			}
			instance = result
		} else {
			// __new__ exists but isn't callable, fall back to default
			instance = NewInstance(c)
		}
	} else {
		// No __new__, use default instance creation
		instance = NewInstance(c)
	}

	// Call __init__ if it exists and instance is of the right type
	// Only call __init__ if __new__ returned an instance of this class
	if inst, ok := instance.(*Instance); ok && inst.Class == c {
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
// Implements Python's descriptor protocol lookup order:
// 1. Data descriptors from class (has __set__ or __delete__)
// 2. Instance __dict__
// 3. Non-data descriptors from class (has __get__ only)
// 4. Class attributes
func (i *Instance) GetAttr(name string) (Value, bool) {
	// Check special attributes first
	if name == "__class__" {
		return i.Class, true
	}

	// Step 1: Check for data descriptor in class
	// A data descriptor has __set__ or __delete__ methods
	if classAttr, _, ok := i.Class.GetMethodWithClass(name); ok {
		// Check if it's a data descriptor (has __set__ or __delete__)
		hasSet := false
		hasDelete := false
		if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if _, exists := obj.GetAttr("__set__"); exists {
				hasSet = true
			}
			if _, exists := obj.GetAttr("__delete__"); exists {
				hasDelete = true
			}
		}

		// If it's a data descriptor, invoke __get__
		if hasSet || hasDelete {
			if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
				if getMethod, exists := obj.GetAttr("__get__"); exists {
					if callable, ok := getMethod.(Callable); ok {
						// Call descriptor.__get__(instance, type)
						result, err := callable.Call([]Value{i, i.Class}, nil)
						if err == nil {
							return result, true
						}
					}
				}
			}
			// If __get__ doesn't exist or fails, return the descriptor itself
			return classAttr, true
		}
	} else if classAttr, ok := i.Class.GetClassAttr(name); ok {
		// Check class attributes for data descriptors too
		hasSet := false
		hasDelete := false
		if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if _, exists := obj.GetAttr("__set__"); exists {
				hasSet = true
			}
			if _, exists := obj.GetAttr("__delete__"); exists {
				hasDelete = true
			}
		}

		// If it's a data descriptor, invoke __get__
		if hasSet || hasDelete {
			if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
				if getMethod, exists := obj.GetAttr("__get__"); exists {
					if callable, ok := getMethod.(Callable); ok {
						// Call descriptor.__get__(instance, type)
						result, err := callable.Call([]Value{i, i.Class}, nil)
						if err == nil {
							return result, true
						}
					}
				}
			}
			// If __get__ doesn't exist or fails, return the descriptor itself
			return classAttr, true
		}
	}

	// Step 2: Check instance __dict__
	if attr, ok := i.Attributes[name]; ok {
		return attr, true
	}

	// Step 3: Check for non-data descriptor in class (has __get__ but not __set__ or __delete__)
	if classAttr, defClass, ok := i.Class.GetMethodWithClass(name); ok {
		// Check if it has __get__ (making it a non-data descriptor)
		if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if getMethod, exists := obj.GetAttr("__get__"); exists {
				if callable, ok := getMethod.(Callable); ok {
					// Call descriptor.__get__(instance, type)
					result, err := callable.Call([]Value{i, i.Class}, nil)
					if err == nil {
						return result, true
					}
				}
			}
		}

		// Not a descriptor, check if it's a regular method that needs binding
		if callable, ok := classAttr.(interface {
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
		return classAttr, true
	}

	// Step 4: Check class attributes (non-descriptors)
	if attr, ok := i.Class.GetClassAttr(name); ok {
		// Check if it has __get__
		if obj, ok := attr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if getMethod, exists := obj.GetAttr("__get__"); exists {
				if callable, ok := getMethod.(Callable); ok {
					// Call descriptor.__get__(instance, type)
					result, err := callable.Call([]Value{i, i.Class}, nil)
					if err == nil {
						return result, true
					}
				}
			}
		}
		return attr, true
	}

	return nil, false
}

// SetAttr implements Object interface for instances
// Implements Python's descriptor protocol for setting:
// 1. Check for data descriptor with __set__ in class
// 2. Set in instance __dict__
func (i *Instance) SetAttr(name string, value Value) error {
	// Check for data descriptor in class with __set__
	if classAttr, _, ok := i.Class.GetMethodWithClass(name); ok {
		if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if setMethod, exists := obj.GetAttr("__set__"); exists {
				if callable, ok := setMethod.(Callable); ok {
					// Call descriptor.__set__(instance, value)
					_, err := callable.Call([]Value{i, value}, nil)
					return err
				}
			}
		}
	} else if classAttr, ok := i.Class.GetClassAttr(name); ok {
		// Check class attributes for descriptors with __set__
		if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if setMethod, exists := obj.GetAttr("__set__"); exists {
				if callable, ok := setMethod.(Callable); ok {
					// Call descriptor.__set__(instance, value)
					_, err := callable.Call([]Value{i, value}, nil)
					return err
				}
			}
		}
	}

	// No descriptor with __set__, set in instance __dict__
	i.Attributes[name] = value
	return nil
}

// DelAttr deletes an attribute from the instance
// Implements Python's descriptor protocol for deletion:
// 1. Check for descriptor with __delete__ in class
// 2. Delete from instance __dict__
func (i *Instance) DelAttr(name string) error {
	// Check for descriptor in class with __delete__
	if classAttr, _, ok := i.Class.GetMethodWithClass(name); ok {
		if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if deleteMethod, exists := obj.GetAttr("__delete__"); exists {
				if callable, ok := deleteMethod.(Callable); ok {
					// Call descriptor.__delete__(instance)
					_, err := callable.Call([]Value{i}, nil)
					return err
				}
			}
		}
	} else if classAttr, ok := i.Class.GetClassAttr(name); ok {
		// Check class attributes for descriptors with __delete__
		if obj, ok := classAttr.(interface{ GetAttr(string) (Value, bool) }); ok {
			if deleteMethod, exists := obj.GetAttr("__delete__"); exists {
				if callable, ok := deleteMethod.(Callable); ok {
					// Call descriptor.__delete__(instance)
					_, err := callable.Call([]Value{i}, nil)
					return err
				}
			}
		}
	}

	// No descriptor with __delete__, delete from instance __dict__
	if _, ok := i.Attributes[name]; !ok {
		return fmt.Errorf("'%s' object has no attribute '%s'", i.Class.Name, name)
	}
	delete(i.Attributes, name)
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

// GetAttr implements attribute access for bound instance methods
// Provides default values for standard function attributes
func (bm *BoundInstanceMethod) GetAttr(name string) (Value, bool) {
	// Try to get attributes from the underlying method first
	if methodWithAttrs, ok := bm.Method.(interface {
		GetAttr(string) (Value, bool)
	}); ok {
		if val, ok := methodWithAttrs.GetAttr(name); ok {
			return val, true
		}
	}

	// Provide default values for standard function attributes
	switch name {
	case "__name__":
		// Try to get name from underlying method
		if methodWithAttrs, ok := bm.Method.(interface {
			GetAttr(string) (Value, bool)
		}); ok {
			if val, ok := methodWithAttrs.GetAttr("__name__"); ok {
				return val, true
			}
		}
		return StringValue("<method>"), true
	case "__qualname__":
		// Include class name in qualified name
		if bm.DefiningClass != nil {
			if methodWithAttrs, ok := bm.Method.(interface {
				GetAttr(string) (Value, bool)
			}); ok {
				if nameVal, ok := methodWithAttrs.GetAttr("__name__"); ok {
					if nameStr, ok := nameVal.(StringValue); ok {
						return StringValue(fmt.Sprintf("%s.%s", bm.DefiningClass.Name, string(nameStr))), true
					}
				}
			}
		}
		return StringValue("<method>"), true
	case "__module__":
		// Bound instance methods inherit module from their defining class or default to __main__
		return StringValue("__main__"), true
	case "__doc__":
		// Try to get doc from underlying method
		if methodWithAttrs, ok := bm.Method.(interface {
			GetAttr(string) (Value, bool)
		}); ok {
			if val, ok := methodWithAttrs.GetAttr("__doc__"); ok {
				return val, true
			}
		}
		return None, true
	case "__annotations__":
		return NewDict(), true
	case "__type_params__":
		return TupleValue{}, true
	case "__dict__":
		return NewDict(), true
	case "__self__":
		// Return the bound instance
		return bm.Instance, true
	case "__func__":
		// Return the underlying function if it's a Value
		if methodVal, ok := bm.Method.(Value); ok {
			return methodVal, true
		}
		// Otherwise return None
		return None, true
	}

	return nil, false
}

// Super represents access to parent class methods
type Super struct {
	BaseObject
	Class       *Class
	Instance    *Instance // For instance methods (self)
	TargetClass *Class    // For class methods/__new__ (cls)
}

// BoundSuperMethod wraps a method from a Super object to inject __class__ when called
type BoundSuperMethod struct {
	BaseObject
	Method Value  // The underlying method
	Class  *Class // The class to set as __class__
}

// Call implements Callable interface for BoundSuperMethod
func (bsm *BoundSuperMethod) Call(args []Value, ctx *Context) (Value, error) {
	// Create a new context with __class__ set
	methodCtx := NewContext(ctx)
	methodCtx.Define("__class__", bsm.Class)

	// Call the underlying method
	if callable, ok := bsm.Method.(interface {
		Call([]Value, *Context) (Value, error)
	}); ok {
		return callable.Call(args, methodCtx)
	}
	return nil, fmt.Errorf("bound super method is not callable")
}

// Type returns the bound super method type
func (bsm *BoundSuperMethod) Type() Type {
	return MethodType
}

// String returns the string representation
func (bsm *BoundSuperMethod) String() string {
	return fmt.Sprintf("<bound super method of %s>", bsm.Class.Name)
}

// NewSuper creates a new super object for instance methods
func NewSuper(class *Class, instance *Instance) *Super {
	return &Super{
		BaseObject: *NewBaseObject(Type("super")),
		Class:      class,
		Instance:   instance,
	}
}

// NewSuperForClass creates a new super object for class methods and __new__
func NewSuperForClass(class *Class, targetClass *Class) *Super {
	return &Super{
		BaseObject:  *NewBaseObject(Type("super")),
		Class:       class,
		TargetClass: targetClass,
	}
}

// Type returns the super type
func (s *Super) Type() Type {
	return Type("super")
}

// String returns the string representation
func (s *Super) String() string {
	if s.Instance == nil {
		return fmt.Sprintf("<super: %s, <class>>", s.Class.Name)
	}
	return fmt.Sprintf("<super: %s, <%s object>>", s.Class.Name, s.Instance.Class.Name)
}

// GetAttr gets an attribute from the parent class
func (s *Super) GetAttr(name string) (Value, bool) {
	// Helper function to bind method to instance
	bindMethod := func(method Value, defClass *Class) Value {
		if callable, ok := method.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			// Create bound method if we have an instance
			if s.Instance != nil {
				return &BoundInstanceMethod{
					Instance:      s.Instance,
					Method:        callable,
					DefiningClass: defClass,
				}
			}
		}
		return method
	}

	// super() should SKIP s.Class and look in parent classes only
	// This is the key difference from instance.GetAttr which checks the instance's class first
	//
	// HOWEVER: For special methods like __new__ that exist on the root metaclass (type),
	// we need to check s.Class itself if it has no parents. This handles the case where
	// a metaclass calls super().__new__() and the metaclass's parent is type.

	// Look in parent classes using MRO
	if len(s.Class.Parents) > 0 {
		for _, parent := range s.Class.Parents {
			if method, defClass, ok := parent.GetMethodWithClass(name); ok {
				return bindMethod(method, defClass), true
			}
		}
	} else if s.Class.Parent != nil {
		// Fallback to single parent for backward compatibility
		if method, defClass, ok := s.Class.Parent.GetMethodWithClass(name); ok {
			return bindMethod(method, defClass), true
		}

		// Also try GetAttr on the parent class
		if method, ok := s.Class.Parent.GetAttr(name); ok {
			return bindMethod(method, s.Class.Parent), true
		}
	} else {
		// No parents - this is the root class (like 'type' or 'object')
		// For special methods, check the class itself
		// This is needed because type.__new__ exists on type, and when a metaclass
		// inherits from type and calls super().__new__(), we need to find type.__new__
		if method, ok := s.Class.GetMethod(name); ok {
			return bindMethod(method, s.Class), true
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

// GenericAlias represents a parameterized generic type like list[int] or dict[str, int]
type GenericAlias struct {
	BaseObject
	Origin *Class  // The origin class (e.g., list, dict)
	Args   []Value // The type parameters (e.g., [int] for list[int])
}

// NewGenericAlias creates a new generic alias
func NewGenericAlias(origin *Class, args []Value) *GenericAlias {
	return &GenericAlias{
		BaseObject: *NewBaseObject(Type("GenericAlias")),
		Origin:     origin,
		Args:       args,
	}
}

// Type returns the generic alias type
func (ga *GenericAlias) Type() Type {
	return Type("GenericAlias")
}

// String returns the string representation
func (ga *GenericAlias) String() string {
	if len(ga.Args) == 0 {
		return ga.Origin.Name
	}
	argStrs := make([]string, len(ga.Args))
	for i, arg := range ga.Args {
		argStrs[i] = PrintValue(arg)
	}
	return fmt.Sprintf("%s[%s]", ga.Origin.Name, strings.Join(argStrs, ", "))
}

// GetAttr implements Object interface for generic aliases
func (ga *GenericAlias) GetAttr(name string) (Value, bool) {
	// Special attributes for generic aliases
	switch name {
	case "__origin__":
		return ga.Origin, true
	case "__args__":
		return TupleValue(ga.Args), true
	}
	return ga.BaseObject.GetAttr(name)
}

// UnionType represents a union of types like int | str
type UnionType struct {
	BaseObject
	Types []Value // The types in the union
}

// NewUnionType creates a new union type
func NewUnionType(types []Value) *UnionType {
	return &UnionType{
		BaseObject: *NewBaseObject(Type("UnionType")),
		Types:      types,
	}
}

// Type returns the union type
func (ut *UnionType) Type() Type {
	return Type("UnionType")
}

// String returns the string representation
func (ut *UnionType) String() string {
	if len(ut.Types) == 0 {
		return "UnionType[]"
	}
	typeStrs := make([]string, len(ut.Types))
	for i, t := range ut.Types {
		typeStrs[i] = PrintValue(t)
	}
	return strings.Join(typeStrs, " | ")
}

// GetAttr implements Object interface for union types
func (ut *UnionType) GetAttr(name string) (Value, bool) {
	// Special attributes for union types
	switch name {
	case "__args__":
		return TupleValue(ut.Types), true
	case "__or__":
		// Support chaining: int | str | float
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__or__ takes exactly 1 argument")
			}
			// Add the new type to the union
			newTypes := append([]Value{}, ut.Types...)
			newTypes = append(newTypes, args[0])
			return NewUnionType(newTypes), nil
		}), true
	}
	return ut.BaseObject.GetAttr(name)
}

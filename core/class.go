package core

import (
	"fmt"
	"strings"
)

// Class represents a class definition
type Class struct {
	BaseObject
	Name        string            // Class name
	Module      string            // Module name (__module__ attribute)
	Doc         string            // Docstring (__doc__ attribute)
	Parent      *Class            // Parent class (for inheritance) - deprecated, use Parents
	Parents     []*Class          // Parent classes (for multiple inheritance)
	Methods     map[string]Value  // Class methods
	Attributes  map[string]Value  // Class attributes
	Constructor *MethodDescriptor // __init__ method
	SlotNames   []string          // __slots__ attribute (if defined)
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
		Module:     "__main__", // Default module
		Parent:     parent,     // Keep for backward compatibility
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
		Module:     "__main__", // Default module
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
	// Debug for PurePath.__init__
	if (c.Name == "PurePath" || c.Name == "Path") && name == "__init__" {
		fmt.Printf("[DEBUG GetMethodWithClass] Looking for %s.%s, in Methods: %v\n", c.Name, name, c.Methods[name] != nil)
	}

	// Check this class first
	if method, ok := c.Methods[name]; ok {
		if (c.Name == "PurePath" || c.Name == "Path") && name == "__init__" {
			fmt.Printf("[DEBUG GetMethodWithClass] FOUND %s.%s in Methods\n", c.Name, name)
		}
		return method, c, true
	}

	// Debug when not found
	if (c.Name == "PurePath" || c.Name == "Path") && name == "__init__" {
		fmt.Printf("[DEBUG GetMethodWithClass] NOT found in %s.Methods, checking parents\n", c.Name)
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
	// Debug for PurePath.__init__
	if (c.Name == "PurePath" || c.Name == "Path") && name == "__init__" {
		fmt.Printf("[DEBUG SetMethod] %s.%s being set in Methods map (before: %v, after: will be %T)\n", c.Name, name, c.Methods[name] != nil, method)
	}

	c.Methods[name] = method

	// Debug verification
	if (c.Name == "PurePath" || c.Name == "Path") && name == "__init__" {
		fmt.Printf("[DEBUG SetMethod] %s.%s set complete, now in Methods: %v\n", c.Name, name, c.Methods[name] != nil)
	}

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
	// Special handling for __name__
	if name == "__name__" {
		return StringValue(c.Name), true
	}

	// Special handling for __module__
	if name == "__module__" {
		return StringValue(c.Module), true
	}

	// Special handling for __doc__
	if name == "__doc__" {
		if c.Doc != "" {
			return StringValue(c.Doc), true
		}
		// Return None if no docstring
		return None, true
	}

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

		// Add special class attributes that are computed on-the-fly
		// These must be included in __dict__ for CPython compatibility

		// __name__
		nameKey := ValueToKey(StringValue("__name__"))
		dict.SetWithKey(nameKey, StringValue("__name__"), StringValue(c.Name))

		// __mro__ - Method Resolution Order
		// Use a descriptor so that type.__dict__['__mro__'].__get__ works
		mroDescriptor := NewMRODescriptor(c)
		mroKey := ValueToKey(StringValue("__mro__"))
		dict.SetWithKey(mroKey, StringValue("__mro__"), mroDescriptor)

		// __dict__ - Add a descriptor for __dict__ itself
		// This allows type.__dict__['__dict__'].__get__ to work (used by inspect.py)
		dictDescriptor := NewDictDescriptor()
		dictKey := ValueToKey(StringValue("__dict__"))
		dict.SetWithKey(dictKey, StringValue("__dict__"), dictDescriptor)

		// __bases__ - Direct parent classes
		var bases []Value
		if c.Parent != nil {
			bases = append(bases, c.Parent)
		}
		for i, parent := range c.Parents {
			if i == 0 && c.Parent != nil {
				continue
			}
			bases = append(bases, parent)
		}
		basesKey := ValueToKey(StringValue("__bases__"))
		dict.SetWithKey(basesKey, StringValue("__bases__"), TupleValue(bases))

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
	// Delegate to CallWithKeywords with no keyword arguments
	return c.CallWithKeywords(args, nil, ctx)
}

// CallWithKeywords implements keyword argument support for class instantiation
// This is the generic solution that unlocks full Python compatibility for all classes
func (c *Class) CallWithKeywords(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	// Python instance creation flow:
	// 1. Call __new__ to create the instance
	// 2. If __new__ returns an instance of the class, call __init__ on it
	// 3. Return the instance

	var instance Value

	// Check for __new__ method
	if newMethod, ok := c.GetMethod("__new__"); ok {
		// __new__ is a static method that takes the class as first argument
		// Prepend class as first argument
		newArgs := append([]Value{c}, args...)

		// Call __new__ with positional args only
		// In Python, __new__ typically only receives positional args,
		// while keyword args are handled by __init__
		if callable, ok := newMethod.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
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
	// Call __init__ if __new__ returned an instance of this class or a subclass
	if inst, ok := instance.(*Instance); ok && IsInstanceOf(inst, c) {
		if initMethod, ok := c.GetMethod("__init__"); ok {
			// Debug for pathlib classes
			if c.Name == "PosixPath" || c.Name == "PurePath" || c.Name == "PurePosixPath" || c.Name == "Path" {
				fmt.Printf("[DEBUG Class.CallWithKeywords] Calling %s.__init__ with %d args, %d kwargs\n", c.Name, len(args), len(kwargs))
			}

			// Create a new context with __class__ set to the class whose __init__ we're calling
			// This allows super() to work correctly inside __init__
			initCtx := NewContext(ctx)
			initCtx.Define("__class__", c)

			// Prepend instance as first argument (self)
			initArgs := append([]Value{instance}, args...)

			// Try CallWithKeywords first if method supports it
			if kwargsCallable, ok := initMethod.(interface {
				CallWithKeywords([]Value, map[string]Value, *Context) (Value, error)
			}); ok {
				_, err := kwargsCallable.CallWithKeywords(initArgs, kwargs, initCtx)
				if err != nil {
					return nil, fmt.Errorf("error in %s.__init__: %v", c.Name, err)
				}
			} else if callable, ok := initMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				// __init__ doesn't support kwargs - call with positional args only
				// If kwargs were provided, this is an error
				if len(kwargs) > 0 {
					return nil, fmt.Errorf("%s.__init__ does not support keyword arguments (method type: %T)", c.Name, initMethod)
				}
				_, err := callable.Call(initArgs, initCtx)
				if err != nil {
					return nil, fmt.Errorf("error in %s.__init__: %v", c.Name, err)
				}
			}
		}
	} else {
		// Debug: Log when __init__ is NOT called
		if inst, ok := instance.(*Instance); ok {
			if c.Name == "PosixPath" || c.Name == "PurePath" || c.Name == "PurePosixPath" || c.Name == "Path" {
				fmt.Printf("[DEBUG Class.CallWithKeywords] SKIPPING __init__ for %s because inst.Class=%s != c=%s\n", c.Name, inst.Class.Name, c.Name)
			}
		}
	}

	return instance, nil
}

// Instance represents an instance of a class
type Instance struct {
	BaseObject
	Class      *Class           // The class this is an instance of
	Attributes map[string]Value // Instance attributes (__dict__)
	SlotValues []Value          // Slot values (if class uses __slots__)
}

// NewInstance creates a new instance of a class
func NewInstance(class *Class) *Instance {
	inst := &Instance{
		BaseObject: *NewBaseObject(Type("instance")),
		Class:      class,
		Attributes: make(map[string]Value),
	}

	// If class has __slots__, initialize slot values
	if class.HasSlots() {
		inst.SlotValues = make([]Value, len(class.SlotNames))
		// Initialize all slots to nil (unset)
		for i := range inst.SlotValues {
			inst.SlotValues[i] = nil
		}

		// Debug for Path-related classes
		if class.Name == "PosixPath" || class.Name == "Path" || class.Name == "PurePath" {
			fmt.Printf("[DEBUG NewInstance] %s: %d slots - %v\n", class.Name, len(class.SlotNames), class.SlotNames)
		}
	}

	return inst
}

// TupleInstance represents a tuple that is an instance of a class (e.g., namedtuple)
type TupleInstance struct {
	Data  TupleValue // The underlying tuple data
	Class *Class     // The class this tuple is an instance of
}

// NewTupleInstance creates a new tuple instance of a class
func NewTupleInstance(class *Class, data TupleValue) *TupleInstance {
	return &TupleInstance{
		Data:  data,
		Class: class,
	}
}

// Type returns the type of the tuple instance
func (t *TupleInstance) Type() Type {
	return Type(t.Class.Name)
}

// String returns the string representation
func (t *TupleInstance) String() string {
	return t.Data.String()
}

// Iterator returns an iterator for the tuple
func (t *TupleInstance) Iterator() Iterator {
	return t.Data.Iterator()
}

// Len returns the length of the tuple
func (t *TupleInstance) Len() int {
	return len(t.Data)
}

// Items returns the tuple items
func (t *TupleInstance) Items() []Value {
	return t.Data
}

// GetAttr gets an attribute from the tuple's class
func (t *TupleInstance) GetAttr(name string) (Value, bool) {
	// Special case for __class__
	if name == "__class__" {
		return t.Class, true
	}
	// Look up in the class
	if attr, found := t.Class.GetAttr(name); found {
		return attr, true
	}
	return nil, false
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
	// Debug for PosixPath._raw_paths
	if (i.Class.Name == "PosixPath" || i.Class.Name == "PurePath" || i.Class.Name == "Path") && name == "_raw_paths" {
		fmt.Printf("[DEBUG SetAttr] %s.%s = %v\n", i.Class.Name, name, value)
	}

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
	// Debug for Path-related classes
	if (i.Class.Name == "PosixPath" || i.Class.Name == "Path" || i.Class.Name == "PurePath") && name == "_raw_paths" {
		fmt.Printf("[DEBUG SetAttr] %s.%s being set in Attributes dict (no descriptor found)\n", i.Class.Name, name)
	}
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

// CallWithKeywords implements keyword argument support for bound instance methods
func (bm *BoundInstanceMethod) CallWithKeywords(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
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

	// Check if the underlying method supports keyword arguments
	if kwargsMethod, ok := bm.Method.(interface {
		CallWithKeywords([]Value, map[string]Value, *Context) (Value, error)
	}); ok {
		return kwargsMethod.CallWithKeywords(callArgs, kwargs, methodCtx)
	}

	// If method doesn't support kwargs but is a Callable, try calling it with kwargs as positional
	// This handles cases where the method's Call interface may internally support kwargs
	if len(kwargs) > 0 {
		// Try to use eval.EvalKwargCall which handles UserFunctions with signatures
		fmt.Printf("[DEBUG BoundInstanceMethod] Method type: %T\n", bm.Method)
		fmt.Printf("[DEBUG BoundInstanceMethod] Falling back to regular Call - kwargs will be lost\n")
		// For now, we just error. TODO: integrate with kwarg_eval
		return nil, fmt.Errorf("method does not support keyword arguments")
	}

	// Fall back to regular Call
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
	// Debug for pathlib classes
	if bsm.Class.Name == "PurePath" || bsm.Class.Name == "Path" || bsm.Class.Name == "PosixPath" {
		fmt.Printf("[DEBUG BoundSuperMethod.Call] Calling method from %s with %d args\n", bsm.Class.Name, len(args))
	}
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
				// Debug for pathlib classes
				if name == "__init__" && (s.Instance.Class.Name == "PosixPath" || s.Instance.Class.Name == "Path") {
					fmt.Printf("[DEBUG Super.GetAttr] Binding __init__ from class %s for instance %s (method type: %T)\n", defClass.Name, s.Instance.Class.Name, method)
				}
				// Use BoundSuperMethod to ensure __class__ is set correctly
				// This allows super() to work correctly in parent class methods
				return &BoundSuperMethod{
					BaseObject: *NewBaseObject(MethodType),
					Method: &BoundInstanceMethod{
						Instance:      s.Instance,
						Method:        callable,
						DefiningClass: defClass,
					},
					Class: defClass,
				}
			}
		}
		return method
	}

	// super() returns a proxy that looks up methods starting from s.Class
	// When you call super().__init__() inside Path.__init__:
	// 1. superForm() creates Super(class=PurePath) - skipping Path itself
	// 2. Super.GetAttr("__init__") should look IN PurePath (not PurePath's parents)
	// 3. If not found in PurePath, then check PurePath's parents
	//
	// The key: s.Class is ALREADY the parent to start searching from,
	// so we need to check s.Class itself first, then s.Class's parents.

	// First check s.Class itself (the starting point for MRO)
	if method, defClass, ok := s.Class.GetMethodWithClass(name); ok {
		return bindMethod(method, defClass), true
	}
	if method, ok := s.Class.GetAttr(name); ok {
		return bindMethod(method, s.Class), true
	}

	// Then look in s.Class's parent classes using MRO
	if len(s.Class.Parents) > 0 {
		// Debug for pathlib
		if name == "__init__" && (s.Instance.Class.Name == "PosixPath" || s.Instance.Class.Name == "Path") {
			fmt.Printf("[DEBUG Super.GetAttr] s.Class=%s, s.Class.Parents=%v\n", s.Class.Name, func() []string {
				names := make([]string, len(s.Class.Parents))
				for i, p := range s.Class.Parents {
					names[i] = p.Name
				}
				return names
			}())
		}
		for _, parent := range s.Class.Parents {
			// Debug for pathlib
			if name == "__init__" && (s.Instance.Class.Name == "PosixPath" || s.Instance.Class.Name == "Path") {
				fmt.Printf("[DEBUG Super.GetAttr] Checking parent %s for __init__\n", parent.Name)
			}
			// First try GetMethodWithClass (faster, checks Methods map)
			if method, defClass, ok := parent.GetMethodWithClass(name); ok {
				if name == "__init__" && (s.Instance.Class.Name == "PosixPath" || s.Instance.Class.Name == "Path") {
					fmt.Printf("[DEBUG Super.GetAttr] Found __init__ in %s.Methods\n", defClass.Name)
				}
				return bindMethod(method, defClass), true
			}
			// Fallback to GetAttr to check Attributes (for Python-defined methods)
			if method, ok := parent.GetAttr(name); ok {
				if name == "__init__" && (s.Instance.Class.Name == "PosixPath" || s.Instance.Class.Name == "Path") {
					fmt.Printf("[DEBUG Super.GetAttr] Found __init__ in %s via GetAttr\n", parent.Name)
				}
				return bindMethod(method, parent), true
			}
			if name == "__init__" && (s.Instance.Class.Name == "PosixPath" || s.Instance.Class.Name == "Path") {
				fmt.Printf("[DEBUG Super.GetAttr] __init__ NOT found in parent %s\n", parent.Name)
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

// HasSlots returns true if a class uses __slots__
func (c *Class) HasSlots() bool {
	return len(c.SlotNames) > 0
}

// GetSlotIndex returns the index of a slot by name, or -1 if not found
func (c *Class) GetSlotIndex(name string) int {
	for i, slotName := range c.SlotNames {
		if slotName == name {
			return i
		}
	}
	return -1
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

// MRODescriptor is a descriptor for accessing __mro__ attribute
// It implements the descriptor protocol with __get__ method
type MRODescriptor struct {
	BaseObject
	class *Class // The class whose MRO this descriptor provides
}

// NewMRODescriptor creates a new MRO descriptor
func NewMRODescriptor(class *Class) *MRODescriptor {
	return &MRODescriptor{
		BaseObject: *NewBaseObject(Type("getset_descriptor")),
		class:      class,
	}
}

// GetAttr implements attribute access for MRODescriptor
func (m *MRODescriptor) GetAttr(name string) (Value, bool) {
	if name == "__get__" {
		// Return a function that implements the __get__ method of the descriptor protocol
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// __get__(self, obj, type=None)
			// For class-level access, obj will be None
			// Return the MRO tuple
			mro := []Value{m.class}
			current := m.class.Parent
			for current != nil {
				mro = append(mro, current)
				current = current.Parent
			}
			// Also include parents from multiple inheritance
			if len(m.class.Parents) > 0 {
				for i, parent := range m.class.Parents {
					if i == 0 && m.class.Parent != nil {
						continue
					}
					p := parent
					for p != nil {
						alreadyInMRO := false
						for _, existing := range mro {
							if existingClass, ok := existing.(*Class); ok {
								if existingClass == p {
									alreadyInMRO = true
									break
								}
							}
						}
						if !alreadyInMRO {
							mro = append(mro, p)
						}
						p = p.Parent
					}
				}
			}
			return TupleValue(mro), nil
		}), true
	}
	return m.BaseObject.GetAttr(name)
}

// DictDescriptor is a descriptor for accessing __dict__ attribute of classes
// It implements the descriptor protocol with __get__ method
type DictDescriptor struct {
	BaseObject
}

// NewDictDescriptor creates a new __dict__ descriptor
func NewDictDescriptor() *DictDescriptor {
	return &DictDescriptor{
		BaseObject: *NewBaseObject(Type("getset_descriptor")),
	}
}

// GetAttr implements attribute access for DictDescriptor
func (d *DictDescriptor) GetAttr(name string) (Value, bool) {
	if name == "__get__" {
		// Return a function that implements the __get__ method of the descriptor protocol
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// __get__(self, obj, type=None)
			// args[0] is the instance (or None for class-level access)
			// args[1] is the type (optional)

			if len(args) < 1 {
				return nil, fmt.Errorf("__get__ requires at least 1 argument")
			}

			obj := args[0]

			// If accessing from a class (obj is None), we need to get the class from args[1]
			// If accessing from an instance, obj is the instance
			if obj == None || obj == nil {
				// Class-level access: type.__dict__['__dict__']
				// Need to get the class from args[1]
				if len(args) < 2 {
					return nil, fmt.Errorf("__get__ on None requires type argument")
				}
				targetClass, ok := args[1].(*Class)
				if !ok {
					return nil, fmt.Errorf("__get__ second argument must be a class")
				}
				// Build the class's __dict__ directly (don't call GetAttr to avoid recursion)
				dict := NewDict()
				for methodName, method := range targetClass.Methods {
					keyStr := StringValue(methodName)
					dict.SetWithKey(ValueToKey(keyStr), keyStr, method)
				}
				for attrName, attr := range targetClass.Attributes {
					keyStr := StringValue(attrName)
					dict.SetWithKey(ValueToKey(keyStr), keyStr, attr)
				}
				return dict, nil
			}

			// Instance access: instance.__dict__
			// Return the instance's __dict__
			if instance, ok := obj.(*Instance); ok {
				dict := NewDict()
				for attrName, attrValue := range instance.Attributes {
					keyStr := StringValue(attrName)
					dict.SetWithKey(ValueToKey(keyStr), keyStr, attrValue)
				}
				return dict, nil
			}

			// For classes accessed as instances
			if class, ok := obj.(*Class); ok {
				dict := NewDict()
				for methodName, method := range class.Methods {
					keyStr := StringValue(methodName)
					dict.SetWithKey(ValueToKey(keyStr), keyStr, method)
				}
				for attrName, attr := range class.Attributes {
					keyStr := StringValue(attrName)
					dict.SetWithKey(ValueToKey(keyStr), keyStr, attr)
				}
				return dict, nil
			}

			return nil, fmt.Errorf("object has no __dict__")
		}), true
	}
	return d.BaseObject.GetAttr(name)
}

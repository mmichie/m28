package core

import (
	"errors"
	"fmt"
	"os"
	"strings"
)

// Class represents a class definition
type Class struct {
	BaseObject
	Name        string            // Class name
	Module      string            // Module name (__module__ attribute)
	Doc         string            // Docstring (__doc__ attribute)
	Parents     []*Class          // Parent classes (for multiple inheritance)
	Methods     map[string]Value  // Class methods
	Attributes  map[string]Value  // Class attributes
	Annotations *DictValue        // Type annotations (__annotations__ attribute)
	Constructor *MethodDescriptor // __init__ method
	SlotNames   []string          // __slots__ attribute (if defined)
	Metaclass   Value             // Metaclass of this class (type by default)
}

// NewClass creates a new class
func NewClass(name string, parent *Class) *Class {
	var parents []*Class
	if parent != nil {
		parents = []*Class{parent}
	}
	return &Class{
		BaseObject:  *NewBaseObject(Type("class")),
		Name:        name,
		Module:      "__main__", // Default module
		Parents:     parents,
		Methods:     make(map[string]Value),
		Attributes:  make(map[string]Value),
		Annotations: NewDict(), // Initialize empty annotations dict
	}
}

// NewClassWithParents creates a new class with multiple parents
func NewClassWithParents(name string, parents []*Class) *Class {
	return &Class{
		BaseObject:  *NewBaseObject(Type("class")),
		Name:        name,
		Module:      "__main__", // Default module
		Parents:     parents,
		Methods:     make(map[string]Value),
		Attributes:  make(map[string]Value),
		Annotations: NewDict(), // Initialize empty annotations dict
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
	// Use breadth-first search to match Python's C3 linearization behavior
	// This ensures we check all direct parents before checking grandparents
	// First, check all direct parents (non-recursively)
	for _, parent := range c.Parents {
		if method, ok := parent.Methods[name]; ok {
			return method, true
		}
	}
	// Then check grandparents recursively
	for _, parent := range c.Parents {
		if method, ok := parent.GetMethod(name); ok {
			return method, true
		}
	}

	return nil, false
}

// GetMethodWithClass looks up a method and returns the class where it was defined
func (c *Class) GetMethodWithClass(name string) (Value, *Class, bool) {
	// Check this class first - look in both Methods and Attributes
	// This is important for when a subclass sets __eq__ = None to block inherited __eq__
	if method, ok := c.Methods[name]; ok {
		return method, c, true
	}

	// Also check class attributes - needed for __eq__ = None pattern
	// Only check for special methods (dunder methods) to avoid interference with regular methods
	if strings.HasPrefix(name, "__") && strings.HasSuffix(name, "__") {
		if attr, ok := c.Attributes[name]; ok {
			return attr, c, true
		}
	}

	// Check parent classes using MRO (Method Resolution Order)
	// Use breadth-first search to match Python's C3 linearization behavior
	if len(c.Parents) > 0 {
		// First, check all direct parents (non-recursively)
		for _, parent := range c.Parents {
			if method, ok := parent.Methods[name]; ok {
				return method, parent, true
			}
		}
		// Then check grandparents recursively
		for _, parent := range c.Parents {
			if method, defClass, ok := parent.GetMethodWithClass(name); ok {
				return method, defClass, true
			}
		}
	}

	return nil, nil, false
}

// IsSubclass checks if child is a subclass of parent (strict or not)
// Returns true if child == parent or child inherits from parent
func IsSubclass(child, parent *Class) bool {
	if child == nil || parent == nil {
		return false
	}
	if child == parent {
		return true
	}
	for _, p := range child.Parents {
		if IsSubclass(p, parent) {
			return true
		}
	}
	return false
}

// IsStrictSubclass checks if child is a strict subclass of parent (child != parent)
func IsStrictSubclass(child, parent *Class) bool {
	if child == nil || parent == nil {
		return false
	}
	if child == parent {
		return false
	}
	return IsSubclass(child, parent)
}

// SetMethod adds a method to the class
func (c *Class) SetMethod(name string, method Value) {
	c.Methods[name] = method
}

// GetClassAttr gets a class attribute
func (c *Class) GetClassAttr(name string) (Value, bool) {
	// Check this class first
	if attr, ok := c.Attributes[name]; ok {
		return attr, true
	}

	// Check parent classes using MRO (breadth-first)
	if len(c.Parents) > 0 {
		// First, check all direct parents (non-recursively)
		for _, parent := range c.Parents {
			if attr, ok := parent.Attributes[name]; ok {
				return attr, true
			}
		}
		// Then check grandparents recursively
		for _, parent := range c.Parents {
			if attr, ok := parent.GetClassAttr(name); ok {
				return attr, true
			}
		}
	}

	return nil, false
}

// SetClassAttr sets a class attribute
func (c *Class) SetClassAttr(name string, value Value) {
	c.Attributes[name] = value
}

// GetAttr implements Object interface for classes
func (c *Class) GetAttr(name string) (Value, bool) {
	// Special handling for __class__ - a class's __class__ is its metaclass
	// For most classes, this is the `type` metaclass
	if name == "__class__" {
		// Return the stored metaclass if available
		if c.Metaclass != nil {
			return c.Metaclass, true
		}
		// Check if a custom metaclass is set as a class attribute
		if metaclass, ok := c.GetClassAttr("__class__"); ok {
			return metaclass, true
		}
		// Default to TypeMetaclass (the `type` builtin)
		// Note: TypeMetaclass should be set during initialization
		// For now, return a simple type representation
		return c, true // Return self for now - this mimics Python's type
	}

	// Special handling for __name__
	if name == "__name__" {
		return StringValue(c.Name), true
	}

	// Special handling for __qualname__
	// In Python, __qualname__ is the qualified name including any enclosing classes
	// For now, we just use the class name (same as __name__)
	// TODO(M28-2854): Track nested class definitions to provide full qualified names
	if name == "__qualname__" {
		return StringValue(c.Name), true
	}

	// Special handling for __module__
	if name == "__module__" {
		return StringValue(c.Module), true
	}

	// Special handling for __setattr__ - needed for class attribute assignment
	// Python classes inherit __setattr__ from object/type
	// Note: This is accessed via Instance.GetAttr which wraps it in BoundInstanceMethod
	// BoundInstanceMethod prepends self, so we always receive 3 args: (self, name, value)
	if name == "__setattr__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// When called through BoundInstanceMethod, we get (self, name, value)
			if len(args) < 2 {
				return nil, &TypeError{
					Message: fmt.Sprintf("__setattr__() missing required arguments"),
				}
			}

			// Handle both bound (3 args from BoundInstanceMethod) and direct (2 args) calls
			var self Value
			var attrNameVal Value
			var attrValue Value

			if len(args) == 3 {
				// Normal case: BoundInstanceMethod provides (self, name, value)
				self = args[0]
				attrNameVal = args[1]
				attrValue = args[2]
			} else if len(args) == 2 {
				// Direct call without binding: assume args are (name, value)
				// This shouldn't normally happen, but handle it gracefully
				// We can't do anything without self, so return error
				return nil, &TypeError{
					Message: "descriptor '__setattr__' requires an instance",
				}
			} else {
				return nil, &TypeError{
					Message: fmt.Sprintf("__setattr__() takes 3 arguments, got %d", len(args)),
				}
			}

			attrName, ok := attrNameVal.(StringValue)
			if !ok {
				return nil, NewTypeError("string", attrNameVal, "attribute name must be a string")
			}

			// Set the attribute on the object
			if obj, ok := self.(Object); ok {
				obj.SetAttr(string(attrName), attrValue)
				return None, nil
			}
			return nil, NewTypeError("object", self, "__setattr__ first argument must be an object")
		}), true
	}

	// Special handling for __doc__
	if name == "__doc__" {
		if c.Doc != "" {
			return StringValue(c.Doc), true
		}
		// Return None if no docstring
		return None, true
	}

	// Special handling for __annotations__
	if name == "__annotations__" {
		if c.Annotations != nil {
			return c.Annotations, true
		}
		// Return empty dict if no annotations
		return NewDict(), true
	}

	// Special handling for __bases__ (Direct parent classes)
	if name == "__bases__" {
		bases := make([]Value, len(c.Parents))
		for i, parent := range c.Parents {
			bases[i] = parent
		}
		return TupleValue(bases), true
	}

	// Special handling for __mro__ (Method Resolution Order)
	if name == "__mro__" {
		// Build the MRO tuple using C3 linearization
		// Simplified algorithm: breadth-first traversal with deduplication
		mro := []Value{c}
		seen := make(map[*Class]bool)
		seen[c] = true

		// Queue of classes to process
		queue := []*Class{}

		// Add direct parents to queue
		queue = append(queue, c.Parents...)

		// Process queue in breadth-first order
		for len(queue) > 0 {
			cls := queue[0]
			queue = queue[1:]

			if cls == nil || seen[cls] {
				continue
			}

			mro = append(mro, cls)
			seen[cls] = true

			// Add this class's parents to the queue
			queue = append(queue, cls.Parents...)
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
		bases := make([]Value, len(c.Parents))
		for i, parent := range c.Parents {
			bases[i] = parent
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
				return nil, &TypeError{Message: "__or__ takes exactly 1 argument"}
			}
			return NewUnionType([]Value{c, args[0]}), nil
		}), true
	}

	// Special handling for __mro_entries__ (PEP 560 - Python 3.7+)
	// This allows typing constructs like Generic to substitute themselves during class creation
	// For regular classes, we just return the bases unchanged
	if name == "__mro_entries__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: fmt.Sprintf("__mro_entries__ takes exactly 1 argument, got %d", len(args))}
			}
			// Regular classes return the bases tuple unchanged
			// Typing constructs (like Generic) override this to return actual base classes
			return args[0], nil
		}), true
	}

	// First check methods
	if method, ok := c.GetMethod(name); ok {
		// Invoke descriptor protocol for classmethods and staticmethods
		if descriptor, hasGet := method.(interface{ GetAttr(string) (Value, bool) }); hasGet {
			if getMethod, found := descriptor.GetAttr("__get__"); found {
				if callable, ok := getMethod.(Callable); ok {
					// Call __get__(None, class) for class attribute access
					result, err := callable.Call([]Value{None, c}, nil)
					if err == nil {
						return result, true
					}
				}
			}
		}
		return method, true
	}

	// Then check attributes
	if attr, ok := c.GetClassAttr(name); ok {
		// Invoke descriptor protocol if the attribute has __get__
		if descriptor, hasGet := attr.(interface{ GetAttr(string) (Value, bool) }); hasGet {
			if getMethod, found := descriptor.GetAttr("__get__"); found {
				if callable, ok := getMethod.(Callable); ok {
					// Call __get__(None, class) for class attribute access
					result, err := callable.Call([]Value{None, c}, nil)
					if err == nil {
						return result, true
					}
				}
			}
		}
		return attr, true
	}

	// Provide default magic methods that Python classes automatically have
	switch name {
	case "__eq__":
		// Default __eq__ for classes and instances
		// Note: When called as bound method (via instance.GetAttr), args = [self, other]
		// When called via type dispatch (CallDunder), args = [other] and self is implicit
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			var self, other Value

			// Handle both bound method calls (2 args) and direct calls (1 arg)
			if len(args) == 2 {
				// Bound method: args = [self, other]
				self = args[0]
				other = args[1]
			} else if len(args) == 1 {
				// Direct call via type dispatch: args = [other], self is the class
				self = Value(c)
				other = args[0]
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("__eq__() takes 1 or 2 arguments (%d given)", len(args))}
			}

			// For instances, use identity comparison (object default behavior)
			if _, ok := self.(*Instance); ok {
				// Python's default object.__eq__ compares by identity
				// If identical, return True; otherwise return NotImplemented to allow reflected operation
				if self == other {
					return BoolValue(true), nil
				}
				return NotImplemented, nil
			}

			// For classes, compare by name if both are classes
			if selfClass, ok := self.(*Class); ok {
				if otherClass, ok := other.(*Class); ok {
					return BoolValue(selfClass.Name == otherClass.Name), nil
				}

				// Check if other has GetClass() method (for type wrappers like StrType, TypeType, etc.)
				if classGetter, ok := other.(interface{ GetClass() *Class }); ok {
					otherClass := classGetter.GetClass()
					return BoolValue(selfClass.Name == otherClass.Name), nil
				}
			}

			// For other types, use pointer equality
			return BoolValue(self == other), nil
		}), true
	case "__ne__":
		// Default __ne__ is negation of __eq__
		// Note: When called as bound method (via instance.GetAttr), args = [self, other]
		// When called via type dispatch (CallDunder), args = [other] and self is implicit
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			var self, other Value

			// Handle both bound method calls (2 args) and direct calls (1 arg)
			if len(args) == 2 {
				// Bound method: args = [self, other]
				self = args[0]
				other = args[1]
			} else if len(args) == 1 {
				// Direct call via type dispatch: args = [other], self is the class
				self = Value(c)
				other = args[0]
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("__ne__() takes 1 or 2 arguments (%d given)", len(args))}
			}

			// Get __eq__ method from self
			var eqMethod Value
			var ok bool
			if selfWithAttr, hasAttr := self.(interface{ GetAttr(string) (Value, bool) }); hasAttr {
				eqMethod, ok = selfWithAttr.GetAttr("__eq__")
			} else {
				eqMethod, ok = c.GetAttr("__eq__")
			}

			if !ok {
				// Fallback to identity comparison
				return BoolValue(self != other), nil
			}

			// Call __eq__(other) and negate the result
			if callable, ok := eqMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				// If eqMethod is a bound method, just pass [other]
				// If it's unbound, pass [self, other]
				var eqArgs []Value
				if _, isBound := eqMethod.(*BoundInstanceMethod); isBound {
					eqArgs = []Value{other}
				} else {
					eqArgs = []Value{other} // For class methods, other is the only arg needed
				}
				result, err := callable.Call(eqArgs, ctx)
				if err != nil {
					return nil, err
				}
				return BoolValue(!IsTruthy(result)), nil
			}
			// Fallback if __eq__ is not callable
			return BoolValue(self != other), nil
		}), true
	case "__repr__":
		// Default __repr__ returns a placeholder function
		// Classes use this as a type marker (e.g., dict.__repr__, list.__repr__)
		// Can be called two ways:
		//  - With 0 args when called on the class itself: Class.__repr__()
		//  - With 1 arg (self) when called on an instance: instance.__repr__()
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				// Called on the class itself - return class representation
				return StringValue(fmt.Sprintf("<class '%s'>", c.Name)), nil
			} else if len(args) == 1 {
				// Called on an instance - return instance representation
				return StringValue(fmt.Sprintf("<%s object at %p>", c.Name, args[0])), nil
			}
			return nil, &TypeError{Message: fmt.Sprintf("__repr__ takes 0 or 1 arguments, got %d", len(args))}
		}), true
	case "__init__":
		// Default __init__ that all classes inherit from object
		// object.__init__(self, *args, **kwargs) does nothing but accepts any arguments
		// This ensures all classes have __init__ even if they don't define it
		return &BuiltinFunctionWithKwargs{
			BaseObject: *NewBaseObject(FunctionType),
			Name:       "__init__",
			Fn: func(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
				// Default __init__ does nothing, just return None
				return None, nil
			},
		}, true
	}

	// Check metaclass for methods (Python behavior: class attributes come from metaclass)
	// This allows classes with metaclass=ABCMeta to access methods like register()
	if c.Metaclass != nil {
		// Cast metaclass to *Class
		if metaclass, ok := c.Metaclass.(*Class); ok {
			// Try to get the attribute from the metaclass
			if metaclassAttr, found := metaclass.GetMethod(name); found {
				// Wrap it as a classmethod that binds the class as first argument
				if callable, isCallable := metaclassAttr.(Callable); isCallable {
					return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
						// Prepend the class as first argument
						newArgs := make([]Value, len(args)+1)
						newArgs[0] = c
						copy(newArgs[1:], args)
						return callable.Call(newArgs, ctx)
					}), true
				}
				return metaclassAttr, true
			}
			// Also check metaclass attributes
			if metaclassAttr, found := metaclass.GetClassAttr(name); found {
				// Wrap callables as classmethods
				if callable, isCallable := metaclassAttr.(Callable); isCallable {
					return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
						// Prepend the class as first argument
						newArgs := make([]Value, len(args)+1)
						newArgs[0] = c
						copy(newArgs[1:], args)
						return callable.Call(newArgs, ctx)
					}), true
				}
				return metaclassAttr, true
			}
		}
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

// DelAttr deletes an attribute from the class
// Implements Python's attribute deletion for classes
func (c *Class) DelAttr(name string) error {
	// Check if the attribute exists in Methods
	if _, ok := c.Methods[name]; ok {
		delete(c.Methods, name)
		return nil
	}

	// Check if the attribute exists in Attributes
	if _, ok := c.Attributes[name]; ok {
		delete(c.Attributes, name)
		return nil
	}

	// Attribute doesn't exist
	return &AttributeError{ObjType: c.Name, AttrName: name}
}

// Call implements Callable interface for classes (instantiation)
func (c *Class) Call(args []Value, ctx *Context) (Value, error) {
	// Delegate to CallWithKeywords with no keyword arguments
	return c.CallWithKeywords(args, nil, ctx)
}

// CallWithKeywords implements keyword argument support for class instantiation
// This is the generic solution that unlocks full Python compatibility for all classes
func (c *Class) CallWithKeywords(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	// Python metaclass protocol:
	// If the class has a metaclass with __call__, invoke that first
	// This allows metaclasses to control instance creation (e.g., for enum singletons)
	if c.Metaclass != nil {
		if metaclass, ok := c.Metaclass.(*Class); ok {
			if callMethod, ok := metaclass.GetMethod("__call__"); ok {
				// Prepend the class as first argument to __call__
				callArgs := append([]Value{c}, args...)

				// Create a child context with __class__ set to the metaclass
				// This allows super() without arguments to work correctly in __call__
				callCtx := NewContext(ctx)
				callCtx.Define("__class__", metaclass)

				// Try CallWithKeywords first if method supports it
				if kwargsCallable, ok := callMethod.(interface {
					CallWithKeywords([]Value, map[string]Value, *Context) (Value, error)
				}); ok {
					return kwargsCallable.CallWithKeywords(callArgs, kwargs, callCtx)
				} else if callable, ok := callMethod.(interface {
					Call([]Value, *Context) (Value, error)
				}); ok {
					if len(kwargs) > 0 {
						return nil, &TypeError{Message: "metaclass __call__ does not support keyword arguments"}
					}
					return callable.Call(callArgs, callCtx)
				}
			}
		}
	}

	// Default Python instance creation flow:
	// 1. Call __new__ to create the instance
	// 2. If __new__ returns an instance of the class, call __init__ on it
	// 3. Return the instance

	var instance Value

	// Check for __new__ method
	if newMethod, ok := c.GetMethod("__new__"); ok {
		// __new__ is a static method that takes the class as first argument
		// Prepend class as first argument
		newArgs := append([]Value{c}, args...)

		// Check if it's a ClassMethodValue (need to unwrap it)
		if cm, ok := newMethod.(*ClassMethodValue); ok {
			newMethod = cm.Function
		}

		// Call __new__ with positional args only
		// In Python, __new__ typically only receives positional args,
		// while keyword args are handled by __init__
		if callable, ok := newMethod.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			result, err := callable.Call(newArgs, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in %s.__new__: %w", c.Name, err)
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
		if initMethod, definingClass, ok := c.GetMethodWithClass("__init__"); ok {
			// CRITICAL: Set __class__ to the DEFINING class, not the instance's class
			// This is required for super() to work correctly in inherited methods
			// When Child has no __init__ but inherits Parent.__init__, and Parent.__init__
			// calls super().__init__(), super() must search from Parent's position in MRO,
			// not Child's position
			initCtx := NewContext(ctx)
			initCtx.Define("__class__", definingClass)

			// Prepend instance as first argument (self)
			initArgs := append([]Value{instance}, args...)

			// Try CallWithKeywords first if method supports it
			if kwargsCallable, ok := initMethod.(interface {
				CallWithKeywords([]Value, map[string]Value, *Context) (Value, error)
			}); ok {
				if c.Name == "TestProgram" {
					Log.Debug(SubsystemEval, "About to call __init__", "class", c.Name)
				}
				_, err := kwargsCallable.CallWithKeywords(initArgs, kwargs, initCtx)
				if err != nil {
					if c.Name == "TestProgram" {
						Log.Debug(SubsystemEval, "Class __init__ returned error", "class", c.Name, "error", err)
					}
					// Check if it's SystemExit - propagate it without wrapping
					var sysExit *SystemExit
					if errors.As(err, &sysExit) {
						return nil, err
					}
					return nil, fmt.Errorf("error in %s.__init__: %w", c.Name, err)
				}
			} else if callable, ok := initMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				// __init__ doesn't support kwargs - call with positional args only
				// If kwargs were provided, this is an error
				if len(kwargs) > 0 {
					return nil, &TypeError{Message: fmt.Sprintf("%s.__init__ does not support keyword arguments (method type: %T)", c.Name, initMethod)}
				}
				_, err := callable.Call(initArgs, initCtx)
				if err != nil {
					// Check if it's SystemExit - propagate it without wrapping
					var sysExit *SystemExit
					if errors.As(err, &sysExit) {
						return nil, err
					}
					return nil, fmt.Errorf("error in %s.__init__: %w", c.Name, err)
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

// ListInstance represents an instance of a class that inherits from list
type ListInstance struct {
	Data  *ListValue // The underlying list data
	Class *Class     // The class this list is an instance of
}

// IteratorWrapper wraps a Go Iterator to make it a Value with __next__
type IteratorWrapper struct {
	Iterator Iterator
}

// Type returns the type
func (w *IteratorWrapper) Type() Type {
	return "iterator"
}

// String returns string representation
func (w *IteratorWrapper) String() string {
	return "<iterator>"
}

// GetAttr implements __next__ method
func (w *IteratorWrapper) GetAttr(name string) (Value, bool) {
	if name == "__next__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			val, hasNext := w.Iterator.Next()
			if !hasNext {
				return nil, &StopIteration{}
			}
			return val, nil
		}), true
	}
	return nil, false
}

// BoundListMethod represents a method bound to a ListInstance
type BoundListMethod struct {
	ListInstance *ListInstance
	Method       interface {
		Call([]Value, *Context) (Value, error)
	}
	DefiningClass *Class
}

// Type returns the type
func (b *BoundListMethod) Type() Type {
	return "method"
}

// String returns string representation
func (b *BoundListMethod) String() string {
	return "<bound method>"
}

// Call calls the method with the ListInstance as the first argument
func (b *BoundListMethod) Call(args []Value, ctx *Context) (Value, error) {
	// Prepend the ListInstance as self
	newArgs := append([]Value{b.ListInstance}, args...)
	return b.Method.Call(newArgs, ctx)
}

// NewListInstance creates a new list instance of a class
func NewListInstance(class *Class, data *ListValue) *ListInstance {
	return &ListInstance{
		Data:  data,
		Class: class,
	}
}

// Type returns the type of the list instance
func (l *ListInstance) Type() Type {
	return Type(l.Class.Name)
}

// String returns the string representation
func (l *ListInstance) String() string {
	return l.Data.String()
}

// Iterator returns an iterator for the list
func (l *ListInstance) Iterator() Iterator {
	return l.Data.Iterator()
}

// Len returns the length of the list
func (l *ListInstance) Len() int {
	return l.Data.Len()
}

// Items returns the list items
func (l *ListInstance) Items() []Value {
	return l.Data.Items()
}

// GetAttr gets an attribute from the list's class
func (l *ListInstance) GetAttr(name string) (Value, bool) {
	// Special case for __class__
	if name == "__class__" {
		return l.Class, true
	}

	// Delegate list methods to the underlying list data
	switch name {
	case "__len__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return NumberValue(l.Data.Len()), nil
		}), true
	case "__iter__":
		// Return a function that creates an iterator wrapper
		// This matches Python behavior where __iter__ returns an iterator object
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// Create an iterator wrapper that implements __next__
			iter := l.Data.Iterator()
			wrapper := &IteratorWrapper{Iterator: iter}
			return wrapper, nil
		}), true
	case "__getitem__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: "__getitem__ takes exactly 1 argument"}
			}
			// Delegate to list's __getitem__
			if getItem, ok := l.Data.GetAttr("__getitem__"); ok {
				if callable, ok := getItem.(interface {
					Call([]Value, *Context) (Value, error)
				}); ok {
					return callable.Call(args, ctx)
				}
			}
			return nil, &AttributeError{ObjType: "list", AttrName: "__getitem__"}
		}), true
	}

	// Look up in the class
	if attr, defClass, found := l.Class.GetMethodWithClass(name); found {
		// Check if it's callable - if so, bind it as a method
		if callable, ok := attr.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			// Create bound method with ListInstance as receiver
			// We need to create a wrapper since BoundInstanceMethod expects *Instance
			boundMethod := &BoundListMethod{
				ListInstance:  l,
				Method:        callable,
				DefiningClass: defClass,
			}
			return boundMethod, true
		}
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

	// Handle __traceback__ - return None by default for exceptions
	// This is needed for CPython's traceback module compatibility
	if name == "__traceback__" {
		// Check if it's already set in instance attributes
		if attr, ok := i.Attributes[name]; ok {
			return attr, true
		}
		// Default to None for exception instances
		return None, true
	}

	// Handle __dict__ - return instance attributes as a dict
	if name == "__dict__" {
		dict := NewDict()
		for k, v := range i.Attributes {
			// Use SetValue to properly format the key with ValueToKey
			err := dict.SetValue(StringValue(k), v)
			if err != nil {
				// If SetValue fails, fall back to SetWithKey
				dict.SetWithKey(k, StringValue(k), v)
			}
		}
		return dict, true
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
		// Check if it has __get__ (making it a non-data descriptor)
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

	// Step 5: Call __getattr__ if it exists (Python protocol)
	// This is called when normal attribute lookup fails
	if getattrMethod, _, ok := i.Class.GetMethodWithClass("__getattr__"); ok {
		if callable, ok := getattrMethod.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			// Call instance.__getattr__(name)
			// Create bound method that includes self
			boundMethod := &BoundInstanceMethod{
				Instance: i,
				Method:   callable,
			}
			// Call with the attribute name
			result, err := boundMethod.Call([]Value{StringValue(name)}, nil)
			if err == nil {
				return result, true
			}
			// If __getattr__ raises AttributeError, we should still return nil, false
			// to maintain consistent error reporting
		}
	}

	// Step 6: Check class-level special attributes (like __setattr__)
	// These are provided by Class.GetAttr() for attributes that all classes inherit
	// Skip __getitem__ and __or__ here - these are for type syntax (list[int], int | str)
	// not for instance attribute access. Only use them for instances if explicitly defined.
	if name != "__getitem__" && name != "__or__" {
		if classAttr, ok := i.Class.GetAttr(name); ok {
			// For callable attributes, create a bound method
			if callable, ok := classAttr.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				boundMethod := &BoundInstanceMethod{
					Instance: i,
					Method:   callable,
				}
				return boundMethod, true
			}
			return classAttr, true
		}
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
		return &AttributeError{ObjType: i.Class.Name, AttrName: name}
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

	// Debug output for super() investigation (controlled by M28_DEBUG_SUPER env var)
	debugSuper := os.Getenv("M28_DEBUG_SUPER") != ""
	if debugSuper {
		if methodFunc, ok := bm.Method.(interface{ String() string }); ok {
			defClassName := "nil"
			if bm.DefiningClass != nil {
				defClassName = bm.DefiningClass.Name
			}
			fmt.Fprintf(os.Stderr, "[DEBUG BoundInstanceMethod.Call] Method: %s, DefiningClass: %s, Instance.Class: %v, classForSuper: %v\n",
				methodFunc.String(),
				defClassName,
				bm.Instance.Class.Name,
				classForSuper.Name)
		}
	}

	// CRITICAL: Always set __class__ to the defining class for this method
	// This is required for super() to work correctly in parent class methods
	// When Parent.__init__ calls super().__init__(), super() needs to use
	// Parent's __class__, not Child's, so it finds GrandParent.__init__
	if debugSuper {
		prevClass := "nil"
		if ctx != nil {
			if val, err := ctx.Lookup("__class__"); err == nil {
				if cls, ok := val.(*Class); ok {
					prevClass = cls.Name
				}
			}
		}
		fmt.Fprintf(os.Stderr, "[DEBUG BoundInstanceMethod] Setting __class__ from %s to %s\n", prevClass, classForSuper.Name)
	}
	methodCtx.Define("__class__", classForSuper)
	// Note: Don't define super here - let it come from the builtin function
	// which will look up __class__ from the context

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

	// CRITICAL: Always set __class__ to the defining class for this method
	// This is required for super() to work correctly in parent class methods
	// When Parent.__init__ calls super().__init__(), super() needs to use
	// Parent's __class__, not Child's, so it finds GrandParent.__init__
	debugSuper := os.Getenv("M28_DEBUG_SUPER") != ""
	if debugSuper {
		prevClass := "nil"
		if ctx != nil {
			if val, err := ctx.Lookup("__class__"); err == nil {
				if cls, ok := val.(*Class); ok {
					prevClass = cls.Name
				}
			}
		}
		fmt.Fprintf(os.Stderr, "[DEBUG BoundInstanceMethod] Setting __class__ from %s to %s\n", prevClass, classForSuper.Name)
	}
	methodCtx.Define("__class__", classForSuper)
	// Note: Don't define super here - let it come from the builtin function

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
		// 		fmt.Printf("[DEBUG BoundInstanceMethod] Method type: %T\n", bm.Method)
		// 		fmt.Printf("[DEBUG BoundInstanceMethod] Falling back to regular Call - kwargs will be lost\n")
		// For now, we just error. TODO(M28-8da4): integrate with kwarg_eval
		return nil, &TypeError{Message: "method does not support keyword arguments"}
	}

	// Fall back to regular Call
	return bm.Method.Call(callArgs, methodCtx)
}

// GetAttr implements attribute access for bound instance methods
// Provides default values for standard function attributes
func (bm *BoundInstanceMethod) GetAttr(name string) (Value, bool) {
	// Handle __get__ specially - bound methods should return themselves unchanged
	// when accessed via the descriptor protocol
	if name == "__get__" {
		// Return a function that returns self when called
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// Descriptor protocol: __get__(self, instance, owner)
			// For bound methods, we just return ourselves unchanged
			return bm, nil
		}), true
	}

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

// CallWithKeywords implements CallWithKeywords interface for BoundSuperMethod
func (bsm *BoundSuperMethod) CallWithKeywords(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	// Create a new context with __class__ set
	methodCtx := NewContext(ctx)
	methodCtx.Define("__class__", bsm.Class)

	// Check if the underlying method supports keyword arguments
	if kwargsMethod, ok := bsm.Method.(interface {
		CallWithKeywords([]Value, map[string]Value, *Context) (Value, error)
	}); ok {
		return kwargsMethod.CallWithKeywords(args, kwargs, methodCtx)
	}

	// If method doesn't support kwargs but we have kwargs, error
	if len(kwargs) > 0 {
		return nil, &TypeError{Message: "method does not support keyword arguments"}
	}

	// Fall back to regular Call
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
	debugSuperGetAttr := os.Getenv("M28_DEBUG_SUPER") != ""
	if debugSuperGetAttr {
		instClass := "nil"
		if s.Instance != nil {
			instClass = s.Instance.Class.Name
		}
		fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Looking for %s, s.Class=%s, Instance.Class=%s\n",
			name, s.Class.Name, instClass)
		if name == "__class__" {
			// Print stack trace for __class__ lookups to debug the loop
			fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Stack trace for __class__ lookup:\n")
			// Just print a simple marker - full stack trace would be too verbose
			fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] __class__ requested on Super object\n")
		}
	}

	// Special handling for __class__ attribute - return the class we're wrapping
	if name == "__class__" {
		if debugSuperGetAttr {
			fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Returning s.Class for __class__ attribute\n")
		}
		return s.Class, true
	}

	// Helper function to bind method to instance or class
	bindMethod := func(method Value, defClass *Class) Value {
		if callable, ok := method.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			// Create bound method if we have an instance
			if s.Instance != nil {
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
			// For class methods (like __init_subclass__), bind to TargetClass
			if s.TargetClass != nil {
				// Create a bound classmethod that prepends TargetClass as first argument
				// and sets DefiningClass so super() works correctly inside the method
				if funcVal, ok := method.(Value); ok {
					return &BoundClassMethod{
						Class:         s.TargetClass,
						Function:      funcVal,
						DefiningClass: defClass,
					}
				}
			}
		}
		return method
	}

	// For multiple inheritance, we need to use the instance's MRO
	// and search starting from s.Class (inclusive)
	// Example: ArgumentParser with MRO [ArgumentParser, _AttributeHolder, _ActionsContainer, object]
	// When super() is called in ArgumentParser, s.Class = _AttributeHolder
	// We should search [_AttributeHolder, _ActionsContainer, object]
	if s.Instance != nil {
		// Get the instance's MRO
		mroVal, hasMRO := s.Instance.Class.GetAttr("__mro__")
		if hasMRO {
			if mro, ok := mroVal.(TupleValue); ok {
				// Find s.Class in the MRO
				startIdx := -1
				for i, cls := range mro {
					if c, ok := cls.(*Class); ok && c == s.Class {
						startIdx = i
						break
					}
				}

				if startIdx >= 0 {
					// Search the MRO starting AFTER s.Class (not inclusive)
					// super() in a method should find parent class methods, not the current class
					for i := startIdx + 1; i < len(mro); i++ {
						if cls, ok := mro[i].(*Class); ok {
							if debugSuperGetAttr {
								fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Checking MRO[%d] = %s\n", i, cls.Name)
							}
							// Check Methods map directly (don't use GetMethodWithClass as it traverses parents)
							// We'll find parent methods in the next MRO iteration
							if method, ok := cls.Methods[name]; ok {
								if debugSuperGetAttr {
									fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Found %s in %s.Methods\n", name, cls.Name)
								}
								return bindMethod(method, cls), true
							}
							// Then check Attributes for Python-defined methods
							if attr, ok := cls.Attributes[name]; ok {
								if debugSuperGetAttr {
									fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Found %s in %s.Attributes\n", name, cls.Name)
								}
								return bindMethod(attr, cls), true
							}
							if debugSuperGetAttr {
								fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] %s NOT found in %s\n", name, cls.Name)
							}
						}
					}
					// Not found in MRO
					if debugSuperGetAttr {
						fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] %s not found in MRO\n", name)
					}
					return nil, false
				}
			}
		}
	}

	// Fallback to old behavior when there's no instance or no MRO
	// This handles super() in class methods and other edge cases
	//
	// IMPORTANT: Don't use GetAttr/GetMethodWithClass here as they can trigger
	// descriptor protocol and cause infinite recursion. Only check direct dicts.

	// First check s.Class's parent classes using MRO
	// Note: We skip s.Class itself and start from parents, because super()
	// should find methods in parent classes, not the current class
	if len(s.Class.Parents) > 0 {
		for _, parent := range s.Class.Parents {
			// Check Methods map directly
			if method, ok := parent.Methods[name]; ok {
				if debugSuperGetAttr {
					fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Found %s in %s.Methods\n", name, parent.Name)
				}
				return bindMethod(method, parent), true
			}
			// Check Attributes map directly
			if attr, ok := parent.Attributes[name]; ok {
				if debugSuperGetAttr {
					fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Found %s in %s.Attributes\n", name, parent.Name)
				}
				return bindMethod(attr, parent), true
			}
			// Recursively check parent's parents
			if method, defClass, ok := parent.GetMethodWithClass(name); ok {
				if debugSuperGetAttr {
					fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Found %s via %s.GetMethodWithClass (defined in %s)\n", name, parent.Name, defClass.Name)
				}
				return bindMethod(method, defClass), true
			}
		}
	} else {
		// No parents - this is the root class (like 'type' or 'object')
		// For special methods, check the class's Methods map directly
		if method, ok := s.Class.Methods[name]; ok {
			if debugSuperGetAttr {
				fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Found %s in root class %s.Methods\n", name, s.Class.Name)
			}
			return bindMethod(method, s.Class), true
		}
		// Check Attributes too
		if attr, ok := s.Class.Attributes[name]; ok {
			if debugSuperGetAttr {
				fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] Found %s in root class %s.Attributes\n", name, s.Class.Name)
			}
			return bindMethod(attr, s.Class), true
		}
	}

	if debugSuperGetAttr {
		fmt.Fprintf(os.Stderr, "[DEBUG Super.GetAttr] %s not found in fallback path\n", name)
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
	// Use BFS to check all ancestors with multiple inheritance
	seen := make(map[*Class]bool)
	queue := []*Class{instance.Class}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		if current == nil || seen[current] {
			continue
		}
		seen[current] = true

		if current == class {
			return true
		}

		queue = append(queue, current.Parents...)
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
			// Return the MRO tuple using BFS
			mro := []Value{m.class}
			seen := make(map[*Class]bool)
			seen[m.class] = true
			queue := append([]*Class{}, m.class.Parents...)

			for len(queue) > 0 {
				cls := queue[0]
				queue = queue[1:]

				if cls == nil || seen[cls] {
					continue
				}
				seen[cls] = true
				mro = append(mro, cls)
				queue = append(queue, cls.Parents...)
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

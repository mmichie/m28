package core

import (
	"fmt"
)

// PropertyValue represents a Python property descriptor
type PropertyValue struct {
	Getter  Value
	Setter  Value
	Deleter Value
}

// Type implements Value.Type
func (p *PropertyValue) Type() Type {
	return "property"
}

// String implements Value.String
func (p *PropertyValue) String() string {
	return "<property>"
}

// GetAttr retrieves an attribute
func (p *PropertyValue) GetAttr(name string) (Value, bool) {
	switch name {
	case "fget":
		return p.Getter, true
	case "fset":
		if p.Setter != nil {
			return p.Setter, true
		}
	case "fdel":
		if p.Deleter != nil {
			return p.Deleter, true
		}
	case "setter":
		// Return a function that creates a new property with a setter
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("setter() takes exactly 1 argument")
			}
			return &PropertyValue{
				Getter:  p.Getter,
				Setter:  args[0],
				Deleter: p.Deleter,
			}, nil
		}), true
	case "deleter":
		// Return a function that creates a new property with a deleter
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("deleter() takes exactly 1 argument")
			}
			return &PropertyValue{
				Getter:  p.Getter,
				Setter:  p.Setter,
				Deleter: args[0],
			}, nil
		}), true
	case "__get__":
		// Descriptor protocol __get__ method
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) < 2 {
				return nil, fmt.Errorf("__get__() takes at least 2 arguments")
			}
			instance := args[0]
			// If called on class, return the descriptor itself
			if instance == None {
				return p, nil
			}
			// Call the getter with the instance
			if p.Getter == nil {
				return nil, fmt.Errorf("unreadable attribute")
			}
			callable, ok := p.Getter.(Callable)
			if !ok {
				return nil, fmt.Errorf("property getter is not callable")
			}
			return callable.Call([]Value{instance}, ctx)
		}), true
	case "__set__":
		// Descriptor protocol __set__ method
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("__set__() takes exactly 2 arguments")
			}
			instance := args[0]
			value := args[1]
			// Call the setter with the instance and value
			if p.Setter == nil {
				return nil, fmt.Errorf("can't set attribute")
			}
			callable, ok := p.Setter.(Callable)
			if !ok {
				return nil, fmt.Errorf("property setter is not callable")
			}
			_, err := callable.Call([]Value{instance, value}, ctx)
			return None, err
		}), true
	case "__delete__":
		// Descriptor protocol __delete__ method
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__delete__() takes exactly 1 argument")
			}
			instance := args[0]
			// Call the deleter with the instance
			if p.Deleter == nil {
				return nil, fmt.Errorf("can't delete attribute")
			}
			callable, ok := p.Deleter.(Callable)
			if !ok {
				return nil, fmt.Errorf("property deleter is not callable")
			}
			_, err := callable.Call([]Value{instance}, ctx)
			return None, err
		}), true
	}
	return nil, false
}

// SetAttr sets an attribute
func (p *PropertyValue) SetAttr(name string, value Value) error {
	return fmt.Errorf("cannot set attributes on property")
}

// wrapperDelegateAttr returns the attributes that staticmethod and classmethod
// objects expose: __func__/__wrapped__ point at the wrapped function, and a set
// of identifying dunder attributes are delegated to it, matching CPython.
// It intentionally does not handle __get__/__set__/__delete__ so the descriptor
// protocol continues to be resolved by the concrete type switches in class.go.
func wrapperDelegateAttr(fn Value, name string) (Value, bool) {
	switch name {
	case "__func__", "__wrapped__":
		return fn, true
	case "__module__", "__qualname__", "__name__", "__doc__", "__annotations__":
		if obj, ok := fn.(interface {
			GetAttr(string) (Value, bool)
		}); ok {
			return obj.GetAttr(name)
		}
	}
	return nil, false
}

// StaticMethodValue represents a Python static method
type StaticMethodValue struct {
	Function Value
}

// Type implements Value.Type
func (s *StaticMethodValue) Type() Type {
	return "staticmethod"
}

// String implements Value.String
func (s *StaticMethodValue) String() string {
	return fmt.Sprintf("<staticmethod(%s)>", Repr(s.Function))
}

// GetAttr implements attribute access for static methods.
func (s *StaticMethodValue) GetAttr(name string) (Value, bool) {
	return wrapperDelegateAttr(s.Function, name)
}

// Call implements Callable.Call
func (s *StaticMethodValue) Call(args []Value, ctx *Context) (Value, error) {
	if callable, ok := s.Function.(Callable); ok {
		return callable.Call(args, ctx)
	}
	return nil, fmt.Errorf("staticmethod function is not callable")
}

// ClassMethodValue represents a Python class method
type ClassMethodValue struct {
	Function Value
}

// Type implements Value.Type
func (c *ClassMethodValue) Type() Type {
	return "classmethod"
}

// String implements Value.String
func (c *ClassMethodValue) String() string {
	return fmt.Sprintf("<classmethod(%s)>", Repr(c.Function))
}

// GetAttr implements attribute access for classmethods
func (c *ClassMethodValue) GetAttr(name string) (Value, bool) {
	// Implement descriptor protocol __get__
	if name == "__get__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if len(args) < 2 {
				return nil, fmt.Errorf("__get__() missing required positional arguments: 'instance' and 'owner'")
			}
			// args[0] is instance (can be None)
			// args[1] is owner (the class)
			owner := args[1]

			// Return a bound classmethod
			return &BoundClassMethod{
				Class:    owner,
				Function: c.Function,
			}, nil
		}), true
	}
	// __func__, __wrapped__, and delegated identifying dunder attributes.
	return wrapperDelegateAttr(c.Function, name)
}

// BoundClassMethod represents a class method bound to a specific class
type BoundClassMethod struct {
	Class         Value  // The class passed as first argument (e.g., the subclass)
	Function      Value  // The underlying function
	DefiningClass *Class // The class where the method is defined (for super() support)
}

// Type implements Value.Type
func (b *BoundClassMethod) Type() Type {
	return MethodType
}

// String implements Value.String
func (b *BoundClassMethod) String() string {
	return fmt.Sprintf("<bound classmethod of %s>", b.Class.String())
}

// Call implements Callable.Call
// Automatically injects the class as the first argument
func (b *BoundClassMethod) Call(args []Value, ctx *Context) (Value, error) {
	// Prepend the class to the arguments
	classMethodArgs := make([]Value, len(args)+1)
	classMethodArgs[0] = b.Class
	copy(classMethodArgs[1:], args)

	// Create a child context with __class__ set if we know the defining class
	// This allows super() without arguments to work correctly
	callCtx := ctx
	if b.DefiningClass != nil {
		callCtx = NewContext(ctx)
		callCtx.Define("__class__", b.DefiningClass)
	}

	// Call the underlying function with class as first arg
	if callable, ok := b.Function.(Callable); ok {
		return callable.Call(classMethodArgs, callCtx)
	}
	return nil, fmt.Errorf("classmethod function is not callable")
}

// CallWithKeywords implements keyword argument support for classmethods
// Automatically injects the class as the first argument
func (b *BoundClassMethod) CallWithKeywords(args []Value, kwargs *Kwargs, ctx *Context) (Value, error) {
	// Prepend the class to the positional arguments
	classMethodArgs := make([]Value, len(args)+1)
	classMethodArgs[0] = b.Class
	copy(classMethodArgs[1:], args)

	// Create a child context with __class__ set if we know the defining class
	// This allows super() without arguments to work correctly
	callCtx := ctx
	if b.DefiningClass != nil {
		callCtx = NewContext(ctx)
		callCtx.Define("__class__", b.DefiningClass)
	}

	// Call the underlying function with class as first arg and keywords
	if kwargsFunc, ok := b.Function.(interface {
		CallWithKeywords([]Value, *Kwargs, *Context) (Value, error)
	}); ok {
		return kwargsFunc.CallWithKeywords(classMethodArgs, kwargs, callCtx)
	}

	// Fallback: if underlying function doesn't support keywords, only allow empty kwargs
	if kwargs.Len() > 0 {
		return nil, fmt.Errorf("classmethod function does not support keyword arguments")
	}

	// Call with just positional args
	if callable, ok := b.Function.(Callable); ok {
		return callable.Call(classMethodArgs, callCtx)
	}
	return nil, fmt.Errorf("classmethod function is not callable")
}

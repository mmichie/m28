package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterDecorators registers built-in decorators
func RegisterDecorators(ctx *core.Context) {
	// Register the macro decorator
	ctx.Define("macro", core.NewBuiltinFunction(macroDecorator()))

	// property, staticmethod and classmethod as proper classes
	ctx.Define("property", createPropertyClass())
	ctx.Define("staticmethod", createStaticmethodClass())
	ctx.Define("classmethod", createClassmethodClass())
}

// macroDecorator creates the built-in macro decorator
// Usage: (@macro (def func (args) body))
// Marks a function as a macro that receives unevaluated arguments
func macroDecorator() func(args []core.Value, ctx *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("macro", args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// The argument should be a function
		fn := v.Get(0)

		// Check if it has GetAttr/SetAttr methods (it's an object with attributes)
		if obj, ok := fn.(interface {
			GetAttr(string) (core.Value, bool)
			SetAttr(string, core.Value) error
		}); ok {
			// Set the __macro__ attribute to true
			if err := obj.SetAttr("__macro__", core.True); err != nil {
				return nil, err
			}
		} else {
			return nil, errors.NewTypeError("macro", "function with attributes", fmt.Sprintf("%v", fn.Type()))
		}

		// Return the modified function
		return fn, nil
	}
}

// PropertyType represents the property class
type PropertyType struct {
	*core.Class
}

// GetClass returns the embedded Class (for inheritance support)
func (p *PropertyType) GetClass() *core.Class {
	return p.Class
}

// Call creates a PropertyValue instead of a generic instance
func (p *PropertyType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("property() takes at least 1 argument (%d given)", len(args))
	}

	getter := args[0]
	var setter, deleter core.Value

	// property(fget=None, fset=None, fdel=None, doc=None)
	if len(args) >= 2 {
		setter = args[1]
	}
	if len(args) >= 3 {
		deleter = args[2]
	}
	// doc parameter (args[3]) is ignored for now

	// Create a property object
	prop := &core.PropertyValue{
		Getter:  getter,
		Setter:  setter,
		Deleter: deleter,
	}

	return prop, nil
}

// CallWithKeywords delegates to Call since property() doesn't accept keyword arguments
// This prevents property descriptors from being wrapped in Instance objects
func (p *PropertyType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("property() does not accept keyword arguments")
	}
	return p.Call(args, ctx)
}

// StaticmethodType represents the staticmethod class
type StaticmethodType struct {
	*core.Class
}

// GetClass returns the embedded Class (for inheritance support)
func (s *StaticmethodType) GetClass() *core.Class {
	return s.Class
}

// Call creates a StaticMethodValue instead of a generic instance
func (s *StaticmethodType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("staticmethod() takes exactly 1 argument (%d given)", len(args))
	}

	function := args[0]

	// Wrap the function in a StaticMethod object
	sm := &core.StaticMethodValue{
		Function: function,
	}

	return sm, nil
}

// CallWithKeywords delegates to Call since staticmethod() doesn't accept keyword arguments
// This prevents static methods from being wrapped in Instance objects
func (s *StaticmethodType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("staticmethod() does not accept keyword arguments")
	}
	return s.Call(args, ctx)
}

// ClassmethodType represents the classmethod class
type ClassmethodType struct {
	*core.Class
}

// GetClass returns the embedded Class (for inheritance support)
func (c *ClassmethodType) GetClass() *core.Class {
	return c.Class
}

// Call creates a ClassMethodValue instead of a generic instance
func (c *ClassmethodType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("classmethod() takes exactly 1 argument (%d given)", len(args))
	}

	function := args[0]

	// Wrap the function in a ClassMethod object
	cm := &core.ClassMethodValue{
		Function: function,
	}

	return cm, nil
}

// CallWithKeywords delegates to Call since classmethod() doesn't accept keyword arguments
// This prevents class methods from being wrapped in Instance objects
func (c *ClassmethodType) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(kwargs) > 0 {
		return nil, fmt.Errorf("classmethod() does not accept keyword arguments")
	}
	return c.Call(args, ctx)
}

// createPropertyClass creates the property class
func createPropertyClass() *PropertyType {
	class := core.NewClass("property", nil)
	return &PropertyType{Class: class}
}

// createStaticmethodClass creates the staticmethod class
func createStaticmethodClass() *StaticmethodType {
	class := core.NewClass("staticmethod", nil)
	return &StaticmethodType{Class: class}
}

// createClassmethodClass creates the classmethod class
func createClassmethodClass() *ClassmethodType {
	class := core.NewClass("classmethod", nil)
	return &ClassmethodType{Class: class}
}

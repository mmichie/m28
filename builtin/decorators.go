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

	// property - converts a method into a property (getter)
	ctx.Define("property", core.NewBuiltinFunction(propertyDecorator))

	// staticmethod - makes a method static (no self parameter)
	ctx.Define("staticmethod", core.NewBuiltinFunction(staticmethodDecorator))

	// classmethod - makes a method receive the class as first parameter
	ctx.Define("classmethod", core.NewBuiltinFunction(classmethodDecorator))
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

// propertyDecorator creates a property from a getter function
// Usage: @property
//
//	def name(self): return self._name
func propertyDecorator(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("property() takes exactly 1 argument (%d given)", len(args))
	}

	getter := args[0]

	// Create a property object
	// A property is a special object that intercepts attribute access
	prop := &core.PropertyValue{
		Getter:  getter,
		Setter:  nil,
		Deleter: nil,
	}

	return prop, nil
}

// staticmethodDecorator creates a static method
// Usage: @staticmethod
//
//	def func(): pass
func staticmethodDecorator(args []core.Value, ctx *core.Context) (core.Value, error) {
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

// classmethodDecorator creates a class method
// Usage: @classmethod
//
//	def func(cls): pass
func classmethodDecorator(args []core.Value, ctx *core.Context) (core.Value, error) {
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

package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// RegisterEssentialBuiltins registers additional essential built-in functions
func RegisterEssentialBuiltins(ctx *core.Context) {
	// raise is now a special form in evaluator.go to support more flexible syntax
	// Commented out to avoid conflict with the special form
	// ctx.Define("raise", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
	// 	...
	// }))

	// error() - alias for raise() for backward compatibility
	ctx.Define("error", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return nil, fmt.Errorf("RuntimeError")
		}
		if len(args) > 1 {
			return nil, fmt.Errorf("error() takes at most 1 argument (%d given)", len(args))
		}

		// Get the error message
		var message string
		switch v := args[0].(type) {
		case core.StringValue:
			message = string(v)
		default:
			message = v.String()
		}

		// Return a generic error that will be caught by try/except
		return nil, fmt.Errorf(message)
	}))

	// all() - return True if all elements are true (or if iterable is empty)
	ctx.Define("all", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("all() takes exactly one argument (%d given)", len(args))
		}

		// First check if it implements Iterable interface
		if iterable, ok := args[0].(core.Iterable); ok {
			iter := iterable.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		}

		// Handle specific types that might not pass the interface check
		switch v := args[0].(type) {
		case core.ListValue:
			for _, item := range v {
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		case core.TupleValue:
			for _, item := range v {
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		case *core.SetValue:
			// Use the Iterator method we added
			iter := v.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if !core.IsTruthy(item) {
					return core.False, nil
				}
			}
			return core.True, nil
		default:
			return nil, fmt.Errorf("all() argument must be an iterable, got type %T", args[0])
		}
	}))

	// any() - return True if any element is true (False if iterable is empty)
	ctx.Define("any", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("any() takes exactly one argument (%d given)", len(args))
		}

		// First check if it implements Iterable interface
		if iterable, ok := args[0].(core.Iterable); ok {
			iter := iterable.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		}

		// Handle specific types that might not pass the interface check
		switch v := args[0].(type) {
		case core.ListValue:
			for _, item := range v {
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		case core.TupleValue:
			for _, item := range v {
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		case *core.SetValue:
			// Use the Iterator method we added
			iter := v.Iterator()
			for {
				item, hasNext := iter.Next()
				if !hasNext {
					break
				}
				if core.IsTruthy(item) {
					return core.True, nil
				}
			}
			return core.False, nil
		default:
			return nil, fmt.Errorf("any() argument must be an iterable, got type %T", args[0])
		}
	}))

	// round() - now registered in numeric.go

	// divmod() - now registered in numeric.go

	// hasattr() - check if object has attribute
	ctx.Define("hasattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("hasattr() takes exactly 2 arguments (%d given)", len(args))
		}

		obj := args[0]
		nameVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("hasattr() attribute name must be a string")
		}
		name := string(nameVal)

		// Check for the attribute
		if objWithAttrs, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			_, found := objWithAttrs.GetAttr(name)
			return core.BoolValue(found), nil
		}

		// Check type descriptor methods
		desc := core.GetTypeDescriptorForValue(obj)
		if desc != nil {
			if _, ok := desc.Methods[name]; ok {
				return core.True, nil
			}
			if _, ok := desc.Properties[name]; ok {
				return core.True, nil
			}
		}

		return core.False, nil
	}))

	// getattr() - get attribute from object
	ctx.Define("getattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("getattr() takes 2 or 3 arguments (%d given)", len(args))
		}

		obj := args[0]
		nameVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getattr() attribute name must be a string")
		}
		name := string(nameVal)

		// Try to get the attribute
		if objWithAttrs, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if val, found := objWithAttrs.GetAttr(name); found {
				return val, nil
			}
		}

		// Check type descriptor methods
		desc := core.GetTypeDescriptorForValue(obj)
		if desc != nil {
			if method, ok := desc.Methods[name]; ok {
				// Create bound method
				return &core.BoundMethod{
					Receiver: obj,
					Method:   method,
					TypeDesc: desc,
				}, nil
			}
			if prop, ok := desc.Properties[name]; ok && prop.Getter != nil {
				return prop.Getter(obj)
			}
		}

		// Return default if provided
		if len(args) == 3 {
			return args[2], nil
		}

		return nil, fmt.Errorf("'%s' object has no attribute '%s'", obj.Type(), name)
	}))

	// setattr() - set attribute on object
	ctx.Define("setattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 3 {
			return nil, fmt.Errorf("setattr() takes exactly 3 arguments (%d given)", len(args))
		}

		obj := args[0]
		nameVal, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("setattr() attribute name must be a string")
		}
		name := string(nameVal)
		value := args[2]

		// Try to set the attribute
		if objWithAttrs, ok := obj.(interface {
			SetAttr(string, core.Value) error
		}); ok {
			if err := objWithAttrs.SetAttr(name, value); err != nil {
				return nil, err
			}
			return core.None, nil
		}

		// Check if it's a read-only property
		desc := core.GetTypeDescriptorForValue(obj)
		if desc != nil {
			if prop, ok := desc.Properties[name]; ok && prop.ReadOnly {
				return nil, fmt.Errorf("can't set attribute '%s' of '%s' objects", name, obj.Type())
			}
		}

		return nil, fmt.Errorf("'%s' object has no attribute '%s'", obj.Type(), name)
	}))

	// callable() - check if object is callable
	// callable() - now registered in functional.go
}

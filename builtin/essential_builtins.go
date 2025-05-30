package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
	"math"
)

// RegisterEssentialBuiltins registers additional essential built-in functions
func RegisterEssentialBuiltins(ctx *core.Context) {
	// all() - return True if all elements are true (or if iterable is empty)
	ctx.Define("all", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("all() takes exactly one argument (%d given)", len(args))
		}

		// Handle different iterable types
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
			// SetValue doesn't have a public Items() method, so we'll skip for now
			// TODO: Add iteration support for sets
			return nil, fmt.Errorf("all() does not yet support sets")
		default:
			return nil, fmt.Errorf("all() argument must be an iterable")
		}
	}))

	// any() - return True if any element is true (False if iterable is empty)
	ctx.Define("any", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("any() takes exactly one argument (%d given)", len(args))
		}

		// Handle different iterable types
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
			// SetValue doesn't have a public Items() method, so we'll skip for now
			// TODO: Add iteration support for sets
			return nil, fmt.Errorf("any() does not yet support sets")
		default:
			return nil, fmt.Errorf("any() argument must be an iterable")
		}
	}))

	// round() - round a number to given precision
	ctx.Define("round", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 || len(args) > 2 {
			return nil, fmt.Errorf("round() takes 1 or 2 arguments (%d given)", len(args))
		}

		// Get the number to round
		num, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("round() argument must be a number, not '%s'", args[0].Type())
		}

		// Get precision (default 0)
		precision := 0
		if len(args) == 2 {
			p, ok := args[1].(core.NumberValue)
			if !ok {
				return nil, fmt.Errorf("round() precision must be an integer")
			}
			precision = int(p)
		}

		// Perform rounding
		multiplier := math.Pow(10, float64(precision))
		rounded := math.Round(float64(num)*multiplier) / multiplier

		return core.NumberValue(rounded), nil
	}))

	// divmod() - return quotient and remainder
	ctx.Define("divmod", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("divmod() takes exactly 2 arguments (%d given)", len(args))
		}

		// Get dividend and divisor
		a, ok1 := args[0].(core.NumberValue)
		b, ok2 := args[1].(core.NumberValue)
		if !ok1 || !ok2 {
			return nil, fmt.Errorf("divmod() arguments must be numbers")
		}

		if b == 0 {
			return nil, fmt.Errorf("integer division or modulo by zero")
		}

		// Calculate quotient and remainder
		quotient := math.Floor(float64(a) / float64(b))
		remainder := float64(a) - quotient*float64(b)

		// Return as tuple
		return core.TupleValue{
			core.NumberValue(quotient),
			core.NumberValue(remainder),
		}, nil
	}))

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
	ctx.Define("callable", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("callable() takes exactly 1 argument (%d given)", len(args))
		}

		// Check if implements Callable interface
		_, isCallable := args[0].(core.Callable)
		return core.BoolValue(isCallable), nil
	}))
}

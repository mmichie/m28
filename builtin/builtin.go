// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterAllBuiltins registers all built-in functions in the provided context
func RegisterAllBuiltins(ctx *core.Context) {
	// Register arithmetic functions
	RegisterArithmeticFunctions(ctx)

	// Register comparison functions
	RegisterComparisonFunctions(ctx)

	// Register string functions
	RegisterStringFunctions(ctx)

	// Don't call registerIOBuiltins as RegisterIOFunctions handles it

	// Register list functions
	RegisterListFunctions(ctx)

	// Register dictionary functions
	RegisterDictFunctions(ctx)

	// Register type functions
	registerTypeBuiltins(ctx)

	// Register type checking functions (isinstance, issubclass, type conversions)
	registerTypeCheckingBuiltins(ctx)

	// Register essential built-in functions (all, any, round, etc.)
	RegisterEssentialBuiltins(ctx)

	// Register mathematical functions
	RegisterMathFunctions(ctx)

	// Register random functions
	RegisterRandomFunctions(ctx)

	// Register utility functions
	RegisterUtilityFunctions(ctx)

	// Register async/concurrent functions
	RegisterAsyncBuiltins(ctx)

	// Register I/O functions
	RegisterIOFunctions(ctx)

	// Register assertion functions
	RegisterAssertBuiltins(ctx)

	// Register Python standard library modules
	RegisterJSONModule(ctx)
	RegisterOSModule(ctx)
	RegisterDateTimeModule(ctx)
	RegisterTimeModule(ctx)
	RegisterShutil(ctx)
	RegisterPathlib(ctx)
}

// registerTypeBuiltins registers type-related functions
func registerTypeBuiltins(ctx *core.Context) {
	// Type: (type val) - returns type object
	ctx.Define("type", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("type requires 1 argument")
		}

		// Get type descriptor
		desc := core.GetTypeDescriptorForValue(args[0])
		if desc == nil {
			// Fallback for types without descriptors
			typeName := args[0].Type()
			return core.StringValue(fmt.Sprintf("<type '%s'>", string(typeName))), nil
		}

		// Return a string representation of the type
		return core.StringValue(fmt.Sprintf("<type '%s'>", desc.PythonName)), nil
	}))

	// Python-style type checking functions
	// is_none - check if value is None/nil
	ctx.Define("is_none", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("is_none expects 1 argument, got %d", len(args))
		}
		_, isNil := args[0].(core.NilValue)
		return core.BoolValue(isNil), nil
	}))

	// NOTE: isinstance, issubclass, int, float, str, bool are now in type_checking.go

	// dir - list attributes of an object
	ctx.Define("dir", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("dir expects 1 argument, got %d", len(args))
		}

		desc := core.GetTypeDescriptorForValue(args[0])
		if desc == nil {
			// Return empty list for types without descriptors
			return core.EmptyList, nil
		}

		// Get all attribute names
		names := desc.GetAttributeNames()
		result := make(core.ListValue, len(names))
		for i, name := range names {
			result[i] = core.StringValue(name)
		}

		return result, nil
	}))

	// hasattr - check if object has attribute
	ctx.Define("hasattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("hasattr expects 2 arguments, got %d", len(args))
		}

		attrName, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("hasattr attribute name must be a string")
		}

		// Try to get the attribute
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			_, found := obj.GetAttr(string(attrName))
			return core.BoolValue(found), nil
		}

		return core.False, nil
	}))

	// getattr - get attribute from object
	ctx.Define("getattr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("getattr expects 2 or 3 arguments, got %d", len(args))
		}

		attrName, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getattr attribute name must be a string")
		}

		// Try to get the attribute
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			val, found := obj.GetAttr(string(attrName))
			if found {
				return val, nil
			}
		}

		// Return default if provided
		if len(args) == 3 {
			return args[2], nil
		}

		// Otherwise, attribute error
		desc := core.GetTypeDescriptorForValue(args[0])
		typeName := "object"
		if desc != nil {
			typeName = desc.PythonName
		}
		return nil, fmt.Errorf("'%s' object has no attribute '%s'", typeName, string(attrName))
	}))

	// Collection constructors
	// list - create a new list (overrides the one from RegisterListFunctions)
	ctx.Define("list", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyList, nil
		}
		if len(args) == 1 {
			// Python-style: Convert iterable to list
			switch v := args[0].(type) {
			case core.ListValue:
				// Copy the list
				result := make(core.ListValue, len(v))
				copy(result, v)
				return result, nil
			case core.TupleValue:
				// Convert tuple to list
				result := make(core.ListValue, len(v))
				copy(result, v)
				return result, nil
			case core.StringValue:
				// Convert string to list of characters
				str := string(v)
				result := make(core.ListValue, len(str))
				for i, ch := range str {
					result[i] = core.StringValue(string(ch))
				}
				return result, nil
			default:
				// Check if it implements Iterable interface
				if iterable, ok := v.(core.Iterable); ok {
					result := make(core.ListValue, 0)
					iter := iterable.Iterator()
					for {
						val, hasNext := iter.Next()
						if !hasNext {
							break
						}
						result = append(result, val)
					}
					return result, nil
				}
				// If it's not an iterable, just create a list with this one element
				return core.ListValue{v}, nil
			}
		}
		// Multiple arguments: create a list from all arguments
		return core.ListValue(args), nil
	}))

	// dict - create a new dictionary
	// NOTE: Commented out because RegisterDictFunctions already provides a full implementation
	// ctx.Define("dict", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
	// 	if len(args) == 0 {
	// 		return core.NewDict(), nil
	// 	}
	// 	// TODO: Add support for dict(key=value) syntax later
	// 	return nil, fmt.Errorf("dict() with arguments not yet implemented")
	// }))

	// tuple - create a new tuple
	ctx.Define("tuple", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyTuple, nil
		}
		if len(args) == 1 {
			// Convert iterable to tuple
			switch v := args[0].(type) {
			case core.TupleValue:
				// Copy the tuple
				result := make(core.TupleValue, len(v))
				copy(result, v)
				return result, nil
			case core.ListValue:
				// Convert list to tuple
				result := make(core.TupleValue, len(v))
				copy(result, v)
				return result, nil
			case core.StringValue:
				// Convert string to tuple of characters
				str := string(v)
				result := make(core.TupleValue, len(str))
				for i, ch := range str {
					result[i] = core.StringValue(string(ch))
				}
				return result, nil
			default:
				return nil, fmt.Errorf("tuple() argument must be an iterable, not '%s'", v.Type())
			}
		}
		return nil, fmt.Errorf("tuple() takes at most 1 argument (%d given)", len(args))
	}))

	// slice - create a slice object
	ctx.Define("slice", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		var start, stop, step core.Value

		switch len(args) {
		case 0:
			return nil, fmt.Errorf("slice expected at least 1 argument, got 0")
		case 1:
			// slice(stop)
			start = core.Nil
			stop = args[0]
			step = core.Nil
		case 2:
			// slice(start, stop)
			start = args[0]
			stop = args[1]
			step = core.Nil
		case 3:
			// slice(start, stop, step)
			start = args[0]
			stop = args[1]
			step = args[2]
		default:
			return nil, fmt.Errorf("slice expected at most 3 arguments, got %d", len(args))
		}

		// Validate that arguments are None or integers
		if start != core.Nil {
			if _, ok := start.(core.NumberValue); !ok {
				return nil, fmt.Errorf("slice indices must be integers or None, not %s", start.Type())
			}
		}
		if stop != core.Nil {
			if _, ok := stop.(core.NumberValue); !ok {
				return nil, fmt.Errorf("slice indices must be integers or None, not %s", stop.Type())
			}
		}
		if step != core.Nil {
			if _, ok := step.(core.NumberValue); !ok {
				return nil, fmt.Errorf("slice indices must be integers or None, not %s", step.Type())
			}
			// Check that step is not zero
			if num, ok := step.(core.NumberValue); ok && int64(num) == 0 {
				return nil, fmt.Errorf("slice step cannot be zero")
			}
		}

		return &core.SliceValue{
			Start: start,
			Stop:  stop,
			Step:  step,
		}, nil
	}))

	// len - get length of collection
	ctx.Define("len", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("len() takes exactly one argument (%d given)", len(args))
		}

		// Try to get __len__ method
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if lenMethod, found := obj.GetAttr("__len__"); found {
				// Call the __len__ method
				if callable, ok := lenMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}
					// Ensure it returns a number
					if num, ok := result.(core.NumberValue); ok {
						// Validate it's non-negative
						if float64(num) < 0 {
							return nil, fmt.Errorf("__len__ should return a non-negative integer")
						}
						// Check if it's an integer
						if float64(num) != float64(int(num)) {
							return nil, fmt.Errorf("__len__ should return an integer, not %.2f", float64(num))
						}
						return num, nil
					}
					return nil, fmt.Errorf("__len__ should return an integer")
				}
			}
		}

		// Fallback for types without __len__
		switch v := args[0].(type) {
		case core.StringValue:
			return core.NumberValue(len(string(v))), nil
		case core.ListValue:
			return core.NumberValue(len(v)), nil
		case core.TupleValue:
			return core.NumberValue(len(v)), nil
		case *core.DictValue:
			return core.NumberValue(v.Size()), nil
		case *core.SetValue:
			return core.NumberValue(v.Size()), nil
		default:
			return nil, fmt.Errorf("object of type '%s' has no len()", v.Type())
		}
	}))

	// set - create a new set
	ctx.Define("set", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NewSet(), nil
		}
		if len(args) == 1 {
			// Convert iterable to set
			set := core.NewSet()
			switch v := args[0].(type) {
			case core.ListValue:
				for _, elem := range v {
					set.Add(elem)
				}
				return set, nil
			case core.TupleValue:
				for _, elem := range v {
					set.Add(elem)
				}
				return set, nil
			case core.StringValue:
				// Convert string to set of characters
				str := string(v)
				for _, ch := range str {
					charVal := core.StringValue(string(ch))
					set.Add(charVal)
				}
				return set, nil
			default:
				return nil, fmt.Errorf("set() argument must be an iterable, not '%s'", v.Type())
			}
		}
		return nil, fmt.Errorf("set() takes at most 1 argument (%d given)", len(args))
	}))
}

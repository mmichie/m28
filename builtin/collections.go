package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// convertToSlice converts an iterable value to a slice of values
// This is shared logic for list(), tuple(), and set() constructors
func convertToSlice(arg core.Value) ([]core.Value, error) {
	// Try to convert from different types
	if list, ok := types.AsList(arg); ok {
		// Copy the list
		result := make([]core.Value, list.Len())
		copy(result, list.Items())
		return result, nil
	}

	if tuple, ok := types.AsTuple(arg); ok {
		// Convert tuple to slice
		result := make([]core.Value, len(tuple))
		copy(result, tuple)
		return result, nil
	}

	if str, ok := types.AsString(arg); ok {
		// Convert string to slice of characters
		result := make([]core.Value, len(str))
		for i, ch := range str {
			result[i] = core.StringValue(string(ch))
		}
		return result, nil
	}

	// Check if it implements Iterable interface
	if iterable, ok := types.AsIterable(arg); ok {
		result := make([]core.Value, 0)
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

	// If it's not an iterable, just create a slice with this one element
	return []core.Value{arg}, nil
}

// RegisterCollections registers collection constructor functions
func RegisterCollections(ctx *core.Context) {
	// list - Python list constructor
	ctx.Define("list", core.NewNamedBuiltinFunction("list", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyList, nil
		}
		if len(args) == 1 {
			// Python-style: Convert iterable to list
			items, err := convertToSlice(args[0])
			if err != nil {
				return nil, err
			}
			return core.NewList(items...), nil
		}
		// Multiple arguments: create a list from all arguments
		return core.NewList(args...), nil
	}))

	// tuple - use the TupleTypeClass which has __new__ support
	// Create a callable tuple constructor that exposes __new__
	tupleFunc := core.NewNamedBuiltinFunction("tuple", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyTuple, nil
		}
		if len(args) == 1 {
			// Python-style: Convert iterable to tuple
			items, err := convertToSlice(args[0])
			if err != nil {
				return nil, err
			}
			return core.TupleValue(items), nil
		}
		// Multiple arguments: create a tuple from all arguments
		return core.TupleValue(args), nil
	})

	// Add __new__ classmethod from TupleTypeClass
	if newMethod, ok := TupleTypeClass.GetMethod("__new__"); ok {
		tupleFunc.SetAttr("__new__", newMethod)
	}

	ctx.Define("tuple", tupleFunc)

	// dict - Python dict class
	// Create as a class so dict.fromkeys and other class methods can be accessed
	dictClass := createDictClass()
	SetDictTypeClass(dictClass) // Store globally for type() function
	ctx.Define("dict", dictClass)

	// set - create a new set
	ctx.Define("set", core.NewNamedBuiltinFunction("set", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("set", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.NewSet(), nil
		}

		// Convert iterable to set
		set := core.NewSet()
		arg := args[0]

		if list, ok := types.AsList(arg); ok {
			for _, elem := range list.Items() {
				set.Add(elem)
			}
			return set, nil
		}

		if tuple, ok := types.AsTuple(arg); ok {
			for _, elem := range tuple {
				set.Add(elem)
			}
			return set, nil
		}

		if str, ok := types.AsString(arg); ok {
			// Convert string to set of characters
			for _, ch := range str {
				charVal := core.StringValue(string(ch))
				set.Add(charVal)
			}
			return set, nil
		}

		// Check if it implements Iterable interface
		if iterable, ok := types.AsIterable(arg); ok {
			iter := iterable.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				set.Add(val)
			}
			return set, nil
		}

		return nil, fmt.Errorf("set() argument must be an iterable, not '%s'", arg.Type())
	}))

	// frozenset - create a new immutable frozenset
	ctx.Define("frozenset", core.NewNamedBuiltinFunction("frozenset", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Use the type descriptor's constructor
		desc := core.GetTypeDescriptor(core.FrozenSetType)
		if desc != nil && desc.Constructor != nil {
			return desc.Constructor(args, ctx)
		}
		return nil, fmt.Errorf("frozenset type not registered")
	}))

	// slice - create a slice object
	ctx.Define("slice", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("slice", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		var start, stop, step core.Value

		switch v.Count() {
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
		}

		// Validate that arguments are None or integers
		if start != core.Nil {
			if !types.IsNumber(start) {
				return nil, fmt.Errorf("slice indices must be integers or None, not %s", start.Type())
			}
		}
		if stop != core.Nil {
			if !types.IsNumber(stop) {
				return nil, fmt.Errorf("slice indices must be integers or None, not %s", stop.Type())
			}
		}
		if step != core.Nil {
			if !types.IsNumber(step) {
				return nil, fmt.Errorf("slice indices must be integers or None, not %s", step.Type())
			}
			// Check that step is not zero
			if num, ok := types.AsNumber(step); ok && int64(num) == 0 {
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
	ctx.Define("len", core.NewNamedBuiltinFunction("len", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("len", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		arg := args[0]

		// Try to get __len__ method
		if obj, ok := arg.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if lenMethod, found := obj.GetAttr("__len__"); found {
				// Call the __len__ method
				if callable, ok := types.AsCallable(lenMethod); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}
					// Ensure it returns a number
					if num, ok := types.AsNumber(result); ok {
						// Validate it's non-negative
						if num < 0 {
							return nil, fmt.Errorf("__len__ should return a non-negative integer")
						}
						// Check if it's an integer
						if num != float64(int(num)) {
							return nil, fmt.Errorf("__len__ should return an integer, not %.2f", num)
						}
						return core.NumberValue(num), nil
					}
					return nil, fmt.Errorf("__len__ should return an integer")
				}
			}
		}

		// Fallback for types without __len__
		if str, ok := types.AsString(arg); ok {
			return core.NumberValue(len(str)), nil
		}
		if list, ok := types.AsList(arg); ok {
			return core.NumberValue(list.Len()), nil
		}
		if tuple, ok := types.AsTuple(arg); ok {
			return core.NumberValue(len(tuple)), nil
		}
		if dict, ok := types.AsDict(arg); ok {
			return core.NumberValue(dict.Size()), nil
		}
		if set, ok := types.AsSet(arg); ok {
			return core.NumberValue(set.Size()), nil
		}

		return nil, fmt.Errorf("object of type '%s' has no len()", arg.Type())
	}))

	// bytes - create a new bytes object
	ctx.Define("bytes", core.NewNamedBuiltinFunction("bytes", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Get the type descriptor for bytes to use its constructor
		desc := core.GetTypeDescriptor(core.BytesType)
		if desc == nil {
			return nil, fmt.Errorf("bytes type not registered")
		}
		return desc.Constructor(args, ctx)
	}))

	// bytearray - create a new bytearray object
	ctx.Define("bytearray", core.NewNamedBuiltinFunction("bytearray", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Get the type descriptor for bytearray to use its constructor
		desc := core.GetTypeDescriptor(core.ByteArrayType)
		if desc == nil {
			return nil, fmt.Errorf("bytearray type not registered")
		}
		return desc.Constructor(args, ctx)
	}))

	// memoryview - create a memoryview object (stub implementation)
	// For now, just return the input object wrapped as a memoryview-like type
	ctx.Define("memoryview", core.NewNamedBuiltinFunction("memoryview", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("memoryview() takes exactly 1 argument (%d given)", len(args))
		}
		// For now, just return a simple wrapper
		// Python's memoryview is mainly used for buffer protocol
		// We can implement more functionality later if needed
		return args[0], nil
	}))
}

// ListType represents the list class that can be called and used as a base class
type ListType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (l *ListType) GetClass() *core.Class {
	return l.Class
}

// Call implements the callable interface for list() construction
func (l *ListType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.EmptyList, nil
	}
	if len(args) == 1 {
		// Python-style: Convert iterable to list
		items, err := convertToSlice(args[0])
		if err != nil {
			return nil, err
		}
		return core.NewList(items...), nil
	}
	// Multiple arguments: create a list from all arguments
	return core.NewList(args...), nil
}

// DictType represents the dict class that can be called and has class methods
type DictType struct {
	*core.Class
}

// GetClass returns the embedded Class for use as a parent class
func (d *DictType) GetClass() *core.Class {
	return d.Class
}

// Call implements the callable interface for dict() construction
func (d *DictType) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	dict := core.NewDict()

	// Handle different argument patterns:
	// 1. dict() - empty dict
	// 2. dict(iterable) - from iterable of pairs
	// 3. dict(key1, val1, key2, val2, ...) - traditional key-value pairs

	if len(args) == 0 {
		// Empty dict
		return dict, nil
	}

	// Check if args are key-value pairs (even number of args > 1)
	if len(args) > 1 && len(args)%2 == 0 {
		// Traditional key-value pairs syntax: dict("key1", val1, "key2", val2)
		for i := 0; i < len(args); i += 2 {
			keyVal := args[i]
			value := args[i+1]

			// Use SetValue for proper key conversion
			if err := dict.SetValue(keyVal, value); err != nil {
				return nil, err
			}
		}
		return dict, nil
	}

	// Single argument - should be an iterable
	if len(args) == 1 {
		// Convert from another dict or iterable of pairs
		switch v := args[0].(type) {
		case *core.DictValue:
			// Copy the dictionary
			// TODO: Implement proper dict copying
		case *core.ListValue:
			// Expect list of pairs
			for i, item := range v.Items() {
				pair, ok := item.(*core.ListValue)
				if !ok || pair.Len() != 2 {
					tuple, ok := item.(core.TupleValue)
					if !ok || len(tuple) != 2 {
						return nil, fmt.Errorf("dict update sequence element #%d is not a sequence", i)
					}
					pair = core.NewList(tuple)
				}
				key, ok := pair.Items()[0].(core.StringValue)
				if !ok {
					return nil, fmt.Errorf("dict key must be string")
				}
				dict.Set(string(key), pair.Items()[1])
			}
		default:
			return nil, fmt.Errorf("dict() argument must be a dict or iterable of pairs")
		}
		return dict, nil
	}

	// Odd number of args > 1
	return nil, fmt.Errorf("dict() takes an even number of positional arguments for key-value pairs")
}

// createListClass creates the list class that can be used as a base class
func createListClass() *ListType {
	class := core.NewClass("list", nil)
	// List doesn't have class methods like dict.fromkeys currently
	// But having it as a Class allows it to be inherited from
	return &ListType{Class: class}
}

// createDictClass creates the dict class with class methods like fromkeys
func createDictClass() *DictType {
	class := core.NewClass("dict", nil)

	// Add fromkeys class method
	// dict.fromkeys(iterable, value=None) -> new dict with keys from iterable
	class.SetMethod("fromkeys", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("fromkeys() takes 1 or 2 positional arguments (%d given)", len(args))
		}

		iterable := args[0]
		var value core.Value = core.None
		if len(args) == 2 {
			value = args[1]
		}

		dict := core.NewDict()

		// Convert iterable to list of keys
		switch v := iterable.(type) {
		case *core.ListValue:
			for _, key := range v.Items() {
				keyStr, ok := key.(core.StringValue)
				if !ok {
					keyStr = core.StringValue(key.String())
				}
				dict.Set(string(keyStr), value)
			}
		case core.TupleValue:
			for _, key := range v {
				keyStr, ok := key.(core.StringValue)
				if !ok {
					keyStr = core.StringValue(key.String())
				}
				dict.Set(string(keyStr), value)
			}
		case core.StringValue:
			// String is iterable in Python - each character is a key
			for _, ch := range string(v) {
				dict.Set(string(ch), value)
			}
		default:
			return nil, fmt.Errorf("fromkeys() argument must be an iterable")
		}

		return dict, nil
	}))

	return &DictType{Class: class}
}

package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterCollections registers collection constructor functions
func RegisterCollections(ctx *core.Context) {
	// list - create a new list
	ctx.Define("list", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyList, nil
		}
		if len(args) == 1 {
			// Python-style: Convert iterable to list
			arg := args[0]

			// Try to convert from different types
			if list, ok := types.AsList(arg); ok {
				// Copy the list
				result := make(core.ListValue, len(list))
				copy(result, list)
				return result, nil
			}

			if tuple, ok := types.AsTuple(arg); ok {
				// Convert tuple to list
				result := make(core.ListValue, len(tuple))
				copy(result, tuple)
				return result, nil
			}

			if str, ok := types.AsString(arg); ok {
				// Convert string to list of characters
				result := make(core.ListValue, len(str))
				for i, ch := range str {
					result[i] = core.StringValue(string(ch))
				}
				return result, nil
			}

			// Check if it implements Iterable interface
			if iterable, ok := types.AsIterable(arg); ok {
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
			return core.ListValue{arg}, nil
		}
		// Multiple arguments: create a list from all arguments
		return core.ListValue(args), nil
	}))

	// tuple - create a new tuple
	ctx.Define("tuple", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("tuple", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			return core.EmptyTuple, nil
		}

		// Convert iterable to tuple
		arg := args[0]

		if tuple, ok := types.AsTuple(arg); ok {
			// Copy the tuple
			result := make(core.TupleValue, len(tuple))
			copy(result, tuple)
			return result, nil
		}

		if list, ok := types.AsList(arg); ok {
			// Convert list to tuple
			result := make(core.TupleValue, len(list))
			copy(result, list)
			return result, nil
		}

		if str, ok := types.AsString(arg); ok {
			// Convert string to tuple of characters
			result := make(core.TupleValue, len(str))
			for i, ch := range str {
				result[i] = core.StringValue(string(ch))
			}
			return result, nil
		}

		// Check if it implements Iterable interface
		if iterable, ok := types.AsIterable(arg); ok {
			result := make(core.TupleValue, 0)
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

		return nil, fmt.Errorf("tuple() argument must be an iterable, not '%s'", arg.Type())
	}))

	// dict - create a new dictionary
	ctx.Define("dict", NewKwargsBuiltinFunction("dict", func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
		dict := core.NewDict()

		// Handle positional arguments
		if len(args) > 1 {
			return nil, fmt.Errorf("dict() takes at most 1 positional argument (%d given)", len(args))
		}

		if len(args) == 1 {
			// Convert from another dict or iterable of pairs
			switch v := args[0].(type) {
			case *core.DictValue:
				// Copy the dictionary
				// Note: DictValue doesn't have a public Items() method
				// We'll need to iterate using the dict methods
				// For now, just create an empty dict
				// TODO: Implement proper dict copying
			case core.ListValue:
				// Expect list of pairs
				for i, item := range v {
					pair, ok := item.(core.ListValue)
					if !ok || len(pair) != 2 {
						tuple, ok := item.(core.TupleValue)
						if !ok || len(tuple) != 2 {
							return nil, fmt.Errorf("dict update sequence element #%d is not a sequence", i)
						}
						pair = core.ListValue(tuple)
					}
					key, ok := pair[0].(core.StringValue)
					if !ok {
						return nil, fmt.Errorf("dict key must be string")
					}
					dict.Set(string(key), pair[1])
				}
			default:
				return nil, fmt.Errorf("dict() argument must be a dict or iterable of pairs")
			}
		}

		// Handle keyword arguments
		for k, v := range kwargs {
			dict.Set(k, v)
		}

		return dict, nil
	}))

	// set - create a new set
	ctx.Define("set", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
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
			for _, elem := range list {
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

	// frozenset - create a new frozenset (for now, just alias to set)
	ctx.Define("frozenset", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, frozenset is the same as set
		// In the future, we should make it immutable
		v := validation.NewArgs("frozenset", args)
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
			for _, elem := range list {
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

		return nil, fmt.Errorf("frozenset() argument must be an iterable, not '%s'", arg.Type())
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
			return core.NumberValue(len(list)), nil
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
	ctx.Define("bytes", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Get the type descriptor for bytes to use its constructor
		desc := core.GetTypeDescriptor(core.BytesType)
		if desc == nil {
			return nil, fmt.Errorf("bytes type not registered")
		}
		return desc.Constructor(args, ctx)
	}))

	// bytearray - create a new bytearray object
	ctx.Define("bytearray", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Get the type descriptor for bytearray to use its constructor
		desc := core.GetTypeDescriptor(core.ByteArrayType)
		if desc == nil {
			return nil, fmt.Errorf("bytearray type not registered")
		}
		return desc.Constructor(args, ctx)
	}))
}

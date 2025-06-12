package builtin

import (
	"fmt"

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

	// frozenset - create a new frozenset (for now, just alias to set)
	ctx.Define("frozenset", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, frozenset is the same as set
		// In the future, we should make it immutable
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
				return nil, fmt.Errorf("frozenset() argument must be an iterable, not '%s'", v.Type())
			}
		}
		return nil, fmt.Errorf("frozenset() takes at most 1 argument (%d given)", len(args))
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
}

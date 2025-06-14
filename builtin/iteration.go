package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterIteration registers iteration-related functions
func RegisterIteration(ctx *core.Context) {
	// iter - get iterator from iterable
	ctx.Define("iter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("iter() takes 1 or 2 arguments (%d given)", len(args))
		}

		// Single argument: get iterator
		if len(args) == 1 {
			if iterable, ok := args[0].(core.Iterable); ok {
				// For now, convert to list since Iterator doesn't implement Value
				// TODO: Create a proper IteratorValue type
				iter := iterable.Iterator()
				var result core.ListValue
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					result = append(result, val)
				}
				return result, nil
			}
			return nil, fmt.Errorf("'%s' object is not iterable", args[0].Type())
		}

		// Two arguments: callable and sentinel
		// Not implemented yet
		return nil, fmt.Errorf("iter() with sentinel not yet implemented")
	}))

	// next - get next item from iterator
	ctx.Define("next", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("next() takes 1 or 2 arguments (%d given)", len(args))
		}

		// Get the iterator
		iterator := args[0]

		// Check if it has a __next__ method
		if obj, ok := iterator.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if nextMethod, found := obj.GetAttr("__next__"); found {
				// Call the __next__ method
				if callable, ok := nextMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						// If StopIteration and default provided, return default
						if len(args) == 2 {
							return args[1], nil
						}
						return nil, err
					}
					return result, nil
				}
			}
		}

		// If no __next__ method and default provided, return default
		if len(args) == 2 {
			return args[1], nil
		}

		return nil, fmt.Errorf("'%s' object is not an iterator", iterator.Type())
	}))

	// enumerate - return enumerate object
	ctx.Define("enumerate", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("enumerate expected 1 or 2 arguments, got %d", len(args))
		}

		start := 0
		if len(args) == 2 {
			if num, ok := args[1].(core.NumberValue); ok {
				start = int(num)
			} else {
				return nil, fmt.Errorf("enumerate() start argument must be an integer")
			}
		}

		// Convert to list for now (should return iterator in future)
		var items []core.Value
		switch v := args[0].(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		case core.StringValue:
			// Convert string to list of characters
			str := string(v)
			items = make([]core.Value, len(str))
			for i, ch := range str {
				items[i] = core.StringValue(string(ch))
			}
		default:
			return nil, fmt.Errorf("enumerate() argument must be a sequence")
		}

		result := make(core.ListValue, len(items))
		for i, item := range items {
			result[i] = core.TupleValue{core.NumberValue(i + start), item}
		}

		return result, nil
	}))

	// range - return range object
	ctx.Define("range", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		var start, stop, step int

		switch len(args) {
		case 1:
			// range(stop)
			if num, ok := args[0].(core.NumberValue); ok {
				start = 0
				stop = int(num)
				step = 1
			} else {
				return nil, fmt.Errorf("range() argument must be an integer")
			}
		case 2:
			// range(start, stop)
			if num1, ok := args[0].(core.NumberValue); ok {
				if num2, ok := args[1].(core.NumberValue); ok {
					start = int(num1)
					stop = int(num2)
					step = 1
				} else {
					return nil, fmt.Errorf("range() arguments must be integers")
				}
			} else {
				return nil, fmt.Errorf("range() arguments must be integers")
			}
		case 3:
			// range(start, stop, step)
			if num1, ok := args[0].(core.NumberValue); ok {
				if num2, ok := args[1].(core.NumberValue); ok {
					if num3, ok := args[2].(core.NumberValue); ok {
						start = int(num1)
						stop = int(num2)
						step = int(num3)
						if step == 0 {
							return nil, fmt.Errorf("range() step argument must not be zero")
						}
					} else {
						return nil, fmt.Errorf("range() arguments must be integers")
					}
				} else {
					return nil, fmt.Errorf("range() arguments must be integers")
				}
			} else {
				return nil, fmt.Errorf("range() arguments must be integers")
			}
		default:
			return nil, fmt.Errorf("range expected 1 to 3 arguments, got %d", len(args))
		}

		// Generate the range as a list for now (should be a range object)
		var result core.ListValue
		if step > 0 {
			for i := start; i < stop; i += step {
				result = append(result, core.NumberValue(i))
			}
		} else {
			for i := start; i > stop; i += step {
				result = append(result, core.NumberValue(i))
			}
		}

		return result, nil
	}))

	// reversed - return reversed iterator
	ctx.Define("reversed", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("reversed() takes exactly one argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case core.ListValue:
			// Create reversed copy
			result := make(core.ListValue, len(v))
			for i, j := 0, len(v)-1; i < len(v); i, j = i+1, j-1 {
				result[i] = v[j]
			}
			return result, nil
		case core.TupleValue:
			// Create reversed tuple
			result := make(core.TupleValue, len(v))
			for i, j := 0, len(v)-1; i < len(v); i, j = i+1, j-1 {
				result[i] = v[j]
			}
			return result, nil
		case core.StringValue:
			// Reverse string
			runes := []rune(string(v))
			for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
				runes[i], runes[j] = runes[j], runes[i]
			}
			return core.StringValue(string(runes)), nil
		default:
			return nil, fmt.Errorf("'%s' object is not reversible", v.Type())
		}
	}))

	// zip - zip iterables together
	ctx.Define("zip", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyList, nil
		}

		// Convert all arguments to lists
		lists := make([][]core.Value, len(args))
		minLen := -1

		for i, arg := range args {
			switch v := arg.(type) {
			case core.ListValue:
				lists[i] = v
			case core.TupleValue:
				lists[i] = v
			case core.StringValue:
				// Convert string to list of characters
				str := string(v)
				lists[i] = make([]core.Value, len(str))
				for j, ch := range str {
					lists[i][j] = core.StringValue(string(ch))
				}
			default:
				return nil, fmt.Errorf("zip() argument #%d is not iterable", i+1)
			}

			if minLen == -1 || len(lists[i]) < minLen {
				minLen = len(lists[i])
			}
		}

		// Create result
		result := make(core.ListValue, minLen)
		for i := 0; i < minLen; i++ {
			tuple := make(core.TupleValue, len(lists))
			for j := range lists {
				tuple[j] = lists[j][i]
			}
			result[i] = tuple
		}

		return result, nil
	}))
}

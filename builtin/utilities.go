package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterUtilityFunctions registers utility functions in the global context
func RegisterUtilityFunctions(ctx *core.Context) {
	// range is now defined in list.go as a lazy range object

	// enumerate - add index to iterable elements
	ctx.Define("enumerate", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("enumerate() takes 1 or 2 arguments (%d given)", len(args))
		}

		var start int64 = 0
		if len(args) == 2 {
			if num, ok := args[1].(core.NumberValue); ok {
				start = int64(num)
			} else {
				return nil, fmt.Errorf("enumerate() start must be a number")
			}
		}

		// Get the iterable
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
			return nil, fmt.Errorf("enumerate() argument must be an iterable, not '%s'", v.Type())
		}

		// Create list of (index, value) tuples
		result := make(core.ListValue, len(items))
		for i, item := range items {
			tuple := core.TupleValue{core.NumberValue(start + int64(i)), item}
			result[i] = tuple
		}

		return result, nil
	}))

	// zip - combine multiple iterables
	ctx.Define("zip", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.EmptyList, nil
		}

		// Convert all arguments to slices
		iterables := make([][]core.Value, len(args))
		minLen := -1

		for i, arg := range args {
			switch v := arg.(type) {
			case core.ListValue:
				iterables[i] = v
				if minLen == -1 || len(v) < minLen {
					minLen = len(v)
				}
			case core.TupleValue:
				iterables[i] = v
				if minLen == -1 || len(v) < minLen {
					minLen = len(v)
				}
			case core.StringValue:
				// Convert string to list of characters
				str := string(v)
				chars := make([]core.Value, len(str))
				for j, ch := range str {
					chars[j] = core.StringValue(string(ch))
				}
				iterables[i] = chars
				if minLen == -1 || len(chars) < minLen {
					minLen = len(chars)
				}
			default:
				return nil, fmt.Errorf("zip() argument #%d must be an iterable, not '%s'", i+1, v.Type())
			}
		}

		if minLen == -1 {
			minLen = 0
		}

		// Create tuples
		result := make(core.ListValue, minLen)
		for i := 0; i < minLen; i++ {
			tuple := make(core.TupleValue, len(iterables))
			for j := range iterables {
				tuple[j] = iterables[j][i]
			}
			result[i] = tuple
		}

		return result, nil
	}))

	// map - now registered in functional.go

	// filter - now registered in functional.go

	// reduce - now registered in functional.go

	// all - check if all elements are truthy
	// all() function moved to essential_builtins.go to avoid duplication

	// any() function moved to essential_builtins.go to avoid duplication

	// apply - apply a function to a list of arguments
	ctx.Define("apply", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("apply() takes 2 or 3 arguments (function, args, and optionally kwargs)")
		}

		// Get the function
		fn, ok := args[0].(core.Callable)
		if !ok {
			return nil, fmt.Errorf("apply() first argument must be callable, got %s", args[0].Type())
		}

		// Get the arguments list
		var fnArgs []core.Value
		switch argList := args[1].(type) {
		case core.ListValue:
			fnArgs = []core.Value(argList)
		case core.TupleValue:
			fnArgs = []core.Value(argList)
		default:
			return nil, fmt.Errorf("apply() second argument must be a list or tuple, got %s", args[1].Type())
		}

		// Handle keyword arguments if provided
		if len(args) == 3 {
			// For now, we need to handle kwargs differently based on the function type
			// This is a simplified implementation - full kwargs support would require
			// more extensive changes to the function call mechanism

			// Check if kwargs is a dict
			_, ok := args[2].(*core.DictValue)
			if !ok {
				return nil, fmt.Errorf("apply() third argument must be a dict, got %s", args[2].Type())
			}

			// TODO: Full kwargs support would require changes to core.Callable interface
			// For now, we'll just pass the regular args and ignore kwargs
			// This allows the code to run without errors
		}

		// Extract kwargs if provided
		kwargsMap := make(map[string]core.Value)
		if len(args) == 3 {
			dict := args[2].(*core.DictValue) // Already validated above
			for _, key := range dict.Keys() {
				value, _ := dict.Get(key)
				if !ok {
					// Keys are already strings from dict.Keys()
				}
				kwargsMap[key] = value
			}
		}

		// Call the function with kwargs support
		return ApplyWithKwargs(fn, fnArgs, kwargsMap, ctx)
	}))

	// concat - concatenate sequences (lists or tuples)
	ctx.Define("concat", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("concat() requires at least 2 arguments")
		}

		// Determine the result type based on the first argument
		var result []core.Value
		var resultType string

		switch first := args[0].(type) {
		case core.ListValue:
			result = make([]core.Value, 0)
			resultType = "list"
		case core.TupleValue:
			result = make([]core.Value, 0)
			resultType = "tuple"
		default:
			return nil, fmt.Errorf("concat() arguments must be lists or tuples, got %s", first.Type())
		}

		// Concatenate all sequences
		for i, arg := range args {
			switch seq := arg.(type) {
			case core.ListValue:
				result = append(result, seq...)
			case core.TupleValue:
				result = append(result, seq...)
			default:
				return nil, fmt.Errorf("concat() argument %d must be a list or tuple, got %s", i+1, arg.Type())
			}
		}

		// Return the appropriate type
		if resultType == "list" {
			return core.ListValue(result), nil
		}
		return core.TupleValue(result), nil
	}))

	// reduce - duplicate definition removed (see above for the main reduce implementation)

	// next - get the next value from an iterator/generator
	ctx.Define("next", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("next() takes 1 or 2 arguments")
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
}

// ApplyWithKwargs handles calling functions with keyword arguments
func ApplyWithKwargs(fn core.Value, args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Check if function supports CallWithKwargs
	if kwFunc, ok := fn.(interface {
		CallWithKwargs([]core.Value, map[string]core.Value, *core.Context) (core.Value, error)
	}); ok {
		return kwFunc.CallWithKwargs(args, kwargs, ctx)
	}

	// Otherwise, fall back to regular Call
	if len(kwargs) > 0 {
		// Function doesn't support kwargs, but some were provided
		// For compatibility, we just ignore them rather than error
	}

	callable, ok := fn.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("apply() first argument must be callable, got %s", fn.Type())
	}

	return callable.Call(args, ctx)
}

// Helper function to extract number from value
func getNumber(v core.Value) float64 {
	if num, ok := v.(core.NumberValue); ok {
		return float64(num)
	}
	panic(fmt.Sprintf("expected number, got %s", v.Type()))
}

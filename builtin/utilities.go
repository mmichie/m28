package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterUtilityFunctions registers utility functions in the global context
func RegisterUtilityFunctions(ctx *core.Context) {
	// range - generate a sequence of numbers
	ctx.Define("range", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 3 {
			return nil, fmt.Errorf("range() takes 1 to 3 arguments (%d given)", len(args))
		}

		var start, stop, step float64

		switch len(args) {
		case 1:
			// range(stop)
			stop = getNumber(args[0])
			start = 0
			step = 1
		case 2:
			// range(start, stop)
			start = getNumber(args[0])
			stop = getNumber(args[1])
			step = 1
		case 3:
			// range(start, stop, step)
			start = getNumber(args[0])
			stop = getNumber(args[1])
			step = getNumber(args[2])
			if step == 0 {
				return nil, fmt.Errorf("range() step argument must not be zero")
			}
		}

		// Generate the range
		result := core.ListValue{}
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

	// map - apply function to all elements
	ctx.Define("map", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("map() takes exactly 2 arguments (%d given)", len(args))
		}

		// Get the function
		fn, ok := args[0].(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		})
		if !ok {
			return nil, fmt.Errorf("map() first argument must be callable")
		}

		// Get the iterable
		var items []core.Value
		switch v := args[1].(type) {
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
			return nil, fmt.Errorf("map() second argument must be an iterable, not '%s'", v.Type())
		}

		// Apply function to each element
		result := make(core.ListValue, len(items))
		for i, item := range items {
			val, err := fn.Call([]core.Value{item}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in map at index %d: %v", i, err)
			}
			result[i] = val
		}

		return result, nil
	}))

	// filter - filter elements based on predicate
	ctx.Define("filter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("filter() takes exactly 2 arguments (%d given)", len(args))
		}

		// Get the function (or None for identity)
		var fn interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		}
		if _, isNil := args[0].(core.NilValue); !isNil {
			var ok bool
			fn, ok = args[0].(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			})
			if !ok {
				return nil, fmt.Errorf("filter() first argument must be callable or None")
			}
		}

		// Get the iterable
		var items []core.Value
		switch v := args[1].(type) {
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
			return nil, fmt.Errorf("filter() second argument must be an iterable, not '%s'", v.Type())
		}

		// Filter elements
		result := core.ListValue{}
		for _, item := range items {
			var keep bool
			if fn == nil {
				// Use truthiness test
				keep = core.IsTruthy(item)
			} else {
				// Call the predicate
				val, err := fn.Call([]core.Value{item}, ctx)
				if err != nil {
					return nil, fmt.Errorf("error in filter predicate: %v", err)
				}
				keep = core.IsTruthy(val)
			}

			if keep {
				result = append(result, item)
			}
		}

		return result, nil
	}))

	// reduce - reduce iterable to single value
	ctx.Define("reduce", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 || len(args) > 3 {
			return nil, fmt.Errorf("reduce() takes 2 or 3 arguments (%d given)", len(args))
		}

		// Get the function
		fn, ok := args[0].(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		})
		if !ok {
			return nil, fmt.Errorf("reduce() first argument must be callable")
		}

		// Get the iterable
		var items []core.Value
		switch v := args[1].(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		default:
			return nil, fmt.Errorf("reduce() second argument must be an iterable, not '%s'", v.Type())
		}

		// Get initial value
		var accumulator core.Value
		startIdx := 0

		if len(args) == 3 {
			accumulator = args[2]
		} else {
			if len(items) == 0 {
				return nil, fmt.Errorf("reduce() of empty sequence with no initial value")
			}
			accumulator = items[0]
			startIdx = 1
		}

		// Reduce
		for i := startIdx; i < len(items); i++ {
			val, err := fn.Call([]core.Value{accumulator, items[i]}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in reduce at index %d: %v", i, err)
			}
			accumulator = val
		}

		return accumulator, nil
	}))

	// all - check if all elements are truthy
	ctx.Define("all", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("all() takes exactly one argument (%d given)", len(args))
		}

		// Get the iterable
		var items []core.Value
		switch v := args[0].(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		default:
			return nil, fmt.Errorf("all() argument must be an iterable, not '%s'", v.Type())
		}

		// Check all elements
		for _, item := range items {
			if !core.IsTruthy(item) {
				return core.False, nil
			}
		}

		return core.True, nil
	}))

	// any - check if any element is truthy
	ctx.Define("any", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("any() takes exactly one argument (%d given)", len(args))
		}

		// Get the iterable
		var items []core.Value
		switch v := args[0].(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		default:
			return nil, fmt.Errorf("any() argument must be an iterable, not '%s'", v.Type())
		}

		// Check any element
		for _, item := range items {
			if core.IsTruthy(item) {
				return core.True, nil
			}
		}

		return core.False, nil
	}))
}

// Helper function to extract number from value
func getNumber(v core.Value) float64 {
	if num, ok := v.(core.NumberValue); ok {
		return float64(num)
	}
	panic(fmt.Sprintf("expected number, got %s", v.Type()))
}
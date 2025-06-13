package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterFunctional registers functional programming functions
func RegisterFunctional(ctx *core.Context) {
	// map - apply function to every item
	ctx.Define("map", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("map() requires at least 2 arguments")
		}

		fn := args[0]
		callable, ok := fn.(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		})
		if !ok {
			return nil, fmt.Errorf("first argument to map must be callable")
		}

		// For now, only support single iterable
		if len(args) != 2 {
			return nil, fmt.Errorf("map() with multiple iterables not yet supported")
		}

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
			return nil, fmt.Errorf("map() argument 2 must be iterable")
		}

		// Apply function to each item
		result := make(core.ListValue, len(items))
		for i, item := range items {
			val, err := callable.Call([]core.Value{item}, ctx)
			if err != nil {
				return nil, err
			}
			result[i] = val
		}

		return result, nil
	}))

	// filter - filter items based on predicate
	ctx.Define("filter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("filter() takes exactly 2 arguments (%d given)", len(args))
		}

		// Get the predicate function (can be None)
		var predicate interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		}
		if args[0] != core.Nil {
			var ok bool
			predicate, ok = args[0].(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			})
			if !ok {
				return nil, fmt.Errorf("filter expected first argument to be callable or None")
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
			return nil, fmt.Errorf("filter() argument 2 must be iterable")
		}

		// Filter items
		result := make(core.ListValue, 0)
		for _, item := range items {
			keep := false
			if predicate == nil {
				// If predicate is None, use truthiness
				keep = core.IsTruthy(item)
			} else {
				// Call predicate
				val, err := predicate.Call([]core.Value{item}, ctx)
				if err != nil {
					return nil, err
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
			return nil, fmt.Errorf("reduce() argument 2 must be iterable")
		}

		// Get initial value if provided
		var result core.Value
		startIdx := 0
		if len(args) == 3 {
			result = args[2]
		} else {
			if len(items) == 0 {
				return nil, fmt.Errorf("reduce() of empty sequence with no initial value")
			}
			result = items[0]
			startIdx = 1
		}

		// Reduce
		for i := startIdx; i < len(items); i++ {
			var err error
			result, err = fn.Call([]core.Value{result, items[i]}, ctx)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	}))

	// all - return True if all elements are true
	// all() function moved to essential_builtins.go to avoid duplication

	// any() function moved to essential_builtins.go to avoid duplication

	// callable - check if object is callable
	ctx.Define("callable", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("callable() takes exactly one argument (%d given)", len(args))
		}

		// Check if the object implements the Call interface
		_, ok := args[0].(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		})

		return core.BoolValue(ok), nil
	}))
}

package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterFunctional registers functional programming functions
func RegisterFunctional(ctx *core.Context) {
	// map - apply function to every item
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
	ctx.Define("reduce", core.NewBuiltinFunction(ReduceFunc))

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

// ReduceFunc reduces a list to a single value by applying a function
func ReduceFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("reduce requires 2 or 3 arguments")
	}

	// Get the reducing function
	fn, ok := args[0].(core.Callable)
	if !ok {
		return nil, fmt.Errorf("first argument must be callable")
	}

	// Handle both Python-style and current argument order
	var coll interface{}
	var initialValue core.Value
	var hasInitial bool

	if len(args) == 2 {
		// Two arguments: (reduce function collection)
		switch c := args[1].(type) {
		case core.ListValue:
			coll = c
		case core.TupleValue:
			coll = c
		default:
			return nil, fmt.Errorf("reduce expects a list or tuple, got %s", args[1].Type())
		}
		hasInitial = false
	} else {
		// Three arguments - could be either order
		// Try Python order first: (reduce function initial collection)
		switch c := args[2].(type) {
		case core.ListValue:
			coll = c
			initialValue = args[1]
			hasInitial = true
		case core.TupleValue:
			coll = c
			initialValue = args[1]
			hasInitial = true
		default:
			// Try original order: (reduce function collection initial)
			switch c := args[1].(type) {
			case core.ListValue:
				coll = c
				initialValue = args[2]
				hasInitial = true
			case core.TupleValue:
				coll = c
				initialValue = args[2]
				hasInitial = true
			default:
				return nil, fmt.Errorf("reduce expects a list or tuple as second or third argument")
			}
		}
	}

	// Get initial value and first element index
	var result core.Value
	var startIdx int

	if hasInitial {
		result = initialValue
		startIdx = 0
	} else {
		// No initial value, use first element
		switch c := coll.(type) {
		case core.ListValue:
			if len(c) == 0 {
				return nil, fmt.Errorf("cannot reduce empty list without initial value")
			}
			result = c[0]
		case core.TupleValue:
			if len(c) == 0 {
				return nil, fmt.Errorf("cannot reduce empty tuple without initial value")
			}
			result = c[0]
		}
		startIdx = 1
	}

	// Reduce the collection
	var err error
	switch c := coll.(type) {
	case core.ListValue:
		for i := startIdx; i < len(c); i++ {
			result, err = fn.Call([]core.Value{result, c[i]}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in reduce function: %v", err)
			}
		}
	case core.TupleValue:
		for i := startIdx; i < len(c); i++ {
			result, err = fn.Call([]core.Value{result, c[i]}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in reduce function: %v", err)
			}
		}
	}

	return result, nil
}

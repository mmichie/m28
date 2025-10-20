package modules

import (
	"fmt"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitItertoolsModule creates and returns the itertools module
func InitItertoolsModule() *core.DictValue {
	itertoolsModule := core.NewDict()

	// chain - chain multiple iterables together
	itertoolsModule.Set("chain", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		result := make([]core.Value, 0)
		for _, arg := range args {
			iterable, err := types.RequireIterable(arg, "chain() argument")
			if err != nil {
				return nil, err
			}
			iter := iterable.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				result = append(result, val)
			}
		}
		return core.NewList(result...), nil
	}))

	// cycle - repeat elements from iterable n times (limited version)
	itertoolsModule.Set("cycle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("cycle", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(0), "cycle() argument")
		if err != nil {
			return nil, err
		}

		// Get the number of cycles (default 3 for safety)
		cycles, _ := v.GetNumberOrDefault(1, 3)
		if cycles < 0 {
			return nil, errors.NewRuntimeError("cycle", "cycles must be non-negative")
		}

		// Collect items
		items := make([]core.Value, 0)
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			items = append(items, val)
		}

		// Repeat items
		result := make([]core.Value, 0)
		for i := 0; i < int(cycles); i++ {
			result = append(result, items...)
		}
		return core.NewList(result...), nil
	}))

	// islice - slice an iterable
	itertoolsModule.Set("islice", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("islice", args)
		if err := v.Range(2, 4); err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(0), "islice() first argument")
		if err != nil {
			return nil, err
		}

		var start, stop, step int

		if v.Count() == 2 {
			// islice(iterable, stop)
			start = 0
			stopVal, err := v.GetNumber(1)
			if err != nil {
				return nil, err
			}
			stop = int(stopVal)
			step = 1
		} else if v.Count() == 3 {
			// islice(iterable, start, stop)
			startVal, err := v.GetNumber(1)
			if err != nil {
				return nil, err
			}
			start = int(startVal)

			stopVal, err := v.GetNumber(2)
			if err != nil {
				return nil, err
			}
			stop = int(stopVal)
			step = 1
		} else {
			// islice(iterable, start, stop, step)
			startVal, err := v.GetNumber(1)
			if err != nil {
				return nil, err
			}
			start = int(startVal)

			stopVal, err := v.GetNumber(2)
			if err != nil {
				return nil, err
			}
			stop = int(stopVal)

			stepVal, err := v.GetNumber(3)
			if err != nil {
				return nil, err
			}
			step = int(stepVal)

			if step <= 0 {
				return nil, errors.NewRuntimeError("islice", "step must be positive")
			}
		}

		result := make([]core.Value, 0)
		iter := iterable.Iterator()
		index := 0

		for {
			val, hasNext := iter.Next()
			if !hasNext || index >= stop {
				break
			}

			if index >= start && (index-start)%step == 0 {
				result = append(result, val)
			}
			index++
		}

		return core.NewList(result...), nil
	}))

	// count - count from start with step (returns list of n values)
	itertoolsModule.Set("count", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("count", args)
		if err := v.Range(0, 3); err != nil {
			return nil, err
		}

		start, _ := v.GetNumberOrDefault(0, 0)
		step, _ := v.GetNumberOrDefault(1, 1)
		n, _ := v.GetNumberOrDefault(2, 10) // Default to 10 values for safety

		result := make([]core.Value, 0, int(n))
		current := start
		for i := 0; i < int(n); i++ {
			result = append(result, core.NumberValue(current))
			current += step
		}
		return core.NewList(result...), nil
	}))

	// repeat - repeat an object n times
	itertoolsModule.Set("repeat", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("repeat", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		value := v.Get(0)
		times, _ := v.GetNumberOrDefault(1, 10) // Default to 10 for safety

		result := make([]core.Value, 0, int(times))
		for i := 0; i < int(times); i++ {
			result = append(result, value)
		}
		return core.NewList(result...), nil
	}))

	// takewhile - yield elements while predicate is true
	itertoolsModule.Set("takewhile", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("takewhile", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		predicate, err := types.RequireCallable(v.Get(0), "takewhile() first argument")
		if err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(1), "takewhile() second argument")
		if err != nil {
			return nil, err
		}

		result := make([]core.Value, 0)
		iter := iterable.Iterator()

		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}

			// Call predicate
			predicateResult, err := predicate.Call([]core.Value{val}, ctx)
			if err != nil {
				return nil, err
			}

			if !core.IsTruthy(predicateResult) {
				break
			}

			result = append(result, val)
		}

		return core.NewList(result...), nil
	}))

	// dropwhile - drop elements while predicate is true, then yield all
	itertoolsModule.Set("dropwhile", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("dropwhile", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		predicate, err := types.RequireCallable(v.Get(0), "dropwhile() first argument")
		if err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(1), "dropwhile() second argument")
		if err != nil {
			return nil, err
		}

		result := make([]core.Value, 0)
		iter := iterable.Iterator()
		dropping := true

		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}

			if dropping {
				// Call predicate
				predicateResult, err := predicate.Call([]core.Value{val}, ctx)
				if err != nil {
					return nil, err
				}

				if !core.IsTruthy(predicateResult) {
					dropping = false
					result = append(result, val)
				}
			} else {
				result = append(result, val)
			}
		}

		return core.NewList(result...), nil
	}))

	// compress - filter iterable by selectors
	itertoolsModule.Set("compress", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("compress", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		data, err := types.RequireIterable(v.Get(0), "compress() first argument")
		if err != nil {
			return nil, err
		}

		selectors, err := types.RequireIterable(v.Get(1), "compress() second argument")
		if err != nil {
			return nil, err
		}

		result := make([]core.Value, 0)
		dataIter := data.Iterator()
		selectorsIter := selectors.Iterator()

		for {
			val, hasData := dataIter.Next()
			sel, hasSelector := selectorsIter.Next()

			if !hasData || !hasSelector {
				break
			}

			if core.IsTruthy(sel) {
				result = append(result, val)
			}
		}

		return core.NewList(result...), nil
	}))

	// product - Cartesian product of input iterables
	itertoolsModule.Set("product", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("product", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// Convert all arguments to lists
		lists := make([][]core.Value, v.Count())
		for i := 0; i < v.Count(); i++ {
			iterable, err := types.RequireIterable(v.Get(i), fmt.Sprintf("product() argument %d", i))
			if err != nil {
				return nil, err
			}

			list := make([]core.Value, 0)
			iter := iterable.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				list = append(list, val)
			}
			lists[i] = list
		}

		// Generate Cartesian product
		result := make([]core.Value, 0)
		var generate func(int, []core.Value)
		generate = func(depth int, current []core.Value) {
			if depth == len(lists) {
				tuple := make(core.TupleValue, len(current))
				copy(tuple, current)
				result = append(result, tuple)
				return
			}

			for _, item := range lists[depth] {
				generate(depth+1, append(current, item))
			}
		}

		generate(0, []core.Value{})
		return core.NewList(result...), nil
	}))

	// combinations - r-length tuples from input iterable
	itertoolsModule.Set("combinations", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("combinations", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(0), "combinations() first argument")
		if err != nil {
			return nil, err
		}

		r, err := v.GetNumber(1)
		if err != nil {
			return nil, err
		}
		rInt := int(r)

		// Convert to list
		items := make([]core.Value, 0)
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			items = append(items, val)
		}

		// Generate combinations
		result := make([]core.Value, 0)
		var generate func(int, int, []core.Value)
		generate = func(start, depth int, current []core.Value) {
			if depth == rInt {
				tuple := make(core.TupleValue, len(current))
				copy(tuple, current)
				result = append(result, tuple)
				return
			}

			for i := start; i < len(items); i++ {
				generate(i+1, depth+1, append(current, items[i]))
			}
		}

		generate(0, 0, []core.Value{})
		return core.NewList(result...), nil
	}))

	// starmap - map function over iterable where each item is unpacked as function arguments
	// starmap(pow, [(2,5), (3,2), (10,3)]) --> 32 9 1000
	itertoolsModule.Set("starmap", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("starmap", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		function, err := types.RequireCallable(v.Get(0), "starmap() first argument")
		if err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(1), "starmap() second argument")
		if err != nil {
			return nil, err
		}

		result := make([]core.Value, 0)
		iter := iterable.Iterator()

		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}

			// Unpack the value as arguments
			var callArgs []core.Value
			switch item := val.(type) {
			case *core.ListValue:
				callArgs = item.Items()
			case core.TupleValue:
				callArgs = []core.Value(item)
			case core.Iterable:
				// Convert iterable to list
				callArgs = make([]core.Value, 0)
				itemIter := item.Iterator()
				for {
					elem, hasElem := itemIter.Next()
					if !hasElem {
						break
					}
					callArgs = append(callArgs, elem)
				}
			default:
				return nil, fmt.Errorf("starmap() argument after * must be an iterable, not %s", val.Type())
			}

			// Call function with unpacked arguments
			funcResult, err := function.Call(callArgs, ctx)
			if err != nil {
				return nil, err
			}

			result = append(result, funcResult)
		}

		return core.NewList(result...), nil
	}))

	return itertoolsModule
}

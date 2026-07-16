package modules

import (
	"fmt"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// repeatIterator is a lazy implementation of itertools.repeat(value[, times]).
// When infinite is true it yields value forever; otherwise it yields value
// exactly remaining more times. Being lazy, repeat(value, huge) allocates
// nothing up front (the old eager version panicked in makeslice for very large
// or overflowed counts and could not represent the infinite form at all).
type repeatIterator struct {
	value     core.Value
	remaining int
	infinite  bool
}

func (r *repeatIterator) Type() core.Type { return "repeat" }

func (r *repeatIterator) String() string {
	if r.infinite {
		return fmt.Sprintf("repeat(%s)", r.value.String())
	}
	return fmt.Sprintf("repeat(%s, %d)", r.value.String(), r.remaining)
}

func (r *repeatIterator) Iterator() core.Iterator { return r }

func (r *repeatIterator) Next() (core.Value, bool) {
	if r.infinite {
		return r.value, true
	}
	if r.remaining <= 0 {
		return nil, false
	}
	r.remaining--
	return r.value, true
}

// Reset is a no-op: itertools iterators are single-pass and do not restart.
func (r *repeatIterator) Reset() {}

// itertoolsIterator is a generic lazy iterator driven by a next closure. It is
// used to make the itertools combinators return true iterators (matching
// CPython, where e.g. next(chain(...)) works and iter(x) is x) instead of
// eagerly materialized lists, and to support infinite inputs.
//
// The closure returns (value, ok, err). An error is surfaced through the Python
// __next__ path (which the for-loop and next() use and which can propagate
// errors); the plain Iterator.Next() path cannot return an error, so it records
// the error in pendingErr and stops, mirroring the existing map()/filter()
// iterators while still letting __next__ re-raise it.
type itertoolsIterator struct {
	name       string
	next       func() (core.Value, bool, error)
	pendingErr error
	done       bool
}

func (it *itertoolsIterator) Type() core.Type { return core.Type(it.name) }

func (it *itertoolsIterator) String() string {
	return fmt.Sprintf("<itertools.%s object>", it.name)
}

func (it *itertoolsIterator) Iterator() core.Iterator { return it }

func (it *itertoolsIterator) Next() (core.Value, bool) {
	if it.done {
		return nil, false
	}
	v, ok, err := it.next()
	if err != nil {
		it.pendingErr = err
		it.done = true
		return nil, false
	}
	if !ok {
		it.done = true
		return nil, false
	}
	return v, true
}

// Reset is a no-op: itertools iterators are single-pass and do not restart.
func (it *itertoolsIterator) Reset() {}

func (it *itertoolsIterator) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return it, nil
		}), true
	case "__next__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if it.done {
				if it.pendingErr != nil {
					err := it.pendingErr
					it.pendingErr = nil
					return nil, err
				}
				return nil, &core.StopIteration{}
			}
			v, ok, err := it.next()
			if err != nil {
				it.done = true
				return nil, err
			}
			if !ok {
				it.done = true
				return nil, &core.StopIteration{}
			}
			return v, nil
		}), true
	}
	return nil, false
}

// sliceIter returns a lazy iterator over an already-computed slice of values.
// It is used by the combinatoric builders (product/combinations/permutations)
// whose results must be fully enumerated anyway but should still be returned as
// iterators rather than lists.
func sliceIter(name string, items []core.Value) *itertoolsIterator {
	i := 0
	return &itertoolsIterator{name: name, next: func() (core.Value, bool, error) {
		if i >= len(items) {
			return nil, false, nil
		}
		v := items[i]
		i++
		return v, true, nil
	}}
}

// collectIterable eagerly drains an iterable into a slice. Used by combinators
// (product/combinations/permutations) that must see all input up front.
func collectIterable(it core.Iterable) []core.Value {
	items := make([]core.Value, 0)
	iter := it.Iterator()
	for {
		v, ok := iter.Next()
		if !ok {
			break
		}
		items = append(items, v)
	}
	return items
}

// InitItertoolsModule creates and returns the itertools module
func InitItertoolsModule() *core.DictValue {
	itertoolsModule := core.NewDict()

	// chain - chain multiple iterables together (class with from_iterable classmethod)
	chainClass := core.NewClass("chain", nil)

	// __new__ method handles chain(*iterables) lazily: iterables are consumed
	// one after another on demand, so chain accepts (and defers) infinite inputs.
	chainClass.SetMethod("__new__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// args[0] is the class itself; args[1:] are the iterables to chain.
		if len(args) < 1 {
			return nil, fmt.Errorf("chain() missing class argument")
		}
		sources := args[1:]
		idx := 0
		var cur core.Iterator
		return &itertoolsIterator{name: "chain", next: func() (core.Value, bool, error) {
			for {
				if cur == nil {
					if idx >= len(sources) {
						return nil, false, nil
					}
					iterable, err := types.RequireIterable(sources[idx], "chain() argument")
					if err != nil {
						return nil, false, err
					}
					idx++
					cur = iterable.Iterator()
				}
				if val, ok := cur.Next(); ok {
					return val, true, nil
				}
				cur = nil
			}
		}}, nil
	}))

	// from_iterable classmethod handles chain.from_iterable(iterable_of_iterables)
	fromIterableFunc := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// args[0] is the iterable of iterables
		if len(args) < 1 {
			return nil, fmt.Errorf("from_iterable() missing required argument")
		}

		iterableOfIterables, err := types.RequireIterable(args[0], "from_iterable() argument")
		if err != nil {
			return nil, err
		}

		outerIter := iterableOfIterables.Iterator()
		var cur core.Iterator
		return &itertoolsIterator{name: "chain", next: func() (core.Value, bool, error) {
			for {
				if cur == nil {
					iterableVal, ok := outerIter.Next()
					if !ok {
						return nil, false, nil
					}
					iterable, err := types.RequireIterable(iterableVal, "from_iterable() argument item")
					if err != nil {
						return nil, false, err
					}
					cur = iterable.Iterator()
				}
				if val, ok := cur.Next(); ok {
					return val, true, nil
				}
				cur = nil
			}
		}}, nil
	})

	// Wrap the function as a classmethod
	chainClass.SetClassAttr("from_iterable", fromIterableFunc)

	itertoolsModule.Set("chain", chainClass)

	// cycle - repeat elements from the iterable endlessly (CPython semantics).
	// On the first pass elements are yielded and saved; once the source is
	// exhausted the saved copy is replayed forever.
	itertoolsModule.Set("cycle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("cycle", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(0), "cycle() argument")
		if err != nil {
			return nil, err
		}

		iter := iterable.Iterator()
		saved := make([]core.Value, 0)
		firstPass := true
		idx := 0
		return &itertoolsIterator{name: "cycle", next: func() (core.Value, bool, error) {
			if firstPass {
				if val, ok := iter.Next(); ok {
					saved = append(saved, val)
					return val, true, nil
				}
				firstPass = false
			}
			if len(saved) == 0 {
				return nil, false, nil
			}
			val := saved[idx]
			idx = (idx + 1) % len(saved)
			return val, true, nil
		}}, nil
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

		// Parse start/stop/step. Any of them may be None, meaning unbounded
		// start (0), unbounded stop (consume forever), or default step (1).
		start, step := 0, 1
		stop, bounded := 0, false
		idxAt := func(i int) (int, bool, error) {
			val := v.Get(i)
			if val == core.None || val == core.Nil {
				return 0, false, nil
			}
			n, err := v.GetNumber(i)
			if err != nil {
				return 0, false, err
			}
			return int(n), true, nil
		}
		if v.Count() == 2 {
			// islice(iterable, stop)
			s, ok, err := idxAt(1)
			if err != nil {
				return nil, err
			}
			stop, bounded = s, ok
		} else {
			// islice(iterable, start, stop[, step])
			if s, ok, err := idxAt(1); err != nil {
				return nil, err
			} else if ok {
				start = s
			}
			s2, ok2, err := idxAt(2)
			if err != nil {
				return nil, err
			}
			stop, bounded = s2, ok2
			if v.Count() >= 4 {
				st, _, err := idxAt(3)
				if err != nil {
					return nil, err
				}
				step = st
				if step <= 0 {
					return nil, errors.NewRuntimeError("islice", "step must be positive")
				}
			}
		}

		iter := iterable.Iterator()
		index := 0
		return &itertoolsIterator{name: "islice", next: func() (core.Value, bool, error) {
			for {
				if bounded && index >= stop {
					return nil, false, nil
				}
				val, ok := iter.Next()
				if !ok {
					return nil, false, nil
				}
				cur := index
				index++
				if cur >= start && (cur-start)%step == 0 {
					return val, true, nil
				}
			}
		}}, nil
	}))

	// count - infinite arithmetic sequence start, start+step, start+2*step, ...
	itertoolsModule.Set("count", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("count", args)
		if err := v.Range(0, 2); err != nil {
			return nil, err
		}

		start, _ := v.GetNumberOrDefault(0, 0)
		step, _ := v.GetNumberOrDefault(1, 1)
		cur := start
		return &itertoolsIterator{name: "count", next: func() (core.Value, bool, error) {
			val := cur
			cur += step
			return core.NumberValue(val), true, nil
		}}, nil
	}))

	// repeat - repeat an object n times
	itertoolsModule.Set("repeat", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("repeat", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		value := v.Get(0)

		// repeat(value) with no count is an infinite iterator.
		if v.Count() < 2 {
			return &repeatIterator{value: value, infinite: true}, nil
		}

		timesVal, err := v.GetNumber(1)
		if err != nil {
			return nil, err
		}
		// Clamp to a valid slice-free range: negative counts yield nothing,
		// and counts beyond int range are treated as "very large" (still lazy,
		// so nothing is materialized) rather than overflowing into makeslice.
		var times int
		maxInt := int(^uint(0) >> 1)
		switch {
		case timesVal <= 0:
			times = 0
		case timesVal >= float64(maxInt):
			// float64 cannot represent maxInt exactly (it rounds up to 2^63),
			// so use >= to catch sys.maxsize-sized counts that would otherwise
			// overflow int conversion into a negative value.
			times = maxInt
		default:
			times = int(timesVal)
		}
		return &repeatIterator{value: value, remaining: times}, nil
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

		iter := iterable.Iterator()
		stopped := false
		return &itertoolsIterator{name: "takewhile", next: func() (core.Value, bool, error) {
			if stopped {
				return nil, false, nil
			}
			val, ok := iter.Next()
			if !ok {
				return nil, false, nil
			}
			res, err := predicate.Call([]core.Value{val}, ctx)
			if err != nil {
				return nil, false, err
			}
			if !core.IsTruthy(res) {
				stopped = true
				return nil, false, nil
			}
			return val, true, nil
		}}, nil
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

		iter := iterable.Iterator()
		dropping := true
		return &itertoolsIterator{name: "dropwhile", next: func() (core.Value, bool, error) {
			for {
				val, ok := iter.Next()
				if !ok {
					return nil, false, nil
				}
				if dropping {
					res, err := predicate.Call([]core.Value{val}, ctx)
					if err != nil {
						return nil, false, err
					}
					if core.IsTruthy(res) {
						continue
					}
					dropping = false
				}
				return val, true, nil
			}
		}}, nil
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

		dataIter := data.Iterator()
		selectorsIter := selectors.Iterator()
		return &itertoolsIterator{name: "compress", next: func() (core.Value, bool, error) {
			for {
				val, hasData := dataIter.Next()
				sel, hasSel := selectorsIter.Next()
				if !hasData || !hasSel {
					return nil, false, nil
				}
				if core.IsTruthy(sel) {
					return val, true, nil
				}
			}
		}}, nil
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
		return sliceIter("product", result), nil
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
		return sliceIter("combinations", result), nil
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

		iter := iterable.Iterator()
		return &itertoolsIterator{name: "starmap", next: func() (core.Value, bool, error) {
			val, ok := iter.Next()
			if !ok {
				return nil, false, nil
			}
			// Unpack the value as arguments.
			var callArgs []core.Value
			switch item := val.(type) {
			case *core.ListValue:
				callArgs = item.Items()
			case core.TupleValue:
				callArgs = []core.Value(item)
			case core.Iterable:
				callArgs = collectIterable(item)
			default:
				return nil, false, fmt.Errorf("starmap() argument after * must be an iterable, not %s", val.Type())
			}
			res, err := function.Call(callArgs, ctx)
			if err != nil {
				return nil, false, err
			}
			return res, true, nil
		}}, nil
	}))

	// permutations - r-length permutations from input iterable
	// permutations(iterable, r=None) -> iterator of tuples
	itertoolsModule.Set("permutations", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("permutations", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(0), "permutations() first argument")
		if err != nil {
			return nil, err
		}

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

		// Get r (default is length of items)
		rInt := len(items)
		if v.Count() == 2 {
			r, err := v.GetNumber(1)
			if err != nil {
				return nil, err
			}
			rInt = int(r)
		}

		// Generate permutations
		result := make([]core.Value, 0)
		var generate func([]int, []core.Value)
		generate = func(used []int, current []core.Value) {
			if len(current) == rInt {
				tuple := make(core.TupleValue, len(current))
				copy(tuple, current)
				result = append(result, tuple)
				return
			}

			for i := 0; i < len(items); i++ {
				// Check if index is already used
				alreadyUsed := false
				for _, u := range used {
					if u == i {
						alreadyUsed = true
						break
					}
				}
				if alreadyUsed {
					continue
				}

				generate(append(used, i), append(current, items[i]))
			}
		}

		generate([]int{}, []core.Value{})
		return sliceIter("permutations", result), nil
	}))

	// accumulate - make an iterator that returns accumulated sums/results
	// accumulate([1,2,3,4,5]) --> 1 3 6 10 15
	// accumulate([1,2,3,4,5], operator.mul) --> 1 2 6 24 120
	itertoolsModule.Set("accumulate", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("accumulate", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(0), "accumulate() first argument")
		if err != nil {
			return nil, err
		}

		// Get optional function (default is addition)
		var fn core.Callable
		if v.Count() == 2 {
			fn, err = types.RequireCallable(v.Get(1), "accumulate() second argument")
			if err != nil {
				return nil, err
			}
		}

		iter := iterable.Iterator()
		var total core.Value
		started := false
		return &itertoolsIterator{name: "accumulate", next: func() (core.Value, bool, error) {
			val, ok := iter.Next()
			if !ok {
				return nil, false, nil
			}
			if !started {
				started = true
				total = val
				return total, true, nil
			}
			if fn != nil {
				newTotal, err := fn.Call([]core.Value{total, val}, ctx)
				if err != nil {
					return nil, false, err
				}
				total = newTotal
			} else {
				totalNum, ok1 := total.(core.NumberValue)
				valNum, ok2 := val.(core.NumberValue)
				if !ok1 || !ok2 {
					return nil, false, errors.NewRuntimeError("accumulate", "default accumulate requires numeric values")
				}
				total = core.NumberValue(float64(totalNum) + float64(valNum))
			}
			return total, true, nil
		}}, nil
	}))

	// filterfalse - filter elements where predicate is false
	itertoolsModule.Set("filterfalse", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("filterfalse", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		predicate, err := types.RequireCallable(v.Get(0), "filterfalse() first argument")
		if err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(1), "filterfalse() second argument")
		if err != nil {
			return nil, err
		}

		iter := iterable.Iterator()
		return &itertoolsIterator{name: "filterfalse", next: func() (core.Value, bool, error) {
			for {
				val, ok := iter.Next()
				if !ok {
					return nil, false, nil
				}
				res, err := predicate.Call([]core.Value{val}, ctx)
				if err != nil {
					return nil, false, err
				}
				if !core.IsTruthy(res) {
					return val, true, nil
				}
			}
		}}, nil
	}))

	return itertoolsModule
}

package builtin

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterFunctional registers functional programming functions using builders
func RegisterFunctional(ctx *core.Context) {
	// map - apply function to all items
	// BEFORE: ~40 lines
	// AFTER: Custom builder
	ctx.Define("map", core.NewBuiltinFunction(MapBuilder()))

	// filter - filter items by predicate
	// BEFORE: ~40 lines
	// AFTER: Custom builder
	ctx.Define("filter", core.NewBuiltinFunction(FilterBuilder()))

	// reduce - reduce sequence to single value
	// BEFORE: ~45 lines
	// AFTER: Custom builder
	ctx.Define("reduce", core.NewBuiltinFunction(ReduceBuilder()))

	// all - check if all elements are truthy
	// BEFORE: ~20 lines
	// AFTER: Custom builder
	ctx.Define("all", core.NewBuiltinFunction(AllBuilder()))

	// any - check if any element is truthy
	// BEFORE: ~20 lines
	// AFTER: Custom builder
	ctx.Define("any", core.NewBuiltinFunction(AnyBuilder()))
}

// MapIterator is a lazy iterator that applies a function to each element
type MapIterator struct {
	fn               core.Callable
	iterators        []core.Iterator
	originalIterables []core.Iterable
	ctx              *core.Context
	exhausted        bool
}

// Type returns the type name
func (m *MapIterator) Type() core.Type {
	return "map"
}

// String returns a string representation
func (m *MapIterator) String() string {
	return "<map object>"
}

// Iterator returns self (map objects are their own iterators)
func (m *MapIterator) Iterator() core.Iterator {
	return m
}

// Next returns the next mapped value
func (m *MapIterator) Next() (core.Value, bool) {
	if m.exhausted {
		return nil, false
	}

	// Collect values from all iterators
	args := make([]core.Value, len(m.iterators))
	for i, iter := range m.iterators {
		val, hasNext := iter.Next()
		if !hasNext {
			m.exhausted = true
			return nil, false
		}
		args[i] = val
	}

	// Apply the function
	result, err := m.fn.Call(args, m.ctx)
	if err != nil {
		// In case of error, stop iteration
		m.exhausted = true
		return nil, false
	}

	return result, true
}

// Reset resets the iterator to the beginning
func (m *MapIterator) Reset() {
	m.exhausted = false
	for i, iterable := range m.originalIterables {
		m.iterators[i] = iterable.Iterator()
	}
}

// GetAttr implements attribute access for map objects
func (m *MapIterator) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return m, nil
		}), true
	case "__next__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			val, hasNext := m.Next()
			if !hasNext {
				return nil, &core.StopIteration{}
			}
			return val, nil
		}), true
	}
	return nil, false
}

// MapBuilder creates the map function
func MapBuilder() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("map", args)

		if err := v.Min(2); err != nil {
			return nil, err
		}

		// First argument must be callable
		fn, err := types.RequireCallable(v.Get(0), "map() first argument")
		if err != nil {
			return nil, err
		}

		// Get all iterables and create their iterators
		iterables := make([]core.Iterable, v.Count()-1)
		iterators := make([]core.Iterator, v.Count()-1)
		for i := 1; i < v.Count(); i++ {
			iter, err := types.RequireIterable(v.Get(i), "map() argument")
			if err != nil {
				return nil, err
			}
			iterables[i-1] = iter
			iterators[i-1] = iter.Iterator()
		}

		// Return a lazy map iterator
		return &MapIterator{
			fn:                fn,
			iterators:         iterators,
			originalIterables: iterables,
			ctx:               ctx,
			exhausted:         false,
		}, nil
	}
}

// FilterBuilder creates the filter function
func FilterBuilder() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("filter", args)

		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// First argument can be callable or None
		var predicate core.Callable
		if !types.IsNil(v.Get(0)) {
			fn, err := types.RequireCallable(v.Get(0), "filter() first argument")
			if err != nil {
				return nil, err
			}
			predicate = fn
		}

		// Second argument must be iterable
		iterable, err := types.RequireIterable(v.Get(1), "filter() second argument")
		if err != nil {
			return nil, err
		}

		// TODO(M28-5beb): Return filter iterator when implemented
		// For now, return a simple list implementation
		result := make([]core.Value, 0)
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			// If predicate is nil, use truthiness
			if predicate == nil {
				if types.IsTruthy(val) {
					result = append(result, val)
				}
			} else {
				keep, err := predicate.Call([]core.Value{val}, ctx)
				if err != nil {
					return nil, err
				}
				if types.IsTruthy(keep) {
					result = append(result, val)
				}
			}
		}
		return core.NewList(result...), nil
	}
}

// ReduceBuilder creates the reduce function
func ReduceBuilder() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("reduce", args)

		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		// First argument must be callable
		fn, err := types.RequireCallable(v.Get(0), "reduce() first argument")
		if err != nil {
			return nil, err
		}

		// Second argument must be iterable
		iterable, err := types.RequireIterable(v.Get(1), "reduce() second argument")
		if err != nil {
			return nil, err
		}

		// Get iterator
		iterator := iterable.Iterator()

		// Initialize accumulator
		var accumulator core.Value
		if v.Count() == 3 {
			// Initial value provided
			accumulator = v.Get(2)
		} else {
			// Use first value from iterator
			val, hasNext := iterator.Next()
			if !hasNext {
				return nil, errors.NewValueError("reduce", "reduce() of empty sequence with no initial value")
			}
			accumulator = val
		}

		// Reduce
		for {
			val, hasNext := iterator.Next()
			if !hasNext {
				break
			}

			result, err := fn.Call([]core.Value{accumulator, val}, ctx)
			if err != nil {
				return nil, err
			}
			accumulator = result
		}

		return accumulator, nil
	}
}

// AllBuilder creates the all function
func AllBuilder() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("all", args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Get iterable
		iterable, err := types.RequireIterable(v.Get(0), "all() argument")
		if err != nil {
			return nil, err
		}

		// Check all elements
		iterator := iterable.Iterator()
		for {
			val, hasNext := iterator.Next()
			if !hasNext {
				break
			}

			if !types.IsTruthy(val) {
				return core.BoolValue(false), nil
			}
		}

		return core.BoolValue(true), nil
	}
}

// AnyBuilder creates the any function
func AnyBuilder() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("any", args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Get iterable
		iterable, err := types.RequireIterable(v.Get(0), "any() argument")
		if err != nil {
			return nil, err
		}

		// Check any element
		iterator := iterable.Iterator()
		for {
			val, hasNext := iterator.Next()
			if !hasNext {
				break
			}

			if types.IsTruthy(val) {
				return core.BoolValue(true), nil
			}
		}

		return core.BoolValue(false), nil
	}
}

// Migration Statistics:
// Functions migrated: 5 functional programming functions
// Original lines: ~185 lines
// Migrated lines: ~130 lines
// Reduction: ~30% with cleaner structure

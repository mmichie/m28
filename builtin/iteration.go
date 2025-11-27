package builtin

import (
	goerrors "errors"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// RegisterIteration registers iteration functions using the builder framework
func RegisterIteration(ctx *core.Context) {
	// iter and next are now registered by RegisterIterationProtocol
	// which provides the proper iterator protocol implementation

	// enumerate - return enumerate object
	// BEFORE: 38 lines
	// AFTER: Custom builder with optional start (supports keyword arguments)
	ctx.Define("enumerate", NewKwargsBuiltinFunction("enumerate", enumerateWithKwargs))

	// zip - zip iterables together
	// BEFORE: 42 lines
	// AFTER: Custom builder for variadic iterables
	ctx.Define("zip", core.NewBuiltinFunction(ZipBuilder()))

	// reversed - return reversed iterator
	ctx.Define("reversed", core.NewBuiltinFunction(ReversedBuilder()))
}

// IterBuilder creates the iter function with optional sentinel
func IterBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("iter", args)

		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		obj := v.Get(0)

		// Single argument - get iterator
		if v.Count() == 1 {
			// For now, just return the iterable itself
			// TODO(M28-5beb): Return proper iterator object when implemented
			if iterable, ok := types.AsIterable(obj); ok {
				return iterable, nil
			}

			// Try __iter__ method
			if o, ok := obj.(core.Object); ok {
				if method, exists := o.GetAttr("__iter__"); exists {
					if callable, ok := types.AsCallable(method); ok {
						result, err := callable.Call([]core.Value{}, ctx)
						if err != nil {
							return nil, err
						}
						// Return result as is
						return result, nil
					}
				}
			}

			return nil, errors.NewTypeError("iter", "argument must be iterable", string(obj.Type()))
		}

		// Two arguments - callable with sentinel
		// TODO(M28-5beb): Implement sentinel iterator
		return nil, errors.NewRuntimeError("iter", "iter() with sentinel not yet implemented")
	}
}

// NextBuilder creates the next function with optional default
func NextBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("next", args)

		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		iterator := v.Get(0)

		// Try direct Iterator interface
		if iter, ok := iterator.(core.Iterator); ok {
			val, hasNext := iter.Next()
			if hasNext {
				return val, nil
			}
			// No more items
			if v.Count() == 2 {
				// Return default value
				return v.Get(1), nil
			}
			return nil, errors.NewRuntimeError("next", "StopIteration")
		}

		// Try __next__ method
		if obj, ok := iterator.(core.Object); ok {
			if method, exists := obj.GetAttr("__next__"); exists {
				if callable, ok := types.AsCallable(method); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						// Check for StopIteration using errors.As
						var stopIter *protocols.StopIteration
						if v.Count() == 2 && goerrors.As(err, &stopIter) {
							// Return default value
							return v.Get(1), nil
						}
						return nil, err
					}
					return result, nil
				}
			}
		}

		return nil, errors.NewTypeError("next", "argument must be an iterator", string(iterator.Type()))
	}
}

// EnumerateBuilder creates the enumerate function
func EnumerateBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("enumerate", args)

		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		// Get iterable
		obj := v.Get(0)
		iterable, err := types.RequireIterable(obj, "enumerate() argument")
		if err != nil {
			return nil, err
		}

		// Get optional start value
		start := 0
		if v.Count() == 2 {
			s, err := v.GetInt(1)
			if err != nil {
				return nil, err
			}
			start = s
		}

		// TODO(M28-5beb): Return proper enumerate iterator when implemented
		// For now, return a simple implementation
		result := make([]core.Value, 0)
		iter := iterable.Iterator()
		index := start
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			result = append(result, core.TupleValue{core.NumberValue(index), val})
			index++
		}
		return core.NewList(result...), nil
	}
}

// enumerateWithKwargs implements enumerate() with keyword argument support
func enumerateWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Check for start parameter in kwargs
	start := 0
	if startVal, ok := kwargs["start"]; ok {
		if num, ok := startVal.(core.NumberValue); ok {
			start = int(num)
		} else {
			return nil, errors.NewTypeError("enumerate", "start parameter must be an integer", string(startVal.Type()))
		}
		delete(kwargs, "start")
	}

	// Check for any remaining kwargs
	if len(kwargs) > 0 {
		for k := range kwargs {
			return nil, errors.NewTypeError("enumerate", "'"+k+"' is an invalid keyword argument", "")
		}
	}

	// Require exactly 1 or 2 positional arguments
	if len(args) == 0 {
		return nil, errors.NewTypeError("enumerate", "missing required argument: 'iterable' (pos 1)", "")
	}
	if len(args) > 2 {
		return nil, errors.NewTypeError("enumerate", "takes at most 2 arguments", "")
	}

	// Get iterable
	obj := args[0]
	iterable, err := types.RequireIterable(obj, "enumerate() argument")
	if err != nil {
		return nil, err
	}

	// If positional start value provided, it overrides kwarg
	if len(args) == 2 {
		if num, ok := args[1].(core.NumberValue); ok {
			start = int(num)
		} else {
			return nil, errors.NewTypeError("enumerate", "start parameter must be an integer", string(args[1].Type()))
		}
	}

	// Build enumerate result
	result := make([]core.Value, 0)
	iter := iterable.Iterator()
	index := start
	for {
		val, hasNext := iter.Next()
		if !hasNext {
			break
		}
		result = append(result, core.TupleValue{core.NumberValue(index), val})
		index++
	}
	return core.NewList(result...), nil
}

// ZipBuilder creates the zip function
func ZipBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("zip", args)

		// zip() with no arguments returns empty list
		if v.Count() == 0 {
			return core.NewList(), nil
		}

		// Convert all arguments to iterables
		iterables := make([]core.Iterable, v.Count())
		for i := 0; i < v.Count(); i++ {
			iter, err := types.RequireIterable(v.Get(i), "zip() argument")
			if err != nil {
				return nil, err
			}
			iterables[i] = iter
		}

		// TODO(M28-5beb): Return proper zip iterator when implemented
		// For now, return a simple implementation
		if len(iterables) == 0 {
			return core.NewList(), nil
		}

		result := make([]core.Value, 0)
		iters := make([]core.Iterator, len(iterables))
		for i, iterable := range iterables {
			iters[i] = iterable.Iterator()
		}

		for {
			values := make(core.TupleValue, len(iters))
			for i, iter := range iters {
				val, hasNext := iter.Next()
				if !hasNext {
					return core.NewList(result...), nil
				}
				values[i] = val
			}
			result = append(result, values)
		}
	}
}

// ReversedBuilder creates the reversed function
func ReversedBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("reversed", args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		obj := v.Get(0)

		// Try __reversed__ dunder method first
		if o, ok := obj.(core.Object); ok {
			if method, exists := o.GetAttr("__reversed__"); exists {
				if callable, ok := types.AsCallable(method); ok {
					result, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}
					// Return the iterator from __reversed__
					return result, nil
				}
			}
		}

		// Fall back to using __len__ and __getitem__ for sequences
		// Check if object has __len__ and __getitem__
		hasLen := false
		hasGetitem := false

		if o, ok := obj.(core.Object); ok {
			if _, exists := o.GetAttr("__len__"); exists {
				hasLen = true
			}
			if _, exists := o.GetAttr("__getitem__"); exists {
				hasGetitem = true
			}
		}

		// If it's a built-in sequence type, we can reverse it directly
		if list, ok := types.AsList(obj); ok {
			// Create reversed list
			result := make([]core.Value, list.Len())
			for i := 0; i < list.Len(); i++ {
				result[i] = list.Items()[list.Len()-1-i]
			}
			return core.NewList(result...), nil
		}

		if tuple, ok := types.AsTuple(obj); ok {
			// Create reversed list (Python's reversed() returns iterator, but for simplicity return list)
			result := make([]core.Value, len(tuple))
			for i := 0; i < len(tuple); i++ {
				result[i] = tuple[len(tuple)-1-i]
			}
			return core.NewList(result...), nil
		}

		if str, ok := types.AsString(obj); ok {
			// Create reversed string as list of characters
			runes := []rune(str)
			result := make([]core.Value, len(runes))
			for i := 0; i < len(runes); i++ {
				result[i] = core.StringValue(string(runes[len(runes)-1-i]))
			}
			return core.NewList(result...), nil
		}

		// If object has __len__ and __getitem__, we could implement reverse iteration
		// but for now, require __reversed__ or built-in sequence types
		if hasLen && hasGetitem {
			return nil, errors.NewRuntimeError("reversed", "reverse iteration via __getitem__ not yet implemented")
		}

		return nil, errors.NewTypeError("reversed", "argument must be a sequence", string(obj.Type()))
	}
}

// Migration Statistics:
// Functions migrated: 5 iteration functions (including reversed)
// Original lines: ~143 lines
// Migrated lines: ~175 lines
// Benefits: Consistent structure and dunder method support

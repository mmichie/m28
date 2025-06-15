package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// iterFunc implements the iter() builtin
func iterFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("iter() takes exactly one argument (%d given)", len(args))
	}

	obj := args[0]

	// First, try calling __iter__ if it exists
	if iter, found, err := types.CallIter(obj, ctx); found {
		if err != nil {
			return nil, err
		}
		return iter, nil
	}

	// Then try protocol-based iteration
	if iter, ok := protocols.GetIterableOps(obj); ok {
		// The iterator should be a valid Value type
		if val, ok := iter.(core.Value); ok {
			return val, nil
		}
		return nil, fmt.Errorf("iterator does not implement core.Value interface")
	}

	// Check if object already is an iterator (has __next__)
	if nextMethod, ok := obj.(core.Object); ok {
		if _, hasNext := nextMethod.GetAttr("__next__"); hasNext {
			return obj, nil
		}
	}

	return nil, fmt.Errorf("'%s' object is not iterable", obj.Type())
}

// nextFunc implements the next() builtin
func nextFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("next() takes 1 or 2 arguments (%d given)", len(args))
	}

	iterator := args[0]
	var defaultValue core.Value
	hasDefault := false

	if len(args) == 2 {
		defaultValue = args[1]
		hasDefault = true
	}

	// First, try calling __next__ if it exists
	if result, found, err := types.CallNext(iterator, ctx); found {
		if err != nil {
			// Check if it's StopIteration
			if _, isStop := err.(*protocols.StopIteration); isStop {
				if hasDefault {
					return defaultValue, nil
				}
				return nil, err
			}
			return nil, err
		}
		return result, nil
	}

	// Try Iterator protocol
	if iter, ok := iterator.(protocols.Iterator); ok {
		val, err := iter.Next()
		if err != nil {
			// Check if it's StopIteration
			if _, isStop := err.(*protocols.StopIteration); isStop {
				if hasDefault {
					return defaultValue, nil
				}
				return nil, err
			}
			return nil, err
		}
		return val, nil
	}

	// Try core.Iterator (old interface)
	if iter, ok := iterator.(core.Iterator); ok {
		val, hasNext := iter.Next()
		if !hasNext {
			if hasDefault {
				return defaultValue, nil
			}
			return nil, &protocols.StopIteration{}
		}
		return val, nil
	}

	return nil, fmt.Errorf("'%s' object is not an iterator", iterator.Type())
}

// RegisterIterationProtocol registers the iteration protocol functions
func RegisterIterationProtocol(ctx *core.Context) {
	// Register iter function
	RegisterBuiltinFunc(ctx, "iter", iterFunc)

	// Register next function
	RegisterBuiltinFunc(ctx, "next", nextFunc)
}

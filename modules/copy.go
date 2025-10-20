package modules

import (
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitCopyModule creates and returns the copy module
func InitCopyModule() *core.DictValue {
	copyModule := core.NewDict()

	// copy - Create a shallow copy of an object
	copyModule.Set("copy", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("copy", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return shallowCopy(v.Get(0))
	}))

	// deepcopy - Create a deep copy of an object
	copyModule.Set("deepcopy", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("deepcopy", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return deepCopy(v.Get(0))
	}))

	return copyModule
}

// shallowCopy creates a shallow copy of a value
func shallowCopy(val core.Value) (core.Value, error) {
	switch v := val.(type) {
	case *core.ListValue:
		// Create new list with same elements
		newList := make([]core.Value, v.Len())
		copy(newList, v.Items())
		return core.NewList(newList...), nil

	case core.TupleValue:
		// Tuples are immutable, but create new one anyway for consistency
		newTuple := make(core.TupleValue, len(v))
		copy(newTuple, v)
		return newTuple, nil

	case *core.DictValue:
		// Create new dict with same key-value pairs
		newDict := core.NewDict()
		for _, key := range v.Keys() {
			if value, ok := v.Get(key); ok {
				newDict.Set(key, value)
			}
		}
		return newDict, nil

	case *core.SetValue:
		// Create new set with same elements
		newSet := core.NewSet()
		iter := v.Iterator()
		for {
			elem, hasNext := iter.Next()
			if !hasNext {
				break
			}
			newSet.Add(elem)
		}
		return newSet, nil

	case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue:
		// Primitives are immutable, return as-is
		return v, nil

	default:
		// For other types, try to return as-is
		// Could implement copy protocol (__copy__) here in the future
		return v, nil
	}
}

// deepCopy creates a deep copy of a value
func deepCopy(val core.Value) (core.Value, error) {
	switch v := val.(type) {
	case *core.ListValue:
		// Recursively copy all elements
		newList := make([]core.Value, v.Len())
		for i, elem := range v.Items() {
			copied, err := deepCopy(elem)
			if err != nil {
				return nil, err
			}
			newList[i] = copied
		}
		return core.NewList(newList...), nil

	case core.TupleValue:
		// Recursively copy all elements
		newTuple := make(core.TupleValue, len(v))
		for i, elem := range v {
			copied, err := deepCopy(elem)
			if err != nil {
				return nil, err
			}
			newTuple[i] = copied
		}
		return newTuple, nil

	case *core.DictValue:
		// Recursively copy all key-value pairs
		newDict := core.NewDict()
		for _, key := range v.Keys() {
			if value, ok := v.Get(key); ok {
				copiedValue, err := deepCopy(value)
				if err != nil {
					return nil, err
				}
				newDict.Set(key, copiedValue)
			}
		}
		return newDict, nil

	case *core.SetValue:
		// Recursively copy all elements
		newSet := core.NewSet()
		iter := v.Iterator()
		for {
			elem, hasNext := iter.Next()
			if !hasNext {
				break
			}
			copied, err := deepCopy(elem)
			if err != nil {
				return nil, err
			}
			newSet.Add(copied)
		}
		return newSet, nil

	case core.NumberValue, core.StringValue, core.BoolValue, core.NilValue:
		// Primitives are immutable, return as-is
		return v, nil

	case *core.FrozenSetValue:
		// Frozensets are immutable, but recursively copy elements for consistency
		newFrozenSet := core.NewFrozenSet()
		iter := v.Iterator()
		for {
			elem, hasNext := iter.Next()
			if !hasNext {
				break
			}
			copied, err := deepCopy(elem)
			if err != nil {
				return nil, err
			}
			newFrozenSet.Add(copied)
		}
		return newFrozenSet, nil

	default:
		// For other types, try shallow copy
		// Could implement deepcopy protocol (__deepcopy__) here in the future
		return shallowCopy(v)
	}
}

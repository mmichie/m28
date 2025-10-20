package builtin

import (
	"fmt"
	"sort"

	"github.com/mmichie/m28/core"
)

// isCallable checks if a value is callable (function, method, etc)
func isCallable(v core.Value) bool {
	// Check if it implements the Callable interface
	_, ok := v.(core.Callable)
	return ok
}

// applyKey applies the key function to a value
func applyKey(keyFunc core.Value, value core.Value, ctx *core.Context) (core.Value, error) {
	if keyFunc == nil {
		return value, nil
	}

	// Call the key function with the value
	if fn, ok := keyFunc.(core.Callable); ok {
		return fn.Call([]core.Value{value}, ctx)
	}

	return nil, fmt.Errorf("key is not callable")
}

// SortedWithKwargs is the keyword-argument version of sorted
func SortedWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("sorted requires at least 1 argument")
	}

	// Extract keyword arguments
	var keyFunc core.Value
	reverse := false

	if key, hasKey := kwargs["key"]; hasKey {
		if !isCallable(key) {
			return nil, fmt.Errorf("key argument must be a callable function")
		}
		keyFunc = key
	}

	if rev, hasRev := kwargs["reverse"]; hasRev {
		if b, ok := rev.(core.BoolValue); ok {
			reverse = bool(b)
		} else {
			return nil, fmt.Errorf("reverse argument must be a boolean")
		}
	}

	// Check for unknown keyword arguments
	for k := range kwargs {
		if k != "key" && k != "reverse" {
			return nil, fmt.Errorf("sorted() got an unexpected keyword argument '%s'", k)
		}
	}

	// First argument is the sequence to sort
	var items []core.Value
	switch v := args[0].(type) {
	case *core.ListValue:
		items = make([]core.Value, v.Len())
		copy(items, v.Items())
	case core.TupleValue:
		items = make([]core.Value, len(v))
		copy(items, v)
	case core.StringValue:
		items = make([]core.Value, 0, len(v))
		for _, ch := range string(v) {
			items = append(items, core.StringValue(string(ch)))
		}
	default:
		return nil, fmt.Errorf("sorted expects a sequence, got %s", v.Type())
	}

	// Handle legacy positional reverse parameter for backwards compatibility
	if len(args) > 1 {
		if b, ok := args[1].(core.BoolValue); ok {
			reverse = bool(b)
		}
	}

	// Create a slice of items with their keys for sorting
	type keyedItem struct {
		value    core.Value
		keyValue core.Value
	}

	keyedItems := make([]keyedItem, len(items))
	for i, item := range items {
		key, err := applyKey(keyFunc, item, ctx)
		if err != nil {
			return nil, fmt.Errorf("error applying key function: %v", err)
		}
		keyedItems[i] = keyedItem{value: item, keyValue: key}
	}

	// Sort using Go's sort package
	sort.Slice(keyedItems, func(i, j int) bool {
		cmp := core.Compare(keyedItems[i].keyValue, keyedItems[j].keyValue)
		if reverse {
			return cmp > 0
		}
		return cmp < 0
	})

	// Extract the sorted values
	result := make([]core.Value, len(keyedItems))
	for i, ki := range keyedItems {
		result[i] = ki.value
	}

	return core.NewList(result...), nil
}

// MinWithKwargs is the keyword-argument version of min
func MinWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("min expected at least 1 argument, got 0")
	}

	// Extract keyword arguments
	var keyFunc core.Value

	if key, hasKey := kwargs["key"]; hasKey {
		if !isCallable(key) {
			return nil, fmt.Errorf("key argument must be a callable function")
		}
		keyFunc = key
	}

	// Check for unknown keyword arguments
	for k := range kwargs {
		if k != "key" {
			return nil, fmt.Errorf("min() got an unexpected keyword argument '%s'", k)
		}
	}

	// Handle single iterable argument
	var values []core.Value
	if len(args) == 1 {
		switch v := args[0].(type) {
		case *core.ListValue:
			if v.Len() == 0 {
				return nil, fmt.Errorf("min() arg is an empty sequence")
			}
			values = v.Items()
		case core.TupleValue:
			if len(v) == 0 {
				return nil, fmt.Errorf("min() arg is an empty sequence")
			}
			values = v
		default:
			values = args
		}
	} else {
		values = args
	}

	// Find minimum
	minVal := values[0]
	minKey, err := applyKey(keyFunc, minVal, ctx)
	if err != nil {
		return nil, err
	}

	for i := 1; i < len(values); i++ {
		currKey, err := applyKey(keyFunc, values[i], ctx)
		if err != nil {
			return nil, err
		}

		if core.Compare(currKey, minKey) < 0 {
			minVal = values[i]
			minKey = currKey
		}
	}

	return minVal, nil
}

// MaxWithKwargs is the keyword-argument version of max
func MaxWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("max expected at least 1 argument, got 0")
	}

	// Extract keyword arguments
	var keyFunc core.Value

	if key, hasKey := kwargs["key"]; hasKey {
		if !isCallable(key) {
			return nil, fmt.Errorf("key argument must be a callable function")
		}
		keyFunc = key
	}

	// Check for unknown keyword arguments
	for k := range kwargs {
		if k != "key" {
			return nil, fmt.Errorf("max() got an unexpected keyword argument '%s'", k)
		}
	}

	// Handle single iterable argument
	var values []core.Value
	if len(args) == 1 {
		switch v := args[0].(type) {
		case *core.ListValue:
			if v.Len() == 0 {
				return nil, fmt.Errorf("max() arg is an empty sequence")
			}
			values = v.Items()
		case core.TupleValue:
			if len(v) == 0 {
				return nil, fmt.Errorf("max() arg is an empty sequence")
			}
			values = v
		default:
			values = args
		}
	} else {
		values = args
	}

	// Find maximum
	maxVal := values[0]
	maxKey, err := applyKey(keyFunc, maxVal, ctx)
	if err != nil {
		return nil, err
	}

	for i := 1; i < len(values); i++ {
		currKey, err := applyKey(keyFunc, values[i], ctx)
		if err != nil {
			return nil, err
		}

		if core.Compare(currKey, maxKey) > 0 {
			maxVal = values[i]
			maxKey = currKey
		}
	}

	return maxVal, nil
}

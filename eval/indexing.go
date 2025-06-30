package eval

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// GetItemForm implements the get-item special form for index access
// (get-item obj key) -> value at index/key
func GetItemForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("get-item requires exactly 2 arguments, got %d", len(args))
	}

	// Evaluate the object
	obj, err := Eval(args[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Evaluate the key/index
	key, err := Eval(args[1], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating key: %v", err)
	}

	// Check if key is a slice object
	if slice, ok := key.(*core.SliceValue); ok {
		// Handle slicing with slice object
		return handleSliceObject(obj, slice)
	}

	// First, try dunder method __getitem__
	if result, found, err := types.CallGetItem(obj, key, ctx); found {
		return result, err
	}

	// Then try protocol-based indexing
	if indexable, ok := protocols.GetIndexableOps(obj); ok {
		return indexable.GetIndex(key)
	}

	// Fall back to type-specific handling (shouldn't reach here with proper protocol support)
	return nil, fmt.Errorf("'%s' object is not subscriptable", obj.Type())
}

// SetItemForm implements the set-item special form for index assignment
// (set-item obj key value) -> value
func SetItemForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("set-item requires exactly 3 arguments, got %d", len(args))
	}

	// Evaluate all arguments
	obj, err := Eval(args[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	key, err := Eval(args[1], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating key: %v", err)
	}

	value, err := Eval(args[2], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating value: %v", err)
	}

	// fmt.Printf("DEBUG SetItemForm: obj type=%s, key=%v, value=%v\n", obj.Type(), key, value)

	// First, try dunder method __setitem__
	if found, err := types.CallSetItem(obj, key, value, ctx); found {
		// fmt.Printf("DEBUG: CallSetItem found __setitem__, err=%v\n", err)
		if err != nil {
			return nil, err
		}
		return value, nil
	}
	// fmt.Printf("DEBUG: CallSetItem did not find __setitem__, trying protocol\n")

	// Then try protocol-based indexing
	if indexable, ok := protocols.GetIndexableOps(obj); ok {
		err = indexable.SetIndex(key, value)
		if err != nil {
			return nil, err
		}
		return value, nil
	}

	// Fall back to error (shouldn't reach here with proper protocol support)
	return nil, fmt.Errorf("'%s' object does not support item assignment", obj.Type())
}

// SliceForm implements the slice special form
// (slice obj start end step) -> sliced object
func SliceForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 4 {
		return nil, fmt.Errorf("slice requires exactly 4 arguments, got %d", len(args))
	}

	// Evaluate the object
	obj, err := Eval(args[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Helper function to get integer from value or None
	getInt := func(v core.Value, defaultVal *int) (*int, error) {
		val, err := Eval(v, ctx)
		if err != nil {
			return nil, err
		}

		// Check if it's None
		if _, isNil := val.(core.NilValue); isNil {
			return defaultVal, nil
		}

		// Must be a number
		num, ok := val.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("slice indices must be integers or None, not %s", val.Type())
		}

		i := int(num)
		return &i, nil
	}

	// Get slice parameters
	start, err := getInt(args[1], nil)
	if err != nil {
		return nil, fmt.Errorf("error evaluating start: %v", err)
	}

	end, err := getInt(args[2], nil)
	if err != nil {
		return nil, fmt.Errorf("error evaluating end: %v", err)
	}

	defaultStep := 1
	step, err := getInt(args[3], &defaultStep)
	if err != nil {
		return nil, fmt.Errorf("error evaluating step: %v", err)
	}

	if step != nil && *step == 0 {
		return nil, fmt.Errorf("slice step cannot be zero")
	}

	// Handle different object types
	switch v := obj.(type) {
	case core.ListValue:
		return sliceList(v, start, end, step)

	case core.TupleValue:
		result, err := sliceList(core.ListValue(v), start, end, step)
		if err != nil {
			return nil, err
		}
		// Convert back to tuple
		if list, ok := result.(core.ListValue); ok {
			return core.TupleValue(list), nil
		}
		return result, nil

	case core.StringValue:
		return sliceString(string(v), start, end, step)

	default:
		return nil, fmt.Errorf("'%s' object is not subscriptable", obj.Type())
	}
}

// sliceList performs slicing on a list
func sliceList(list core.ListValue, start, end, step *int) (core.Value, error) {
	length := len(list)

	// Normalize indices
	startIdx, endIdx, stepVal := normalizeSliceIndices(length, start, end, step)

	// Build result
	result := make(core.ListValue, 0)

	if stepVal > 0 {
		for i := startIdx; i < endIdx; i += stepVal {
			if i >= 0 && i < length {
				result = append(result, list[i])
			}
		}
	} else {
		for i := startIdx; i > endIdx; i += stepVal {
			if i >= 0 && i < length {
				result = append(result, list[i])
			}
		}
	}

	return result, nil
}

// sliceString performs slicing on a string
func sliceString(str string, start, end, step *int) (core.Value, error) {
	length := len(str)

	// Normalize indices
	startIdx, endIdx, stepVal := normalizeSliceIndices(length, start, end, step)

	// Build result
	result := ""

	if stepVal > 0 {
		for i := startIdx; i < endIdx; i += stepVal {
			if i >= 0 && i < length {
				result += string(str[i])
			}
		}
	} else {
		for i := startIdx; i > endIdx; i += stepVal {
			if i >= 0 && i < length {
				result += string(str[i])
			}
		}
	}

	return core.StringValue(result), nil
}

// normalizeSliceIndices converts slice parameters to normalized indices
func normalizeSliceIndices(length int, start, end, step *int) (int, int, int) {
	// Get step value
	stepVal := 1
	if step != nil {
		stepVal = *step
	}

	// Determine defaults based on step direction
	var startIdx, endIdx int

	if stepVal > 0 {
		// Forward slice
		startIdx = 0
		endIdx = length

		if start != nil {
			startIdx = *start
			if startIdx < 0 {
				startIdx += length
			}
			if startIdx < 0 {
				startIdx = 0
			}
			if startIdx > length {
				startIdx = length
			}
		}

		if end != nil {
			endIdx = *end
			if endIdx < 0 {
				endIdx += length
			}
			if endIdx < 0 {
				endIdx = 0
			}
			if endIdx > length {
				endIdx = length
			}
		}
	} else {
		// Backward slice
		startIdx = length - 1
		endIdx = -length - 1

		if start != nil {
			startIdx = *start
			if startIdx < 0 {
				startIdx += length
			}
			if startIdx < -1 {
				startIdx = -1
			}
			if startIdx >= length {
				startIdx = length - 1
			}
		}

		if end != nil {
			endIdx = *end
			if endIdx < 0 {
				endIdx += length
			}
			// endIdx is already exclusive, no need to adjust
		}
	}

	return startIdx, endIdx, stepVal
}

// handleSliceObject handles slicing with a slice object
func handleSliceObject(obj core.Value, slice *core.SliceValue) (core.Value, error) {
	// Extract slice parameters
	var start, stop, step *int

	// Convert slice values to integers
	if slice.Start != nil && slice.Start != core.Nil {
		if num, ok := slice.Start.(core.NumberValue); ok {
			val := int(num)
			start = &val
		} else {
			return nil, fmt.Errorf("slice indices must be integers or None")
		}
	}

	if slice.Stop != nil && slice.Stop != core.Nil {
		if num, ok := slice.Stop.(core.NumberValue); ok {
			val := int(num)
			stop = &val
		} else {
			return nil, fmt.Errorf("slice indices must be integers or None")
		}
	}

	if slice.Step != nil && slice.Step != core.Nil {
		if num, ok := slice.Step.(core.NumberValue); ok {
			val := int(num)
			if val == 0 {
				return nil, fmt.Errorf("slice step cannot be zero")
			}
			step = &val
		} else {
			return nil, fmt.Errorf("slice indices must be integers or None")
		}
	}

	// Handle different object types
	switch v := obj.(type) {
	case core.ListValue:
		return sliceList(v, start, stop, step)
	case core.TupleValue:
		result, err := sliceList(core.ListValue(v), start, stop, step)
		if err != nil {
			return nil, err
		}
		// Convert result back to tuple
		if list, ok := result.(core.ListValue); ok {
			return core.TupleValue(list), nil
		}
		return result, nil
	case core.StringValue:
		return sliceString(string(v), start, stop, step)
	default:
		return nil, fmt.Errorf("'%s' object is not subscriptable", v.Type())
	}
}

// DelItemForm implements the del-item special form for index deletion
// (del-item obj key) -> nil
func DelItemForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("del-item requires exactly 2 arguments, got %d", len(args))
	}

	// Evaluate the object
	obj, err := Eval(args[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Evaluate the key/index
	key, err := Eval(args[1], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating key: %v", err)
	}

	// First, try dunder method __delitem__
	if found, err := types.CallDelItem(obj, key, ctx); found {
		if err != nil {
			return nil, err
		}
		return core.Nil, nil
	}

	// Then try protocol-based indexing
	if indexable, ok := protocols.GetIndexableOps(obj); ok {
		err = indexable.DeleteIndex(key)
		if err != nil {
			return nil, err
		}
		return core.Nil, nil
	}

	// Fall back to error
	return nil, fmt.Errorf("'%s' object does not support item deletion", obj.Type())
}

// RegisterIndexing registers the indexing special forms
func RegisterIndexing() {
	RegisterSpecialForm("get-item", GetItemForm)
	RegisterSpecialForm("set-item", SetItemForm)
	RegisterSpecialForm("del-item", DelItemForm)
	RegisterSpecialForm("__slice__", SliceForm) // Internal name to avoid conflict with slice() builtin
}

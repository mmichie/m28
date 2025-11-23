package eval

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// handleSliceAssignment handles slice assignment (obj[start:end] = values)
func handleSliceAssignment(obj core.Value, slice *core.SliceValue, value core.Value) (core.Value, error) {
	// Only support slice assignment for lists
	list, ok := obj.(*core.ListValue)
	if !ok {
		return nil, fmt.Errorf("slice assignment only supported for lists, not %s", obj.Type())
	}

	// Extract slice parameters
	var start, stop *int

	// Convert slice values to integers
	if slice.Start != nil && slice.Start != core.Nil {
		val, err := types.ToIndex(slice.Start, nil)
		if err != nil {
			return nil, err
		}
		start = &val
	}

	if slice.Stop != nil && slice.Stop != core.Nil {
		val, err := types.ToIndex(slice.Stop, nil)
		if err != nil {
			return nil, err
		}
		stop = &val
	}

	// For now, only support step=None (which means step=1)
	if slice.Step != nil && slice.Step != core.Nil {
		return nil, fmt.Errorf("slice assignment with step not yet supported")
	}

	// Convert value to a list of values to insert
	var valuesList *core.ListValue
	switch v := value.(type) {
	case *core.ListValue:
		valuesList = v
	case core.TupleValue:
		valuesList = core.NewList([]core.Value(v)...)
	case core.StringValue:
		// String unpacking: each character becomes an element
		items := make([]core.Value, 0, len(string(v)))
		for _, ch := range string(v) {
			items = append(items, core.StringValue(string(ch)))
		}
		valuesList = core.NewList(items...)
	default:
		// Try to iterate
		if iterable, ok := value.(core.Iterable); ok {
			items := make([]core.Value, 0)
			iter := iterable.Iterator()
			for {
				item, ok := iter.Next()
				if !ok {
					break
				}
				items = append(items, item)
			}
			valuesList = core.NewList(items...)
		} else {
			return nil, fmt.Errorf("can only assign an iterable to a slice")
		}
	}

	// Use the built-in SetSlice method
	err := list.SetSlice(start, stop, valuesList)
	if err != nil {
		return nil, err
	}

	// Return the assigned value to support chained assignments
	return value, nil
}

// GetItemForm implements the get-item special form for index access
// (get-item obj key) -> value at index/key
func GetItemForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, fmt.Errorf("get-item requires exactly 2 arguments, got %d", args.Len())
	}

	// Evaluate the object
	obj, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Evaluate the key/index
	key, err := Eval(args.Items()[1], ctx)
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
	return nil, fmt.Errorf("'%s' object is not subscriptable", core.GetPythonTypeName(obj))
}

// SetItemForm implements the set-item special form for index assignment
// (set-item obj key value) -> value
func SetItemForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 3 {
		return nil, fmt.Errorf("set-item requires exactly 3 arguments, got %d", args.Len())
	}

	// WORKAROUND: Check if value argument is a list literal that lost its "list-literal" symbol
	// This can happen due to a transpiler bug with slice assignment
	// Only wrap if the first element is NOT a symbol (i.e., it's definitely data, not code)
	valueArg := args.Items()[2]
	if listVal, ok := valueArg.(*core.ListValue); ok {
		if listVal.Len() > 0 {
			// Only wrap if first element is NOT a symbol
			if _, ok := listVal.Items()[0].(core.SymbolValue); !ok {
				// First element is not a symbol, so this is a list literal with data
				valueArg = core.NewList(append([]core.Value{core.SymbolValue("list-literal")}, listVal.Items()...)...)
			}
		}
	}

	// Evaluate all arguments
	obj, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	key, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating key: %v", err)
	}

	value, err := Eval(valueArg, ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating value: %v", err)
	}

	// Handle slice assignment (obj[start:end] = values)
	if sliceVal, ok := key.(*core.SliceValue); ok {
		return handleSliceAssignment(obj, sliceVal, value)
	}

	// fmt.Printf("DEBUG SetItemForm: obj type=%s, key=%v, value=%v\n", obj.Type(), key, value)

	// First, try dunder method __setitem__
	if found, err := types.CallSetItem(obj, key, value, ctx); found {
		// fmt.Printf("DEBUG: CallSetItem found __setitem__, err=%v\n", err)
		if err != nil {
			return nil, err
		}
		// Return the assigned value to support chained assignments
		return value, nil
	}
	// fmt.Printf("DEBUG: CallSetItem did not find __setitem__, trying protocol\n")

	// Then try protocol-based indexing
	if indexable, ok := protocols.GetIndexableOps(obj); ok {
		err = indexable.SetIndex(key, value)
		if err != nil {
			return nil, err
		}
		// Return the assigned value to support chained assignments
		return value, nil
	}

	// Fall back to error (shouldn't reach here with proper protocol support)
	return nil, fmt.Errorf("'%s' object does not support item assignment", obj.Type())
}

// SliceForm implements the slice special form
// (slice obj start end step) -> sliced object
func SliceForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 4 {
		return nil, fmt.Errorf("slice requires exactly 4 arguments, got %d", args.Len())
	}

	// Evaluate the object
	obj, err := Eval(args.Items()[0], ctx)
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

		// Use ToIndex to convert value to integer (supports __index__)
		i, err := types.ToIndex(val, ctx)
		if err != nil {
			return nil, err
		}

		return &i, nil
	}

	// Get slice parameters
	start, err := getInt(args.Items()[1], nil)
	if err != nil {
		return nil, fmt.Errorf("error evaluating start: %v", err)
	}

	end, err := getInt(args.Items()[2], nil)
	if err != nil {
		return nil, fmt.Errorf("error evaluating end: %v", err)
	}

	defaultStep := 1
	step, err := getInt(args.Items()[3], &defaultStep)
	if err != nil {
		return nil, fmt.Errorf("error evaluating step: %v", err)
	}

	if step != nil && *step == 0 {
		return nil, fmt.Errorf("slice step cannot be zero")
	}

	// Handle different object types
	switch v := obj.(type) {
	case *core.ListValue:
		return sliceList(v.Items(), start, end, step)

	case core.TupleValue:
		result, err := sliceList(v, start, end, step)
		if err != nil {
			return nil, err
		}
		// Convert back to tuple
		if list, ok := result.(*core.ListValue); ok {
			return core.TupleValue(list.Items()), nil
		}
		return result, nil

	case core.StringValue:
		return sliceString(string(v), start, end, step)

	case core.BytesValue:
		return sliceBytes(v, start, end, step)

	case *core.ByteArrayValue:
		return sliceByteArray(v, start, end, step)

	default:
		return nil, fmt.Errorf("'%s' object is not subscriptable", core.GetPythonTypeName(obj))
	}
}

// sliceList performs slicing on a list
func sliceList(list []core.Value, start, end, step *int) (core.Value, error) {
	length := len(list)

	// Normalize indices
	startIdx, endIdx, stepVal := normalizeSliceIndices(length, start, end, step)

	// Build result
	result := make([]core.Value, 0)

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

	return core.NewList(result...), nil
}

// sliceString performs slicing on a string (rune-based for Unicode support)
func sliceString(str string, start, end, step *int) (core.Value, error) {
	// Convert to runes for proper Unicode indexing
	runes := []rune(str)
	length := len(runes)

	// Normalize indices
	startIdx, endIdx, stepVal := normalizeSliceIndices(length, start, end, step)

	// Build result using rune slice
	var resultRunes []rune

	if stepVal > 0 {
		for i := startIdx; i < endIdx; i += stepVal {
			if i >= 0 && i < length {
				resultRunes = append(resultRunes, runes[i])
			}
		}
	} else {
		for i := startIdx; i > endIdx; i += stepVal {
			if i >= 0 && i < length {
				resultRunes = append(resultRunes, runes[i])
			}
		}
	}

	return core.StringValue(string(resultRunes)), nil
}

// sliceBytes performs slicing on bytes
func sliceBytes(bytes core.BytesValue, start, end, step *int) (core.Value, error) {
	length := len(bytes)

	// Normalize indices
	startIdx, endIdx, stepVal := normalizeSliceIndices(length, start, end, step)

	// Build result
	result := make([]byte, 0)

	if stepVal > 0 {
		for i := startIdx; i < endIdx; i += stepVal {
			if i >= 0 && i < length {
				result = append(result, bytes[i])
			}
		}
	} else {
		for i := startIdx; i > endIdx; i += stepVal {
			if i >= 0 && i < length {
				result = append(result, bytes[i])
			}
		}
	}

	return core.BytesValue(result), nil
}

// sliceByteArray performs slicing on bytearray
func sliceByteArray(ba *core.ByteArrayValue, start, end, step *int) (core.Value, error) {
	data := ba.GetData() // We'll need to add this getter method
	length := len(data)

	// Normalize indices
	startIdx, endIdx, stepVal := normalizeSliceIndices(length, start, end, step)

	// Build result
	result := make([]byte, 0)

	if stepVal > 0 {
		for i := startIdx; i < endIdx; i += stepVal {
			if i >= 0 && i < length {
				result = append(result, data[i])
			}
		}
	} else {
		for i := startIdx; i > endIdx; i += stepVal {
			if i >= 0 && i < length {
				result = append(result, data[i])
			}
		}
	}

	return core.NewByteArray(result), nil
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

	// Convert slice values to integers using ToIndex (supports __index__)
	if slice.Start != nil && slice.Start != core.Nil {
		val, err := types.ToIndex(slice.Start, nil)
		if err != nil {
			return nil, err
		}
		start = &val
	}

	if slice.Stop != nil && slice.Stop != core.Nil {
		val, err := types.ToIndex(slice.Stop, nil)
		if err != nil {
			return nil, err
		}
		stop = &val
	}

	if slice.Step != nil && slice.Step != core.Nil {
		val, err := types.ToIndex(slice.Step, nil)
		if err != nil {
			return nil, err
		}
		if val == 0 {
			return nil, fmt.Errorf("slice step cannot be zero")
		}
		step = &val
	}

	// Handle different object types
	switch v := obj.(type) {
	case *core.ListValue:
		return sliceList(v.Items(), start, stop, step)
	case core.TupleValue:
		result, err := sliceList(v, start, stop, step)
		if err != nil {
			return nil, err
		}
		// Convert result back to tuple
		if list, ok := result.(*core.ListValue); ok {
			return core.TupleValue(list.Items()), nil
		}
		return result, nil
	case core.StringValue:
		return sliceString(string(v), start, stop, step)
	case core.BytesValue:
		return sliceBytes(v, start, stop, step)
	case *core.ByteArrayValue:
		return sliceByteArray(v, start, stop, step)
	default:
		return nil, fmt.Errorf("'%s' object is not subscriptable", core.GetPythonTypeName(v))
	}
}

// DelItemForm implements the del-item special form for index deletion
// (del-item obj key) -> nil
func DelItemForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, fmt.Errorf("del-item requires exactly 2 arguments, got %d", args.Len())
	}

	// Evaluate the object
	obj, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}

	// Evaluate the key/index
	key, err := Eval(args.Items()[1], ctx)
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
	// Note: slice() builtin function is registered in builtin/collections.go
}

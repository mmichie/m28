package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
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

	// Handle different object types
	switch v := obj.(type) {
	case core.ListValue:
		// List indexing requires numeric index
		idx, ok := key.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("list indices must be integers, not %s", key.Type())
		}
		
		index := int(idx)
		if index < 0 {
			index = len(v) + index
		}
		
		if index < 0 || index >= len(v) {
			return nil, &core.IndexError{Index: index, Length: len(v)}
		}
		
		return v[index], nil

	case core.TupleValue:
		// Tuple indexing
		idx, ok := key.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("tuple indices must be integers, not %s", key.Type())
		}
		
		index := int(idx)
		if index < 0 {
			index = len(v) + index
		}
		
		if index < 0 || index >= len(v) {
			return nil, &core.IndexError{Index: index, Length: len(v)}
		}
		
		return v[index], nil

	case *core.DictValue:
		// Dictionary key access
		// Convert key to string
		keyStr := ""
		switch k := key.(type) {
		case core.StringValue:
			keyStr = string(k)
		case core.NumberValue:
			keyStr = fmt.Sprintf("%g", float64(k))
		case core.SymbolValue:
			keyStr = string(k)
		default:
			keyStr = key.String()
		}
		
		// Check if key exists
		if val, exists := v.Get(keyStr); exists {
			return val, nil
		}
		
		// If not found, return KeyError
		return nil, &core.KeyError{Key: key}

	case core.StringValue:
		// String indexing
		idx, ok := key.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("string indices must be integers, not %s", key.Type())
		}
		
		str := string(v)
		index := int(idx)
		if index < 0 {
			index = len(str) + index
		}
		
		if index < 0 || index >= len(str) {
			return nil, &core.IndexError{Index: index, Length: len(str)}
		}
		
		return core.StringValue(str[index:index+1]), nil

	default:
		return nil, fmt.Errorf("'%s' object is not subscriptable", obj.Type())
	}
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

	// Handle different object types
	switch v := obj.(type) {
	case core.ListValue:
		// List assignment requires numeric index
		idx, ok := key.(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("list indices must be integers, not %s", key.Type())
		}
		
		index := int(idx)
		if index < 0 {
			index = len(v) + index
		}
		
		if index < 0 || index >= len(v) {
			return nil, &core.IndexError{Index: index, Length: len(v)}
		}
		
		v[index] = value
		return value, nil

	case *core.DictValue:
		// Dictionary key assignment
		keyStr := ""
		switch k := key.(type) {
		case core.StringValue:
			keyStr = string(k)
		case core.NumberValue:
			keyStr = fmt.Sprintf("%g", float64(k))
		case core.SymbolValue:
			keyStr = string(k)
		default:
			keyStr = key.String()
		}
		
		v.Set(keyStr, value)
		return value, nil

	default:
		return nil, fmt.Errorf("'%s' object does not support item assignment", obj.Type())
	}
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

// RegisterIndexing registers the indexing special forms
func RegisterIndexing() {
	RegisterSpecialForm("get-item", GetItemForm)
	RegisterSpecialForm("set-item", SetItemForm)
	RegisterSpecialForm("slice", SliceForm)
}
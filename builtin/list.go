// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterListFunctions registers list-related functions in the global context
func RegisterListFunctions(ctx *core.Context) {
	// List creation and conversion
	ctx.Define("list", core.NewBuiltinFunction(ListFunc))
	ctx.Define("range", core.NewBuiltinFunction(RangeFunc))

	// List operations
	ctx.Define("first", core.NewBuiltinFunction(FirstFunc))
	ctx.Define("rest", core.NewBuiltinFunction(RestFunc))
	ctx.Define("nth", core.NewBuiltinFunction(NthFunc))
	ctx.Define("append", core.NewBuiltinFunction(AppendFunc))
	ctx.Define("concat", core.NewBuiltinFunction(ConcatFunc))
	ctx.Define("length", core.NewBuiltinFunction(LengthFunc))
	ctx.Define("map", core.NewBuiltinFunction(MapFunc))
	ctx.Define("filter", core.NewBuiltinFunction(FilterFunc))
	ctx.Define("reduce", core.NewBuiltinFunction(ReduceFunc))
	ctx.Define("sorted", core.NewBuiltinFunction(SortedFunc))
	ctx.Define("reversed", core.NewBuiltinFunction(ReversedFunc))
}

// ListFunc creates a list from the given arguments
func ListFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.ListValue(args), nil
}

// RangeFunc creates a list of numbers from start to end
func RangeFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 3 {
		return nil, fmt.Errorf("range requires 1-3 arguments: end or start, end[, step]")
	}

	var start, end, step float64 = 0, 0, 1

	// Parse arguments
	switch len(args) {
	case 1:
		// Only end provided, start = 0, step = 1
		if num, ok := args[0].(core.NumberValue); ok {
			end = float64(num)
		} else {
			return nil, fmt.Errorf("end must be a number")
		}
	case 2:
		// Start and end provided, step = 1
		if num, ok := args[0].(core.NumberValue); ok {
			start = float64(num)
		} else {
			return nil, fmt.Errorf("start must be a number")
		}
		if num, ok := args[1].(core.NumberValue); ok {
			end = float64(num)
		} else {
			return nil, fmt.Errorf("end must be a number")
		}
	case 3:
		// Start, end, and step provided
		if num, ok := args[0].(core.NumberValue); ok {
			start = float64(num)
		} else {
			return nil, fmt.Errorf("start must be a number")
		}
		if num, ok := args[1].(core.NumberValue); ok {
			end = float64(num)
		} else {
			return nil, fmt.Errorf("end must be a number")
		}
		if num, ok := args[2].(core.NumberValue); ok {
			step = float64(num)
			if step == 0 {
				return nil, fmt.Errorf("step cannot be zero")
			}
		} else {
			return nil, fmt.Errorf("step must be a number")
		}
	}

	// Create the list
	result := make(core.ListValue, 0)

	// Handle positive and negative steps properly
	if step > 0 {
		for i := start; i < end; i += step {
			result = append(result, core.NumberValue(i))
		}
	} else {
		for i := start; i > end; i += step {
			result = append(result, core.NumberValue(i))
		}
	}

	return result, nil
}

// FirstFunc returns the first element of a list
func FirstFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("first requires 1 argument")
	}

	switch v := args[0].(type) {
	case core.ListValue:
		if len(v) == 0 {
			return core.Nil, nil
		}
		return v[0], nil
	case core.TupleValue:
		if len(v) == 0 {
			return core.Nil, nil
		}
		return v[0], nil
	case core.StringValue:
		if len(v) == 0 {
			return core.StringValue(""), nil
		}
		return core.StringValue(string(v)[0:1]), nil
	default:
		return nil, fmt.Errorf("first expects a sequence, got %s", v.Type())
	}
}

// RestFunc returns all elements after the first element
func RestFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("rest requires 1 argument")
	}

	switch v := args[0].(type) {
	case core.ListValue:
		if len(v) <= 1 {
			return core.ListValue{}, nil
		}
		return v[1:], nil
	case core.TupleValue:
		if len(v) <= 1 {
			return core.TupleValue{}, nil
		}
		return v[1:], nil
	case core.StringValue:
		if len(v) <= 1 {
			return core.StringValue(""), nil
		}
		return core.StringValue(string(v)[1:]), nil
	default:
		return nil, fmt.Errorf("rest expects a sequence, got %s", v.Type())
	}
}

// NthFunc returns the nth element of a list
func NthFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("nth requires 2 arguments")
	}

	// Get the index
	var idx int
	if num, ok := args[1].(core.NumberValue); ok {
		idx = int(num)
		if idx < 0 {
			return nil, fmt.Errorf("index must be non-negative")
		}
	} else {
		return nil, fmt.Errorf("index must be a number")
	}

	// Get the nth element based on the sequence type
	switch v := args[0].(type) {
	case core.ListValue:
		if idx >= len(v) {
			return nil, fmt.Errorf("index out of bounds: %d", idx)
		}
		return v[idx], nil
	case core.TupleValue:
		if idx >= len(v) {
			return nil, fmt.Errorf("index out of bounds: %d", idx)
		}
		return v[idx], nil
	case core.StringValue:
		if idx >= len(v) {
			return nil, fmt.Errorf("index out of bounds: %d", idx)
		}
		return core.StringValue(string(v)[idx : idx+1]), nil
	default:
		return nil, fmt.Errorf("nth expects a sequence, got %s", v.Type())
	}
}

// AppendFunc appends an element to a list
func AppendFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("append requires at least 2 arguments")
	}

	switch list := args[0].(type) {
	case core.ListValue:
		return core.ListValue(append(list, args[1:]...)), nil
	case core.TupleValue:
		return core.TupleValue(append(list, args[1:]...)), nil
	default:
		return nil, fmt.Errorf("append expects a list or tuple, got %s", list.Type())
	}
}

// ConcatFunc concatenates multiple lists
func ConcatFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("concat requires at least 1 argument")
	}

	// Determine the type of the first argument
	switch first := args[0].(type) {
	case core.ListValue:
		// Concatenate lists
		result := make(core.ListValue, len(first))
		copy(result, first)

		for _, arg := range args[1:] {
			if list, ok := arg.(core.ListValue); ok {
				result = append(result, list...)
			} else {
				return nil, fmt.Errorf("cannot concatenate %s to list", arg.Type())
			}
		}
		return result, nil

	case core.TupleValue:
		// Concatenate tuples
		result := make(core.TupleValue, len(first))
		copy(result, first)

		for _, arg := range args[1:] {
			if tuple, ok := arg.(core.TupleValue); ok {
				result = append(result, tuple...)
			} else {
				return nil, fmt.Errorf("cannot concatenate %s to tuple", arg.Type())
			}
		}
		return result, nil

	case core.StringValue:
		// Concatenate strings
		result := string(first)
		for _, arg := range args[1:] {
			if str, ok := arg.(core.StringValue); ok {
				result += string(str)
			} else {
				return nil, fmt.Errorf("cannot concatenate %s to string", arg.Type())
			}
		}
		return core.StringValue(result), nil

	default:
		return nil, fmt.Errorf("concat expects a sequence, got %s", first.Type())
	}
}

// LengthFunc returns the length of a list
func LengthFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("length requires 1 argument")
	}

	switch v := args[0].(type) {
	case core.ListValue:
		return core.NumberValue(len(v)), nil
	case core.TupleValue:
		return core.NumberValue(len(v)), nil
	case core.StringValue:
		return core.NumberValue(len(v)), nil
	case *core.DictValue:
		return core.NumberValue(v.Size()), nil
	case *core.SetValue:
		return core.NumberValue(v.Size()), nil
	default:
		return nil, fmt.Errorf("length expects a collection, got %s", v.Type())
	}
}

// MapFunc applies a function to each element of a list
func MapFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("map requires 2 arguments")
	}

	// Get the function
	fn, ok := args[0].(core.Callable)
	if !ok {
		return nil, fmt.Errorf("first argument must be callable")
	}

	// Apply the function to each element based on collection type
	switch coll := args[1].(type) {
	case core.ListValue:
		result := make(core.ListValue, len(coll))
		for i, item := range coll {
			mappedVal, err := fn.Call([]core.Value{item}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error applying function: %v", err)
			}
			result[i] = mappedVal
		}
		return result, nil

	case core.TupleValue:
		result := make(core.TupleValue, len(coll))
		for i, item := range coll {
			mappedVal, err := fn.Call([]core.Value{item}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error applying function: %v", err)
			}
			result[i] = mappedVal
		}
		return result, nil

	default:
		return nil, fmt.Errorf("map expects a list or tuple, got %s", coll.Type())
	}
}

// FilterFunc filters a list based on a predicate function
func FilterFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("filter requires 2 arguments")
	}

	// Get the predicate function
	pred, ok := args[0].(core.Callable)
	if !ok {
		return nil, fmt.Errorf("first argument must be callable")
	}

	// Filter elements based on collection type
	switch coll := args[1].(type) {
	case core.ListValue:
		result := make(core.ListValue, 0)
		for _, item := range coll {
			testResult, err := pred.Call([]core.Value{item}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error applying predicate: %v", err)
			}

			if core.IsTruthy(testResult) {
				result = append(result, item)
			}
		}
		return result, nil

	case core.TupleValue:
		result := make(core.TupleValue, 0)
		for _, item := range coll {
			testResult, err := pred.Call([]core.Value{item}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error applying predicate: %v", err)
			}

			if core.IsTruthy(testResult) {
				result = append(result, item)
			}
		}
		return result, nil

	default:
		return nil, fmt.Errorf("filter expects a list or tuple, got %s", coll.Type())
	}
}

// ReduceFunc reduces a list to a single value by applying a function
func ReduceFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("reduce requires 2 or 3 arguments")
	}

	// Get the reducing function
	fn, ok := args[0].(core.Callable)
	if !ok {
		return nil, fmt.Errorf("first argument must be callable")
	}

	// Get the collection
	var coll interface{}
	switch c := args[1].(type) {
	case core.ListValue:
		coll = c
	case core.TupleValue:
		coll = c
	default:
		return nil, fmt.Errorf("reduce expects a list or tuple, got %s", args[1].Type())
	}

	// Get initial value and first element index
	var result core.Value
	var startIdx int

	if len(args) == 3 {
		// Initial value provided
		result = args[2]
		startIdx = 0
	} else {
		// No initial value, use first element
		switch c := coll.(type) {
		case core.ListValue:
			if len(c) == 0 {
				return nil, fmt.Errorf("cannot reduce empty list without initial value")
			}
			result = c[0]
		case core.TupleValue:
			if len(c) == 0 {
				return nil, fmt.Errorf("cannot reduce empty tuple without initial value")
			}
			result = c[0]
		}
		startIdx = 1
	}

	// Reduce the collection
	var err error
	switch c := coll.(type) {
	case core.ListValue:
		for i := startIdx; i < len(c); i++ {
			result, err = fn.Call([]core.Value{result, c[i]}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in reduce function: %v", err)
			}
		}
	case core.TupleValue:
		for i := startIdx; i < len(c); i++ {
			result, err = fn.Call([]core.Value{result, c[i]}, ctx)
			if err != nil {
				return nil, fmt.Errorf("error in reduce function: %v", err)
			}
		}
	}

	return result, nil
}

// SortedFunc returns a sorted copy of a list
func SortedFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("sorted requires 1 or 2 arguments")
	}

	// Get the sequence to sort
	var items []core.Value
	switch v := args[0].(type) {
	case core.ListValue:
		// Make a copy
		items = make([]core.Value, len(v))
		copy(items, v)
	case core.TupleValue:
		// Convert to list
		items = make([]core.Value, len(v))
		copy(items, v)
	case core.StringValue:
		// Convert string to list of characters
		items = make([]core.Value, 0, len(v))
		for _, ch := range string(v) {
			items = append(items, core.StringValue(string(ch)))
		}
	default:
		return nil, fmt.Errorf("sorted expects a sequence, got %s", v.Type())
	}

	// Check for reverse parameter
	reverse := false
	if len(args) == 2 {
		// For now, assume second arg is reverse=True/False
		if b, ok := args[1].(core.BoolValue); ok {
			reverse = bool(b)
		}
	}

	// Sort using a simple comparison
	// TODO: This is a basic implementation, should handle custom key functions
	n := len(items)
	for i := 0; i < n-1; i++ {
		for j := 0; j < n-i-1; j++ {
			shouldSwap := false

			// Compare based on type
			switch a := items[j].(type) {
			case core.NumberValue:
				if b, ok := items[j+1].(core.NumberValue); ok {
					if reverse {
						shouldSwap = a < b
					} else {
						shouldSwap = a > b
					}
				}
			case core.StringValue:
				if b, ok := items[j+1].(core.StringValue); ok {
					if reverse {
						shouldSwap = string(a) < string(b)
					} else {
						shouldSwap = string(a) > string(b)
					}
				}
			}

			if shouldSwap {
				items[j], items[j+1] = items[j+1], items[j]
			}
		}
	}

	// Return as a list
	return core.ListValue(items), nil
}

// ReversedFunc returns a reversed copy of a sequence
func ReversedFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("reversed requires 1 argument")
	}

	switch v := args[0].(type) {
	case core.ListValue:
		// Create reversed copy
		result := make(core.ListValue, len(v))
		for i := 0; i < len(v); i++ {
			result[i] = v[len(v)-1-i]
		}
		return result, nil

	case core.TupleValue:
		// Return reversed as list
		result := make(core.ListValue, len(v))
		for i := 0; i < len(v); i++ {
			result[i] = v[len(v)-1-i]
		}
		return result, nil

	case core.StringValue:
		// Reverse string
		runes := []rune(string(v))
		for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
			runes[i], runes[j] = runes[j], runes[i]
		}
		return core.StringValue(string(runes)), nil

	default:
		return nil, fmt.Errorf("reversed expects a sequence, got %s", v.Type())
	}
}

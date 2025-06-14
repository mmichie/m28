// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterListFunctions registers list-related functions in the global context
func RegisterListFunctions(ctx *core.Context) {
	// List creation and conversion
	// list is now registered in collections.go with better implementation
	ctx.Define("range", core.NewBuiltinFunction(RangeFunc))

	// List operations
	ctx.Define("first", core.NewBuiltinFunction(FirstFunc))
	ctx.Define("rest", core.NewBuiltinFunction(RestFunc))
	ctx.Define("nth", core.NewBuiltinFunction(NthFunc))
	ctx.Define("append", core.NewBuiltinFunction(AppendFunc))
	ctx.Define("concat", core.NewBuiltinFunction(ConcatFunc))
	ctx.Define("length", core.NewBuiltinFunction(LengthFunc))
	// map, filter, reduce are now registered in functional.go
	ctx.Define("sorted", NewKwargsBuiltinFunction("sorted", SortedWithKwargs))
	ctx.Define("reversed", core.NewBuiltinFunction(ReversedFunc))
}

// RangeFunc creates a lazy range object
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

	// Create and return a range object
	rangeVal, err := core.NewRangeValue(start, end, step)
	if err != nil {
		return nil, err
	}
	return rangeVal, nil
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

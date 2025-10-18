// Package types provides utilities for type checking and conversion in M28
package types

import (
	"github.com/mmichie/m28/core"
)

// TypeSwitch provides a fluent interface for type-based branching
type TypeSwitch struct {
	value   core.Value
	handled bool
	result  core.Value
	err     error
}

// TypeCase represents a single case in a type switch
type TypeCase struct {
	predicate func(core.Value) bool
	handler   func(core.Value) (core.Value, error)
}

// Switch creates a new TypeSwitch for the given value
func Switch(v core.Value) *TypeSwitch {
	return &TypeSwitch{value: v}
}

// Number handles the case where value is a number
func (ts *TypeSwitch) Number(fn func(float64) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if num, ok := AsNumber(ts.value); ok {
		ts.result, ts.err = fn(num)
		ts.handled = true
	}
	return ts
}

// Complex handles the case where value is a complex number
func (ts *TypeSwitch) Complex(fn func(complex128) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if c, ok := AsComplex(ts.value); ok {
		ts.result, ts.err = fn(c)
		ts.handled = true
	}
	return ts
}

// String handles the case where value is a string
func (ts *TypeSwitch) String(fn func(string) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if str, ok := AsString(ts.value); ok {
		ts.result, ts.err = fn(str)
		ts.handled = true
	}
	return ts
}

// Bool handles the case where value is a boolean
func (ts *TypeSwitch) Bool(fn func(bool) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if b, ok := AsBool(ts.value); ok {
		ts.result, ts.err = fn(b)
		ts.handled = true
	}
	return ts
}

// List handles the case where value is a list
func (ts *TypeSwitch) List(fn func(core.ListValue) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if list, ok := AsList(ts.value); ok {
		ts.result, ts.err = fn(list)
		ts.handled = true
	}
	return ts
}

// Dict handles the case where value is a dictionary
func (ts *TypeSwitch) Dict(fn func(*core.DictValue) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if dict, ok := AsDict(ts.value); ok {
		ts.result, ts.err = fn(dict)
		ts.handled = true
	}
	return ts
}

// Tuple handles the case where value is a tuple
func (ts *TypeSwitch) Tuple(fn func(core.TupleValue) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if tuple, ok := AsTuple(ts.value); ok {
		ts.result, ts.err = fn(tuple)
		ts.handled = true
	}
	return ts
}

// Set handles the case where value is a set
func (ts *TypeSwitch) Set(fn func(*core.SetValue) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if set, ok := AsSet(ts.value); ok {
		ts.result, ts.err = fn(set)
		ts.handled = true
	}
	return ts
}

// Callable handles the case where value is callable
func (ts *TypeSwitch) Callable(fn func(core.Callable) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if callable, ok := AsCallable(ts.value); ok {
		ts.result, ts.err = fn(callable)
		ts.handled = true
	}
	return ts
}

// Nil handles the case where value is nil
func (ts *TypeSwitch) Nil(fn func() (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if IsNil(ts.value) {
		ts.result, ts.err = fn()
		ts.handled = true
	}
	return ts
}

// Case handles a custom predicate
func (ts *TypeSwitch) Case(predicate func(core.Value) bool, fn func(core.Value) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if predicate(ts.value) {
		ts.result, ts.err = fn(ts.value)
		ts.handled = true
	}
	return ts
}

// Type handles the case where value matches a specific type
func (ts *TypeSwitch) Type(typeName core.Type, fn func(core.Value) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	if ts.value.Type() == typeName {
		ts.result, ts.err = fn(ts.value)
		ts.handled = true
	}
	return ts
}

// Default handles any value that hasn't been handled yet
func (ts *TypeSwitch) Default(fn func(core.Value) (core.Value, error)) *TypeSwitch {
	if ts.handled {
		return ts
	}

	ts.result, ts.err = fn(ts.value)
	ts.handled = true
	return ts
}

// Result returns the result of the type switch
func (ts *TypeSwitch) Result() (core.Value, error) {
	if !ts.handled {
		return nil, nil
	}
	return ts.result, ts.err
}

// Execute is an alias for Result for better readability
func (ts *TypeSwitch) Execute() (core.Value, error) {
	return ts.Result()
}

// Must panics if there's an error, otherwise returns the result
func (ts *TypeSwitch) Must() core.Value {
	result, err := ts.Result()
	if err != nil {
		panic(err)
	}
	return result
}

// Examples of usage:
//
// // Basic type switch
// result, err := types.Switch(value).
//     Number(func(n float64) (core.Value, error) {
//         return core.NumberValue(n * 2), nil
//     }).
//     String(func(s string) (core.Value, error) {
//         return core.StringValue(s + s), nil
//     }).
//     Default(func(v core.Value) (core.Value, error) {
//         return nil, fmt.Errorf("unsupported type: %s", v.Type())
//     }).
//     Execute()
//
// // Custom predicate
// result, err := types.Switch(value).
//     Case(types.IsContainer, func(v core.Value) (core.Value, error) {
//         // Handle any container type
//         return core.NumberValue(float64(getSize(v))), nil
//     }).
//     Default(func(v core.Value) (core.Value, error) {
//         return core.NumberValue(1), nil
//     }).
//     Execute()

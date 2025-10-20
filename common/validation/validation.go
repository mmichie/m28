// Package validation provides utilities for validating function arguments in M28.
// It eliminates repetitive validation code while maintaining clear error messages.
package validation

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/core"
)

// Args provides a fluent interface for validating function arguments.
// It encapsulates the function name and arguments to provide context in error messages.
type Args struct {
	function string
	args     []core.Value
}

// NewArgs creates a new argument validator for a function.
// Example:
//
//	v := validation.NewArgs("abs", args)
//	if err := v.Exact(1); err != nil {
//	    return nil, err
//	}
func NewArgs(functionName string, args []core.Value) *Args {
	return &Args{
		function: functionName,
		args:     args,
	}
}

// Count validation methods

// Exact validates that exactly n arguments were provided.
func (a *Args) Exact(n int) error {
	if len(a.args) != n {
		return errors.NewArgumentError(a.function, n, len(a.args))
	}
	return nil
}

// Range validates that the argument count is within min and max (inclusive).
func (a *Args) Range(min, max int) error {
	count := len(a.args)
	if count < min || count > max {
		return errors.NewArgumentRangeError(a.function, min, max, count)
	}
	return nil
}

// Min validates that at least min arguments were provided.
func (a *Args) Min(min int) error {
	if len(a.args) < min {
		var message string
		if min == 1 {
			message = "at least 1 argument"
		} else {
			message = "at least " + string(rune(min+'0')) + " arguments"
		}
		return &errors.M28Error{
			Type:     errors.ArgumentError,
			Function: a.function,
			Message:  message + " required",
		}
	}
	return nil
}

// Max validates that at most max arguments were provided.
func (a *Args) Max(max int) error {
	if len(a.args) > max {
		var message string
		if max == 1 {
			message = "at most 1 argument"
		} else {
			message = "at most " + string(rune(max+'0')) + " arguments"
		}
		return &errors.M28Error{
			Type:     errors.ArgumentError,
			Function: a.function,
			Message:  message + " allowed",
		}
	}
	return nil
}

// Type extraction methods

// GetNumber extracts a number from the argument at the given index.
// Returns an error if the index is out of bounds or the value is not a number.
func (a *Args) GetNumber(index int) (float64, error) {
	if index >= len(a.args) || index < 0 {
		return 0, errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	if num, ok := a.args[index].(core.NumberValue); ok {
		return float64(num), nil
	}

	return 0, errors.NewTypeError(a.function, "number", string(a.args[index].Type()))
}

// GetInt extracts an integer from the argument at the given index.
// Returns an error if the value is not an integer.
func (a *Args) GetInt(index int) (int, error) {
	num, err := a.GetNumber(index)
	if err != nil {
		return 0, err
	}

	intVal := int(num)
	if float64(intVal) != num {
		return 0, errors.NewValueErrorf(a.function, "argument %d must be an integer", index+1)
	}

	return intVal, nil
}

// GetString extracts a string from the argument at the given index.
func (a *Args) GetString(index int) (string, error) {
	if index >= len(a.args) || index < 0 {
		return "", errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	if str, ok := a.args[index].(core.StringValue); ok {
		return string(str), nil
	}

	return "", errors.NewTypeError(a.function, "string", string(a.args[index].Type()))
}

// GetBool extracts a boolean from the argument at the given index.
func (a *Args) GetBool(index int) (bool, error) {
	if index >= len(a.args) || index < 0 {
		return false, errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	if b, ok := a.args[index].(core.BoolValue); ok {
		return bool(b), nil
	}

	return false, errors.NewTypeError(a.function, "bool", string(a.args[index].Type()))
}

// GetList extracts a list from the argument at the given index.
func (a *Args) GetList(index int) (*core.ListValue, error) {
	if index >= len(a.args) || index < 0 {
		return nil, errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	if list, ok := a.args[index].(*core.ListValue); ok {
		return list, nil
	}

	return nil, errors.NewTypeError(a.function, "list", string(a.args[index].Type()))
}

// GetDict extracts a dictionary from the argument at the given index.
func (a *Args) GetDict(index int) (*core.DictValue, error) {
	if index >= len(a.args) || index < 0 {
		return nil, errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	if dict, ok := a.args[index].(*core.DictValue); ok {
		return dict, nil
	}

	return nil, errors.NewTypeError(a.function, "dict", string(a.args[index].Type()))
}

// GetCallable extracts a callable (function) from the argument at the given index.
func (a *Args) GetCallable(index int) (core.Callable, error) {
	if index >= len(a.args) || index < 0 {
		return nil, errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	if callable, ok := a.args[index].(core.Callable); ok {
		return callable, nil
	}

	return nil, errors.NewTypeError(a.function, "function", string(a.args[index].Type()))
}

// GetSequence extracts any sequence type (list, tuple, string) from the argument.
// Returns the value and a type indicator.
func (a *Args) GetSequence(index int) (core.Value, error) {
	if index >= len(a.args) || index < 0 {
		return nil, errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	val := a.args[index]
	switch val.(type) {
	case *core.ListValue, core.TupleValue, core.StringValue:
		return val, nil
	default:
		return nil, errors.NewTypeError(a.function, "sequence", string(val.Type()))
	}
}

// GetIterable extracts an iterable value from the argument.
func (a *Args) GetIterable(index int) (core.Value, error) {
	if index >= len(a.args) || index < 0 {
		return nil, errors.NewArgumentError(a.function, index+1, len(a.args))
	}

	val := a.args[index]

	// Check if it implements Iterable interface
	if _, ok := val.(core.Iterable); ok {
		return val, nil
	}

	// Check for types that are iterable but might not implement the interface
	switch val.(type) {
	case *core.ListValue, core.TupleValue, core.StringValue, *core.DictValue, *core.SetValue:
		return val, nil
	default:
		// Check if it has __iter__ method
		if obj, ok := val.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if _, found := obj.GetAttr("__iter__"); found {
				return val, nil
			}
		}
		return nil, errors.NewTypeError(a.function, "iterable", string(val.Type()))
	}
}

// Optional type extraction - returns default if index out of bounds

// GetNumberOrDefault extracts a number or returns a default value.
func (a *Args) GetNumberOrDefault(index int, defaultVal float64) (float64, error) {
	if index >= len(a.args) {
		return defaultVal, nil
	}
	return a.GetNumber(index)
}

// GetStringOrDefault extracts a string or returns a default value.
func (a *Args) GetStringOrDefault(index int, defaultVal string) (string, error) {
	if index >= len(a.args) {
		return defaultVal, nil
	}
	return a.GetString(index)
}

// GetBoolOrDefault extracts a bool or returns a default value.
func (a *Args) GetBoolOrDefault(index int, defaultVal bool) (bool, error) {
	if index >= len(a.args) {
		return defaultVal, nil
	}
	return a.GetBool(index)
}

// Utility methods

// Get returns the raw argument at the given index, or nil if out of bounds.
func (a *Args) Get(index int) core.Value {
	if index >= len(a.args) || index < 0 {
		return nil
	}
	return a.args[index]
}

// Count returns the number of arguments.
func (a *Args) Count() int {
	return len(a.args)
}

// All returns all arguments as a slice.
func (a *Args) All() []core.Value {
	return a.args
}

// paramName generates a parameter name for error messages.
func (a *Args) paramName(index int) string {
	// In the future, this could use actual parameter names if available
	return "argument " + string(rune(index+1+'0'))
}

// Helper functions for common patterns

// ExtractNumbers extracts all arguments as numbers.
// Useful for functions that operate on multiple numeric arguments.
func (a *Args) ExtractNumbers() ([]float64, error) {
	numbers := make([]float64, len(a.args))
	for i, arg := range a.args {
		if num, ok := arg.(core.NumberValue); ok {
			numbers[i] = float64(num)
		} else {
			return nil, errors.NewTypeErrorf(a.function,
				"all arguments must be numbers, argument %d is %s", i+1, string(arg.Type()))
		}
	}
	return numbers, nil
}

// ExtractStrings extracts all arguments as strings.
func (a *Args) ExtractStrings() ([]string, error) {
	strings := make([]string, len(a.args))
	for i, arg := range a.args {
		if str, ok := arg.(core.StringValue); ok {
			strings[i] = string(str)
		} else {
			return nil, errors.NewTypeErrorf(a.function,
				"all arguments must be strings, argument %d is %s", i+1, string(arg.Type()))
		}
	}
	return strings, nil
}

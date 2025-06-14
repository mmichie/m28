// Package builders provides function builders that eliminate boilerplate
// in builtin function implementations. It builds on the validation framework
// to provide a clean, declarative way to define builtin functions.
package builders

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// BuiltinFunc is the standard signature for builtin functions
type BuiltinFunc func([]core.Value, *core.Context) (core.Value, error)

// Builder is the interface for all function builders
type Builder interface {
	Build() BuiltinFunc
}

// baseBuilder provides common functionality for all builders
type baseBuilder struct {
	name string
}

// UnaryNumberFunc creates a function that operates on a single number
func UnaryNumber(name string, fn func(float64) (float64, error)) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		result, err := fn(num)
		if err != nil {
			return nil, errors.Wrap(err, errors.RuntimeError, name)
		}

		return core.NumberValue(result), nil
	}
}

// UnaryNumberSimple is for functions that can't fail
func UnaryNumberSimple(name string, fn func(float64) float64) BuiltinFunc {
	return UnaryNumber(name, func(n float64) (float64, error) {
		return fn(n), nil
	})
}

// BinaryNumber creates a function that operates on two numbers
func BinaryNumber(name string, fn func(float64, float64) (float64, error)) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(2); err != nil {
			return nil, err
		}

		a, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		b, err := v.GetNumber(1)
		if err != nil {
			return nil, err
		}

		result, err := fn(a, b)
		if err != nil {
			return nil, errors.Wrap(err, errors.RuntimeError, name)
		}

		return core.NumberValue(result), nil
	}
}

// BinaryNumberSimple is for functions that can't fail
func BinaryNumberSimple(name string, fn func(float64, float64) float64) BuiltinFunc {
	return BinaryNumber(name, func(a, b float64) (float64, error) {
		return fn(a, b), nil
	})
}

// VariadicNumber creates a function that operates on multiple numbers
func VariadicNumber(name string, minArgs int, fn func([]float64) (float64, error)) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Min(minArgs); err != nil {
			return nil, err
		}

		numbers, err := v.ExtractNumbers()
		if err != nil {
			return nil, err
		}

		result, err := fn(numbers)
		if err != nil {
			return nil, errors.Wrap(err, errors.RuntimeError, name)
		}

		return core.NumberValue(result), nil
	}
}

// UnaryString creates a function that operates on a single string
func UnaryString(name string, fn func(string) (string, error)) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		str, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		result, err := fn(str)
		if err != nil {
			return nil, errors.Wrap(err, errors.RuntimeError, name)
		}

		return core.StringValue(result), nil
	}
}

// UnaryStringSimple is for string functions that can't fail
func UnaryStringSimple(name string, fn func(string) string) BuiltinFunc {
	return UnaryString(name, func(s string) (string, error) {
		return fn(s), nil
	})
}

// BinaryString creates a function that operates on two strings
func BinaryString(name string, fn func(string, string) (string, error)) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(2); err != nil {
			return nil, err
		}

		a, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		b, err := v.GetString(1)
		if err != nil {
			return nil, err
		}

		result, err := fn(a, b)
		if err != nil {
			return nil, errors.Wrap(err, errors.RuntimeError, name)
		}

		return core.StringValue(result), nil
	}
}

// PredicateNumber creates a function that tests a number condition
func PredicateNumber(name string, fn func(float64) bool) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		if fn(num) {
			return core.True, nil
		}
		return core.False, nil
	}
}

// PredicateString creates a function that tests a string condition
func PredicateString(name string, fn func(string) bool) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		str, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		if fn(str) {
			return core.True, nil
		}
		return core.False, nil
	}
}

// UnarySequence creates a function that operates on any sequence type
func UnarySequence(name string, fn func(core.Value) (core.Value, error)) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		seq, err := v.GetSequence(0)
		if err != nil {
			return nil, err
		}

		result, err := fn(seq)
		if err != nil {
			return nil, errors.Wrap(err, errors.RuntimeError, name)
		}

		return result, nil
	}
}

// UnaryAny creates a function that accepts any single argument
func UnaryAny(name string, fn func(core.Value) (core.Value, error)) BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		result, err := fn(v.Get(0))
		if err != nil {
			return nil, errors.Wrap(err, errors.RuntimeError, name)
		}

		return result, nil
	}
}

// WithOptional creates a function with an optional second argument
func WithOptional[T any](name string, extractRequired func(*validation.Args) (T, error),
	extractOptional func(*validation.Args, int) (core.Value, error),
	fn func(T, core.Value) (core.Value, error)) BuiltinFunc {

	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)

		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		required, err := extractRequired(v)
		if err != nil {
			return nil, err
		}

		var optional core.Value = core.Nil
		if v.Count() > 1 {
			optional, err = extractOptional(v, 1)
			if err != nil {
				return nil, err
			}
		}

		return fn(required, optional)
	}
}

// NumberToValue converts a number function to return core.Value
func NumberToValue(fn func(float64) (float64, error)) func(float64) (core.Value, error) {
	return func(n float64) (core.Value, error) {
		result, err := fn(n)
		if err != nil {
			return nil, err
		}
		return core.NumberValue(result), nil
	}
}

// StringToValue converts a string function to return core.Value
func StringToValue(fn func(string) (string, error)) func(string) (core.Value, error) {
	return func(s string) (core.Value, error) {
		result, err := fn(s)
		if err != nil {
			return nil, err
		}
		return core.StringValue(result), nil
	}
}

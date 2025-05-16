package core

import (
	"strings"
)

// This file implements ObjProtocol adapters for string type

// StringAdapter adapts string to ObjProtocol
type StringAdapter struct {
	str string
}

// NewStringAdapter creates a new adapter for string
func NewStringAdapter(str string) *StringAdapter {
	return &StringAdapter{str: str}
}

// GetProp retrieves a property or method
func (a *StringAdapter) GetProp(name string) (LispValue, bool) {
	// Methods
	switch name {
	case "length", "len":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			return float64(len(a.str)), nil
		}), true
	case "upper":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 0 {
				return nil, ErrWrongArgCount("upper", 0, len(args))
			}
			return strings.ToUpper(a.str), nil
		}), true
	case "lower":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 0 {
				return nil, ErrWrongArgCount("lower", 0, len(args))
			}
			return strings.ToLower(a.str), nil
		}), true
	case "strip":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) > 1 {
				return nil, ErrWrongArgCount("strip", 0, len(args))
			}

			if len(args) == 0 {
				return strings.TrimSpace(a.str), nil
			}

			cutset, ok := args[0].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[0]))
			}

			return strings.Trim(a.str, cutset), nil
		}), true
	case "split":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("split", 1, len(args))
			}

			sep, ok := args[0].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[0]))
			}

			parts := strings.Split(a.str, sep)
			result := make(LispList, len(parts))
			for i, part := range parts {
				result[i] = part
			}

			return result, nil
		}), true
	case "replace":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 2 {
				return nil, ErrWrongArgCount("replace", 2, len(args))
			}

			old, ok := args[0].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[0]))
			}

			new, ok := args[1].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[1]))
			}

			return strings.ReplaceAll(a.str, old, new), nil
		}), true
	case "contains":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("contains", 1, len(args))
			}

			substr, ok := args[0].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[0]))
			}

			return strings.Contains(a.str, substr), nil
		}), true
	case "startswith":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("startswith", 1, len(args))
			}

			prefix, ok := args[0].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[0]))
			}

			return strings.HasPrefix(a.str, prefix), nil
		}), true
	case "endswith":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("endswith", 1, len(args))
			}

			suffix, ok := args[0].(string)
			if !ok {
				return nil, ErrTypeMismatch("string", TypeOf(args[0]))
			}

			return strings.HasSuffix(a.str, suffix), nil
		}), true
	}

	// Character access by index
	if idx, err := ParseIndex(name); err == nil && idx >= 0 && idx < len(a.str) {
		return string(a.str[idx]), true
	}

	return nil, false
}

// SetProp sets a property value
func (a *StringAdapter) SetProp(name string, value LispValue) error {
	// Strings are immutable, so we can't set properties
	return ErrDotNoPropertyf(name)
}

// HasMethodP checks if a method exists
func (a *StringAdapter) HasMethodP(name string) bool {
	switch name {
	case "length", "len", "upper", "lower", "strip", "split", "replace", "contains", "startswith", "endswith":
		return true
	}
	return false
}

// CallMethodP calls a method with arguments
func (a *StringAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	switch name {
	case "length", "len":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("len", 0, len(args))
		}
		return float64(len(a.str)), nil
	case "upper":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("upper", 0, len(args))
		}
		return strings.ToUpper(a.str), nil
	case "lower":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("lower", 0, len(args))
		}
		return strings.ToLower(a.str), nil
	case "strip":
		if len(args) > 1 {
			return nil, ErrWrongArgCount("strip", 0, len(args))
		}

		if len(args) == 0 {
			return strings.TrimSpace(a.str), nil
		}

		cutset, ok := args[0].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[0]))
		}

		return strings.Trim(a.str, cutset), nil
	case "split":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("split", 1, len(args))
		}

		sep, ok := args[0].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[0]))
		}

		parts := strings.Split(a.str, sep)
		result := make(LispList, len(parts))
		for i, part := range parts {
			result[i] = part
		}

		return result, nil
	case "replace":
		if len(args) != 2 {
			return nil, ErrWrongArgCount("replace", 2, len(args))
		}

		old, ok := args[0].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[0]))
		}

		new, ok := args[1].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[1]))
		}

		return strings.ReplaceAll(a.str, old, new), nil
	case "contains":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("contains", 1, len(args))
		}

		substr, ok := args[0].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[0]))
		}

		return strings.Contains(a.str, substr), nil
	case "startswith":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("startswith", 1, len(args))
		}

		prefix, ok := args[0].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[0]))
		}

		return strings.HasPrefix(a.str, prefix), nil
	case "endswith":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("endswith", 1, len(args))
		}

		suffix, ok := args[0].(string)
		if !ok {
			return nil, ErrTypeMismatch("string", TypeOf(args[0]))
		}

		return strings.HasSuffix(a.str, suffix), nil
	}

	return nil, ErrDotNoMethodf(name)
}

// Ensure StringAdapter implements ObjProtocol
var _ ObjProtocol = (*StringAdapter)(nil)

// Define a string type that implements AdaptableLispValue
type StringValue string

// AsObject implements AdaptableLispValue for StringValue
func (s StringValue) AsObject() ObjProtocol {
	return &StringAdapter{str: string(s)}
}

// Define helper functions for converting between string and StringValue
func ToStringValue(s string) StringValue {
	return StringValue(s)
}

func FromStringValue(s StringValue) string {
	return string(s)
}

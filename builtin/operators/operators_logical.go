package operators

import (
	goerrors "errors"
	"reflect"
	"strings"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// Logical Operators

// Not implements the not operator using protocol-based dispatch
func Not() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("not", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// Try __bool__ dunder method
		if boolVal, found, err := types.CallBool(args[0], ctx); found {
			if err != nil {
				return nil, err
			}
			return core.BoolValue(!boolVal), nil
		}

		// Fall back to IsTruthy
		return core.BoolValue(!core.IsTruthy(args[0])), nil
	}
}

// And implements the and operator using protocol-based dispatch
func And() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// and with no arguments returns True
		if len(args) == 0 {
			return core.True, nil
		}

		// Short-circuit evaluation
		var result core.Value = core.True
		for _, arg := range args {
			// Check truthiness using __bool__ if available
			var truthy bool
			if boolVal, found, err := types.CallBool(arg, ctx); found {
				if err != nil {
					return nil, err
				}
				truthy = boolVal
			} else {
				truthy = core.IsTruthy(arg)
			}

			if !truthy {
				return arg, nil // Return the first falsy value
			}
			result = arg
		}

		// All values were truthy, return the last one
		return result, nil
	}
}

// Or implements the or operator using protocol-based dispatch
func Or() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// or with no arguments returns False
		if len(args) == 0 {
			return core.False, nil
		}

		// Short-circuit evaluation
		for _, arg := range args {
			// Check truthiness using __bool__ if available
			var truthy bool
			if boolVal, found, err := types.CallBool(arg, ctx); found {
				if err != nil {
					return nil, err
				}
				truthy = boolVal
			} else {
				truthy = core.IsTruthy(arg)
			}

			if truthy {
				return arg, nil // Return the first truthy value
			}
		}

		// All values were falsy, return the last one
		return args[len(args)-1], nil
	}
}

// In implements the in operator using protocol-based dispatch
func In() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("in", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		value := args[0]
		container := args[1]

		// Try __contains__ on container
		if result, found, err := types.CallDunder(container, "__contains__", []core.Value{value}, ctx); found {
			if err != nil {
				return nil, err
			}
			// Convert result to boolean using Python's truthiness rules
			// None/nil is treated as False (this matches Python behavior where
			// __contains__ returning None is treated as False)
			if result == nil || result == core.None || result == core.Nil {
				return core.False, nil
			}
			if b, ok := result.(core.BoolValue); ok {
				return b, nil
			}
			// For other types, use truthiness (e.g., non-empty string is True)
			return core.BoolValue(core.IsTruthy(result)), nil
		}

		// Use protocol-based container operations
		if cont, ok := protocols.AsContainer(container); ok {
			return core.BoolValue(cont.Contains(value)), nil
		}

		// Fall back to type-based implementation
		switch c := container.(type) {
		case core.StringValue:
			if str, ok := value.(core.StringValue); ok {
				return core.BoolValue(strings.Contains(string(c), string(str))), nil
			}
			return core.BoolValue(false), nil

		case *core.ListValue:
			for _, item := range c.Items() {
				if core.EqualValues(value, item) {
					return core.BoolValue(true), nil
				}
			}
			return core.BoolValue(false), nil

		case core.TupleValue:
			for _, item := range c {
				if core.EqualValues(value, item) {
					return core.BoolValue(true), nil
				}
			}
			return core.BoolValue(false), nil

		case *core.SetValue:
			return core.BoolValue(c.Contains(value)), nil

		case *core.DictValue:
			// For dicts, check if key exists
			_, exists := c.GetValue(value)
			return core.BoolValue(exists), nil

		default:
			// Try duck-typing for dict-like objects (e.g., OrderedDict)
			// Try __getitem__ to check if key exists
			if _, found, err := types.CallDunder(container, "__getitem__", []core.Value{value}, ctx); found {
				if err != nil {
					// __getitem__ raised an error (likely KeyError), key doesn't exist
					// Check if it's a KeyError using errors.As
					var keyErr *core.KeyError
					if goerrors.As(err, &keyErr) {
						return core.BoolValue(false), nil
					}
					// Some other error, propagate it
					return nil, err
				}
				// __getitem__ succeeded, key exists
				return core.BoolValue(true), nil
			}
			// Try iterating through the container (for generators, iterators, etc.)
			if iterable, ok := container.(core.Iterable); ok {
				iter := iterable.Iterator()
				for {
					item, hasNext := iter.Next()
					if !hasNext {
						break
					}
					if core.EqualValues(value, item) {
						return core.BoolValue(true), nil
					}
				}
				return core.BoolValue(false), nil
			}
			return nil, errors.NewTypeError("in", "argument must be iterable", string(container.Type()))
		}
	}
}

// NotIn implements the "not in" operator using protocol-based dispatch
func NotIn() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("not in", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Call the In operator
		inFunc := In()
		result, err := inFunc(args, ctx)
		if err != nil {
			return nil, err
		}

		// Negate the result
		if b, ok := result.(core.BoolValue); ok {
			return core.BoolValue(!b), nil
		}

		return nil, errors.NewTypeError("not in", "in operator should return a boolean", string(result.Type()))
	}
}

// Is implements the "is" operator for identity comparison
// Checks if two values are the same object (not just equal)
func Is() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("is", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		left := args[0]
		right := args[1]

		// Handle None specially - None is always identical to None
		if types.IsNil(left) && types.IsNil(right) {
			return core.BoolValue(true), nil
		}
		if types.IsNil(left) || types.IsNil(right) {
			return core.BoolValue(false), nil
		}

		// Handle NotImplemented specially - it's a singleton
		_, leftIsNotImpl := left.(*core.NotImplementedValue)
		_, rightIsNotImpl := right.(*core.NotImplementedValue)
		if leftIsNotImpl && rightIsNotImpl {
			// Both are NotImplemented - they're the same singleton
			return core.BoolValue(true), nil
		}
		if leftIsNotImpl || rightIsNotImpl {
			// Only one is NotImplemented
			return core.BoolValue(false), nil
		}

		// Handle Ellipsis specially - it's a singleton
		_, leftIsEllipsis := left.(core.EllipsisValue)
		_, rightIsEllipsis := right.(core.EllipsisValue)
		if leftIsEllipsis && rightIsEllipsis {
			// Both are Ellipsis - they're the same singleton
			return core.BoolValue(true), nil
		}
		if leftIsEllipsis || rightIsEllipsis {
			// Only one is Ellipsis
			return core.BoolValue(false), nil
		}

		// Handle booleans - True is True, False is False
		leftBool, leftIsBool := left.(core.BoolValue)
		rightBool, rightIsBool := right.(core.BoolValue)
		if leftIsBool && rightIsBool {
			return core.BoolValue(leftBool == rightBool), nil
		}
		if leftIsBool || rightIsBool {
			return core.BoolValue(false), nil
		}

		// Handle ListValue specially - slices can't be compared with ==
		leftList, leftIsList := left.(*core.ListValue)
		rightList, rightIsList := right.(*core.ListValue)
		if leftIsList && rightIsList {
			// For slices, check if they point to the same backing array
			// Compare the slice pointers using reflect
			leftPtr := reflect.ValueOf(leftList).Pointer()
			rightPtr := reflect.ValueOf(rightList).Pointer()
			return core.BoolValue(leftPtr == rightPtr && leftList.Len() == rightList.Len()), nil
		}
		if leftIsList || rightIsList {
			return core.BoolValue(false), nil
		}

		// For reference types (dicts, sets, objects), use pointer comparison
		switch left.(type) {
		case *core.DictValue, *core.SetValue:
			// Use pointer equality - checks if same object in memory
			return core.BoolValue(left == right), nil
		case core.Object:
			// For objects, check if they're the same instance
			return core.BoolValue(left == right), nil
		}

		// For value types (numbers, strings), Python interns small values
		// Handle string interning - Python interns string literals
		leftStr, leftIsStr := left.(core.StringValue)
		rightStr, rightIsStr := right.(core.StringValue)
		if leftIsStr && rightIsStr {
			// In Python, string literals and many strings are interned
			// Two equal strings should be considered the same object
			return core.BoolValue(string(leftStr) == string(rightStr)), nil
		}

		// For numbers, only small integers are interned in Python (-5 to 256)
		// For simplicity, we don't implement number interning yet
		// This means 'x is y' for numbers will generally be False unless
		// they're actually the same object in memory
		return core.BoolValue(false), nil
	}
}

// IsNot implements the "is not" operator
func IsNot() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("is not", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Call the Is operator
		isFunc := Is()
		result, err := isFunc(args, ctx)
		if err != nil {
			return nil, err
		}

		// Negate the result
		if b, ok := result.(core.BoolValue); ok {
			return core.BoolValue(!b), nil
		}

		return nil, errors.NewTypeError("is not", "is operator should return a boolean", string(result.Type()))
	}
}

// Bitwise Operators

// positiveValue implements unary + operator

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

// membershipEqual reports whether `value` matches `item` for the `in` operator,
// honoring a custom/reflected __eq__ (see core.ContainsEqual). Used by the
// container fallbacks below for objects whose type has no __contains__ dunder.
func membershipEqual(value, item core.Value, ctx *core.Context) (bool, error) {
	return core.ContainsEqual(value, item, ctx)
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
				eq, err := membershipEqual(value, item, ctx)
				if err != nil {
					return nil, err
				}
				if eq {
					return core.BoolValue(true), nil
				}
			}
			return core.BoolValue(false), nil

		case core.TupleValue:
			for _, item := range c {
				eq, err := membershipEqual(value, item, ctx)
				if err != nil {
					return nil, err
				}
				if eq {
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
			if _, isInstance := container.(*core.Instance); isInstance {
				// User-defined classes that expose __getitem__ but neither
				// __contains__ nor __iter__ follow CPython's old-style sequence
				// protocol: probe obj[0], obj[1], ..., comparing each item to
				// value and stopping when __getitem__ raises IndexError (the
				// index is the position, never the searched value).
				for i := 0; ; i++ {
					item, found, err := types.CallDunder(container, "__getitem__", []core.Value{core.NumberValue(float64(i))}, ctx)
					if !found {
						break // no __getitem__; fall through to other strategies
					}
					if err != nil {
						if core.IsIndexError(err) {
							// Reached the end of the sequence without a match.
							return core.BoolValue(false), nil
						}
						return nil, err
					}
					eq, err := membershipEqual(value, item, ctx)
					if err != nil {
						return nil, err
					}
					if eq {
						return core.BoolValue(true), nil
					}
				}
			} else if _, found, err := types.CallDunder(container, "__getitem__", []core.Value{value}, ctx); found {
				// Native dict-like objects (e.g. OrderedDict) that lack
				// __contains__: treat __getitem__ as a key lookup — success
				// means the key is present, KeyError means it is absent.
				if err != nil {
					var keyErr *core.KeyError
					if goerrors.As(err, &keyErr) {
						return core.BoolValue(false), nil
					}
					return nil, err
				}
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
					if core.SameObject(value, item) || core.EqualValues(value, item) {
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

		// Handle TupleValue specially - slices can't be compared with ==.
		// Tuple identity is backing-array identity (x is x; y = x are the same
		// object), matching id(); separately-built tuples are different objects.
		// Empty tuples compare equal, like CPython's interned (). Previously
		// tuples fell through to the default and even `x is x` was False.
		leftTuple, leftIsTuple := left.(core.TupleValue)
		rightTuple, rightIsTuple := right.(core.TupleValue)
		if leftIsTuple && rightIsTuple {
			if len(leftTuple) == 0 || len(rightTuple) == 0 {
				return core.BoolValue(len(leftTuple) == 0 && len(rightTuple) == 0), nil
			}
			leftPtr := reflect.ValueOf(leftTuple).Pointer()
			rightPtr := reflect.ValueOf(rightTuple).Pointer()
			return core.BoolValue(leftPtr == rightPtr && len(leftTuple) == len(rightTuple)), nil
		}
		if leftIsTuple || rightIsTuple {
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

		// Python interns small integers (-5 to 256) so 'x is y' is True
		// when x and y are equal small ints. Mirror that here.
		leftNum, leftIsNum := left.(core.NumberValue)
		rightNum, rightIsNum := right.(core.NumberValue)
		if leftIsNum && rightIsNum {
			lf, rf := float64(leftNum), float64(rightNum)
			if lf == rf && lf == float64(int64(lf)) {
				i := int64(lf)
				if i >= -5 && i <= 256 {
					return core.BoolValue(true), nil
				}
			}
			return core.BoolValue(false), nil
		}

		// For other types, default to false.
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

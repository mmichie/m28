package operators

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// Comparison Operators

// Equal implements the == operator using protocol-based dispatch
func Equal() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("==", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareEqual(args[0], args[1], ctx)
	}
}

// compareEqual compares two values for equality
func compareEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Special handling for Class objects - use the default __eq__ from type, not instance methods
	// This prevents trying to call instance methods like TestCase.__eq__(self, other) on the class itself
	if leftClass, ok := left.(*core.Class); ok {
		// For classes, compare by identity or use the built-in __eq__ from Class.GetAttr
		// which returns a bound BuiltinFunction, not instance methods
		// Just use the fallback equality check
		if rightClass, ok := right.(*core.Class); ok {
			// Both are classes - compare by name
			return core.BoolValue(leftClass.Name == rightClass.Name), nil
		}
		// Left is class, right is not - they're not equal
		return core.BoolValue(false), nil
	}

	// Python operator precedence: if right's class is a strict subclass of left's class,
	// try right's method first
	var leftClass, rightClass *core.Class
	if leftInst, ok := left.(*core.Instance); ok {
		leftClass = leftInst.Class
	}
	if rightInst, ok := right.(*core.Instance); ok {
		rightClass = rightInst.Class
	}

	// If right operand's class is a strict subclass of left operand's class,
	// try right operand's __eq__ first
	if leftClass != nil && rightClass != nil && core.IsStrictSubclass(rightClass, leftClass) {
		// Try __eq__ on right operand first (reflected operation)
		if result, found, err := types.CallDunder(right, "__eq__", []core.Value{left}, ctx); found {
			return result, err
		}
		// Then try __eq__ on left operand
		if result, found, err := types.CallDunder(left, "__eq__", []core.Value{right}, ctx); found {
			return result, err
		}
	} else {
		// Normal order: try left first, then right
		// Try __eq__ on left operand
		if result, found, err := types.CallDunder(left, "__eq__", []core.Value{right}, ctx); found {
			return result, err
		}

		// Special handling for Class objects on the right side too
		if _, ok := right.(*core.Class); ok {
			// Right is a class, left is not - they're not equal
			return core.BoolValue(false), nil
		}

		// Try __eq__ on right operand (Python doesn't have __req__)
		if result, found, err := types.CallDunder(right, "__eq__", []core.Value{left}, ctx); found {
			return result, err
		}
	}

	// Fall back to built-in equality
	return core.BoolValue(core.EqualValues(left, right)), nil
}

// NotEqual implements the != operator using protocol-based dispatch
func NotEqual() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("!=", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareNotEqual(args[0], args[1], ctx)
	}
}

// compareNotEqual compares two values for inequality
func compareNotEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Python operator precedence: if right's class is a strict subclass of left's class,
	// try right's method first
	var leftClass, rightClass *core.Class
	if leftInst, ok := left.(*core.Instance); ok {
		leftClass = leftInst.Class
	}
	if rightInst, ok := right.(*core.Instance); ok {
		rightClass = rightInst.Class
	}

	// If right operand's class is a strict subclass of left operand's class,
	// try right operand's __ne__ first
	if leftClass != nil && rightClass != nil && core.IsStrictSubclass(rightClass, leftClass) {
		// Try __ne__ on right operand first (reflected operation)
		if result, found, err := types.CallDunder(right, "__ne__", []core.Value{left}, ctx); found {
			return result, err
		}
		// Then try __ne__ on left operand
		if result, found, err := types.CallDunder(left, "__ne__", []core.Value{right}, ctx); found {
			return result, err
		}
	} else {
		// Normal order: try left first, then right
		// Try __ne__ on left operand
		if result, found, err := types.CallDunder(left, "__ne__", []core.Value{right}, ctx); found {
			return result, err
		}

		// Try __ne__ on right operand
		if result, found, err := types.CallDunder(right, "__ne__", []core.Value{left}, ctx); found {
			return result, err
		}
	}

	// Fall back to negation of equality
	equal, err := compareEqual(left, right, ctx)
	if err != nil {
		return nil, err
	}
	return core.BoolValue(!core.IsTruthy(equal)), nil
}

// LessThan implements the < operator using protocol-based dispatch
func LessThan() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("<", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareLessThan(args[0], args[1], ctx)
	}
}

// compareLessThan compares if left < right
func compareLessThan(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __lt__ on left operand
	if result, found, err := types.CallDunder(left, "__lt__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __gt__ on right operand (reverse comparison)
	if result, found, err := types.CallDunder(right, "__gt__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp < 0), nil
		}
	}

	// Handle BigInt comparison
	leftBig, leftIsBig := left.(core.BigIntValue)
	rightBig, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		// Convert both to BigInt for comparison
		if !leftIsBig {
			if leftNum, ok := left.(core.NumberValue); ok {
				if !core.IsInteger(float64(leftNum)) {
					return nil, errors.NewTypeError("<",
						"'<' not supported between instances of",
						"'float' and 'int'")
				}
				leftBig = core.PromoteToBigInt(leftNum)
			} else {
				return nil, errors.NewTypeError("<",
					"'<' not supported between instances of",
					"'"+string(left.Type())+"' and 'int'")
			}
		}
		if !rightIsBig {
			if rightNum, ok := right.(core.NumberValue); ok {
				if !core.IsInteger(float64(rightNum)) {
					return nil, errors.NewTypeError("<",
						"'<' not supported between instances of",
						"'int' and 'float'")
				}
				rightBig = core.PromoteToBigInt(rightNum)
			} else {
				return nil, errors.NewTypeError("<",
					"'<' not supported between instances of",
					"'int' and '"+string(right.Type())+"'")
			}
		}

		return core.BoolValue(leftBig.GetBigInt().Cmp(rightBig.GetBigInt()) < 0), nil
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum < rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("<",
						"'<' not supported between instances of",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr < rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("<",
						"'<' not supported between instances of",
						"'str' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("<",
				"'<' not supported for",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// LessThanOrEqual implements the <= operator using protocol-based dispatch
func LessThanOrEqual() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("<=", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareLessThanOrEqual(args[0], args[1], ctx)
	}
}

// compareLessThanOrEqual compares if left <= right
func compareLessThanOrEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __le__ on left operand
	if result, found, err := types.CallDunder(left, "__le__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __ge__ on right operand (reverse comparison)
	if result, found, err := types.CallDunder(right, "__ge__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp <= 0), nil
		}
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum <= rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("<=",
						"'<=' not supported between instances of",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr <= rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("<=",
						"'<=' not supported between instances of",
						"'str' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("<=",
				"'<=' not supported for",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// GreaterThan implements the > operator using protocol-based dispatch
func GreaterThan() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(">", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareGreaterThan(args[0], args[1], ctx)
	}
}

// compareGreaterThan compares if left > right
func compareGreaterThan(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __gt__ on left operand
	if result, found, err := types.CallDunder(left, "__gt__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __lt__ on right operand (reverse comparison)
	if result, found, err := types.CallDunder(right, "__lt__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp > 0), nil
		}
	}

	// Handle BigInt comparison
	leftBig, leftIsBig := left.(core.BigIntValue)
	rightBig, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		// Convert both to BigInt for comparison
		if !leftIsBig {
			if leftNum, ok := left.(core.NumberValue); ok {
				if !core.IsInteger(float64(leftNum)) {
					return nil, errors.NewTypeError(">",
						"'>' not supported between instances of",
						"'float' and 'int'")
				}
				leftBig = core.PromoteToBigInt(leftNum)
			} else {
				return nil, errors.NewTypeError(">",
					"'>' not supported between instances of",
					"'"+string(left.Type())+"' and 'int'")
			}
		}
		if !rightIsBig {
			if rightNum, ok := right.(core.NumberValue); ok {
				if !core.IsInteger(float64(rightNum)) {
					return nil, errors.NewTypeError(">",
						"'>' not supported between instances of",
						"'int' and 'float'")
				}
				rightBig = core.PromoteToBigInt(rightNum)
			} else {
				return nil, errors.NewTypeError(">",
					"'>' not supported between instances of",
					"'int' and '"+string(right.Type())+"'")
			}
		}

		return core.BoolValue(leftBig.GetBigInt().Cmp(rightBig.GetBigInt()) > 0), nil
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum > rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError(">",
						"'>' not supported between instances of",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr > rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError(">",
						"'>' not supported between instances of",
						"'str' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError(">",
				"'>' not supported for",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// GreaterThanOrEqual implements the >= operator using protocol-based dispatch
func GreaterThanOrEqual() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(">=", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return compareGreaterThanOrEqual(args[0], args[1], ctx)
	}
}

// compareGreaterThanOrEqual compares if left >= right
func compareGreaterThanOrEqual(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __ge__ on left operand
	if result, found, err := types.CallDunder(left, "__ge__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __le__ on right operand (reverse comparison)
	if result, found, err := types.CallDunder(right, "__le__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Use protocol-based comparison
	if leftCmp, ok := protocols.AsComparable(left); ok {
		cmp, err := leftCmp.Compare(right)
		if err == nil {
			return core.BoolValue(cmp >= 0), nil
		}
	}

	// Fall back to type-based comparison
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.BoolValue(leftNum >= rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError(">=",
						"'>=' not supported between instances of",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.BoolValue(leftStr >= rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError(">=",
						"'>=' not supported between instances of",
						"'str' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError(">=",
				"'>=' not supported for",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

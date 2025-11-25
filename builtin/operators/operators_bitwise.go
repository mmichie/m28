package operators

import (
	"math"
	"math/big"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/modules"
)

// BitwiseInvert implements the ~ operator (bitwise NOT)
func BitwiseInvert() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("~", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return invertValue(args[0], ctx)
	}
}

// invertValue inverts a single value
func invertValue(value core.Value, ctx *core.Context) (core.Value, error) {
	// Try __invert__ dunder method
	if result, found, err := types.CallDunder(value, "__invert__", []core.Value{}, ctx); found {
		return result, err
	}

	// Fall back to integer bitwise inversion
	return types.Switch(value).
		Bool(func(b bool) (core.Value, error) {
			// Emit DeprecationWarning for ~bool (Python 3.12+)
			// This operation is deprecated and will be removed in Python 3.14
			if err := emitBoolInvertWarning(ctx); err != nil {
				// If warnings are set to "error" mode, this will return an error
				return nil, err
			}

			// In Python, bool is a subclass of int, so True is 1 and False is 0
			// ~False = ~0 = -1, ~True = ~1 = -2
			intVal := 0
			if b {
				intVal = 1
			}
			return core.NumberValue(float64(^intVal)), nil
		}).
		Number(func(n float64) (core.Value, error) {
			// Check if it's an integer
			if n != float64(int(n)) {
				return nil, errors.NewTypeError("~",
					"bad operand type for unary ~",
					"'float'")
			}
			// Bitwise NOT for integer
			return core.NumberValue(float64(^int(n))), nil
		}).
		Default(func(v core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("~",
				"bad operand type for unary ~",
				"'"+string(v.Type())+"'")
		}).
		Execute()
}

// LeftShift implements the << operator
func LeftShift() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("<<", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return leftShiftTwo(args[0], args[1], ctx)
	}
}

// leftShiftTwo performs left shift: left << right
func leftShiftTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __lshift__ on left operand
	if result, found, err := types.CallDunder(left, "__lshift__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rlshift__ on right operand
	if result, found, err := types.CallDunder(right, "__rlshift__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Check for BigInt operands
	_, leftIsBig := left.(core.BigIntValue)
	_, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		return bigIntLeftShift(left, right)
	}

	// Fall back to integer left shift
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Validate integer and shift amount
					if !core.IsInteger(leftNum) || !core.IsInteger(rightNum) || rightNum < 0 {
						return nil, errors.NewTypeError("<<",
							"unsupported operand type(s)",
							"'float' or negative shift")
					}

					// If number is too large for int64, promote to BigInt
					if core.ShouldPromoteToBigInt(leftNum) {
						return bigIntLeftShift(left, right)
					}

					// Check if result would overflow - estimate bits needed
					shiftAmount := uint64(rightNum)
					if leftNum != 0 {
						// Result will be approximately leftNum * 2^shiftAmount
						// If this would exceed int64, use BigInt
						bitsNeeded := math.Log2(math.Abs(leftNum)) + float64(shiftAmount)
						if bitsNeeded > 53 {
							return bigIntLeftShift(left, right)
						}
					}

					// Safe int64 shift
					result := int64(leftNum) << shiftAmount
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("<<",
						"unsupported operand type(s)",
						"'int' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("<<",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"' and '"+string(right.Type())+"'")
		}).
		Execute()
}

// bigIntLeftShift performs left shift using arbitrary precision integers
func bigIntLeftShift(left, right core.Value) (core.Value, error) {
	// Convert left to big.Int
	var leftBig *big.Int
	switch l := left.(type) {
	case core.BigIntValue:
		leftBig = l.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(l)) {
			return nil, errors.NewTypeError("<<", "unsupported operand", "'float'")
		}
		leftBig = core.PromoteToBigInt(l).GetBigInt()
	default:
		return nil, errors.NewTypeError("<<", "unsupported operand type", string(left.Type()))
	}

	// Get shift amount as uint
	var shiftAmount uint
	switch r := right.(type) {
	case core.BigIntValue:
		if r.Sign() < 0 {
			return nil, errors.NewTypeError("<<", "negative shift count", "")
		}
		// Check if it fits in uint
		if val, ok := r.ToInt64(); ok && val >= 0 {
			shiftAmount = uint(val)
		} else {
			return nil, errors.NewTypeError("<<", "shift count too large", "")
		}
	case core.NumberValue:
		if !core.IsInteger(float64(r)) || float64(r) < 0 {
			return nil, errors.NewTypeError("<<", "invalid shift count", "")
		}
		shiftAmount = uint(r)
	default:
		return nil, errors.NewTypeError("<<", "unsupported operand type", string(right.Type()))
	}

	// Perform the shift
	result := new(big.Int).Lsh(leftBig, shiftAmount)
	bigIntResult := core.NewBigInt(result)

	// Try to demote to NumberValue if it fits
	if numVal, ok := core.DemoteToNumber(bigIntResult); ok {
		return numVal, nil
	}

	return bigIntResult, nil
}

// RightShift implements the >> operator
func RightShift() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(">>", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return rightShiftTwo(args[0], args[1], ctx)
	}
}

// rightShiftTwo performs right shift: left >> right
func rightShiftTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __rshift__ on left operand
	if result, found, err := types.CallDunder(left, "__rshift__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rrshift__ on right operand
	if result, found, err := types.CallDunder(right, "__rrshift__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Check for BigInt operands
	_, leftIsBig := left.(core.BigIntValue)
	_, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		return bigIntRightShift(left, right)
	}

	// Fall back to integer right shift
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Validate integer and shift amount
					if !core.IsInteger(leftNum) || !core.IsInteger(rightNum) || rightNum < 0 {
						return nil, errors.NewTypeError(">>",
							"unsupported operand type(s)",
							"'float' or negative shift")
					}

					// If number is too large for int64, promote to BigInt
					if core.ShouldPromoteToBigInt(leftNum) {
						return bigIntRightShift(left, right)
					}

					// Safe int64 shift
					shiftAmount := uint64(rightNum)
					result := int64(leftNum) >> shiftAmount
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError(">>",
						"unsupported operand type(s)",
						"'int' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError(">>",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"' and '"+string(right.Type())+"'")
		}).
		Execute()
}

// bigIntRightShift performs right shift using arbitrary precision integers
func bigIntRightShift(left, right core.Value) (core.Value, error) {
	// Convert left to big.Int
	var leftBig *big.Int
	switch l := left.(type) {
	case core.BigIntValue:
		leftBig = l.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(l)) {
			return nil, errors.NewTypeError(">>", "unsupported operand", "'float'")
		}
		leftBig = core.PromoteToBigInt(l).GetBigInt()
	default:
		return nil, errors.NewTypeError(">>", "unsupported operand type", string(left.Type()))
	}

	// Get shift amount as uint
	var shiftAmount uint
	switch r := right.(type) {
	case core.BigIntValue:
		if r.Sign() < 0 {
			return nil, errors.NewTypeError(">>", "negative shift count", "")
		}
		// Check if it fits in uint
		if val, ok := r.ToInt64(); ok && val >= 0 {
			shiftAmount = uint(val)
		} else {
			return nil, errors.NewTypeError(">>", "shift count too large", "")
		}
	case core.NumberValue:
		if !core.IsInteger(float64(r)) || float64(r) < 0 {
			return nil, errors.NewTypeError(">>", "invalid shift count", "")
		}
		shiftAmount = uint(r)
	default:
		return nil, errors.NewTypeError(">>", "unsupported operand type", string(right.Type()))
	}

	// Perform the shift
	result := new(big.Int).Rsh(leftBig, shiftAmount)
	bigIntResult := core.NewBigInt(result)

	// Try to demote to NumberValue if it fits
	if numVal, ok := core.DemoteToNumber(bigIntResult); ok {
		return numVal, nil
	}

	return bigIntResult, nil
}

// BitwiseAnd implements the & operator
func BitwiseAnd() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("&", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return bitwiseAndTwo(args[0], args[1], ctx)
	}
}

// bitwiseAndTwo performs bitwise AND: left & right
func bitwiseAndTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __and__ on left operand
	if result, found, err := types.CallDunder(left, "__and__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rand__ on right operand
	if result, found, err := types.CallDunder(right, "__rand__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Check for BigInt operands
	_, leftIsBigInt := left.(core.BigIntValue)
	_, rightIsBigInt := right.(core.BigIntValue)
	if leftIsBigInt || rightIsBigInt {
		return bigIntBitwiseAnd(left, right)
	}

	// Fall back to integer bitwise AND
	return types.Switch(left).
		Bool(func(leftBool bool) (core.Value, error) {
			leftInt := 0
			if leftBool {
				leftInt = 1
			}
			return types.Switch(right).
				Bool(func(rightBool bool) (core.Value, error) {
					// When both operands are bool, return bool
					result := leftBool && rightBool
					return core.BoolValue(result), nil
				}).
				Number(func(rightNum float64) (core.Value, error) {
					if rightNum != float64(int(rightNum)) {
						return nil, errors.NewTypeError("&",
							"unsupported operand type(s)",
							"'int' and 'float'")
					}
					result := leftInt & int(rightNum)
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("&",
						"unsupported operand type(s)",
						"'int' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Bool(func(rightBool bool) (core.Value, error) {
					rightInt := 0
					if rightBool {
						rightInt = 1
					}
					if leftNum != float64(int(leftNum)) {
						return nil, errors.NewTypeError("&",
							"unsupported operand type(s)",
							"'float' and 'int'")
					}
					result := int(leftNum) & rightInt
					return core.NumberValue(float64(result)), nil
				}).
				Number(func(rightNum float64) (core.Value, error) {
					// Check both are integers
					if leftNum != float64(int(leftNum)) || rightNum != float64(int(rightNum)) {
						return nil, errors.NewTypeError("&",
							"unsupported operand type(s)",
							"'float' and 'float'")
					}
					result := int(leftNum) & int(rightNum)
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("&",
						"unsupported operand type(s)",
						"'int' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("&",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"' and '"+string(right.Type())+"'")
		}).
		Execute()
}

// BitwiseOr implements the | operator
func BitwiseOr() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("|", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return bitwiseOrTwo(args[0], args[1], ctx)
	}
}

// bitwiseOrTwo performs bitwise OR: left | right
func bitwiseOrTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __or__ on left operand
	if result, found, err := types.CallDunder(left, "__or__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __ror__ on right operand
	if result, found, err := types.CallDunder(right, "__ror__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Check for BigInt operands
	_, leftIsBigInt := left.(core.BigIntValue)
	_, rightIsBigInt := right.(core.BigIntValue)
	if leftIsBigInt || rightIsBigInt {
		return bigIntBitwiseOr(left, right)
	}

	// Fall back to integer bitwise OR
	return types.Switch(left).
		Bool(func(leftBool bool) (core.Value, error) {
			// Convert bool to int (True=1, False=0)
			leftInt := 0
			if leftBool {
				leftInt = 1
			}
			return types.Switch(right).
				Bool(func(rightBool bool) (core.Value, error) {
					// When both operands are bool, return bool
					result := leftBool || rightBool
					return core.BoolValue(result), nil
				}).
				Number(func(rightNum float64) (core.Value, error) {
					if rightNum != float64(int(rightNum)) {
						return nil, errors.NewTypeError("|",
							"unsupported operand type(s)",
							"'bool' and 'float'")
					}
					result := leftInt | int(rightNum)
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("|",
						"unsupported operand type(s)",
						"'bool' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Bool(func(rightBool bool) (core.Value, error) {
					rightInt := 0
					if rightBool {
						rightInt = 1
					}
					if leftNum != float64(int(leftNum)) {
						return nil, errors.NewTypeError("|",
							"unsupported operand type(s)",
							"'float' and 'bool'")
					}
					result := int(leftNum) | rightInt
					return core.NumberValue(float64(result)), nil
				}).
				Number(func(rightNum float64) (core.Value, error) {
					// Check both are integers
					if leftNum != float64(int(leftNum)) || rightNum != float64(int(rightNum)) {
						return nil, errors.NewTypeError("|",
							"unsupported operand type(s)",
							"'float' and 'float'")
					}
					result := int(leftNum) | int(rightNum)
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("|",
						"unsupported operand type(s)",
						"'int' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("|",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"' and '"+string(right.Type())+"'")
		}).
		Execute()
}

// BitwiseXor implements the ^ operator
func BitwiseXor() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("^", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return bitwiseXorTwo(args[0], args[1], ctx)
	}
}

// bitwiseXorTwo performs bitwise XOR: left ^ right
func bitwiseXorTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __xor__ on left operand
	if result, found, err := types.CallDunder(left, "__xor__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rxor__ on right operand
	if result, found, err := types.CallDunder(right, "__rxor__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Check for BigInt operands
	_, leftIsBig := left.(core.BigIntValue)
	_, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		return bigIntXor(left, right)
	}

	// Fall back to integer bitwise XOR
	return types.Switch(left).
		Bool(func(leftBool bool) (core.Value, error) {
			leftInt := 0
			if leftBool {
				leftInt = 1
			}
			return types.Switch(right).
				Bool(func(rightBool bool) (core.Value, error) {
					// When both operands are bool, return bool
					result := leftBool != rightBool
					return core.BoolValue(result), nil
				}).
				Number(func(rightNum float64) (core.Value, error) {
					if rightNum != float64(int(rightNum)) {
						return nil, errors.NewTypeError("^",
							"unsupported operand type(s)",
							"'bool' and 'float'")
					}
					result := leftInt ^ int(rightNum)
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("^",
						"unsupported operand type(s)",
						"'bool' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Bool(func(rightBool bool) (core.Value, error) {
					rightInt := 0
					if rightBool {
						rightInt = 1
					}
					if leftNum != float64(int(leftNum)) {
						return nil, errors.NewTypeError("^",
							"unsupported operand type(s)",
							"'float' and 'bool'")
					}
					result := int(leftNum) ^ rightInt
					return core.NumberValue(float64(result)), nil
				}).
				Number(func(rightNum float64) (core.Value, error) {
					// Check if both are integers
					if !core.IsInteger(leftNum) || !core.IsInteger(rightNum) {
						return nil, errors.NewTypeError("^",
							"unsupported operand type(s)",
							"'float' and 'float'")
					}

					// If either number is too large, promote to BigInt
					if core.ShouldPromoteToBigInt(leftNum) || core.ShouldPromoteToBigInt(rightNum) {
						return bigIntXor(left, right)
					}

					// Normal case: use int64
					result := int64(leftNum) ^ int64(rightNum)
					return core.NumberValue(float64(result)), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("^",
						"unsupported operand type(s)",
						"'int' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("^",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"' and '"+string(right.Type())+"'")
		}).
		Execute()
}

// bigIntXor performs XOR using arbitrary precision integers
func bigIntXor(left, right core.Value) (core.Value, error) {
	// Convert both to big.Int
	var leftBig, rightBig *big.Int

	switch l := left.(type) {
	case core.BigIntValue:
		leftBig = l.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(l)) {
			return nil, errors.NewTypeError("^", "unsupported operand", "'float'")
		}
		leftBig = core.PromoteToBigInt(l).GetBigInt()
	default:
		return nil, errors.NewTypeError("^", "unsupported operand type", string(left.Type()))
	}

	switch r := right.(type) {
	case core.BigIntValue:
		rightBig = r.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(r)) {
			return nil, errors.NewTypeError("^", "unsupported operand", "'float'")
		}
		rightBig = core.PromoteToBigInt(r).GetBigInt()
	default:
		return nil, errors.NewTypeError("^", "unsupported operand type", string(right.Type()))
	}

	// Perform XOR
	result := new(big.Int).Xor(leftBig, rightBig)
	bigIntResult := core.NewBigInt(result)

	// Try to demote to NumberValue if it fits
	if numVal, ok := core.DemoteToNumber(bigIntResult); ok {
		return numVal, nil
	}

	return bigIntResult, nil
}

// emitBoolInvertWarning emits a DeprecationWarning for using ~ on bool values
// This follows CPython 3.12+ behavior where ~bool is deprecated
func emitBoolInvertWarning(ctx *core.Context) error {
	// Use the warnings module's helper function to emit the warning
	// This ensures proper integration with catch_warnings() and other warning infrastructure
	message := "The unary ~ (invert) operator on bool is deprecated"
	return modules.Warn(message, "DeprecationWarning", ctx)
}

// bigIntBitwiseAnd computes bitwise AND using arbitrary precision integers
func bigIntBitwiseAnd(left, right core.Value) (core.Value, error) {
	var leftBig, rightBig *big.Int

	switch l := left.(type) {
	case core.BigIntValue:
		leftBig = l.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(l)) {
			return nil, errors.NewTypeError("&", "unsupported operand type(s)", "'float'")
		}
		leftBig = core.PromoteToBigInt(l).GetBigInt()
	default:
		return nil, errors.NewTypeError("&", "unsupported operand type(s)", "'"+string(left.Type())+"'")
	}

	switch r := right.(type) {
	case core.BigIntValue:
		rightBig = r.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(r)) {
			return nil, errors.NewTypeError("&", "unsupported operand type(s)", "'float'")
		}
		rightBig = core.PromoteToBigInt(r).GetBigInt()
	default:
		return nil, errors.NewTypeError("&", "unsupported operand type(s)", "'"+string(right.Type())+"'")
	}

	result := new(big.Int).And(leftBig, rightBig)
	bigIntResult := core.NewBigInt(result)

	// Try to demote to NumberValue if it fits
	if numVal, ok := core.DemoteToNumber(bigIntResult); ok {
		return numVal, nil
	}

	return bigIntResult, nil
}

// bigIntBitwiseOr computes bitwise OR using arbitrary precision integers
func bigIntBitwiseOr(left, right core.Value) (core.Value, error) {
	var leftBig, rightBig *big.Int

	switch l := left.(type) {
	case core.BigIntValue:
		leftBig = l.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(l)) {
			return nil, errors.NewTypeError("|", "unsupported operand type(s)", "'float'")
		}
		leftBig = core.PromoteToBigInt(l).GetBigInt()
	default:
		return nil, errors.NewTypeError("|", "unsupported operand type(s)", "'"+string(left.Type())+"'")
	}

	switch r := right.(type) {
	case core.BigIntValue:
		rightBig = r.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(r)) {
			return nil, errors.NewTypeError("|", "unsupported operand type(s)", "'float'")
		}
		rightBig = core.PromoteToBigInt(r).GetBigInt()
	default:
		return nil, errors.NewTypeError("|", "unsupported operand type(s)", "'"+string(right.Type())+"'")
	}

	result := new(big.Int).Or(leftBig, rightBig)
	bigIntResult := core.NewBigInt(result)

	// Try to demote to NumberValue if it fits
	if numVal, ok := core.DemoteToNumber(bigIntResult); ok {
		return numVal, nil
	}

	return bigIntResult, nil
}

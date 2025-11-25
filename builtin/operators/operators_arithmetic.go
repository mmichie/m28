package operators

import (
	"fmt"
	"math"
	"math/big"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// Arithmetic Operators

// Add implements the + operator using protocol-based dispatch
func Add() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("+", args)

		// + operator accepts 0 or more arguments
		// With 0 args, return identity element (0 for numbers, "" for strings)
		if v.Count() == 0 {
			return core.NumberValue(0), nil
		}

		// Single argument: unary plus
		if v.Count() == 1 {
			return positiveValue(args[0], ctx)
		}

		// Multiple arguments - chain additions
		result := args[0]
		for i := 1; i < len(args); i++ {
			var err error
			result, err = addTwo(result, args[i], ctx)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	}
}

// addTwo adds two values using protocol dispatch
func addTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// First, try dunder method on left operand
	if result, found, err := types.CallAdd(left, right, ctx); found {
		return result, err
	}

	// Then try reverse add on right operand
	if result, found, err := types.CallRadd(right, left, ctx); found {
		return result, err
	}

	// Try protocol-based numeric operations
	if leftNum, ok := protocols.GetNumericOps(left); ok {
		return leftNum.Add(right)
	}

	// Fall back to type-based dispatch using TypeSwitch
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.NumberValue(leftNum + rightNum), nil
				}).
				Bool(func(rightBool bool) (core.Value, error) {
					// Python: bools behave like ints (True=1, False=0)
					rightNum := 0.0
					if rightBool {
						rightNum = 1.0
					}
					return core.NumberValue(leftNum + rightNum), nil
				}).
				Complex(func(rightComplex complex128) (core.Value, error) {
					// Number + Complex = Complex
					return core.ComplexValue(complex(leftNum, 0) + rightComplex), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("+",
						"unsupported operand type(s)",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Complex(func(leftComplex complex128) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Complex + Number = Complex
					return core.ComplexValue(leftComplex + complex(rightNum, 0)), nil
				}).
				Complex(func(rightComplex complex128) (core.Value, error) {
					// Complex + Complex = Complex
					return core.ComplexValue(leftComplex + rightComplex), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("+",
						"unsupported operand type(s)",
						"'complex' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		String(func(leftStr string) (core.Value, error) {
			return types.Switch(right).
				String(func(rightStr string) (core.Value, error) {
					return core.StringValue(leftStr + rightStr), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("+",
						"can only concatenate str (not \""+string(r.Type())+"\") to str", "")
				}).
				Execute()
		}).
		List(func(leftList *core.ListValue) (core.Value, error) {
			return types.Switch(right).
				List(func(rightList *core.ListValue) (core.Value, error) {
					result := make([]core.Value, 0, leftList.Len()+rightList.Len())
					result = append(result, leftList.Items()...)
					result = append(result, rightList.Items()...)
					return core.NewList(result...), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("+",
						"can only concatenate list (not \""+string(r.Type())+"\") to list", "")
				}).
				Execute()
		}).
		Type(core.BytesType, func(leftVal core.Value) (core.Value, error) {
			// Bytes + Bytes (concatenation)
			leftBytes := leftVal.(core.BytesValue)
			return types.Switch(right).
				Type(core.BytesType, func(rightVal core.Value) (core.Value, error) {
					rightBytes := rightVal.(core.BytesValue)
					result := make([]byte, 0, len(leftBytes)+len(rightBytes))
					result = append(result, leftBytes...)
					result = append(result, rightBytes...)
					return core.BytesValue(result), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("+",
						"can only concatenate bytes (not \""+string(r.Type())+"\") to bytes", "")
				}).
				Execute()
		}).
		Bool(func(leftBool bool) (core.Value, error) {
			// Python: bools behave like ints in arithmetic (True=1, False=0)
			leftNum := 0.0
			if leftBool {
				leftNum = 1.0
			}
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.NumberValue(leftNum + rightNum), nil
				}).
				Bool(func(rightBool bool) (core.Value, error) {
					rightNum := 0.0
					if rightBool {
						rightNum = 1.0
					}
					return core.NumberValue(leftNum + rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("+",
						"unsupported operand type(s)",
						"'bool' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("+",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// positiveValue implements unary + operator
func positiveValue(value core.Value, ctx *core.Context) (core.Value, error) {
	// Try __pos__ dunder method
	if result, found, err := types.CallDunder(value, "__pos__", []core.Value{}, ctx); found {
		return result, err
	}

	// For numeric types and bools, convert to number
	return types.Switch(value).
		Number(func(n float64) (core.Value, error) {
			return core.NumberValue(n), nil
		}).
		Bool(func(b bool) (core.Value, error) {
			// Python: +True returns 1, +False returns 0 (as int, not bool)
			if b {
				return core.NumberValue(1), nil
			}
			return core.NumberValue(0), nil
		}).
		Default(func(v core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("+",
				"bad operand type for unary +",
				"'"+string(v.Type())+"'")
		}).
		Execute()
}

// Subtract implements the - operator using protocol-based dispatch
func Subtract() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("-", args)

		// - operator requires at least 1 argument
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// Single argument: unary negation
		if v.Count() == 1 {
			return negateValue(args[0], ctx)
		}

		// Multiple arguments: left-to-right subtraction
		result := args[0]
		for i := 1; i < len(args); i++ {
			var err error
			result, err = subtractTwo(result, args[i], ctx)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	}
}

// negateValue negates a single value
func negateValue(value core.Value, ctx *core.Context) (core.Value, error) {
	// Try __neg__ dunder method
	if result, found, err := types.CallDunder(value, "__neg__", []core.Value{}, ctx); found {
		return result, err
	}

	// Try protocol-based numeric operations
	if num, ok := protocols.GetNumericOps(value); ok {
		return num.Negate()
	}

	// Fall back to type-based negation
	return types.Switch(value).
		Number(func(n float64) (core.Value, error) {
			return core.NumberValue(-n), nil
		}).
		Bool(func(b bool) (core.Value, error) {
			// Python: -True returns -1, -False returns 0 (as int, not bool)
			if b {
				return core.NumberValue(-1), nil
			}
			return core.NumberValue(0), nil
		}).
		Default(func(v core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("-",
				"bad operand type for unary -",
				"'"+string(v.Type())+"'")
		}).
		Execute()
}

// subtractTwo subtracts right from left
func subtractTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __sub__ on left operand
	if result, found, err := types.CallSub(left, right, ctx); found {
		return result, err
	}

	// Try __rsub__ on right operand
	if result, found, err := types.CallDunder(right, "__rsub__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Check if we have BigInt operands
	leftBig, leftIsBig := left.(core.BigIntValue)
	rightBig, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		// Promote both to BigInt if needed
		if !leftIsBig {
			if leftNum, ok := left.(core.NumberValue); ok {
				leftBig = core.PromoteToBigInt(leftNum)
			} else {
				return nil, errors.NewTypeError("-", "unsupported operand type(s)",
					"'"+string(left.Type())+"' and '"+string(right.Type())+"'")
			}
		}
		if !rightIsBig {
			if rightNum, ok := right.(core.NumberValue); ok {
				rightBig = core.PromoteToBigInt(rightNum)
			} else {
				return nil, errors.NewTypeError("-", "unsupported operand type(s)",
					"'"+string(left.Type())+"' and '"+string(right.Type())+"'")
			}
		}

		// Compute with BigInt
		result := new(big.Int).Sub(leftBig.GetBigInt(), rightBig.GetBigInt())
		bigIntResult := core.NewBigInt(result)

		// Try to demote to NumberValue if it fits
		if numVal, ok := core.DemoteToNumber(bigIntResult); ok {
			return numVal, nil
		}

		return bigIntResult, nil
	}

	// Try protocol-based numeric operations
	if leftNum, ok := protocols.GetNumericOps(left); ok {
		return leftNum.Subtract(right)
	}

	// Fall back to numeric subtraction
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.NumberValue(leftNum - rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("-",
						"unsupported operand type(s)",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("-",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// Multiply implements the * operator using protocol-based dispatch
func Multiply() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("*", args)

		// * operator accepts 0 or more arguments
		// With 0 args, return identity element (1)
		if v.Count() == 0 {
			return core.NumberValue(1), nil
		}

		// Single argument returns itself
		if v.Count() == 1 {
			return args[0], nil
		}

		// Multiple arguments - chain multiplications
		result := args[0]
		for i := 1; i < len(args); i++ {
			var err error
			result, err = multiplyTwo(result, args[i], ctx)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	}
}

// multiplyTwo multiplies two values
func multiplyTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __mul__ on left operand
	if result, found, err := types.CallMul(left, right, ctx); found {
		return result, err
	}

	// Try __rmul__ on right operand
	if result, found, err := types.CallDunder(right, "__rmul__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Use protocol-based numeric operations
	if leftNum, ok := protocols.GetNumericOps(left); ok {
		return leftNum.Multiply(right)
	}

	// Fall back to type-based dispatch
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			// Number * Number
			if rightNum, ok := types.AsNumber(right); ok {
				return core.NumberValue(leftNum * rightNum), nil
			}
			// Number * String (repetition)
			if rightStr, ok := types.AsString(right); ok {
				if leftNum == float64(int(leftNum)) && leftNum >= 0 {
					count := int(leftNum)
					result := ""
					for i := 0; i < count; i++ {
						result += rightStr
					}
					return core.StringValue(result), nil
				}
				return nil, errors.NewTypeError("*",
					"can't multiply sequence by non-int of type 'float'", "")
			}
			// Number * List (repetition)
			if rightList, ok := types.AsList(right); ok {
				if leftNum == float64(int(leftNum)) && leftNum >= 0 {
					count := int(leftNum)
					result := make([]core.Value, 0, rightList.Len()*count)
					for i := 0; i < count; i++ {
						result = append(result, rightList.Items()...)
					}
					return core.NewList(result...), nil
				}
				return nil, errors.NewTypeError("*",
					"can't multiply sequence by non-int of type 'float'", "")
			}
			// Number * Bytes (repetition)
			if rightBytes, ok := right.(core.BytesValue); ok {
				if leftNum == float64(int(leftNum)) && leftNum >= 0 {
					count := int(leftNum)
					result := make([]byte, 0, len(rightBytes)*count)
					for i := 0; i < count; i++ {
						result = append(result, rightBytes...)
					}
					return core.BytesValue(result), nil
				}
				return nil, errors.NewTypeError("*",
					"can't multiply sequence by non-int of type 'float'", "")
			}
			return nil, errors.NewTypeError("*",
				"unsupported operand type(s)",
				"'float' and '"+string(right.Type())+"'")
		}).
		String(func(leftStr string) (core.Value, error) {
			// String * Number (repetition)
			if rightNum, ok := types.AsNumber(right); ok {
				if rightNum == float64(int(rightNum)) && rightNum >= 0 {
					count := int(rightNum)
					result := ""
					for i := 0; i < count; i++ {
						result += leftStr
					}
					return core.StringValue(result), nil
				}
				return nil, errors.NewTypeError("*",
					"can't multiply sequence by non-int of type 'float'", "")
			}
			return nil, errors.NewTypeError("*",
				"can't multiply sequence of type 'str' by non-int of type '"+string(right.Type())+"'", "")
		}).
		List(func(leftList *core.ListValue) (core.Value, error) {
			// List * Number (repetition)
			if rightNum, ok := types.AsNumber(right); ok {
				if rightNum == float64(int(rightNum)) && rightNum >= 0 {
					count := int(rightNum)
					result := make([]core.Value, 0, leftList.Len()*count)
					for i := 0; i < count; i++ {
						result = append(result, leftList.Items()...)
					}
					return core.NewList(result...), nil
				}
				return nil, errors.NewTypeError("*",
					"can't multiply sequence by non-int of type 'float'", "")
			}
			return nil, errors.NewTypeError("*",
				"can't multiply sequence of type 'list' by non-int of type '"+string(right.Type())+"'", "")
		}).
		Type(core.BytesType, func(leftVal core.Value) (core.Value, error) {
			// Bytes * Number (repetition)
			leftBytes := leftVal.(core.BytesValue)
			if rightNum, ok := types.AsNumber(right); ok {
				if rightNum == float64(int(rightNum)) && rightNum >= 0 {
					count := int(rightNum)
					result := make([]byte, 0, len(leftBytes)*count)
					for i := 0; i < count; i++ {
						result = append(result, leftBytes...)
					}
					return core.BytesValue(result), nil
				}
				return nil, errors.NewTypeError("*",
					"can't multiply sequence by non-int of type 'float'", "")
			}
			return nil, errors.NewTypeError("*",
				"can't multiply sequence of type 'bytes' by non-int of type '"+string(right.Type())+"'", "")
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("*",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// MatMul implements the @ operator (matrix multiplication) using protocol-based dispatch
func MatMul() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("@", args)

		// @ operator requires exactly 2 arguments
		if v.Count() != 2 {
			return nil, fmt.Errorf("@ requires exactly 2 arguments, got %d", v.Count())
		}

		left := args[0]
		right := args[1]

		// Try __matmul__ on left operand
		if result, found, err := types.CallDunder(left, "__matmul__", []core.Value{right}, ctx); found {
			return result, err
		}

		// Try __rmatmul__ on right operand
		if result, found, err := types.CallDunder(right, "__rmatmul__", []core.Value{left}, ctx); found {
			return result, err
		}

		// No __matmul__ or __rmatmul__ method found
		return nil, errors.NewTypeError("@",
			"unsupported operand type(s) for @",
			"'"+string(left.Type())+"' and '"+string(right.Type())+"'")
	}
}

// Divide implements the / operator using protocol-based dispatch
func Divide() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("/", args)

		// / operator requires at least 1 argument
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// Single argument: 1/x
		if v.Count() == 1 {
			return divideValue(core.NumberValue(1), args[0], ctx)
		}

		// Multiple arguments: a / b / c / ...
		result := args[0]
		for i := 1; i < len(args); i++ {
			var err error
			result, err = divideValue(result, args[i], ctx)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	}
}

// divideValue divides left by right
func divideValue(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __truediv__ on left operand
	if result, found, err := types.CallDunder(left, "__truediv__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rtruediv__ on right operand
	if result, found, err := types.CallDunder(right, "__rtruediv__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Try protocol-based numeric operations
	if leftNum, ok := protocols.GetNumericOps(left); ok {
		return leftNum.Divide(right)
	}

	// Fall back to numeric division
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					if rightNum == 0 {
						return nil, &core.ZeroDivisionError{}
					}
					return core.NumberValue(leftNum / rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("/",
						"unsupported operand type(s)",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("/",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// FloorDivide implements the // operator (integer division) using protocol-based dispatch
func FloorDivide() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("//", args)

		// // operator requires at least 1 argument
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// Single argument: 1//x
		if v.Count() == 1 {
			return floorDivideValue(core.NumberValue(1), args[0], ctx)
		}

		// Multiple arguments: a // b // c / ...
		result := args[0]
		for i := 1; i < len(args); i++ {
			var err error
			result, err = floorDivideValue(result, args[i], ctx)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	}
}

// floorDivideValue performs integer division: left // right
func floorDivideValue(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __floordiv__ on left operand
	if result, found, err := types.CallDunder(left, "__floordiv__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rfloordiv__ on right operand
	if result, found, err := types.CallDunder(right, "__rfloordiv__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Fall back to numeric floor division
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					if rightNum == 0 {
						return nil, &core.ZeroDivisionError{}
					}
					// Python-style floor division
					result := math.Floor(leftNum / rightNum)
					return core.NumberValue(result), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("//",
						"unsupported operand type(s)",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("//",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// Modulo implements the % operator using protocol-based dispatch
func Modulo() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("%", args)

		// % operator requires exactly 2 arguments
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return moduloTwo(args[0], args[1], ctx)
	}
}

// moduloTwo computes left % right
func moduloTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __mod__ on left operand
	if result, found, err := types.CallDunder(left, "__mod__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rmod__ on right operand
	if result, found, err := types.CallDunder(right, "__rmod__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Try protocol-based numeric operations
	if leftNum, ok := protocols.GetNumericOps(left); ok {
		return leftNum.Modulo(right)
	}

	// Fall back to numeric modulo
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					if rightNum == 0 {
						return nil, &core.ZeroDivisionError{}
					}
					// Python-compatible modulo
					result := leftNum - rightNum*float64(int(leftNum/rightNum))
					// Adjust for Python's behavior with negative numbers
					if (result < 0 && rightNum > 0) || (result > 0 && rightNum < 0) {
						result += rightNum
					}
					return core.NumberValue(result), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("%",
						"unsupported operand type(s)",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("%",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// Power implements the ** operator using protocol-based dispatch
func Power() func([]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("**", args)

		// ** operator requires exactly 2 arguments
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return powerTwo(args[0], args[1], ctx)
	}
}

// powerTwo computes left ** right
func powerTwo(left, right core.Value, ctx *core.Context) (core.Value, error) {
	// Try __pow__ on left operand
	if result, found, err := types.CallDunder(left, "__pow__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __rpow__ on right operand
	if result, found, err := types.CallDunder(right, "__rpow__", []core.Value{left}, ctx); found {
		return result, err
	}

	// Try protocol-based numeric operations
	if leftNum, ok := protocols.GetNumericOps(left); ok {
		return leftNum.Power(right)
	}

	// Check if we have BigInt operands
	_, leftIsBig := left.(core.BigIntValue)
	_, rightIsBig := right.(core.BigIntValue)

	if leftIsBig || rightIsBig {
		return bigIntPower(left, right)
	}

	// Fall back to numeric power
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Special case: 0 ** 0 = 1 in Python
					if leftNum == 0 && rightNum == 0 {
						return core.NumberValue(1), nil
					}

					// Check if this is integer exponentiation
					leftIsInt := core.IsInteger(leftNum)
					rightIsInt := core.IsInteger(rightNum)

					if leftIsInt && rightIsInt && rightNum >= 0 && rightNum <= 10000 {
						// For integer exponentiation, estimate if result needs BigInt
						// BEFORE computing with float64 to avoid precision loss
						if leftNum != 0 && math.Abs(leftNum) > 1 {
							// Estimate: log2(result) = exp * log2(abs(base))
							// If this exceeds 53 bits, use BigInt for precise computation
							estimatedBits := rightNum * math.Log2(math.Abs(leftNum))
							if estimatedBits > 53 {
								// Use BigInt to preserve precision
								return bigIntPower(left, right)
							}
						}

						// Safe to use float64
						result := math.Pow(leftNum, rightNum)

						// Double-check if result overflowed or needs BigInt
						if math.IsInf(result, 0) {
							return bigIntPower(left, right)
						}

						return core.NumberValue(result), nil
					}

					// For non-integer or negative exponents, use float math
					result := math.Pow(leftNum, rightNum)
					return core.NumberValue(result), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("**",
						"unsupported operand type(s)",
						"'float' and '"+string(r.Type())+"'")
				}).
				Execute()
		}).
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("**",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
}

// bigIntPower computes power using arbitrary precision integers
func bigIntPower(left, right core.Value) (core.Value, error) {
	// Convert both operands to big.Int
	var leftBig, rightBig *big.Int

	switch l := left.(type) {
	case core.BigIntValue:
		leftBig = l.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(l)) {
			return nil, errors.NewTypeError("**",
				"unsupported operand type(s)",
				"'float' (for BigInt power)")
		}
		leftBig = core.PromoteToBigInt(l).GetBigInt()
	default:
		return nil, errors.NewTypeError("**",
			"unsupported operand type(s)",
			"'"+string(left.Type())+"'")
	}

	switch r := right.(type) {
	case core.BigIntValue:
		rightBig = r.GetBigInt()
	case core.NumberValue:
		if !core.IsInteger(float64(r)) {
			return nil, errors.NewTypeError("**",
				"unsupported operand type(s)",
				"'float' (for BigInt power)")
		}
		rightBig = core.PromoteToBigInt(r).GetBigInt()
	default:
		return nil, errors.NewTypeError("**",
			"unsupported operand type(s)",
			"'"+string(right.Type())+"'")
	}

	// Check exponent is non-negative
	if rightBig.Sign() < 0 {
		// Python allows negative exponents but returns float
		// base^(-n) = 1 / base^n
		leftFloat, _ := new(big.Float).SetInt(leftBig).Float64()
		rightFloat, _ := new(big.Float).SetInt(rightBig).Float64()
		result := math.Pow(leftFloat, rightFloat)
		return core.NumberValue(result), nil
	}

	// Check exponent is reasonable size (prevent DoS)
	if rightBig.BitLen() > 31 {
		return nil, fmt.Errorf("exponent too large")
	}

	// Compute base^exp using big.Int
	result := new(big.Int).Exp(leftBig, rightBig, nil)
	bigIntResult := core.NewBigInt(result)

	// Try to demote to NumberValue if it fits
	if numVal, ok := core.DemoteToNumber(bigIntResult); ok {
		return numVal, nil
	}

	return bigIntResult, nil
}

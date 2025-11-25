// Package operators provides arithmetic and comparison operators for M28
package operators

import (
	"fmt"
	"math"
	"math/big"
	"reflect"
	"strings"
	"sync"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
	"github.com/mmichie/m28/modules"
)

// Global operator registry for fast lookup without context traversal
var (
	operatorRegistry   = make(map[string]core.Value)
	operatorRegistryMu sync.RWMutex
)

func init() {
	// Set the hook so context.Lookup can check the operator registry
	core.GetOperatorFunc = GetOperator
}

// GetOperator returns an operator from the global registry (fast path)
// Returns (operator, true) if found, (nil, false) otherwise
func GetOperator(name string) (core.Value, bool) {
	operatorRegistryMu.RLock()
	defer operatorRegistryMu.RUnlock()
	op, exists := operatorRegistry[name]
	return op, exists
}

// registerOperator adds an operator to both context and global registry
func registerOperator(ctx *core.Context, name string, op core.Value) {
	ctx.Define(name, op)

	operatorRegistryMu.Lock()
	operatorRegistry[name] = op
	operatorRegistryMu.Unlock()
}

// RegisterAll registers all operators
func RegisterAll(ctx *core.Context) {
	// Arithmetic operators
	registerOperator(ctx, "+", core.NewBuiltinFunction(Add()))
	registerOperator(ctx, "-", core.NewBuiltinFunction(Subtract()))
	registerOperator(ctx, "*", core.NewBuiltinFunction(Multiply()))
	registerOperator(ctx, "/", core.NewBuiltinFunction(Divide()))
	registerOperator(ctx, "//", core.NewBuiltinFunction(FloorDivide()))
	registerOperator(ctx, "%", core.NewBuiltinFunction(Modulo()))
	registerOperator(ctx, "**", core.NewBuiltinFunction(Power()))
	registerOperator(ctx, "@", core.NewBuiltinFunction(MatMul()))

	// Comparison operators
	registerOperator(ctx, "==", core.NewBuiltinFunction(Equal()))
	registerOperator(ctx, "!=", core.NewBuiltinFunction(NotEqual()))
	registerOperator(ctx, "<", core.NewBuiltinFunction(LessThan()))
	registerOperator(ctx, "<=", core.NewBuiltinFunction(LessThanOrEqual()))
	registerOperator(ctx, ">", core.NewBuiltinFunction(GreaterThan()))
	registerOperator(ctx, ">=", core.NewBuiltinFunction(GreaterThanOrEqual()))

	// Logical operators
	registerOperator(ctx, "not", core.NewBuiltinFunction(Not()))
	registerOperator(ctx, "and", core.NewBuiltinFunction(And()))
	registerOperator(ctx, "or", core.NewBuiltinFunction(Or()))
	registerOperator(ctx, "in", core.NewBuiltinFunction(In()))
	registerOperator(ctx, "not in", core.NewBuiltinFunction(NotIn()))
	registerOperator(ctx, "is", core.NewBuiltinFunction(Is()))
	registerOperator(ctx, "is not", core.NewBuiltinFunction(IsNot()))

	// Bitwise operators
	registerOperator(ctx, "<<", core.NewBuiltinFunction(LeftShift()))
	registerOperator(ctx, ">>", core.NewBuiltinFunction(RightShift()))
	registerOperator(ctx, "&", core.NewBuiltinFunction(BitwiseAnd()))
	registerOperator(ctx, "|", core.NewBuiltinFunction(BitwiseOr()))
	registerOperator(ctx, "^", core.NewBuiltinFunction(BitwiseXor()))
	registerOperator(ctx, "~", core.NewBuiltinFunction(BitwiseInvert()))
}

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
	// Try __ne__ on left operand
	if result, found, err := types.CallDunder(left, "__ne__", []core.Value{right}, ctx); found {
		return result, err
	}

	// Try __ne__ on right operand
	if result, found, err := types.CallDunder(right, "__ne__", []core.Value{left}, ctx); found {
		return result, err
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
			// Ensure we return a boolean
			if b, ok := result.(core.BoolValue); ok {
				return b, nil
			}
			return nil, errors.NewTypeError("in", "__contains__ should return a boolean", string(result.Type()))
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
					// Check if it's a KeyError by looking at the error message
					if strings.Contains(err.Error(), "KeyError") {
						return core.BoolValue(false), nil
					}
					// Some other error, propagate it
					return nil, err
				}
				// __getitem__ succeeded, key exists
				return core.BoolValue(true), nil
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

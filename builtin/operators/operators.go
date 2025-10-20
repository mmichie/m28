// Package operators provides arithmetic and comparison operators for M28
package operators

import (
	"math"
	"reflect"
	"strings"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// RegisterAll registers all operators
func RegisterAll(ctx *core.Context) {
	// Arithmetic operators
	ctx.Define("+", core.NewBuiltinFunction(Add()))
	ctx.Define("-", core.NewBuiltinFunction(Subtract()))
	ctx.Define("*", core.NewBuiltinFunction(Multiply()))
	ctx.Define("/", core.NewBuiltinFunction(Divide()))
	ctx.Define("//", core.NewBuiltinFunction(FloorDivide()))
	ctx.Define("%", core.NewBuiltinFunction(Modulo()))
	ctx.Define("**", core.NewBuiltinFunction(Power()))

	// Comparison operators
	ctx.Define("==", core.NewBuiltinFunction(Equal()))
	ctx.Define("!=", core.NewBuiltinFunction(NotEqual()))
	ctx.Define("<", core.NewBuiltinFunction(LessThan()))
	ctx.Define("<=", core.NewBuiltinFunction(LessThanOrEqual()))
	ctx.Define(">", core.NewBuiltinFunction(GreaterThan()))
	ctx.Define(">=", core.NewBuiltinFunction(GreaterThanOrEqual()))

	// Logical operators
	ctx.Define("not", core.NewBuiltinFunction(Not()))
	ctx.Define("and", core.NewBuiltinFunction(And()))
	ctx.Define("or", core.NewBuiltinFunction(Or()))
	ctx.Define("in", core.NewBuiltinFunction(In()))
	ctx.Define("not in", core.NewBuiltinFunction(NotIn()))
	ctx.Define("is", core.NewBuiltinFunction(Is()))
	ctx.Define("is not", core.NewBuiltinFunction(IsNot()))

	// Bitwise operators
	ctx.Define("<<", core.NewBuiltinFunction(LeftShift()))
	ctx.Define(">>", core.NewBuiltinFunction(RightShift()))
	ctx.Define("&", core.NewBuiltinFunction(BitwiseAnd()))
	ctx.Define("|", core.NewBuiltinFunction(BitwiseOr()))
	ctx.Define("^", core.NewBuiltinFunction(BitwiseXor()))
	ctx.Define("~", core.NewBuiltinFunction(BitwiseInvert()))
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
		Default(func(l core.Value) (core.Value, error) {
			return nil, errors.NewTypeError("*",
				"unsupported operand type(s)",
				"'"+string(l.Type())+"'")
		}).
		Execute()
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

	// Fall back to numeric power
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Special case: 0 ** 0 = 1 in Python
					if leftNum == 0 && rightNum == 0 {
						return core.NumberValue(1), nil
					}
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
	// Try __eq__ on left operand
	if result, found, err := types.CallDunder(left, "__eq__", []core.Value{right}, ctx); found {
		return result, err
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
		// For simplicity, we'll say they're only identical if equal
		// In real Python: small ints (-5 to 256) and some strings are interned
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

	// For numeric types, return the value as-is
	return types.Switch(value).
		Number(func(n float64) (core.Value, error) {
			return core.NumberValue(n), nil
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

	// Fall back to integer left shift
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Check both are integers
					if leftNum != float64(int(leftNum)) {
						return nil, errors.NewTypeError("<<",
							"unsupported operand type(s)",
							"'float' and '"+string(right.Type())+"'")
					}
					if rightNum != float64(int(rightNum)) || rightNum < 0 {
						return nil, errors.NewTypeError("<<",
							"unsupported operand type(s)",
							"'int' and 'float'")
					}
					result := int(leftNum) << uint(rightNum)
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

	// Fall back to integer right shift
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Check both are integers
					if leftNum != float64(int(leftNum)) {
						return nil, errors.NewTypeError(">>",
							"unsupported operand type(s)",
							"'float' and '"+string(right.Type())+"'")
					}
					if rightNum != float64(int(rightNum)) || rightNum < 0 {
						return nil, errors.NewTypeError(">>",
							"unsupported operand type(s)",
							"'int' and 'float'")
					}
					result := int(leftNum) >> uint(rightNum)
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

	// Fall back to integer bitwise AND
	return types.Switch(left).
		Bool(func(leftBool bool) (core.Value, error) {
			leftInt := 0
			if leftBool {
				leftInt = 1
			}
			return types.Switch(right).
				Bool(func(rightBool bool) (core.Value, error) {
					rightInt := 0
					if rightBool {
						rightInt = 1
					}
					result := leftInt & rightInt
					return core.BoolValue(result != 0), nil
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

	// Fall back to integer bitwise OR
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
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

	// Fall back to integer bitwise XOR
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					// Check both are integers
					if leftNum != float64(int(leftNum)) || rightNum != float64(int(rightNum)) {
						return nil, errors.NewTypeError("^",
							"unsupported operand type(s)",
							"'float' and 'float'")
					}
					result := int(leftNum) ^ int(rightNum)
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

// Package operators provides arithmetic and comparison operators for M28
package operators

import (
	"math"
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

		// Single argument returns itself
		if v.Count() == 1 {
			return args[0], nil
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

	// Fall back to type-based dispatch using TypeSwitch
	return types.Switch(left).
		Number(func(leftNum float64) (core.Value, error) {
			return types.Switch(right).
				Number(func(rightNum float64) (core.Value, error) {
					return core.NumberValue(leftNum + rightNum), nil
				}).
				Default(func(r core.Value) (core.Value, error) {
					return nil, errors.NewTypeError("+",
						"unsupported operand type(s)",
						"'float' and '"+string(r.Type())+"'")
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
		List(func(leftList core.ListValue) (core.Value, error) {
			return types.Switch(right).
				List(func(rightList core.ListValue) (core.Value, error) {
					result := make(core.ListValue, 0, len(leftList)+len(rightList))
					result = append(result, leftList...)
					result = append(result, rightList...)
					return result, nil
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
					result := make(core.ListValue, 0, len(rightList)*count)
					for i := 0; i < count; i++ {
						result = append(result, rightList...)
					}
					return result, nil
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
		List(func(leftList core.ListValue) (core.Value, error) {
			// List * Number (repetition)
			if rightNum, ok := types.AsNumber(right); ok {
				if rightNum == float64(int(rightNum)) && rightNum >= 0 {
					count := int(rightNum)
					result := make(core.ListValue, 0, len(leftList)*count)
					for i := 0; i < count; i++ {
						result = append(result, leftList...)
					}
					return result, nil
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

		case core.ListValue:
			for _, item := range c {
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

// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/core"
)

// RegisterArithmeticFunctions registers arithmetic functions in the global context
func RegisterArithmeticFunctions(ctx *core.Context) {
	// Basic arithmetic operations
	ctx.Define("+", core.NewBuiltinFunction(AddFunc))
	ctx.Define("-", core.NewBuiltinFunction(SubtractFunc))
	ctx.Define("*", core.NewBuiltinFunction(MultiplyFunc))
	ctx.Define("/", core.NewBuiltinFunction(DivideFunc))
	ctx.Define("%", core.NewBuiltinFunction(ModuloFunc))
	ctx.Define("**", core.NewBuiltinFunction(PowFunc)) // Power operator

	// Math functions
	ctx.Define("abs", core.NewBuiltinFunction(AbsFunc))
	ctx.Define("sqrt", core.NewBuiltinFunction(SqrtFunc))
	ctx.Define("pow", core.NewBuiltinFunction(PowFunc))
	ctx.Define("max", core.NewBuiltinFunction(MaxFunc))
	ctx.Define("min", core.NewBuiltinFunction(MinFunc))
	ctx.Define("round", core.NewBuiltinFunction(RoundFunc))
	ctx.Define("floor", core.NewBuiltinFunction(FloorFunc))
	ctx.Define("ceil", core.NewBuiltinFunction(CeilFunc))
}

// AddFunc implements the + operation
func AddFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.NumberValue(0), nil
	}

	// For operator overloading, we need to handle multiple arguments by chaining calls
	// Check if we should use __add__ method
	// Skip operator overloading for built-in types that have optimized implementations
	useAddMethod := false
	switch args[0].(type) {
	case core.NumberValue, core.StringValue, core.ListValue:
		// Use built-in fast paths for these types
		useAddMethod = false
	default:
		// Check for __add__ method on custom objects
		if len(args) >= 2 {
			if obj, ok := args[0].(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if _, found := obj.GetAttr("__add__"); found {
					useAddMethod = true
				}
			}
		}
	}
	
	if useAddMethod {
		// Chain __add__ calls for multiple arguments
		result := args[0]
		for i := 1; i < len(args); i++ {
			// Get __add__ method from current result
			if obj, ok := result.(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if method, found := obj.GetAttr("__add__"); found {
					if callable, ok := method.(interface {
						Call([]core.Value, *core.Context) (core.Value, error)
					}); ok {
						var err error
						result, err = callable.Call([]core.Value{args[i]}, ctx)
						if err != nil {
							return nil, err
						}
					}
				}
			}
		}
		return result, nil
	}

	switch first := args[0].(type) {
	case core.NumberValue:
		// Number addition
		result := float64(first)
		for _, arg := range args[1:] {
			if num, ok := arg.(core.NumberValue); ok {
				result += float64(num)
			} else {
				return nil, fmt.Errorf("cannot add %s to number", arg.Type())
			}
		}
		return core.NumberValue(result), nil

	case core.StringValue:
		// String concatenation
		result := string(first)
		for _, arg := range args[1:] {
			if str, ok := arg.(core.StringValue); ok {
				result += string(str)
			} else {
				return nil, fmt.Errorf("cannot concatenate %s to string", arg.Type())
			}
		}
		return core.StringValue(result), nil

	case core.ListValue:
		// List concatenation
		result := make(core.ListValue, len(first))
		copy(result, first)
		for _, arg := range args[1:] {
			if list, ok := arg.(core.ListValue); ok {
				result = append(result, list...)
			} else {
				return nil, fmt.Errorf("cannot concatenate %s to list", arg.Type())
			}
		}
		return result, nil

	default:
		return nil, fmt.Errorf("cannot apply + to %s", first.Type())
	}
}

// SubtractFunc implements the - operation
func SubtractFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("- requires at least one argument")
	}

	if len(args) == 1 {
		// Unary negation
		if num, ok := args[0].(core.NumberValue); ok {
			return core.NumberValue(-float64(num)), nil
		}
		return nil, fmt.Errorf("cannot negate %s", args[0].Type())
	}

	// Check if the first argument has __sub__ method (operator overloading)
	// Skip for built-in numeric types
	if _, isNumber := args[0].(core.NumberValue); !isNumber && len(args) >= 2 {
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if method, found := obj.GetAttr("__sub__"); found {
				if _, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					// Chain __sub__ calls for multiple arguments
					result := args[0]
					for i := 1; i < len(args); i++ {
						if obj, ok := result.(interface {
							GetAttr(string) (core.Value, bool)
						}); ok {
							if method, found := obj.GetAttr("__sub__"); found {
								if subCallable, ok := method.(interface {
									Call([]core.Value, *core.Context) (core.Value, error)
								}); ok {
									var err error
									result, err = subCallable.Call([]core.Value{args[i]}, ctx)
									if err != nil {
										return nil, err
									}
								}
							}
						}
					}
					return result, nil
				}
			}
		}
	}

	// Binary subtraction
	if first, ok := args[0].(core.NumberValue); ok {
		result := float64(first)
		for _, arg := range args[1:] {
			if num, ok := arg.(core.NumberValue); ok {
				result -= float64(num)
			} else {
				return nil, fmt.Errorf("cannot subtract %s from number", arg.Type())
			}
		}
		return core.NumberValue(result), nil
	}

	return nil, fmt.Errorf("cannot apply - to %s", args[0].Type())
}

// MultiplyFunc implements the * operation
func MultiplyFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.NumberValue(1), nil
	}

	// Check if the first argument has __mul__ method (operator overloading)
	// Skip for built-in types that have optimized implementations
	switch args[0].(type) {
	case core.NumberValue, core.StringValue, core.ListValue:
		// Use built-in fast paths for these types
	default:
		if len(args) >= 2 {
			if obj, ok := args[0].(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if method, found := obj.GetAttr("__mul__"); found {
					if _, ok := method.(interface {
						Call([]core.Value, *core.Context) (core.Value, error)
					}); ok {
						// Chain __mul__ calls for multiple arguments
						result := args[0]
						for i := 1; i < len(args); i++ {
							if obj, ok := result.(interface {
								GetAttr(string) (core.Value, bool)
							}); ok {
								if method, found := obj.GetAttr("__mul__"); found {
									if mulCallable, ok := method.(interface {
										Call([]core.Value, *core.Context) (core.Value, error)
									}); ok {
										var err error
										result, err = mulCallable.Call([]core.Value{args[i]}, ctx)
										if err != nil {
											return nil, err
										}
									}
								}
							}
						}
						return result, nil
					}
				}
			}
		}
	}

	switch first := args[0].(type) {
	case core.NumberValue:
		// Number multiplication
		result := float64(first)
		for _, arg := range args[1:] {
			if num, ok := arg.(core.NumberValue); ok {
				result *= float64(num)
			} else {
				return nil, fmt.Errorf("cannot multiply number by %s", arg.Type())
			}
		}
		return core.NumberValue(result), nil

	case core.StringValue:
		// String repetition (only with one number)
		if len(args) != 2 {
			return nil, fmt.Errorf("string repetition requires exactly one number")
		}
		if count, ok := args[1].(core.NumberValue); ok {
			n := int(count)
			if n < 0 {
				return nil, fmt.Errorf("cannot repeat string a negative number of times")
			}
			result := ""
			for i := 0; i < n; i++ {
				result += string(first)
			}
			return core.StringValue(result), nil
		}
		return nil, fmt.Errorf("cannot multiply string by %s", args[1].Type())

	case core.ListValue:
		// List repetition (only with one number)
		if len(args) != 2 {
			return nil, fmt.Errorf("list repetition requires exactly one number")
		}
		if count, ok := args[1].(core.NumberValue); ok {
			n := int(count)
			if n < 0 {
				return nil, fmt.Errorf("cannot repeat list a negative number of times")
			}
			result := make(core.ListValue, 0, len(first)*n)
			for i := 0; i < n; i++ {
				result = append(result, first...)
			}
			return result, nil
		}
		return nil, fmt.Errorf("cannot multiply list by %s", args[1].Type())

	default:
		return nil, fmt.Errorf("cannot apply * to %s", first.Type())
	}
}

// DivideFunc implements the / operation
func DivideFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("/ requires at least one argument")
	}

	if len(args) == 1 {
		// Reciprocal
		if num, ok := args[0].(core.NumberValue); ok {
			if float64(num) == 0 {
				return nil, &core.ZeroDivisionError{}
			}
			return core.NumberValue(1 / float64(num)), nil
		}
		return nil, fmt.Errorf("cannot take reciprocal of %s", args[0].Type())
	}

	// Check if the first argument has __truediv__ method (operator overloading)
	// Skip for built-in numeric types
	if _, isNumber := args[0].(core.NumberValue); !isNumber && len(args) >= 2 {
		if obj, ok := args[0].(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if method, found := obj.GetAttr("__truediv__"); found {
				if _, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					// Chain __truediv__ calls for multiple arguments
					result := args[0]
					for i := 1; i < len(args); i++ {
						if obj, ok := result.(interface {
							GetAttr(string) (core.Value, bool)
						}); ok {
							if method, found := obj.GetAttr("__truediv__"); found {
								if divCallable, ok := method.(interface {
									Call([]core.Value, *core.Context) (core.Value, error)
								}); ok {
									var err error
									result, err = divCallable.Call([]core.Value{args[i]}, ctx)
									if err != nil {
										return nil, err
									}
								}
							}
						}
					}
					return result, nil
				}
			}
		}
	}

	// Division
	if first, ok := args[0].(core.NumberValue); ok {
		result := float64(first)
		for _, arg := range args[1:] {
			if num, ok := arg.(core.NumberValue); ok {
				if float64(num) == 0 {
					return nil, &core.ZeroDivisionError{}
				}
				result /= float64(num)
			} else {
				return nil, fmt.Errorf("cannot divide number by %s", arg.Type())
			}
		}
		return core.NumberValue(result), nil
	}

	return nil, fmt.Errorf("cannot apply / to %s", args[0].Type())
}

// ModuloFunc implements the % operation
func ModuloFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("%% requires exactly 2 arguments")
	}

	if a, ok := args[0].(core.NumberValue); ok {
		if b, ok := args[1].(core.NumberValue); ok {
			if float64(b) == 0 {
				return nil, fmt.Errorf("modulo by zero")
			}
			return core.NumberValue(math.Mod(float64(a), float64(b))), nil
		}
		return nil, fmt.Errorf("cannot calculate modulo with %s", args[1].Type())
	}

	return nil, fmt.Errorf("cannot apply %% to %s", args[0].Type())
}

// AbsFunc implements the abs function
func AbsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("abs requires exactly 1 argument")
	}

	if num, ok := args[0].(core.NumberValue); ok {
		return core.NumberValue(math.Abs(float64(num))), nil
	}

	return nil, fmt.Errorf("cannot calculate absolute value of %s", args[0].Type())
}

// SqrtFunc implements the sqrt function
func SqrtFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sqrt requires exactly 1 argument")
	}

	if num, ok := args[0].(core.NumberValue); ok {
		if float64(num) < 0 {
			return nil, fmt.Errorf("cannot calculate square root of negative number")
		}
		return core.NumberValue(math.Sqrt(float64(num))), nil
	}

	return nil, fmt.Errorf("cannot calculate square root of %s", args[0].Type())
}

// PowFunc implements the pow function
func PowFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("pow requires exactly 2 arguments")
	}

	if base, ok := args[0].(core.NumberValue); ok {
		if exp, ok := args[1].(core.NumberValue); ok {
			return core.NumberValue(math.Pow(float64(base), float64(exp))), nil
		}
		return nil, fmt.Errorf("exponent must be a number, got %s", args[1].Type())
	}

	return nil, fmt.Errorf("base must be a number, got %s", args[0].Type())
}

// MaxFunc implements the max function
func MaxFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("max requires at least 1 argument")
	}

	if len(args) == 1 {
		if list, ok := args[0].(core.ListValue); ok {
			if len(list) == 0 {
				return nil, fmt.Errorf("cannot calculate max of empty list")
			}
			return maxList(list)
		}
		return args[0], nil
	}

	return maxList(args)
}

// maxList finds the maximum value in a list
func maxList(list []core.Value) (core.Value, error) {
	if len(list) == 0 {
		return nil, fmt.Errorf("cannot calculate max of empty list")
	}

	// Check if all elements are numbers
	if _, ok := list[0].(core.NumberValue); ok {
		max := float64(list[0].(core.NumberValue))
		for _, v := range list[1:] {
			if num, ok := v.(core.NumberValue); ok {
				if float64(num) > max {
					max = float64(num)
				}
			} else {
				return nil, fmt.Errorf("all arguments to max must be numbers, got %s", v.Type())
			}
		}
		return core.NumberValue(max), nil
	}

	// Check if all elements are strings
	if _, ok := list[0].(core.StringValue); ok {
		max := string(list[0].(core.StringValue))
		for _, v := range list[1:] {
			if str, ok := v.(core.StringValue); ok {
				if string(str) > max {
					max = string(str)
				}
			} else {
				return nil, fmt.Errorf("all arguments to max must be strings, got %s", v.Type())
			}
		}
		return core.StringValue(max), nil
	}

	return nil, fmt.Errorf("cannot calculate max of %s", list[0].Type())
}

// MinFunc implements the min function
func MinFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("min requires at least 1 argument")
	}

	if len(args) == 1 {
		if list, ok := args[0].(core.ListValue); ok {
			if len(list) == 0 {
				return nil, fmt.Errorf("cannot calculate min of empty list")
			}
			return minList(list)
		}
		return args[0], nil
	}

	return minList(args)
}

// minList finds the minimum value in a list
func minList(list []core.Value) (core.Value, error) {
	if len(list) == 0 {
		return nil, fmt.Errorf("cannot calculate min of empty list")
	}

	// Check if all elements are numbers
	if _, ok := list[0].(core.NumberValue); ok {
		min := float64(list[0].(core.NumberValue))
		for _, v := range list[1:] {
			if num, ok := v.(core.NumberValue); ok {
				if float64(num) < min {
					min = float64(num)
				}
			} else {
				return nil, fmt.Errorf("all arguments to min must be numbers, got %s", v.Type())
			}
		}
		return core.NumberValue(min), nil
	}

	// Check if all elements are strings
	if _, ok := list[0].(core.StringValue); ok {
		min := string(list[0].(core.StringValue))
		for _, v := range list[1:] {
			if str, ok := v.(core.StringValue); ok {
				if string(str) < min {
					min = string(str)
				}
			} else {
				return nil, fmt.Errorf("all arguments to min must be strings, got %s", v.Type())
			}
		}
		return core.StringValue(min), nil
	}

	return nil, fmt.Errorf("cannot calculate min of %s", list[0].Type())
}

// RoundFunc implements the round function
func RoundFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("round requires exactly 1 argument")
	}

	if num, ok := args[0].(core.NumberValue); ok {
		return core.NumberValue(math.Round(float64(num))), nil
	}

	return nil, fmt.Errorf("cannot round %s", args[0].Type())
}

// FloorFunc implements the floor function
func FloorFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("floor requires exactly 1 argument")
	}

	if num, ok := args[0].(core.NumberValue); ok {
		return core.NumberValue(math.Floor(float64(num))), nil
	}

	return nil, fmt.Errorf("cannot floor %s", args[0].Type())
}

// CeilFunc implements the ceil function
func CeilFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("ceil requires exactly 1 argument")
	}

	if num, ok := args[0].(core.NumberValue); ok {
		return core.NumberValue(math.Ceil(float64(num))), nil
	}

	return nil, fmt.Errorf("cannot ceil %s", args[0].Type())
}

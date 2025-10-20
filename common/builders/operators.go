package builders

import (
	"math"
	"strings"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// OperatorHandler represents a handler for a specific type combination
type OperatorHandler struct {
	// Check returns true if this handler can handle the given arguments
	Check func(args []core.Value) bool
	// Handle processes the arguments and returns the result
	Handle func(args []core.Value, ctx *core.Context) (core.Value, error)
}

// OperatorBuilder builds operators with overloading support
type OperatorBuilder struct {
	name         string
	dunderMethod string
	handlers     []OperatorHandler
	minArgs      int
	maxArgs      int
}

// NewOperator creates a new operator builder
func NewOperator(name, dunderMethod string) *OperatorBuilder {
	return &OperatorBuilder{
		name:         name,
		dunderMethod: dunderMethod,
		handlers:     []OperatorHandler{},
		minArgs:      2,
		maxArgs:      -1, // unlimited by default
	}
}

// WithArgs sets the argument count constraints
func (ob *OperatorBuilder) WithArgs(min, max int) *OperatorBuilder {
	ob.minArgs = min
	ob.maxArgs = max
	return ob
}

// WithHandler adds a type-specific handler
func (ob *OperatorBuilder) WithHandler(handler OperatorHandler) *OperatorBuilder {
	ob.handlers = append(ob.handlers, handler)
	return ob
}

// Build creates the operator function
func (ob *OperatorBuilder) Build() BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(ob.name, args)

		// Validate argument count
		if ob.maxArgs < 0 {
			if err := v.Min(ob.minArgs); err != nil {
				return nil, err
			}
		} else {
			if err := v.Range(ob.minArgs, ob.maxArgs); err != nil {
				return nil, err
			}
		}

		// For operators with 2+ args, check for dunder method first
		if len(args) >= 2 && ob.dunderMethod != "" {
			// Check if first argument has the dunder method
			if obj, ok := args[0].(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if method, found := obj.GetAttr(ob.dunderMethod); found {
					if callable, ok := method.(core.Callable); ok {
						// For variadic operators, chain calls
						result := args[0]
						for i := 1; i < len(args); i++ {
							var err error
							result, err = callable.Call([]core.Value{args[i]}, ctx)
							if err != nil {
								return nil, err
							}
							// Update callable for next iteration if needed
							if i < len(args)-1 {
								if obj, ok := result.(interface {
									GetAttr(string) (core.Value, bool)
								}); ok {
									if method, found := obj.GetAttr(ob.dunderMethod); found {
										if c, ok := method.(core.Callable); ok {
											callable = c
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

		// Try type-specific handlers
		for _, handler := range ob.handlers {
			if handler.Check(args) {
				return handler.Handle(args, ctx)
			}
		}

		// No handler found
		if len(args) > 0 {
			return nil, errors.NewTypeErrorf(ob.name,
				"unsupported operand type(s) for %s: '%s'", ob.name, args[0].Type())
		}

		// For operators with no arguments, try to use the first handler that accepts empty args
		for _, handler := range ob.handlers {
			// Try calling with empty args
			if result, err := handler.Handle(args, ctx); err == nil {
				return result, nil
			}
		}

		return nil, errors.NewRuntimeError(ob.name, "no arguments provided")
	}
}

// Common operator handlers

// NumberHandler creates a handler for numeric operations
func NumberHandler(fn func([]float64) (core.Value, error)) OperatorHandler {
	return OperatorHandler{
		Check: func(args []core.Value) bool {
			// Empty args are valid for operators with identity elements
			if len(args) == 0 {
				return true
			}
			for _, arg := range args {
				if _, ok := arg.(core.NumberValue); !ok {
					return false
				}
			}
			return true
		},
		Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
			numbers := make([]float64, len(args))
			for i, arg := range args {
				numbers[i] = float64(arg.(core.NumberValue))
			}
			return fn(numbers)
		},
	}
}

// StringHandler creates a handler for string operations
func StringHandler(fn func([]string) (core.Value, error)) OperatorHandler {
	return OperatorHandler{
		Check: func(args []core.Value) bool {
			for _, arg := range args {
				if _, ok := arg.(core.StringValue); !ok {
					return false
				}
			}
			return true
		},
		Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
			strings := make([]string, len(args))
			for i, arg := range args {
				strings[i] = string(arg.(core.StringValue))
			}
			return fn(strings)
		},
	}
}

// ListHandler creates a handler for list operations
func ListHandler(fn func([]*core.ListValue) (core.Value, error)) OperatorHandler {
	return OperatorHandler{
		Check: func(args []core.Value) bool {
			for _, arg := range args {
				if _, ok := arg.(*core.ListValue); !ok {
					return false
				}
			}
			return true
		},
		Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
			lists := make([]*core.ListValue, len(args))
			for i, arg := range args {
				lists[i] = arg.(*core.ListValue)
			}
			return fn(lists)
		},
	}
}

// MixedNumberStringHandler handles number * string or string * number
func MixedNumberStringHandler(fn func(string, int) (core.Value, error)) OperatorHandler {
	return OperatorHandler{
		Check: func(args []core.Value) bool {
			if len(args) != 2 {
				return false
			}
			_, strFirst := args[0].(core.StringValue)
			_, numSecond := args[1].(core.NumberValue)
			_, numFirst := args[0].(core.NumberValue)
			_, strSecond := args[1].(core.StringValue)
			return (strFirst && numSecond) || (numFirst && strSecond)
		},
		Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
			var str string
			var num int

			if s, ok := args[0].(core.StringValue); ok {
				str = string(s)
				num = int(args[1].(core.NumberValue))
			} else {
				num = int(args[0].(core.NumberValue))
				str = string(args[1].(core.StringValue))
			}

			return fn(str, num)
		},
	}
}

// Common operators

// Add creates the + operator
func Add() BuiltinFunc {
	return NewOperator("+", "__add__").
		WithArgs(0, -1). // Allow 0 or more arguments
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
			// With no args, return identity element 0
			if len(nums) == 0 {
				return core.NumberValue(0), nil
			}
			result := nums[0]
			for i := 1; i < len(nums); i++ {
				result += nums[i]
			}
			return core.NumberValue(result), nil
		})).
		WithHandler(StringHandler(func(strs []string) (core.Value, error) {
			// With no args, return empty string
			if len(strs) == 0 {
				return core.StringValue(""), nil
			}
			result := strs[0]
			for i := 1; i < len(strs); i++ {
				result += strs[i]
			}
			return core.StringValue(result), nil
		})).
		WithHandler(ListHandler(func(lists []*core.ListValue) (core.Value, error) {
			result := make([]core.Value, 0)
			for _, list := range lists {
				result = append(result, list.Items()...)
			}
			return core.NewList(result...), nil
		})).
		Build()
}

// Multiply creates the * operator
func Multiply() BuiltinFunc {
	return NewOperator("*", "__mul__").
		WithArgs(0, -1). // Allow 0 or more arguments
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
			// With no args, return identity element 1
			if len(nums) == 0 {
				return core.NumberValue(1), nil
			}
			result := nums[0]
			for i := 1; i < len(nums); i++ {
				result *= nums[i]
			}
			return core.NumberValue(result), nil
		})).
		WithHandler(MixedNumberStringHandler(func(str string, num int) (core.Value, error) {
			if num < 0 {
				return core.StringValue(""), nil
			}
			result := ""
			for i := 0; i < num; i++ {
				result += str
			}
			return core.StringValue(result), nil
		})).
		WithHandler(OperatorHandler{
			Check: func(args []core.Value) bool {
				if len(args) != 2 {
					return false
				}
				// list * number or number * list
				_, listFirst := args[0].(*core.ListValue)
				_, numSecond := args[1].(core.NumberValue)
				_, numFirst := args[0].(core.NumberValue)
				_, listSecond := args[1].(*core.ListValue)
				return (listFirst && numSecond) || (numFirst && listSecond)
			},
			Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
				var list *core.ListValue
				var num int

				if l, ok := args[0].(*core.ListValue); ok {
					list = l
					num = int(args[1].(core.NumberValue))
				} else {
					num = int(args[0].(core.NumberValue))
					list = args[1].(*core.ListValue)
				}

				if num < 0 {
					return core.NewList(), nil
				}

				result := make([]core.Value, 0, list.Len()*num)
				for i := 0; i < num; i++ {
					result = append(result, list.Items()...)
				}
				return core.NewList(result...), nil
			},
		}).
		Build()
}

// Subtract creates the - operator (supports unary and variadic)
func Subtract() BuiltinFunc {
	return NewOperator("-", "__sub__").
		WithArgs(1, -1). // Allow 1 or more arguments
		WithHandler(OperatorHandler{
			Check: func(args []core.Value) bool {
				// Check if all args are numbers
				for _, arg := range args {
					if _, ok := arg.(core.NumberValue); !ok {
						return false
					}
				}
				return true
			},
			Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
				if len(args) == 1 {
					// Unary minus
					num := float64(args[0].(core.NumberValue))
					return core.NumberValue(-num), nil
				} else {
					// Variadic subtraction: a - b - c - ...
					result := float64(args[0].(core.NumberValue))
					for i := 1; i < len(args); i++ {
						result -= float64(args[i].(core.NumberValue))
					}
					return core.NumberValue(result), nil
				}
			},
		}).
		Build()
}

// Divide creates the / operator
func Divide() BuiltinFunc {
	return NewOperator("/", "__truediv__").
		WithArgs(1, -1). // Allow 1 or more arguments
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
			if len(nums) == 1 {
				// Single argument: 1/x
				if nums[0] == 0 {
					return nil, &core.ZeroDivisionError{}
				}
				return core.NumberValue(1 / nums[0]), nil
			}
			// Multiple arguments: a / b / c / ...
			result := nums[0]
			for i := 1; i < len(nums); i++ {
				if nums[i] == 0 {
					return nil, &core.ZeroDivisionError{}
				}
				result /= nums[i]
			}
			return core.NumberValue(result), nil
		})).
		Build()
}

// Modulo creates the % operator
func Modulo() BuiltinFunc {
	return NewOperator("%", "__mod__").
		WithArgs(2, 2).
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
			if nums[1] == 0 {
				return nil, &core.ZeroDivisionError{}
			}
			// Go's % operator doesn't match Python's for negative numbers
			// Python: -7 % 3 = 2, Go: -7 % 3 = -1
			// We need to adjust for Python compatibility
			result := math.Mod(nums[0], nums[1])
			if (result < 0 && nums[1] > 0) || (result > 0 && nums[1] < 0) {
				result += nums[1]
			}
			return core.NumberValue(result), nil
		})).
		Build()
}

// Power creates the ** operator
func Power() BuiltinFunc {
	return NewOperator("**", "__pow__").
		WithArgs(2, 2).
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
			return core.NumberValue(math.Pow(nums[0], nums[1])), nil
		})).
		Build()
}

// UnaryNegate creates the unary - operator
func UnaryNegate() BuiltinFunc {
	return NewOperator("unary-", "__neg__").
		WithArgs(1, 1).
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
			return core.NumberValue(-nums[0]), nil
		})).
		Build()
}

// Comparison operators

// ComparisonHandler creates a handler for comparison operations
func ComparisonHandler(numCompare func(float64, float64) bool, strCompare func(string, string) bool) OperatorHandler {
	return OperatorHandler{
		Check: func(args []core.Value) bool {
			if len(args) != 2 {
				return false
			}
			// Check if both are numbers or both are strings
			_, num1 := args[0].(core.NumberValue)
			_, num2 := args[1].(core.NumberValue)
			_, str1 := args[0].(core.StringValue)
			_, str2 := args[1].(core.StringValue)
			return (num1 && num2) || (str1 && str2)
		},
		Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if n1, ok := args[0].(core.NumberValue); ok {
				n2 := args[1].(core.NumberValue)
				return core.BoolValue(numCompare(float64(n1), float64(n2))), nil
			}
			s1 := args[0].(core.StringValue)
			s2 := args[1].(core.StringValue)
			return core.BoolValue(strCompare(string(s1), string(s2))), nil
		},
	}
}

// Equal creates the == operator
func Equal() BuiltinFunc {
	return NewOperator("==", "__eq__").
		WithArgs(2, 2).
		WithHandler(OperatorHandler{
			Check: func(args []core.Value) bool {
				return len(args) == 2
			},
			Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
				return core.BoolValue(core.EqualValues(args[0], args[1])), nil
			},
		}).
		Build()
}

// NotEqual creates the != operator
func NotEqual() BuiltinFunc {
	return NewOperator("!=", "__ne__").
		WithArgs(2, 2).
		WithHandler(OperatorHandler{
			Check: func(args []core.Value) bool {
				return len(args) == 2
			},
			Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
				return core.BoolValue(!core.EqualValues(args[0], args[1])), nil
			},
		}).
		Build()
}

// LessThan creates the < operator
func LessThan() BuiltinFunc {
	return NewOperator("<", "__lt__").
		WithArgs(2, 2).
		WithHandler(ComparisonHandler(
			func(a, b float64) bool { return a < b },
			func(a, b string) bool { return a < b },
		)).
		Build()
}

// LessThanOrEqual creates the <= operator
func LessThanOrEqual() BuiltinFunc {
	return NewOperator("<=", "__le__").
		WithArgs(2, 2).
		WithHandler(ComparisonHandler(
			func(a, b float64) bool { return a <= b },
			func(a, b string) bool { return a <= b },
		)).
		Build()
}

// GreaterThan creates the > operator
func GreaterThan() BuiltinFunc {
	return NewOperator(">", "__gt__").
		WithArgs(2, 2).
		WithHandler(ComparisonHandler(
			func(a, b float64) bool { return a > b },
			func(a, b string) bool { return a > b },
		)).
		Build()
}

// GreaterThanOrEqual creates the >= operator
func GreaterThanOrEqual() BuiltinFunc {
	return NewOperator(">=", "__ge__").
		WithArgs(2, 2).
		WithHandler(ComparisonHandler(
			func(a, b float64) bool { return a >= b },
			func(a, b string) bool { return a >= b },
		)).
		Build()
}

// Logical operators

// Not creates the not operator
func Not() BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewArgumentError("not", 1, len(args))
		}
		// Use IsTruthy which already checks __bool__
		return core.BoolValue(!core.IsTruthy(args[0])), nil
	}
}

// And creates the and operator
func And() BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.True, nil
		}

		var result core.Value = core.True
		for _, arg := range args {
			if !core.IsTruthy(arg) {
				return arg, nil // Return the first falsy value
			}
			result = arg
		}

		// All values were truthy, return the last one
		return result, nil
	}
}

// Or creates the or operator
func Or() BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.False, nil
		}

		for _, arg := range args {
			if core.IsTruthy(arg) {
				return arg, nil // Return the first truthy value
			}
		}

		// All values were falsy, return the last one
		return args[len(args)-1], nil
	}
}

// In creates the in operator
func In() BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, errors.NewArgumentError("in", 2, len(args))
		}

		value := args[0]
		container := args[1]

		// Check for __contains__ method
		if obj, ok := container.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if method, found := obj.GetAttr("__contains__"); found {
				if callable, ok := method.(core.Callable); ok {
					result, err := callable.Call([]core.Value{value}, ctx)
					if err != nil {
						return nil, err
					}
					// Ensure we return a boolean
					if b, ok := result.(core.BoolValue); ok {
						return b, nil
					}
					// The test expects an error for non-boolean returns
					return nil, errors.NewTypeError("in", "__contains__ should return a boolean", string(result.Type()))
				}
			}
		}

		// Built-in container types
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

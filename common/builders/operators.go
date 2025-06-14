package builders

import (
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
		return nil, errors.NewRuntimeError(ob.name, "no arguments provided")
	}
}

// Common operator handlers

// NumberHandler creates a handler for numeric operations
func NumberHandler(fn func([]float64) (core.Value, error)) OperatorHandler {
	return OperatorHandler{
		Check: func(args []core.Value) bool {
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
func ListHandler(fn func([]core.ListValue) (core.Value, error)) OperatorHandler {
	return OperatorHandler{
		Check: func(args []core.Value) bool {
			for _, arg := range args {
				if _, ok := arg.(core.ListValue); !ok {
					return false
				}
			}
			return true
		},
		Handle: func(args []core.Value, ctx *core.Context) (core.Value, error) {
			lists := make([]core.ListValue, len(args))
			for i, arg := range args {
				lists[i] = arg.(core.ListValue)
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
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
			result := nums[0]
			for i := 1; i < len(nums); i++ {
				result += nums[i]
			}
			return core.NumberValue(result), nil
		})).
		WithHandler(StringHandler(func(strs []string) (core.Value, error) {
			result := strs[0]
			for i := 1; i < len(strs); i++ {
				result += strs[i]
			}
			return core.StringValue(result), nil
		})).
		WithHandler(ListHandler(func(lists []core.ListValue) (core.Value, error) {
			result := make(core.ListValue, 0)
			for _, list := range lists {
				result = append(result, list...)
			}
			return result, nil
		})).
		Build()
}

// Multiply creates the * operator
func Multiply() BuiltinFunc {
	return NewOperator("*", "__mul__").
		WithHandler(NumberHandler(func(nums []float64) (core.Value, error) {
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
		Build()
}

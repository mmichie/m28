// Package builtin implements built-in functions for the language.
package builtin

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// RegisterComparisonFunctions registers comparison functions in the global context
func RegisterComparisonFunctions(ctx *core.Context) {
	// Comparison operators
	ctx.Define("==", core.NewBuiltinFunction(EqualFunc))
	ctx.Define("!=", core.NewBuiltinFunction(NotEqualFunc))
	ctx.Define("<", core.NewBuiltinFunction(LessThanFunc))
	ctx.Define("<=", core.NewBuiltinFunction(LessEqualFunc))
	ctx.Define(">", core.NewBuiltinFunction(GreaterThanFunc))
	ctx.Define(">=", core.NewBuiltinFunction(GreaterEqualFunc))

	// Logical operators
	ctx.Define("not", core.NewBuiltinFunction(NotFunc))
	ctx.Define("and", core.NewBuiltinFunction(AndFunc))
	ctx.Define("or", core.NewBuiltinFunction(OrFunc))

	// Membership operator
	ctx.Define("in", core.NewBuiltinFunction(InFunc))
}

// EqualFunc implements the == operator
func EqualFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("== requires exactly 2 arguments")
	}

	// Use core.EqualValues for comprehensive comparison
	return core.BoolValue(core.EqualValues(args[0], args[1])), nil
}

// NotEqualFunc implements the != operator
func NotEqualFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("!= requires exactly 2 arguments")
	}

	result, err := EqualFunc(args, ctx)
	if err != nil {
		return nil, err
	}

	if result == core.True {
		return core.False, nil
	}
	return core.True, nil
}

// LessThanFunc implements the < operator
func LessThanFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("< requires exactly 2 arguments")
	}

	// Compare based on type
	switch a := args[0].(type) {
	case core.NumberValue:
		if b, ok := args[1].(core.NumberValue); ok {
			return core.BoolValue(a < b), nil
		}
		return nil, fmt.Errorf("cannot compare number with %s", args[1].Type())

	case core.StringValue:
		if b, ok := args[1].(core.StringValue); ok {
			return core.BoolValue(a < b), nil
		}
		return nil, fmt.Errorf("cannot compare string with %s", args[1].Type())

	default:
		return nil, fmt.Errorf("< not supported for %s", a.Type())
	}
}

// LessEqualFunc implements the <= operator
func LessEqualFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("<= requires exactly 2 arguments")
	}

	// Compare based on type
	switch a := args[0].(type) {
	case core.NumberValue:
		if b, ok := args[1].(core.NumberValue); ok {
			return core.BoolValue(a <= b), nil
		}
		return nil, fmt.Errorf("cannot compare number with %s", args[1].Type())

	case core.StringValue:
		if b, ok := args[1].(core.StringValue); ok {
			return core.BoolValue(a <= b), nil
		}
		return nil, fmt.Errorf("cannot compare string with %s", args[1].Type())

	default:
		return nil, fmt.Errorf("<= not supported for %s", a.Type())
	}
}

// GreaterThanFunc implements the > operator
func GreaterThanFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("> requires exactly 2 arguments")
	}

	// Compare based on type
	switch a := args[0].(type) {
	case core.NumberValue:
		if b, ok := args[1].(core.NumberValue); ok {
			return core.BoolValue(a > b), nil
		}
		return nil, fmt.Errorf("cannot compare number with %s", args[1].Type())

	case core.StringValue:
		if b, ok := args[1].(core.StringValue); ok {
			return core.BoolValue(a > b), nil
		}
		return nil, fmt.Errorf("cannot compare string with %s", args[1].Type())

	default:
		return nil, fmt.Errorf("> not supported for %s", a.Type())
	}
}

// GreaterEqualFunc implements the >= operator
func GreaterEqualFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf(">= requires exactly 2 arguments")
	}

	// Compare based on type
	switch a := args[0].(type) {
	case core.NumberValue:
		if b, ok := args[1].(core.NumberValue); ok {
			return core.BoolValue(a >= b), nil
		}
		return nil, fmt.Errorf("cannot compare number with %s", args[1].Type())

	case core.StringValue:
		if b, ok := args[1].(core.StringValue); ok {
			return core.BoolValue(a >= b), nil
		}
		return nil, fmt.Errorf("cannot compare string with %s", args[1].Type())

	default:
		return nil, fmt.Errorf(">= not supported for %s", a.Type())
	}
}

// NotFunc implements the not operator
func NotFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not requires exactly 1 argument")
	}

	// Convert to boolean
	var isTruthy bool
	switch v := args[0].(type) {
	case core.BoolValue:
		isTruthy = bool(v)
	case core.NilValue:
		isTruthy = false
	case core.NumberValue:
		isTruthy = float64(v) != 0
	case core.StringValue:
		isTruthy = string(v) != ""
	case core.ListValue:
		isTruthy = len(v) > 0
	case core.TupleValue:
		isTruthy = len(v) > 0
	default:
		isTruthy = true
	}

	return core.BoolValue(!isTruthy), nil
}

// AndFunc implements the and operator
func AndFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.True, nil
	}

	var result core.Value = core.True
	for _, arg := range args {
		// Convert to boolean
		var isTruthy bool
		switch v := arg.(type) {
		case core.BoolValue:
			isTruthy = bool(v)
		case core.NilValue:
			isTruthy = false
		case core.NumberValue:
			isTruthy = float64(v) != 0
		case core.StringValue:
			isTruthy = string(v) != ""
		case core.ListValue:
			isTruthy = len(v) > 0
		case core.TupleValue:
			isTruthy = len(v) > 0
		default:
			isTruthy = true
		}

		if !isTruthy {
			return arg, nil // Return the first falsy value
		}
		result = arg
	}

	// All values were truthy, return the last one
	return result, nil
}

// OrFunc implements the or operator
func OrFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.False, nil
	}

	for _, arg := range args {
		// Convert to boolean
		var isTruthy bool
		switch v := arg.(type) {
		case core.BoolValue:
			isTruthy = bool(v)
		case core.NilValue:
			isTruthy = false
		case core.NumberValue:
			isTruthy = float64(v) != 0
		case core.StringValue:
			isTruthy = string(v) != ""
		case core.ListValue:
			isTruthy = len(v) > 0
		case core.TupleValue:
			isTruthy = len(v) > 0
		default:
			isTruthy = true
		}

		if isTruthy {
			return arg, nil // Return the first truthy value
		}
	}

	// All values were falsy, return the last one
	return args[len(args)-1], nil
}

// InFunc implements the 'in' operator for membership testing
func InFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("in requires exactly 2 arguments: value and container")
	}

	value := args[0]
	container := args[1]

	switch c := container.(type) {
	case core.ListValue:
		// Check if value is in list
		for _, item := range c {
			if equal, _ := EqualFunc([]core.Value{value, item}, ctx); equal == core.True {
				return core.True, nil
			}
		}
		return core.False, nil

	case core.TupleValue:
		// Check if value is in tuple
		for _, item := range c {
			if equal, _ := EqualFunc([]core.Value{value, item}, ctx); equal == core.True {
				return core.True, nil
			}
		}
		return core.False, nil

	case core.StringValue:
		// Check if substring is in string
		if str, ok := value.(core.StringValue); ok {
			return core.BoolValue(strings.Contains(string(c), string(str))), nil
		}
		return core.False, nil

	case *core.SetValue:
		// Check if value is in set
		return core.BoolValue(c.Contains(value)), nil

	case *core.DictValue:
		// Check if key is in dictionary
		if key, ok := value.(core.StringValue); ok {
			_, exists := c.Get(string(key))
			return core.BoolValue(exists), nil
		}
		return core.False, nil

	case core.Object:
		// For other objects, check attributes
		if key, ok := value.(core.StringValue); ok {
			_, found := c.GetAttr(string(key))
			return core.BoolValue(found), nil
		}
		return core.False, nil

	default:
		return nil, fmt.Errorf("'in' operator not supported for %s", container.Type())
	}
}

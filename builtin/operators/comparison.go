package operators

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterComparisonOperators registers comparison operators in the context
func RegisterComparisonOperators(ctx *core.Context) {
	ctx.Define("==", core.NewBuiltinFunction(EqualFunc))
	ctx.Define("!=", core.NewBuiltinFunction(NotEqualFunc))
	ctx.Define("<", core.NewBuiltinFunction(LessThanFunc))
	ctx.Define("<=", core.NewBuiltinFunction(LessEqualFunc))
	ctx.Define(">", core.NewBuiltinFunction(GreaterThanFunc))
	ctx.Define(">=", core.NewBuiltinFunction(GreaterEqualFunc))
}

// EqualFunc implements the == operator
func EqualFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("== requires exactly 2 arguments")
	}

	// Check if the first argument has __eq__ method (operator overloading)
	if obj, ok := args[0].(interface {
		GetAttr(string) (core.Value, bool)
	}); ok {
		if method, found := obj.GetAttr("__eq__"); found {
			if callable, ok := method.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				// Call __eq__ with the second argument
				return callable.Call([]core.Value{args[1]}, ctx)
			}
		}
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

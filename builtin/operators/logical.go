package operators

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// RegisterLogicalOperators registers logical operators in the context
func RegisterLogicalOperators(ctx *core.Context) {
	ctx.Define("and", core.NewBuiltinFunction(AndFunc))
	ctx.Define("or", core.NewBuiltinFunction(OrFunc))
	ctx.Define("not", core.NewBuiltinFunction(NotFunc))
	ctx.Define("in", core.NewBuiltinFunction(InFunc))
}

// NotFunc implements the not operator
func NotFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not requires exactly 1 argument")
	}

	// Use the enhanced IsTruthy which checks for __bool__
	isTruthy := core.IsTruthy(args[0])
	return core.BoolValue(!isTruthy), nil
}

// AndFunc implements the and operator
func AndFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.True, nil
	}

	var result core.Value = core.True
	for _, arg := range args {
		// Use the enhanced IsTruthy which checks for __bool__
		isTruthy := core.IsTruthy(arg)

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
		// Use the enhanced IsTruthy which checks for __bool__
		isTruthy := core.IsTruthy(arg)

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

	// Try to get __contains__ method first
	if obj, ok := container.(interface {
		GetAttr(string) (core.Value, bool)
	}); ok {
		if containsMethod, found := obj.GetAttr("__contains__"); found {
			// Call the __contains__ method
			if callable, ok := containsMethod.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				result, err := callable.Call([]core.Value{value}, ctx)
				if err != nil {
					return nil, err
				}
				// Ensure it returns a boolean
				if boolVal, ok := result.(core.BoolValue); ok {
					return boolVal, nil
				}
				return nil, fmt.Errorf("__contains__ should return a boolean")
			}
		}
	}

	// Fallback to type-specific behavior
	switch c := container.(type) {
	case core.ListValue:
		// Check if value is in list
		for _, item := range c {
			if core.EqualValues(value, item) {
				return core.True, nil
			}
		}
		return core.False, nil

	case core.TupleValue:
		// Check if value is in tuple
		for _, item := range c {
			if core.EqualValues(value, item) {
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
		// First check if the value is hashable
		if !core.IsHashable(value) {
			return core.False, nil
		}
		// Convert value to key format used by dictionary
		keyStr := core.ValueToKey(value)
		_, exists := c.Get(keyStr)
		return core.BoolValue(exists), nil

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

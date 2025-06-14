package builtin

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterNumeric registers numeric functions
func RegisterNumeric(ctx *core.Context) {
	// abs - absolute value
	ctx.Define("abs", core.NewNamedBuiltinFunction("abs", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("abs", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Abs(num)), nil
	}))

	// round - round to n digits
	ctx.Define("round", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("round", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		ndigits, _ := v.GetNumberOrDefault(1, 0)
		ndigitsInt := int(ndigits)

		if ndigitsInt == 0 {
			return core.NumberValue(math.Round(num)), nil
		}

		// Round to n decimal places
		multiplier := math.Pow(10, float64(ndigitsInt))
		return core.NumberValue(math.Round(num*multiplier) / multiplier), nil
	}))

	// divmod - return quotient and remainder
	ctx.Define("divmod", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("divmod", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		a, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		b, err := v.GetNumber(1)
		if err != nil {
			return nil, err
		}

		if b == 0 {
			return nil, &core.ZeroDivisionError{}
		}

		// Python-style division
		quotient := math.Floor(a / b)
		remainder := a - quotient*b

		return core.TupleValue{
			core.NumberValue(quotient),
			core.NumberValue(remainder),
		}, nil
	}))

	// pow - power function (also available as ** operator)
	ctx.Define("pow", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("pow", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		base, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		exp, err := v.GetNumber(1)
		if err != nil {
			return nil, err
		}

		if v.Count() == 3 {
			// Three-argument form: pow(base, exp, mod)
			mod, err := v.GetNumber(2)
			if err != nil {
				return nil, err
			}
			if mod == 0 {
				return nil, fmt.Errorf("pow() 3rd argument cannot be 0")
			}
			// For now, just compute normally and mod
			// In the future, should use modular exponentiation
			result := math.Pow(base, exp)
			return core.NumberValue(math.Mod(result, mod)), nil
		}

		return core.NumberValue(math.Pow(base, exp)), nil
	}))

	// sum - sum of values
	ctx.Define("sum", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("sum", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		// Get the iterable first argument
		iterable, err := v.GetIterable(0)
		if err != nil {
			return nil, err
		}

		// Get optional start value (default 0)
		start, _ := v.GetNumberOrDefault(1, 0)

		// Extract items from the iterable
		var items []core.Value
		switch v := iterable.(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		default:
			return nil, fmt.Errorf("sum() argument must be an iterable, not '%s'", v.Type())
		}

		sum := start
		for _, item := range items {
			num, err := types.RequireNumber(item, "sum() element")
			if err != nil {
				return nil, fmt.Errorf("unsupported operand type(s) for +: 'float' and '%s'", item.Type())
			}
			sum += num
		}

		return core.NumberValue(sum), nil
	}))

	// min - minimum value (with kwargs support)
	ctx.Define("min", NewKwargsBuiltinFunction("min", minWithKwargs))

	// max - maximum value (with kwargs support)
	ctx.Define("max", NewKwargsBuiltinFunction("max", maxWithKwargs))
}

// minWithKwargs implements min() with keyword argument support
func minWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Check for key parameter
	var keyFunc core.Value
	if k, ok := kwargs["key"]; ok {
		keyFunc = k
		delete(kwargs, "key")
	}

	// Check for default parameter
	var defaultValue core.Value
	var hasDefault bool
	if d, ok := kwargs["default"]; ok {
		defaultValue = d
		hasDefault = true
		delete(kwargs, "default")
	}

	// Check for any remaining kwargs
	if len(kwargs) > 0 {
		for k := range kwargs {
			return nil, fmt.Errorf("min() got an unexpected keyword argument '%s'", k)
		}
	}

	if len(args) == 0 {
		return nil, fmt.Errorf("min expected at least 1 argument, got 0")
	}

	// If single iterable argument
	if len(args) == 1 {
		var items []core.Value
		switch v := args[0].(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		default:
			// Not an iterable, treat as single value
			items = args
		}

		if len(items) == 0 {
			if hasDefault {
				return defaultValue, nil
			}
			return nil, fmt.Errorf("min() arg is an empty sequence")
		}

		return findExtreme(items, keyFunc, ctx, true)
	}

	// Multiple arguments
	return findExtreme(args, keyFunc, ctx, true)
}

// maxWithKwargs implements max() with keyword argument support
func maxWithKwargs(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Check for key parameter
	var keyFunc core.Value
	if k, ok := kwargs["key"]; ok {
		keyFunc = k
		delete(kwargs, "key")
	}

	// Check for default parameter
	var defaultValue core.Value
	var hasDefault bool
	if d, ok := kwargs["default"]; ok {
		defaultValue = d
		hasDefault = true
		delete(kwargs, "default")
	}

	// Check for any remaining kwargs
	if len(kwargs) > 0 {
		for k := range kwargs {
			return nil, fmt.Errorf("max() got an unexpected keyword argument '%s'", k)
		}
	}

	if len(args) == 0 {
		return nil, fmt.Errorf("max expected at least 1 argument, got 0")
	}

	// If single iterable argument
	if len(args) == 1 {
		var items []core.Value
		switch v := args[0].(type) {
		case core.ListValue:
			items = v
		case core.TupleValue:
			items = v
		default:
			// Not an iterable, treat as single value
			items = args
		}

		if len(items) == 0 {
			if hasDefault {
				return defaultValue, nil
			}
			return nil, fmt.Errorf("max() arg is an empty sequence")
		}

		return findExtreme(items, keyFunc, ctx, false)
	}

	// Multiple arguments
	return findExtreme(args, keyFunc, ctx, false)
}

// findExtreme finds the minimum or maximum value based on the isMin flag
func findExtreme(items []core.Value, keyFunc core.Value, ctx *core.Context, isMin bool) (core.Value, error) {
	if len(items) == 0 {
		return nil, fmt.Errorf("sequence is empty")
	}

	result := items[0]
	var resultKey core.Value

	// Get key for first item
	if keyFunc != nil {
		if callable, ok := keyFunc.(interface {
			Call([]core.Value, *core.Context) (core.Value, error)
		}); ok {
			key, err := callable.Call([]core.Value{result}, ctx)
			if err != nil {
				return nil, err
			}
			resultKey = key
		} else {
			return nil, fmt.Errorf("key function must be callable")
		}
	} else {
		resultKey = result
	}

	// Compare with rest
	for i := 1; i < len(items); i++ {
		item := items[i]
		var itemKey core.Value

		// Get key for current item
		if keyFunc != nil {
			if callable, ok := keyFunc.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				key, err := callable.Call([]core.Value{item}, ctx)
				if err != nil {
					return nil, err
				}
				itemKey = key
			}
		} else {
			itemKey = item
		}

		// Compare keys
		shouldReplace := false
		switch k1 := resultKey.(type) {
		case core.NumberValue:
			if k2, ok := itemKey.(core.NumberValue); ok {
				if isMin {
					shouldReplace = k2 < k1
				} else {
					shouldReplace = k2 > k1
				}
			} else {
				return nil, fmt.Errorf("'%s' not supported between instances of '%s' and '%s'",
					map[bool]string{true: "<", false: ">"}[isMin], itemKey.Type(), k1.Type())
			}
		case core.StringValue:
			if k2, ok := itemKey.(core.StringValue); ok {
				if isMin {
					shouldReplace = k2 < k1
				} else {
					shouldReplace = k2 > k1
				}
			} else {
				return nil, fmt.Errorf("'%s' not supported between instances of '%s' and '%s'",
					map[bool]string{true: "<", false: ">"}[isMin], itemKey.Type(), k1.Type())
			}
		default:
			return nil, fmt.Errorf("'%s' not supported for type '%s'",
				map[bool]string{true: "<", false: ">"}[isMin], k1.Type())
		}

		if shouldReplace {
			result = item
			resultKey = itemKey
		}
	}

	return result, nil
}

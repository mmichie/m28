package builtin

import (
	"fmt"
	"math"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/protocols"
)

// bankersRound implements banker's rounding (round half to even)
// This matches Python 3's default rounding behavior
func bankersRound(x float64) float64 {
	// Handle special cases
	if math.IsNaN(x) || math.IsInf(x, 0) {
		return x
	}

	// Get the integer and fractional parts
	intPart, fracPart := math.Modf(x)

	// If not exactly .5, use standard rounding
	absFrac := math.Abs(fracPart)
	if absFrac != 0.5 {
		return math.Round(x)
	}

	// Exactly .5: round to nearest even integer
	if int(intPart)%2 == 0 {
		// Already even, round down (toward zero)
		return intPart
	}
	// Odd, round up (away from zero) to make it even
	if x > 0 {
		return intPart + 1
	}
	return intPart - 1
}

// RegisterNumeric registers numeric functions
func RegisterNumeric(ctx *core.Context) {
	// abs - absolute value
	ctx.Define("abs", core.NewNamedBuiltinFunction("abs", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("abs", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// Try __abs__ dunder method first
		if result, found, err := types.CallAbs(val, ctx); found {
			if err != nil {
				return nil, err
			}
			return result, nil
		}

		// Try protocol-based numeric operations
		if numOps, ok := protocols.GetNumericOps(val); ok {
			return numOps.Absolute()
		}

		// Handle bool values explicitly (Python: abs(True) = 1, abs(False) = 0)
		if boolVal, ok := val.(core.BoolValue); ok {
			if bool(boolVal) {
				return core.NumberValue(1), nil
			}
			return core.NumberValue(0), nil
		}

		// Fall back to direct number conversion
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

		val := v.Get(0)

		// Get ndigits if provided
		var ndigits core.Value
		if v.Count() == 2 {
			ndigits = v.Get(1)
		}

		// Try __round__ dunder method first
		if result, found, err := types.CallRound(val, ndigits, ctx); found {
			if err != nil {
				return nil, err
			}
			return result, nil
		}

		// Fall back to built-in rounding
		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		ndigitsVal, _ := v.GetNumberOrDefault(1, 0)
		ndigitsInt := int(ndigitsVal)

		if ndigitsInt == 0 {
			return core.NumberValue(bankersRound(num)), nil
		}

		// Round to n decimal places
		multiplier := math.Pow(10, float64(ndigitsInt))
		return core.NumberValue(bankersRound(num*multiplier) / multiplier), nil
	}))

	// divmod - return quotient and remainder
	ctx.Define("divmod", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("divmod", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		left := v.Get(0)
		right := v.Get(1)

		// Try __divmod__ dunder method on left operand
		if obj, ok := left.(core.Object); ok {
			if method, exists := obj.GetAttr("__divmod__"); exists {
				if callable, ok := method.(core.Callable); ok {
					result, err := callable.Call([]core.Value{right}, ctx)
					if err != nil {
						return nil, err
					}
					return result, nil
				}
			}
		}

		// Try __rdivmod__ dunder method on right operand
		if obj, ok := right.(core.Object); ok {
			if method, exists := obj.GetAttr("__rdivmod__"); exists {
				if callable, ok := method.(core.Callable); ok {
					result, err := callable.Call([]core.Value{left}, ctx)
					if err != nil {
						return nil, err
					}
					return result, nil
				}
			}
		}

		// Fall back to built-in divmod
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
				return nil, &core.ValueError{Message: "pow() 3rd argument cannot be 0"}
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

		// Use the Iterator protocol to support all iterables (including generators)
		sum := start

		if iter, ok := iterable.(core.Iterable); ok {
			iterator := iter.Iterator()
			for {
				item, hasNext := iterator.Next()
				if !hasNext {
					break
				}
				num, err := types.RequireNumber(item, "sum() element")
				if err != nil {
					return nil, &core.TypeError{Message: fmt.Sprintf("unsupported operand type(s) for +: 'float' and '%s'", item.Type())}
				}
				sum += num
			}
		} else {
			// Fallback for types that don't implement Iterable interface
			var items []core.Value
			switch v := iterable.(type) {
			case *core.ListValue:
				items = v.Items()
			case core.TupleValue:
				items = v
			default:
				return nil, &core.TypeError{Message: fmt.Sprintf("sum() argument must be an iterable, not '%s'", v.Type())}
			}

			for _, item := range items {
				num, err := types.RequireNumber(item, "sum() element")
				if err != nil {
					return nil, &core.TypeError{Message: fmt.Sprintf("unsupported operand type(s) for +: 'float' and '%s'", item.Type())}
				}
				sum += num
			}
		}

		return core.NumberValue(sum), nil
	}))

	// min - minimum value (with kwargs support)
	ctx.Define("min", NewKwargsBuiltinFunction("min", minWithKwargs))

	// max - maximum value (with kwargs support)
	ctx.Define("max", NewKwargsBuiltinFunction("max", maxWithKwargs))

	// complex - create a complex number
	ctx.Define("complex", core.NewNamedBuiltinFunction("complex", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Use the type descriptor's constructor for full functionality
		desc := core.GetTypeDescriptor(core.ComplexType)
		if desc == nil {
			return nil, fmt.Errorf("complex type not registered")
		}
		return desc.Constructor(args, ctx)
	}))

	// sqrt - square root
	ctx.Define("sqrt", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("sqrt", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		if num < 0 {
			return nil, &core.ValueError{Message: "math domain error"}
		}

		return core.NumberValue(math.Sqrt(num)), nil
	}))

	// floor - round down
	ctx.Define("floor", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("floor", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Floor(num)), nil
	}))

	// ceil - round up
	ctx.Define("ceil", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("ceil", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(math.Ceil(num)), nil
	}))
}

// extremeWithKwargs is a helper for min/max with keyword argument support
func extremeWithKwargs(funcName string, isMin bool) func([]core.Value, map[string]core.Value, *core.Context) (core.Value, error) {
	return func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
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
				return nil, &core.TypeError{Message: fmt.Sprintf("%s() got an unexpected keyword argument '%s'", funcName, k)}
			}
		}

		if len(args) == 0 {
			return nil, &core.TypeError{Message: fmt.Sprintf("%s expected at least 1 argument, got 0", funcName)}
		}

		// If single iterable argument
		if len(args) == 1 {
			var items []core.Value
			switch v := args[0].(type) {
			case *core.ListValue:
				items = v.Items()
			case core.TupleValue:
				items = v
			case *core.SetValue:
				// Convert set to slice by iterating
				iter := v.Iterator()
				items = make([]core.Value, 0)
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					items = append(items, val)
				}
			case *core.FrozenSetValue:
				// Convert frozenset to slice by iterating
				iter := v.Iterator()
				items = make([]core.Value, 0)
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					items = append(items, val)
				}
			default:
				// Try to use as iterable if it implements the interface
				if iterable, ok := v.(core.Iterable); ok {
					iter := iterable.Iterator()
					items = make([]core.Value, 0)
					for {
						val, hasNext := iter.Next()
						if !hasNext {
							break
						}
						items = append(items, val)
					}
				} else {
					// Not an iterable, treat as single value
					items = args
				}
			}

			if len(items) == 0 {
				if hasDefault {
					return defaultValue, nil
				}
				return nil, &core.ValueError{Message: fmt.Sprintf("%s() arg is an empty sequence", funcName)}
			}

			return findExtreme(items, keyFunc, ctx, isMin)
		}

		// Multiple arguments
		return findExtreme(args, keyFunc, ctx, isMin)
	}
}

// minWithKwargs implements min() with keyword argument support
var minWithKwargs = extremeWithKwargs("min", true)

// maxWithKwargs implements max() with keyword argument support
var maxWithKwargs = extremeWithKwargs("max", false)

// findExtreme finds the minimum or maximum value based on the isMin flag
func findExtreme(items []core.Value, keyFunc core.Value, ctx *core.Context, isMin bool) (core.Value, error) {
	if len(items) == 0 {
		return nil, &core.ValueError{Message: "sequence is empty"}
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
			return nil, &core.TypeError{Message: "key function must be callable"}
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
				return nil, &core.TypeError{Message: fmt.Sprintf("'%s' not supported between instances of '%s' and '%s'",
					map[bool]string{true: "<", false: ">"}[isMin], itemKey.Type(), k1.Type())}
			}
		case core.StringValue:
			if k2, ok := itemKey.(core.StringValue); ok {
				if isMin {
					shouldReplace = k2 < k1
				} else {
					shouldReplace = k2 > k1
				}
			} else {
				return nil, &core.TypeError{Message: fmt.Sprintf("'%s' not supported between instances of '%s' and '%s'",
					map[bool]string{true: "<", false: ">"}[isMin], itemKey.Type(), k1.Type())}
			}
		default:
			return nil, &core.TypeError{Message: fmt.Sprintf("'%s' not supported for type '%s'",
				map[bool]string{true: "<", false: ">"}[isMin], k1.Type())}
		}

		if shouldReplace {
			result = item
			resultKey = itemKey
		}
	}

	return result, nil
}

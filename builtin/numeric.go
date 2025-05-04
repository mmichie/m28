package builtin

import (
	"fmt"
	"math"
	"strconv"

	"github.com/mmichie/m28/core"
)

func RegisterNumericBuiltins() {
	core.RegisterBuiltin("abs", absFunc)
	core.RegisterBuiltin("divmod", divmodFunc)
	core.RegisterBuiltin("float", floatFunc)
	core.RegisterBuiltin("int", intFunc)
	core.RegisterBuiltin("max", maxFunc)
	core.RegisterBuiltin("min", minFunc)
	core.RegisterBuiltin("pow", powFunc)
	core.RegisterBuiltin("round", roundFunc)
	core.RegisterBuiltin("sum", sumFunc)
}

func absFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("abs() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case float64:
		return math.Abs(v), nil
	case int:
		return float64(math.Abs(float64(v))), nil
	default:
		return nil, fmt.Errorf("abs() argument must be a number")
	}
}

func divmodFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("divmod() takes exactly two arguments")
	}
	a, ok1 := args[0].(float64)
	b, ok2 := args[1].(float64)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("divmod() arguments must be numbers")
	}
	quotient := math.Floor(a / b)
	remainder := math.Mod(a, b)
	return core.LispList{quotient, remainder}, nil
}

func floatFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("float() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case float64:
		return v, nil
	case int:
		return float64(v), nil
	case string:
		f, err := strconv.ParseFloat(v, 64)
		if err != nil {
			return nil, fmt.Errorf("invalid literal for float(): %s", v)
		}
		return f, nil
	default:
		return nil, fmt.Errorf("float() argument must be a string or a number")
	}
}

func intFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Special case for type checking - if called with no args, return a type constructor
	if len(args) == 0 {
		return func(targs []core.LispValue, _ core.Environment) (core.LispValue, error) {
			return core.LispSymbol("int"), nil
		}, nil
	}

	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("int() takes 1 or 2 arguments")
	}

	base := 10
	if len(args) == 2 {
		b, ok := args[1].(float64)
		if !ok || b < 2 || b > 36 {
			return nil, fmt.Errorf("int() base must be >= 2 and <= 36")
		}
		base = int(b)
	}

	switch v := args[0].(type) {
	case float64:
		return float64(int(v)), nil
	case string:
		i, err := strconv.ParseInt(v, base, 64)
		if err != nil {
			return nil, fmt.Errorf("invalid literal for int() with base %d: %s", base, v)
		}
		return float64(i), nil
	default:
		return nil, fmt.Errorf("int() argument must be a string or a number")
	}
}

func maxFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("max() takes at least one argument")
	}
	max := args[0]
	for _, arg := range args[1:] {
		if core.Compare(arg, max) > 0 {
			max = arg
		}
	}
	return max, nil
}

func minFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("min() takes at least one argument")
	}
	min := args[0]
	for _, arg := range args[1:] {
		if core.Compare(arg, min) < 0 {
			min = arg
		}
	}
	return min, nil
}

func powFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("pow() takes exactly two arguments")
	}
	base, ok1 := args[0].(float64)
	exp, ok2 := args[1].(float64)
	if !ok1 || !ok2 {
		return nil, fmt.Errorf("pow() arguments must be numbers")
	}
	return math.Pow(base, exp), nil
}

func roundFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("round() takes 1 or 2 arguments")
	}
	number, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("round() argument must be a number")
	}
	decimals := 0
	if len(args) == 2 {
		d, ok := args[1].(float64)
		if !ok {
			return nil, fmt.Errorf("round() second argument must be an integer")
		}
		decimals = int(d)
	}
	factor := math.Pow10(decimals)
	return math.Round(number*factor) / factor, nil
}

func sumFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sum() takes exactly one argument")
	}
	iterable, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("sum() argument must be iterable")
	}
	var sum float64
	for _, item := range iterable {
		num, ok := item.(float64)
		if !ok {
			return nil, fmt.Errorf("sum() can only sum numbers")
		}
		sum += num
	}
	return sum, nil
}

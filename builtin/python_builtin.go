// File: builtin/python_builtins.go

package builtin

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

func RegisterPythonBuiltins() {
	core.RegisterBuiltin("range", rangeFunc)
	core.RegisterBuiltin("len", lenFunc)
	core.RegisterBuiltin("python_print", pythonPrintFunc) // Renamed to avoid conflict
	core.RegisterBuiltin("str", strFunc)
	core.RegisterBuiltin("int", intFunc)
	core.RegisterBuiltin("float", floatFunc)
	core.RegisterBuiltin("python_list", pythonListFunc) // Renamed to avoid conflict
	core.RegisterBuiltin("dict", dictFunc)
	core.RegisterBuiltin("sum", sumFunc)
	core.RegisterBuiltin("max", maxFunc)
	core.RegisterBuiltin("min", minFunc)
}

func rangeFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	var start, stop, step float64
	switch len(args) {
	case 1:
		stop = args[0].(float64)
		start, step = 0, 1
	case 2:
		start, stop = args[0].(float64), args[1].(float64)
		step = 1
	case 3:
		start, stop, step = args[0].(float64), args[1].(float64), args[2].(float64)
	default:
		return nil, fmt.Errorf("range() takes 1-3 arguments")
	}

	result := make(core.LispList, 0)
	for i := start; i < stop; i += step {
		result = append(result, i)
	}
	return result, nil
}

func lenFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("len() takes exactly one argument")
	}

	switch v := args[0].(type) {
	case core.LispList:
		return float64(len(v)), nil
	case string:
		return float64(len(v)), nil
	case *core.PythonicDict:
		return float64(v.Size()), nil // Assuming PythonicDict has a Size() method
	default:
		return nil, fmt.Errorf("object of type '%T' has no len()", v)
	}
}

func pythonPrintFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	strs := make([]string, len(args))
	for i, arg := range args {
		strs[i] = core.PrintValue(arg)
	}
	fmt.Println(strings.Join(strs, " "))
	return core.PythonicNone{}, nil
}

func strFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("str() takes exactly one argument")
	}
	return core.PrintValue(args[0]), nil
}

func intFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("int() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case float64:
		return float64(int(v)), nil
	case string:
		f, err := strconv.ParseFloat(v, 64)
		if err != nil {
			return nil, fmt.Errorf("invalid literal for int() with base 10: '%s'", v)
		}
		return float64(int(f)), nil
	default:
		return nil, fmt.Errorf("int() argument must be a string or a number, not '%T'", v)
	}
}

func floatFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("float() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case float64:
		return v, nil
	case string:
		f, err := strconv.ParseFloat(v, 64)
		if err != nil {
			return nil, fmt.Errorf("could not convert string to float: '%s'", v)
		}
		return f, nil
	default:
		return nil, fmt.Errorf("float() argument must be a string or a number, not '%T'", v)
	}
}

func pythonListFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return core.LispList(args), nil
}

func dictFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args)%2 != 0 {
		return nil, fmt.Errorf("dict() requires an even number of arguments")
	}
	dict := core.NewPythonicDict()
	for i := 0; i < len(args); i += 2 {
		dict.Set(args[i], args[i+1])
	}
	return dict, nil
}

func sumFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sum() takes exactly one argument")
	}
	iterable, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("sum() argument must be an iterable")
	}
	var total float64
	for _, v := range iterable {
		num, ok := v.(float64)
		if !ok {
			return nil, fmt.Errorf("sum() can't sum strings (use ''.join(seq) instead)")
		}
		total += num
	}
	return total, nil
}

func maxFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("max() expected 1 argument, got 0")
	}
	if len(args) == 1 {
		iterable, ok := args[0].(core.LispList)
		if !ok {
			return nil, fmt.Errorf("max() arg is an empty sequence")
		}
		return maxHelper(iterable)
	}
	return maxHelper(args)
}

func minFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("min() expected 1 argument, got 0")
	}
	if len(args) == 1 {
		iterable, ok := args[0].(core.LispList)
		if !ok {
			return nil, fmt.Errorf("min() arg is an empty sequence")
		}
		return minHelper(iterable)
	}
	return minHelper(args)
}

func maxHelper(items []core.LispValue) (core.LispValue, error) {
	if len(items) == 0 {
		return nil, fmt.Errorf("max() arg is an empty sequence")
	}
	max := items[0]
	for _, item := range items[1:] {
		if compareValues(item, max) > 0 {
			max = item
		}
	}
	return max, nil
}

func minHelper(items []core.LispValue) (core.LispValue, error) {
	if len(items) == 0 {
		return nil, fmt.Errorf("min() arg is an empty sequence")
	}
	min := items[0]
	for _, item := range items[1:] {
		if compareValues(item, min) < 0 {
			min = item
		}
	}
	return min, nil
}

func compareValues(a, b core.LispValue) int {
	switch v1 := a.(type) {
	case float64:
		v2, ok := b.(float64)
		if !ok {
			return 1 // Numbers are considered greater than non-numbers
		}
		if v1 < v2 {
			return -1
		} else if v1 > v2 {
			return 1
		}
		return 0
	case string:
		v2, ok := b.(string)
		if !ok {
			return -1 // Strings are considered less than non-strings
		}
		return strings.Compare(v1, v2)
	default:
		return 0 // For other types, consider them equal
	}
}

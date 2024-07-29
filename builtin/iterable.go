package builtin

import (
	"fmt"
	"math"
	"sort"

	"github.com/mmichie/m28/core"
)

func RegisterIterableBuiltins() {
	core.RegisterBuiltin("all", allFunc)
	core.RegisterBuiltin("any", anyFunc)
	core.RegisterBuiltin("enumerate", enumerateFunc)
	core.RegisterBuiltin("filter", filterFunc)
	core.RegisterBuiltin("iter", iterFunc)
	core.RegisterBuiltin("len", lenFunc)
	core.RegisterBuiltin("map", mapFunc)
	core.RegisterBuiltin("next", nextFunc)
	core.RegisterBuiltin("range", rangeFunc)
	core.RegisterBuiltin("reversed", reversedFunc)
	core.RegisterBuiltin("sorted", sortedFunc)
	core.RegisterBuiltin("zip", zipFunc)
	core.RegisterBuiltin("list", listFunc)
}

func allFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("all() takes exactly one argument")
	}
	iterable, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("all() argument must be iterable")
	}
	for _, item := range iterable {
		if !core.IsTruthy(item) {
			return core.PythonicBool(false), nil
		}
	}
	return core.PythonicBool(true), nil
}

func anyFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("any() takes exactly one argument")
	}
	iterable, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("any() argument must be iterable")
	}
	for _, item := range iterable {
		if core.IsTruthy(item) {
			return core.PythonicBool(true), nil
		}
	}
	return core.PythonicBool(false), nil
}

func enumerateFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("enumerate() takes exactly one argument")
	}
	iterable, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("enumerate() argument must be iterable")
	}
	result := make(core.LispList, len(iterable))
	for i, item := range iterable {
		result[i] = core.LispList{float64(i), item}
	}
	return result, nil
}

func iterFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("iter() takes exactly one argument")
	}
	// In this simple implementation, we'll just return the argument if it's iterable
	if _, ok := args[0].(core.LispList); ok {
		return args[0], nil
	}
	return nil, fmt.Errorf("object is not iterable")
}

func nextFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("next() takes exactly one argument")
	}
	iterator, ok := args[0].(core.LispList)
	if !ok || len(iterator) == 0 {
		return nil, fmt.Errorf("StopIteration")
	}
	return iterator[0], nil
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
		return nil, fmt.Errorf("range() takes 1 to 3 arguments")
	}
	var result core.LispList
	for i := start; i < stop; i += step {
		result = append(result, i)
	}
	return result, nil
}

func reversedFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("reversed() takes exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("reversed() argument must be a sequence")
	}
	result := make(core.LispList, len(list))
	for i, j := 0, len(list)-1; i < len(list); i, j = i+1, j-1 {
		result[i] = list[j]
	}
	return result, nil
}

func sortedFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("sorted() takes exactly one argument")
	}
	list, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("sorted() argument must be iterable")
	}
	result := make(core.LispList, len(list))
	copy(result, list)
	sort.Slice(result, func(i, j int) bool {
		return core.Compare(result[i], result[j]) < 0
	})
	return result, nil
}

func zipFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("zip() takes at least one argument")
	}
	iterables := make([]core.LispList, len(args))
	minLen := math.MaxInt32
	for i, arg := range args {
		list, ok := arg.(core.LispList)
		if !ok {
			return nil, fmt.Errorf("zip() arguments must be iterables")
		}
		iterables[i] = list
		if len(list) < minLen {
			minLen = len(list)
		}
	}
	result := make(core.LispList, minLen)
	for i := 0; i < minLen; i++ {
		tuple := make(core.LispList, len(args))
		for j, iterable := range iterables {
			tuple[j] = iterable[i]
		}
		result[i] = tuple
	}
	return result, nil
}

func listFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	return core.LispList(args), nil
}

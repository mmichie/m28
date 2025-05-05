package builtin

import (
	"fmt"
	"math"
	"reflect"
	"sync"

	"github.com/mmichie/m28/core"
)

var (
	evaluator core.Evaluator
	evalOnce  sync.Once
	evalMutex sync.RWMutex
)

func SetEvaluator(e core.Evaluator) {
	evalOnce.Do(func() {
		evalMutex.Lock()
		defer evalMutex.Unlock()
		evaluator = e
	})
}

func getEvaluator() (core.Evaluator, error) {
	evalMutex.RLock()
	defer evalMutex.RUnlock()
	if evaluator == nil {
		return nil, fmt.Errorf("evaluator not set")
	}
	return evaluator, nil
}

func init() {
	core.RegisterBuiltin("number?", isNumber)
	core.RegisterBuiltin("string?", isString)
	core.RegisterBuiltin("symbol?", isSymbol)
	core.RegisterBuiltin("list?", isList)
	core.RegisterBuiltin("not", not)
	core.RegisterBuiltin("print", print)
	core.RegisterBuiltin("assoc", assoc)
	core.RegisterBuiltin("pair?", isPair)
	core.RegisterBuiltin("integer?", isInteger)
	core.RegisterBuiltin("error", errorFunc)
	core.RegisterBuiltin("apply", applyFunc)
	core.RegisterBuiltin("filter", filterFunc)
	core.RegisterBuiltin("match", matchFunc)
	core.RegisterBuiltin("exists?", existsFunc)
	core.RegisterBuiltin("funcall", funcallFunc)
	core.RegisterBuiltin("numberp", numberp)
	core.RegisterBuiltin("symbolp", symbolpFunc)
	core.RegisterBuiltin("symbol->string", symbolToStringFunc)
	core.RegisterBuiltin("atom", atomFunc)
	core.RegisterBuiltin("get", getFunc)
	core.RegisterBuiltin("dict", dictFunc)
	core.RegisterBuiltin("assert", assertFunc)
	core.RegisterBuiltin("len", lenFunc)
}

func isNumber(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number? requires exactly one argument")
	}
	_, ok := args[0].(float64)
	return ok, nil
}

func isString(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string? requires exactly one argument")
	}
	_, ok := args[0].(string)
	return ok, nil
}

func isSymbol(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol? requires exactly one argument")
	}
	_, ok := args[0].(core.LispSymbol)
	return ok, nil
}

func isList(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("list? requires exactly one argument")
	}
	_, ok := args[0].(core.LispList)
	return ok, nil
}

func not(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not requires exactly one argument")
	}
	return !core.IsTruthy(args[0]), nil
}

func print(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		fmt.Print(core.PrintValue(arg), " ")
	}
	fmt.Println()
	return nil, nil
}

func assoc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("assoc requires exactly two arguments: a key and a list")
	}
	key, ok := args[0].(core.LispValue)
	if !ok {
		return nil, fmt.Errorf("assoc first argument must be a Lisp value")
	}
	list, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("assoc second argument must be a list")
	}

	for _, item := range list {
		pair, ok := item.(core.LispList)
		if ok && len(pair) == 2 {
			if core.EqualValues(pair[0], key) {
				return pair, nil
			}
		}
	}
	return nil, nil // Return nil if no matching key is found
}

func isPair(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("pair? requires exactly one argument")
	}
	_, ok := args[0].(core.LispList)
	return ok, nil
}

func isInteger(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("integer? requires exactly one argument")
	}
	switch v := args[0].(type) {
	case float64:
		return math.Floor(v) == v, nil
	case int:
		return true, nil
	default:
		return false, nil
	}
}

func errorFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("error function requires at least one argument")
	}
	errorMsg := core.PrintValue(args[0])
	return nil, fmt.Errorf(errorMsg)
}

func applyFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	if len(args) < 2 {
		return nil, fmt.Errorf("apply requires at least two arguments")
	}

	fn := args[0]
	lastArg := args[len(args)-1]
	middleArgs := args[1 : len(args)-1]

	argList, ok := lastArg.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("last argument to apply must be a list")
	}

	allArgs := append(middleArgs, argList...)

	// If fn is a symbol, we need to look it up in the environment
	if symbol, ok := fn.(core.LispSymbol); ok {
		var found bool
		fn, found = env.Get(symbol)
		if !found {
			return nil, fmt.Errorf("undefined function: %s", symbol)
		}
	}

	return e.Apply(fn, allArgs, env)
}

func filterFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("filter requires exactly two arguments")
	}

	predicate := args[0]
	list, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to filter must be a list")
	}

	result := make(core.LispList, 0)
	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	for _, item := range list {
		predicateResult, err := e.Apply(predicate, []core.LispValue{item}, env)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(predicateResult) {
			result = append(result, item)
		}
	}

	return result, nil
}

func matchFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("match requires exactly two arguments")
	}

	pattern, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("first argument to match must be a list")
	}

	fact, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to match must be a list")
	}

	return matchLists(pattern, fact), nil
}

func matchLists(pattern, fact core.LispList) core.LispValue {
	if len(pattern) != len(fact) {
		return false
	}

	for i, patternItem := range pattern {
		factItem := fact[i]

		if patternItem == core.LispSymbol("?") {
			continue
		}

		if !core.EqualValues(patternItem, factItem) {
			return false
		}
	}

	return true
}

func existsFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("exists? requires exactly two arguments")
	}

	predicate, ok := args[0].(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to exists? must be a function")
	}

	list, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to exists? must be a list")
	}

	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	for _, item := range list {
		result, err := e.Apply(predicate, []core.LispValue{item}, env)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(result) {
			return true, nil
		}
	}

	return false, nil
}

func funcallFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("funcall requires at least one argument")
	}

	fn := args[0]
	fnArgs := args[1:]

	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	return e.Apply(fn, fnArgs, env)
}

func numberp(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("numberp requires exactly one argument")
	}

	_, ok := args[0].(float64)
	return ok, nil
}

func symbolpFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbolp requires exactly one argument")
	}

	_, isSymbol := args[0].(core.LispSymbol)
	return isSymbol, nil
}

func symbolToStringFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol->string requires exactly one argument")
	}

	symbol, isSymbol := args[0].(core.LispSymbol)
	if !isSymbol {
		return nil, fmt.Errorf("symbol->string requires a symbol as its argument")
	}

	return string(symbol), nil
}

func atomFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("atom requires exactly one argument")
	}

	switch args[0].(type) {
	case core.LispSymbol, float64, string, bool, core.PythonicNone:
		return core.PythonicBool(true), nil
	case core.LispList:
		return core.PythonicBool(false), nil
	default:
		return core.PythonicBool(true), nil
	}
}

func dictFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	dict := core.NewPythonicDict()

	// Handle empty dict - no arguments
	if len(args) == 0 {
		return dict, nil
	}

	// Handle a list of key-value pairs format: (dict (list (list k1 v1) (list k2 v2)))
	if len(args) == 1 {
		switch arg := args[0].(type) {
		case core.LispList:
			for _, item := range arg {
				pair, ok := item.(core.LispList)
				if !ok || len(pair) != 2 {
					return nil, fmt.Errorf("dict() requires an iterable of key/value pairs")
				}
				dict.Set(pair[0], pair[1])
			}
			return dict, nil
		}
	}

	// Handle direct key-value pairs format: (dict k1 v1 k2 v2)
	if len(args)%2 != 0 {
		return nil, fmt.Errorf("dict() key-value arguments must come in pairs")
	}

	for i := 0; i < len(args); i += 2 {
		dict.Set(args[i], args[i+1])
	}

	return dict, nil
}

func frozensetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Note: This is a placeholder. Implementing frozenset would require additional data structures.
	return nil, fmt.Errorf("frozenset() is not implemented in this Lisp interpreter")
}

func getattrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Note: This is a placeholder. Implementing getattr would require additional work on object system.
	return nil, fmt.Errorf("getattr() is not implemented in this Lisp interpreter")
}

func globalsFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("globals() takes no arguments")
	}
	return env, nil
}

func hasattrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Note: This is a placeholder. Implementing hasattr would require additional work on object system.
	return nil, fmt.Errorf("hasattr() is not implemented in this Lisp interpreter")
}

func hashFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("hash() takes exactly one argument")
	}
	// This is a very simple hash function and should not be used for cryptographic purposes
	return float64(hashString(fmt.Sprintf("%v", args[0]))), nil
}

func hashString(s string) uint32 {
	h := uint32(2166136261)
	for i := 0; i < len(s); i++ {
		h = (h * 16777619) ^ uint32(s[i])
	}
	return h
}

func idFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("id() takes exactly one argument")
	}
	return float64(reflect.ValueOf(args[0]).Pointer()), nil
}

func localsFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("locals() takes no arguments")
	}
	return env, nil
}

func setFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("set() takes at most 1 argument")
	}
	if len(args) == 0 {
		return make(map[core.LispValue]bool), nil
	}
	iterable, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("set() argument must be iterable")
	}
	set := make(map[core.LispValue]bool)
	for _, item := range iterable {
		set[item] = true
	}
	return set, nil
}

func sliceFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 3 {
		return nil, fmt.Errorf("slice() takes 1 to 3 arguments")
	}

	// Python-style list slicing
	if list, ok := args[0].(core.LispList); ok {
		// Handle slice(list, start, end)
		if len(args) >= 2 {
			start, ok := args[1].(float64)
			if !ok {
				return nil, fmt.Errorf("slice start index must be a number")
			}

			startIdx := int(start)
			if startIdx < 0 {
				startIdx = len(list) + startIdx
			}
			if startIdx < 0 {
				startIdx = 0
			}

			if len(args) == 2 {
				// slice(list, start) - return from start to end
				if startIdx >= len(list) {
					return core.LispList{}, nil
				}
				return list[startIdx:], nil
			} else {
				// slice(list, start, end)
				end, ok := args[2].(float64)
				if !ok {
					return nil, fmt.Errorf("slice end index must be a number")
				}

				endIdx := int(end)
				if endIdx < 0 {
					endIdx = len(list) + endIdx
				}
				if endIdx > len(list) {
					endIdx = len(list)
				}

				if startIdx >= len(list) || startIdx >= endIdx {
					return core.LispList{}, nil
				}

				return list[startIdx:endIdx], nil
			}
		}
	}

	// Python's range-like slice functionality
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
	}
	return core.LispList{start, stop, step}, nil
}

func varsFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("vars() takes at most 1 argument")
	}
	if len(args) == 0 {
		return env, nil
	}
	// Note: This is a simplified implementation. In a full implementation,
	// we would need to handle different types of objects.
	return nil, fmt.Errorf("vars() with an argument is not implemented in this Lisp interpreter")
}

// Additional utility functions that might be useful

func lenFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("len() takes exactly one argument")
	}
	switch v := args[0].(type) {
	case core.LispList:
		return float64(len(v)), nil
	case string:
		return float64(len(v)), nil
	case map[core.LispValue]core.LispValue:
		return float64(len(v)), nil
	case map[core.LispValue]bool:
		return float64(len(v)), nil
	case *core.PythonicDict:
		return float64(v.Size()), nil
	case *core.PythonicSet:
		return float64(v.Size()), nil
	default:
		return nil, fmt.Errorf("object of type '%T' has no len()", v)
	}
}

func mapFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("map() requires at least two arguments")
	}
	fn := args[0]
	iterables := args[1:]

	// Check if all arguments after the function are iterable
	lengths := make([]int, len(iterables))
	for i, arg := range iterables {
		list, ok := arg.(core.LispList)
		if !ok {
			return nil, fmt.Errorf("map() arguments after the first must be iterable")
		}
		lengths[i] = len(list)
	}

	// Find the minimum length
	minLen := lengths[0]
	for _, l := range lengths[1:] {
		if l < minLen {
			minLen = l
		}
	}

	result := make(core.LispList, minLen)
	e, err := getEvaluator()
	if err != nil {
		return nil, err
	}

	for i := 0; i < minLen; i++ {
		args := make([]core.LispValue, len(iterables))
		for j, iterable := range iterables {
			args[j] = iterable.(core.LispList)[i]
		}
		res, err := e.Apply(fn, args, env)
		if err != nil {
			return nil, err
		}
		result[i] = res
	}

	return result, nil
}

// getFunc implements Python's get() function for dictionaries and other mappings
func getFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("get() takes 2 or 3 arguments")
	}

	container := args[0]
	key := args[1]
	var defaultValue core.LispValue = core.PythonicNone{}

	if len(args) == 3 {
		defaultValue = args[2]
	}

	switch c := container.(type) {
	case *core.PythonicDict:
		value, found := c.Get(key)
		if found {
			return value, nil
		}
		return defaultValue, nil

	case map[core.LispValue]core.LispValue:
		value, found := c[key]
		if found {
			return value, nil
		}
		return defaultValue, nil

	case core.LispList:
		index, ok := key.(float64)
		if !ok {
			return nil, fmt.Errorf("list indices must be numbers")
		}
		idx := int(index)
		if idx < 0 || idx >= len(c) {
			return defaultValue, nil
		}
		return c[idx], nil

	default:
		return nil, fmt.Errorf("get() requires a mapping or sequence as first argument, got %T", container)
	}
}

// assertFunc implements Python-style assertions
func assertFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("assert() takes 1 or 2 arguments")
	}

	condition := core.IsTruthy(args[0])

	if !condition {
		// Handle assertion failure
		if len(args) == 2 {
			// With custom message
			message := core.PrintValue(args[1])
			return nil, fmt.Errorf("AssertionError: %s", message)
		} else {
			// Default message
			return nil, fmt.Errorf("AssertionError: assertion failed")
		}
	}

	// Assertion passed
	return core.PythonicBool(true), nil
}

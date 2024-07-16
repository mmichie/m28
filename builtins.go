package m28

import (
	"fmt"
	"strconv"
	"strings"
)

// BuiltinFunc represents a built-in function
type BuiltinFunc func([]LispValue, *Environment) (LispValue, error)

// builtinFuncs maps function names to their implementations
var builtinFuncs map[string]BuiltinFunc

func init() {
	builtinFuncs = map[string]BuiltinFunc{
		// Logical operations
		"and": and,
		"or":  or,
		"not": notFunc,

		// Type checking
		"number?":  isNumber,
		"string?":  isString,
		"symbol?":  isSymbol,
		"list?":    isList,
		"null?":    nullFunc,
		"pair?":    pairFunc,
		"integer?": integerFunc,

		// Other functions
		"print":          printFunc,
		"string-append":  stringAppend,
		"number->string": numberToString,
		"error":          errorFunc,
		"apply":          applyFunc,
		"eq?":            eqFunc,
		"caar":           caarFunc,
		"cadar":          cadarFunc,
		"equal?":         equalFunc,
		"assoc":          assoc,
		"filter":         filter,
		"match":          match,
		"exists?":        exists,
	}
}

func setupBuiltins(env *Environment) {
	env.Set(LispSymbol("nil"), nil)
	env.Set(LispSymbol("#f"), false)
	env.Set(LispSymbol("#t"), true)

	registerArithmeticFuncs(env)
	registerComparisonFuncs(env)
	registerListFuncs(env)

	for name, fn := range builtinFuncs {
		env.Set(LispSymbol(name), LispFunc(fn))
	}
}

// Logical operations

func and(args []LispValue, _ *Environment) (LispValue, error) {
	for _, arg := range args {
		if !IsTruthy(arg) {
			return false, nil
		}
	}
	return true, nil
}

func or(args []LispValue, _ *Environment) (LispValue, error) {
	for _, arg := range args {
		if IsTruthy(arg) {
			return true, nil
		}
	}
	return false, nil
}

func notFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("not expects exactly one argument")
	}
	return !IsTruthy(args[0]), nil
}

// Type checking

func isNumber(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number? expects exactly one argument")
	}
	_, ok := args[0].(float64)
	return ok, nil
}

func isString(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string? expects exactly one argument")
	}
	_, ok := args[0].(string)
	return ok, nil
}

func isSymbol(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("symbol? expects exactly one argument")
	}
	_, ok := args[0].(LispSymbol)
	return ok, nil
}

func isList(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("list? expects exactly one argument")
	}
	_, ok := args[0].(LispList)
	return ok, nil
}

func nullFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("null? expects exactly one argument")
	}
	switch arg := args[0].(type) {
	case LispList:
		return len(arg) == 0, nil
	case nil:
		return true, nil
	default:
		return false, nil
	}
}

func pairFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("pair? expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	return ok && len(list) > 0, nil
}

func integerFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("integer? expects exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return false, nil
	}
	return float64(int(num)) == num, nil
}

// Other functions

func printFunc(args []LispValue, _ *Environment) (LispValue, error) {
	for _, arg := range args {
		fmt.Print(PrintValue(arg), " ")
	}
	fmt.Println()
	return nil, nil
}

func stringAppend(args []LispValue, _ *Environment) (LispValue, error) {
	var parts []string
	for _, arg := range args {
		switch v := arg.(type) {
		case string:
			parts = append(parts, v)
		case LispSymbol:
			parts = append(parts, string(v))
		default:
			parts = append(parts, fmt.Sprint(v))
		}
	}
	return strings.Join(parts, ""), nil
}

func numberToString(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number->string expects exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("number->string expects a number, got %T", args[0])
	}
	return strconv.FormatFloat(num, 'f', -1, 64), nil
}

func errorFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("Error")
	}
	errorMsg := make([]string, len(args))
	for i, arg := range args {
		errorMsg[i] = PrintValue(arg)
	}
	return nil, fmt.Errorf(strings.Join(errorMsg, " "))
}

func applyFunc(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("apply expects at least two arguments")
	}

	fn := args[0]
	lastArg := args[len(args)-1]

	var applyArgs []LispValue
	for _, arg := range args[1 : len(args)-1] {
		applyArgs = append(applyArgs, arg)
	}

	if list, ok := lastArg.(LispList); ok {
		applyArgs = append(applyArgs, list...)
	} else {
		return nil, fmt.Errorf("last argument to apply must be a list")
	}

	return Apply(fn, applyArgs, env)
}

func eqFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("eq? expects exactly two arguments")
	}
	return args[0] == args[1], nil
}

func caarFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("caar expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("caar expects a non-empty list")
	}
	firstElem, ok := list[0].(LispList)
	if !ok || len(firstElem) == 0 {
		return nil, fmt.Errorf("caar expects a list whose first element is a non-empty list")
	}
	return firstElem[0], nil
}

func cadarFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cadar expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) < 2 {
		return nil, fmt.Errorf("cadar expects a list with at least two elements")
	}
	secondElem, ok := list[1].(LispList)
	if !ok || len(secondElem) == 0 {
		return nil, fmt.Errorf("cadar expects a list whose second element is a non-empty list")
	}
	return secondElem[0], nil
}

func equalFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("equal? expects exactly two arguments")
	}
	return EqualValues(args[0], args[1]), nil
}

func assoc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("assoc expects exactly two arguments")
	}
	key := args[0]
	alist, ok := args[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("assoc expects a list as its second argument")
	}
	for _, pair := range alist {
		pairList, ok := pair.(LispList)
		if !ok || len(pairList) < 1 {
			continue
		}
		if equalValues(key, pairList[0]) {
			return pair, nil
		}
	}
	return nil, nil
}

func filter(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("filter expects exactly two arguments")
	}
	predFunc, ok := args[0].(*Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to filter must be a function")
	}
	list, ok := args[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to filter must be a list")
	}

	var result LispList
	for _, item := range list {
		predicateResult, err := Apply(predFunc, []LispValue{item}, env)
		if err != nil {
			return nil, err
		}
		if IsTruthy(predicateResult) {
			result = append(result, item)
		}
	}
	return result, nil
}

func match(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("match expects exactly two arguments")
	}
	pattern := args[0]
	fact := args[1]

	var matchHelper func(p, f LispValue) bool
	matchHelper = func(p, f LispValue) bool {
		switch pt := p.(type) {
		case LispList:
			ft, ok := f.(LispList)
			if !ok {
				return false
			}
			if len(pt) != len(ft) {
				return false
			}
			for i := range pt {
				if !matchHelper(pt[i], ft[i]) {
					return false
				}
			}
			return true
		case LispSymbol:
			if pt == LispSymbol("?") {
				return true
			}
			return equalValues(p, f)
		default:
			return equalValues(p, f)
		}
	}

	return matchHelper(pattern, fact), nil
}

func exists(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("exists? expects exactly two arguments")
	}

	predFunc, ok := args[0].(*Lambda)
	if !ok {
		return nil, fmt.Errorf("first argument to exists? must be a function")
	}

	list, ok := args[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to exists? must be a list")
	}

	for _, item := range list {
		result, err := Apply(predFunc, []LispValue{item}, env)
		if err != nil {
			return nil, err
		}
		if IsTruthy(result) {
			return true, nil
		}
	}

	return false, nil
}

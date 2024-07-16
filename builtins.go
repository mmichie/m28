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
		// Arithmetic operations
		"+": add,
		"-": subtract,
		"*": multiply,
		"/": divide,
		"%": mod,

		// Comparison operations
		"<":  lessThan,
		">":  greaterThan,
		"=":  equals,
		">=": greaterThanOrEqual,
		"<=": lessThanOrEqual,
		"!=": notEqual,

		// List operations
		"car":    car,
		"cdr":    cdr,
		"cons":   cons,
		"list":   list,
		"length": length,

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
		"append":         appendFunc,
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

	for name, fn := range builtinFuncs {
		env.Set(LispSymbol(name), LispFunc(fn))
	}
}

// Arithmetic operations

func arithmeticHelper(args []LispValue, op string) (float64, error) {
	if len(args) == 0 {
		return 0, fmt.Errorf("'%s' expects at least one argument, got none", op)
	}

	var result float64
	switch op {
	case "+":
		result = 0
	case "-", "*", "/":
		if len(args) < 1 {
			return 0, fmt.Errorf("'%s' operation requires at least one operand, got %d", op, len(args))
		}
		result, _ = args[0].(float64)
	case "%":
		if len(args) != 2 {
			return 0, fmt.Errorf("'%s' operation requires exactly two operands, got %d", op, len(args))
		}
	}

	for i, arg := range args {
		num, ok := arg.(float64)
		if !ok {
			return 0, fmt.Errorf("'%s' expects numbers, got %T in argument %d", op, arg, i+1)
		}

		switch op {
		case "+":
			result += num
		case "-":
			if i == 0 {
				continue
			}
			result -= num
		case "*":
			if i == 0 {
				result = num
				continue
			}
			result *= num
		case "/":
			if i == 0 {
				result = num
				continue
			}
			if num == 0 {
				return 0, fmt.Errorf("division by zero in argument %d", i+1)
			}
			result /= num
		case "%":
			if i == 0 {
				result = num
			} else {
				result = float64(int(result) % int(num))
			}
		}
	}

	return result, nil
}

func add(args []LispValue, _ *Environment) (LispValue, error) {
	return arithmeticHelper(args, "+")
}

func subtract(args []LispValue, _ *Environment) (LispValue, error) {
	return arithmeticHelper(args, "-")
}

func multiply(args []LispValue, _ *Environment) (LispValue, error) {
	return arithmeticHelper(args, "*")
}

func divide(args []LispValue, _ *Environment) (LispValue, error) {
	return arithmeticHelper(args, "/")
}

func mod(args []LispValue, _ *Environment) (LispValue, error) {
	return arithmeticHelper(args, "%")
}

// Comparison operations

func compareHelper(args []LispValue, op string) (bool, error) {
	if len(args) < 2 {
		return false, fmt.Errorf("'%s' expects at least two arguments", op)
	}

	prev, ok := args[0].(float64)
	if !ok {
		return false, fmt.Errorf("'%s' expects numbers, got %T", op, args[0])
	}

	for _, arg := range args[1:] {
		num, ok := arg.(float64)
		if !ok {
			return false, fmt.Errorf("'%s' expects numbers, got %T", op, arg)
		}

		switch op {
		case "<":
			if prev >= num {
				return false, nil
			}
		case ">":
			if prev <= num {
				return false, nil
			}
		case ">=":
			if prev < num {
				return false, nil
			}
		case "<=":
			if prev > num {
				return false, nil
			}
		}
		prev = num
	}

	return true, nil
}

func lessThan(args []LispValue, _ *Environment) (LispValue, error) {
	return compareHelper(args, "<")
}

func greaterThan(args []LispValue, _ *Environment) (LispValue, error) {
	return compareHelper(args, ">")
}

func greaterThanOrEqual(args []LispValue, _ *Environment) (LispValue, error) {
	return compareHelper(args, ">=")
}

func lessThanOrEqual(args []LispValue, _ *Environment) (LispValue, error) {
	return compareHelper(args, "<=")
}

func equals(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'=' expects at least two arguments")
	}
	first := args[0]
	for _, arg := range args[1:] {
		if !equalValues(first, arg) {
			return false, nil
		}
	}
	return true, nil
}

func notEqual(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'!=' expects at least two arguments")
	}
	first := args[0]
	for _, arg := range args[1:] {
		if equalValues(first, arg) {
			return false, nil
		}
	}
	return true, nil
}

// List operations

func car(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("car expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("car expects a non-empty list")
	}
	return list[0], nil
}

func cdr(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("cdr expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok || len(list) == 0 {
		return nil, fmt.Errorf("cdr expects a non-empty list")
	}
	return LispList(list[1:]), nil
}

func cons(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("'cons' expects exactly two arguments")
	}
	head, tail := args[0], args[1]
	switch t := tail.(type) {
	case LispList:
		return LispList(append([]LispValue{head}, t...)), nil
	default:
		return LispList{head, tail}, nil
	}
}

func list(args []LispValue, _ *Environment) (LispValue, error) {
	return LispList(args), nil
}

func length(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("'length' expects exactly one argument")
	}
	list, ok := args[0].(LispList)
	if !ok {
		return nil, fmt.Errorf("'length' expects a list, got %T", args[0])
	}
	return float64(len(list)), nil
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
		return nil, fmt.Errorf("'number?' expects exactly one argument")
	}
	_, ok := args[0].(float64)
	return ok, nil
}

func isString(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("'string?' expects exactly one argument")
	}
	_, ok := args[0].(string)
	return ok, nil
}

func isSymbol(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("'symbol?' expects exactly one argument")
	}
	_, ok := args[0].(LispSymbol)
	return ok, nil
}

func isList(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("'list?' expects exactly one argument")
	}
	_, ok := args[0].(LispList)
	return ok, nil
}

func nullFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("'null?' expects exactly one argument")
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
		return nil, fmt.Errorf("'number->string' expects exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("'number->string' expects a number, got %T", args[0])
	}
	return strconv.FormatFloat(num, 'f', -1, 64), nil
}

func appendFunc(args []LispValue, _ *Environment) (LispValue, error) {
	var result LispList
	for _, arg := range args {
		if list, ok := arg.(LispList); ok {
			result = append(result, list...)
		} else {
			result = append(result, arg)
		}
	}
	return result, nil
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
		return nil, fmt.Errorf("'apply' expects at least two arguments")
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
		return nil, fmt.Errorf("last argument to 'apply' must be a list")
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
	return equalValues(args[0], args[1]), nil
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
		predicateResult, err := apply(predFunc, []LispValue{item}, env)
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
		result, err := apply(predFunc, []LispValue{item}, env)
		if err != nil {
			return nil, err
		}
		if IsTruthy(result) {
			return true, nil
		}
	}

	return false, nil
}

// Helper functions

func equalValues(a, b LispValue) bool {
	switch va := a.(type) {
	case float64:
		if vb, ok := b.(float64); ok {
			return va == vb
		}
	case string:
		if vb, ok := b.(string); ok {
			return va == vb
		}
	case LispSymbol:
		if vb, ok := b.(LispSymbol); ok {
			return va == vb
		}
	case LispList:
		if vb, ok := b.(LispList); ok {
			if len(va) != len(vb) {
				return false
			}
			for i := range va {
				if !equalValues(va[i], vb[i]) {
					return false
				}
			}
			return true
		}
	}
	return false
}

func apply(fn LispValue, args []LispValue, env *Environment) (LispValue, error) {
	switch f := fn.(type) {
	case LispFunc:
		return f(args, env)
	case *Lambda:
		if len(args) < len(f.Params) && f.RestParam == "" {
			return nil, fmt.Errorf("not enough arguments for lambda")
		}
		callEnv := NewEnvironment(f.Closure)
		for i, param := range f.Params {
			if i < len(args) {
				callEnv.Define(param, args[i])
			} else {
				callEnv.Define(param, nil)
			}
		}
		if f.RestParam != "" {
			restArgs := args[len(f.Params):]
			callEnv.Define(f.RestParam, LispList(restArgs))
		}
		return EvalExpression(f.Body, callEnv)
	default:
		return nil, fmt.Errorf("not a function: %v", fn)
	}
}

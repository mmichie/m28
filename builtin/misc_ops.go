package builtin

import (
	"fmt"
	"sync"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
)

var (
	evaluator *eval.Evaluator
	evalOnce  sync.Once
	evalMutex sync.RWMutex
)

func SetEvaluator(e *eval.Evaluator) {
	evalOnce.Do(func() {
		evalMutex.Lock()
		defer evalMutex.Unlock()
		evaluator = e
	})
}

func getEvaluator() (*eval.Evaluator, error) {
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
	_, ok := args[0].(int)
	return ok, nil
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

	return e.Apply(fn, allArgs, env)
}

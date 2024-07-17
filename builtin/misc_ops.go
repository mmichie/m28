package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func init() {
	core.RegisterBuiltin("number?", isNumber)
	core.RegisterBuiltin("string?", isString)
	core.RegisterBuiltin("symbol?", isSymbol)
	core.RegisterBuiltin("list?", isList)
	core.RegisterBuiltin("not", not)
	core.RegisterBuiltin("print", print)
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

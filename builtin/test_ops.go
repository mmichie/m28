package builtin

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

func RegisterTestOps() {
	core.RegisterBuiltin("format", formatFunc)
	core.RegisterBuiltin("assert", assertFunc)
	core.RegisterBuiltin("ignore-errors", ignoreErrorsFunc)
}

func formatFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("format requires at least 2 arguments")
	}

	format, ok := args[1].(string)
	if !ok {
		return nil, fmt.Errorf("format string must be a string")
	}

	// Simple implementation, doesn't handle all format directives
	result := format
	for i := 2; i < len(args); i++ {
		result = strings.Replace(result, "~a", core.PrintValue(args[i]), 1)
	}

	fmt.Print(result)
	return nil, nil
}

func assertFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("assert requires exactly 1 argument")
	}

	if !core.IsTruthy(args[0]) {
		return nil, fmt.Errorf("assertion failed")
	}

	return core.LispSymbol("t"), nil
}

func ignoreErrorsFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("ignore-errors requires exactly 1 argument")
	}

	result, err := env.(core.Evaluator).Eval(args[0], env)
	if err != nil {
		return nil, nil // Return nil instead of the error
	}

	return result, nil
}

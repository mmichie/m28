package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalWith(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without context manager support
	if len(args) < 2 {
		return nil, fmt.Errorf("with requires at least 2 arguments")
	}

	withEnv := env.NewEnvironment(env)

	var result core.LispValue
	var err error
	for _, expr := range args[1:] {
		result, err = e.Eval(expr, withEnv)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func EvalReturn(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("return takes at most one argument")
	}

	if len(args) == 0 {
		return nil, nil
	}

	return e.Eval(args[0], env)
}

func EvalYield(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("yield requires exactly one argument")
	}

	return e.Eval(args[0], env)
}

func EvalDel(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without true deletion
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("del arguments must be symbols")
		}
		env.Set(symbol, nil)
	}
	return nil, nil
}

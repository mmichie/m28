package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalGlobal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("global arguments must be symbols")
		}
		env.Define(symbol, nil)
	}
	return nil, nil
}

func EvalNonlocal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without true nonlocal behavior
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("nonlocal arguments must be symbols")
		}
		env.Define(symbol, nil)
	}
	return nil, nil
}

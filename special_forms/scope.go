package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// EvalGlobal handles global variable declarations
func EvalGlobal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("global requires at least one variable name")
	}

	// For each argument, mark it as global
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("global arguments must be symbols, got %T", arg)
		}

		// Find the variable in the environment chain
		// and mark it as global
		if markGlobalVar, ok := env.(interface {
			MarkGlobal(sym core.LispSymbol)
		}); ok {
			markGlobalVar.MarkGlobal(symbol)
		} else {
			return nil, fmt.Errorf("environment doesn't support global variables")
		}
	}

	return core.PythonicNone{}, nil
}

// EvalNonlocal handles nonlocal variable declarations
func EvalNonlocal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("nonlocal requires at least one variable name")
	}

	// For each argument, mark it as nonlocal
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("nonlocal arguments must be symbols, got %T", arg)
		}

		// Find the variable in the environment chain
		// and mark it as nonlocal
		if markNonlocalVar, ok := env.(interface {
			MarkNonlocal(sym core.LispSymbol)
		}); ok {
			markNonlocalVar.MarkNonlocal(symbol)
		} else {
			return nil, fmt.Errorf("environment doesn't support nonlocal variables")
		}
	}

	return core.PythonicNone{}, nil
}

// EvalDel deletes a variable from the environment
func EvalDel(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("del requires at least one argument")
	}

	// Handle each argument
	for _, arg := range args {
		// If it's a symbol, delete the variable
		if symbol, ok := arg.(core.LispSymbol); ok {
			// Use environment interface if available
			if deleter, ok := env.(interface {
				Delete(sym core.LispSymbol) error
			}); ok {
				if err := deleter.Delete(symbol); err != nil {
					return nil, err
				}
			} else {
				return nil, fmt.Errorf("environment doesn't support deleting variables")
			}
		} else {
			// Try to evaluate as an item access (like del a[0] or del d["key"])
			// This would require modification to the evaluator to support this
			return nil, fmt.Errorf("del of non-symbol arguments not implemented yet")
		}
	}

	return core.PythonicNone{}, nil
}

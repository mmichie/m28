package concurrency

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// EvalGo implements the go special form for spawning goroutines
func EvalGo(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("go requires at least one expression to evaluate")
	}

	// Create a new environment that captures the current lexical scope
	goEnv := env.NewEnvironment(nil)

	// Copy current environment values to the new environment
	if collector, ok := env.(core.EnvironmentCollector); ok {
		collector.ForEachSymbol(func(symbol core.LispSymbol, value core.LispValue) {
			goEnv.Define(symbol, value)
		})
	}

	// Spawn a goroutine to evaluate the expression
	go func() {
		expr := args[0]
		_, err := e.Eval(expr, goEnv)
		if err != nil {
			fmt.Printf("Error in goroutine: %v\n", err)
		}
	}()

	// Return None immediately
	return core.PythonicNone{}, nil
}

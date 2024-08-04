package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalImport(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("import requires at least one argument")
	}

	moduleName, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("module name must be a symbol")
	}

	// Here you would implement the actual module importing logic
	// For now, we'll just create a dummy module
	module := core.NewPythonicDict()
	env.Define(moduleName, module)

	return module, nil
}

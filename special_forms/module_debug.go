package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// DebugModule prints the contents of a module
func DebugModule(name string) core.BuiltinFunc {
	return func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("debug_module takes no arguments")
		}

		// Get the module
		moduleVal, ok := env.Get(core.LispSymbol(name))
		if !ok {
			return nil, fmt.Errorf("module %s not found", name)
		}

		// Check if it's a dictionary
		moduleDict, ok := moduleVal.(*core.PythonicDict)
		if !ok {
			return nil, fmt.Errorf("%s is not a module dictionary", name)
		}

		// Print the module contents
		fmt.Printf("Debug module %s:\n", name)
		moduleDict.Iterate(func(key, value core.LispValue) error {
			fmt.Printf("  %v = %v (type: %T)\n", key, value, value)
			return nil
		})

		return core.PythonicNone{}, nil
	}
}

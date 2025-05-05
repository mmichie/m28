package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// DebugModuleFunc is a built-in function to debug modules
func DebugModuleFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("debug_module requires exactly one argument (the module name)")
	}

	// Get the module name
	moduleName, ok := args[0].(core.LispSymbol)
	if !ok {
		moduleNameStr, ok := args[0].(string)
		if !ok {
			return nil, fmt.Errorf("module name must be a symbol or string")
		}
		moduleName = core.LispSymbol(moduleNameStr)
	}

	// Get the module
	moduleVal, ok := env.Get(moduleName)
	if !ok {
		return nil, fmt.Errorf("module %s not found in environment", moduleName)
	}

	// Check if it's a dictionary
	moduleDict, ok := moduleVal.(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("%s is not a module dictionary (actual type: %T)", moduleName, moduleVal)
	}

	// Print the module contents
	fmt.Printf("Debug module %s (type: %T):\n", moduleName, moduleVal)
	fmt.Printf("Module has %d symbols\n", moduleDict.Size())

	moduleDict.Iterate(func(key, value core.LispValue) error {
		fmt.Printf("  %v = %v (type: %T)\n", key, value, value)
		return nil
	})

	return core.PythonicNone{}, nil
}

func init() {
	core.RegisterBuiltin("debug_module", DebugModuleFunc)
	core.RegisterBuiltin("type", TypeOf)
}

// TypeOf returns the type of a value as a string
func TypeOf(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("type() requires exactly one argument")
	}

	// Get the Golang type as a string
	return fmt.Sprintf("%T", args[0]), nil
}

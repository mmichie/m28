package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// ModuleGetFunc provides a special get function for modules
func ModuleGetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("module_get() requires exactly 2 arguments")
	}

	// First argument should be a module
	module, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("first argument must be a module")
	}

	// Second argument should be a symbol or string
	var attrName core.LispSymbol
	switch attr := args[1].(type) {
	case core.LispSymbol:
		attrName = attr
	case string:
		attrName = core.LispSymbol(attr)
	default:
		return nil, fmt.Errorf("attribute name must be a string or symbol")
	}

	// Get the attribute from the module
	if value, ok := module.Get(attrName); ok {
		return value, nil
	}

	// If not found, try as a string key
	if value, ok := module.Get(string(attrName)); ok {
		return value, nil
	}

	// Return None if not found
	return core.PythonicNone{}, nil
}

func init() {
	core.RegisterBuiltin("module_get", ModuleGetFunc)
}

// Package modules provides the _frozen_importlib C extension module stub for M28
// This is CPython's frozen import machinery - M28 doesn't need it since we have our own import system.
// We provide minimal stubs just to satisfy import checks.
package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_FrozenImportlibModule creates and returns the _frozen_importlib module stub
// This is a minimal stub that provides just enough for importlib to work.
// M28 uses its own import system, so these are no-op implementations.
func Init_FrozenImportlibModule() *core.DictValue {
	frozenImportlibModule := core.NewDict()

	// _install() - No-op function for import machinery setup
	// CPython uses this to install the frozen import machinery, but M28 has its own system
	frozenImportlibModule.Set("_install", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op: M28 uses its own import system
		return core.None, nil
	}))

	// _init_module_attrs() - No-op function for initializing module attributes
	// CPython uses this to set __name__, __loader__, etc. on modules
	frozenImportlibModule.Set("_init_module_attrs", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op: M28 handles module attributes differently
		return core.None, nil
	}))

	// _bootstrap - Empty module object representing the bootstrap import machinery
	// This is just a placeholder - CPython's frozen bootstrap code
	frozenImportlibModule.Set("_bootstrap", core.NewDict())

	// __name__ - Module name attribute
	frozenImportlibModule.Set("__name__", core.StringValue("_frozen_importlib"))

	// __doc__ - Module docstring
	frozenImportlibModule.Set("__doc__", core.StringValue("Stub for CPython's frozen import machinery. M28 uses its own import system."))

	return frozenImportlibModule
}

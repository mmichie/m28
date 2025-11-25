// Package modules provides the _frozen_importlib_external C extension module stub for M28
// This is CPython's frozen external import machinery - M28 doesn't need it since we have our own import system.
// We provide minimal stubs just to satisfy import checks.
package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_FrozenImportlibExternalModule creates and returns the _frozen_importlib_external module stub
// This is a minimal stub that provides just enough for importlib to work.
// M28 uses its own import system, so these are no-op implementations.
func Init_FrozenImportlibExternalModule() *core.DictValue {
	frozenImportlibExternalModule := core.NewDict()

	// _install() - No-op function for import machinery setup
	// CPython uses this to install the frozen external import machinery, but M28 has its own system
	frozenImportlibExternalModule.Set("_install", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op: M28 uses its own import system
		return core.None, nil
	}))

	// Stub bootstrap loaders as empty classes/dicts
	// These are part of CPython's import machinery that M28 doesn't need
	// But some code might try to access them, so we provide empty stubs
	frozenImportlibExternalModule.Set("SourceFileLoader", core.NewDict())
	frozenImportlibExternalModule.Set("ExtensionFileLoader", core.NewDict())
	frozenImportlibExternalModule.Set("ModuleSpec", core.NewDict())
	frozenImportlibExternalModule.Set("FileFinder", core.NewDict())

	// __name__ - Module name attribute
	frozenImportlibExternalModule.Set("__name__", core.StringValue("_frozen_importlib_external"))

	// __doc__ - Module docstring
	frozenImportlibExternalModule.Set("__doc__", core.StringValue("Stub for CPython's frozen external import machinery. M28 uses its own import system."))

	return frozenImportlibExternalModule
}

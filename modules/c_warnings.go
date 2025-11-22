package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_WarningsModule creates and returns the _warnings module stub
// This is a minimal stub that provides just enough for importlib to work.
// It intentionally does NOT provide warn/warn_explicit so that warnings.py
// uses its pure Python fallback implementation which has full functionality.
func Init_WarningsModule() *core.DictValue {
	warningsModule := core.NewDict()

	// NOTE: We intentionally do NOT provide warn() and warn_explicit() here.
	// When warnings.py tries "from _warnings import warn", it will fail for those
	// specific imports and fall back to its pure Python implementation.
	// This gives us full warning functionality without implementing the C extension.

	// filters - list of warning filters
	warningsModule.Set("filters", core.NewList())

	// _defaultaction - default warning action
	warningsModule.Set("_defaultaction", core.StringValue("default"))

	// _onceregistry - registry for "once" warnings
	warningsModule.Set("_onceregistry", core.NewDict())

	// _filters_mutated - called when filters list is modified
	warningsModule.Set("_filters_mutated", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would normally invalidate warning caches
		return core.None, nil
	}))

	// _showwarnmsg_impl - implementation for showing warnings
	// This gets replaced by catch_warnings(record=True) with list.append
	showwarnmsgImpl := core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Default: just ignore warnings
		return core.None, nil
	})
	warningsModule.Set("_showwarnmsg_impl", showwarnmsgImpl)

	// showwarning - show a warning
	showwarningFunc := &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "showwarning",
		Fn: func(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
			// Default implementation - calls _showwarnmsg_impl
			// The Python warnings module may replace this
			return core.None, nil
		},
	}
	warningsModule.Set("showwarning", showwarningFunc)

	// _showwarning_orig - original showwarning implementation
	// Used by catch_warnings to reset
	warningsModule.Set("_showwarning_orig", showwarningFunc)

	return warningsModule
}

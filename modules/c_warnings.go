package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_WarningsModule creates and returns the _warnings module stub
// This is a minimal stub that provides the C extension interface for warnings.py
func Init_WarningsModule() *core.DictValue {
	warningsModule := core.NewDict()

	// NOTE: warn / warn_explicit are deliberately NOT provided here.
	//
	// CPython's warnings.py does `from _warnings import (filters, _defaultaction,
	// _onceregistry, warn, warn_explicit, _filters_mutated)` inside a
	// `try/except ImportError`. When _warnings supplies warn/warn_explicit they
	// override warnings.py's pure-Python versions. A Go stub for warn cannot call
	// back into the module-level `_showwarnmsg_impl` hook that catch_warnings(
	// record=True) installs, so a stubbed warn breaks assertWarns / catch_warnings
	// recording (it just prints to stderr).
	//
	// By omitting warn/warn_explicit, the `from _warnings import (...)` above
	// raises ImportError, so warnings.py keeps its own correct pure-Python warn /
	// warn_explicit, which honour filters and _showwarnmsg_impl. This follows the
	// project rule of letting pure-Python stdlib run directly.

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

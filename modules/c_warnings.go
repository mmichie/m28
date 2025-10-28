package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_WarningsModule creates and returns the _warnings module stub
// This is a minimal stub implementation of Python's _warnings C extension module
func Init_WarningsModule() *core.DictValue {
	warningsModule := core.NewDict()

	// warn - issue a warning
	warningsModule.Set("warn", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// For now, just print the warning to stderr (simplified)
		// In a full implementation, this would check filters, etc.
		return core.NilValue{}, nil
	}))

	// warn_explicit - issue a warning with explicit file/line info
	warningsModule.Set("warn_explicit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now
		return core.NilValue{}, nil
	}))

	// filters - list of warning filters
	warningsModule.Set("filters", core.NewList())

	// _defaultaction - default warning action
	warningsModule.Set("_defaultaction", core.StringValue("default"))

	// _onceregistry - registry for "once" warnings
	warningsModule.Set("_onceregistry", core.NewDict())

	return warningsModule
}

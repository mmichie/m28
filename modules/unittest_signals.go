package modules

import (
	"github.com/mmichie/m28/core"
)

// InitUnittestSignalsModule creates a stub for unittest.signals
// This handles SIGINT signal handling for test interruption
func InitUnittestSignalsModule() *core.DictValue {
	signalsModule := core.NewDict()

	// registerResult - Register a test result for signal handling
	signalsModule.Set("registerResult", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: accept the result but don't actually track it
		return core.NilValue{}, nil
	}))

	// removeResult - Remove a test result from signal handling
	signalsModule.Set("removeResult", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: return False (result wasn't registered)
		return core.BoolValue(false), nil
	}))

	// installHandler - Install signal handler for test interruption
	signalsModule.Set("installHandler", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.NilValue{}, nil
	}))

	// removeHandler - Remove signal handler
	signalsModule.Set("removeHandler", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.NilValue{}, nil
	}))

	return signalsModule
}

package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_PythonAsyncioModule creates a minimal asyncio package stub
// This avoids the circular import issues in CPython's asyncio/__init__.py
// which tries to import everything from base_events, sslproto, etc.
//
// For unittest.mock, we need:
// - asyncio.coroutines (submodule will be loaded separately by CPython)
// - iscoroutinefunction (stub that returns False)
//
// TODO: Implement full asyncio support or fix circular import issues in CPython's asyncio
func Init_PythonAsyncioModule() *core.DictValue {
	asyncioModule := core.NewDict()

	// Provide a stub iscoroutinefunction that always returns False
	// unittest.mock checks this to determine if a function is async
	asyncioModule.Set("iscoroutinefunction", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: return False for all inputs
		// Real implementation would check if arg is a coroutine function
		return core.False, nil
	}))

	// Note: asyncio.coroutines will be loaded separately via import system
	// When unittest.mock does "import asyncio", it gets this stub
	// When it does "asyncio.coroutines._is_coroutine", the import system
	// will load asyncio.coroutines.py from CPython

	return asyncioModule
}

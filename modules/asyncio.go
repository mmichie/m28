package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_AsyncioModule creates the _asyncio module stub
// This is a C extension module that provides fast implementations of asyncio core types
func Init_AsyncioModule() *core.DictValue {
	asyncioModule := core.NewDict()

	// Future - class for representing future results
	futureClass := core.NewClass("Future", nil)
	asyncioModule.SetWithKey("Future", core.StringValue("Future"), futureClass)

	// Task - class for wrapping coroutines
	taskClass := core.NewClass("Task", nil)
	asyncioModule.SetWithKey("Task", core.StringValue("Task"), taskClass)

	// get_event_loop() - get the current event loop
	asyncioModule.SetWithKey("get_event_loop", core.StringValue("get_event_loop"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None as stub - Python code will handle it
		return core.None, nil
	}))

	// get_running_loop() - get the currently running event loop
	asyncioModule.SetWithKey("get_running_loop", core.StringValue("get_running_loop"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None as stub - will raise RuntimeError in Python if None
		return core.None, nil
	}))

	// _get_running_loop() - internal function to get running loop (may return None)
	asyncioModule.SetWithKey("_get_running_loop", core.StringValue("_get_running_loop"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// _set_running_loop(loop) - internal function to set running loop
	asyncioModule.SetWithKey("_set_running_loop", core.StringValue("_set_running_loop"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// current_task(loop=None) - get the currently running task
	asyncioModule.SetWithKey("current_task", core.StringValue("current_task"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	return asyncioModule
}

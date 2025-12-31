package modules

import (
	"runtime"

	"github.com/mmichie/m28/core"
)

// InitGCModule creates the gc (garbage collection) module
// Go handles GC automatically, so most functions are no-ops or return sensible defaults
func InitGCModule() *core.DictValue {
	gcModule := core.NewDict()

	// enable() - Enable automatic garbage collection (no-op in Go)
	gcModule.Set("enable", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NilValue{}, nil
	}))

	// disable() - Disable automatic garbage collection (no-op in Go)
	gcModule.Set("disable", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NilValue{}, nil
	}))

	// isenabled() - Returns True if GC is enabled
	gcModule.Set("isenabled", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.BoolValue(true), nil
	}))

	// collect(generation=2) - Run garbage collection, return count of unreachable objects
	gcModule.Set("collect", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Trigger Go GC
		runtime.GC()
		// Return 0 as we can't easily count unreachable objects in Go
		return core.NumberValue(0), nil
	}))

	// get_count() - Return current collection counts as (count0, count1, count2)
	gcModule.Set("get_count", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Go doesn't have generational GC counts, return zeros
		return core.TupleValue{core.NumberValue(0), core.NumberValue(0), core.NumberValue(0)}, nil
	}))

	// get_threshold() - Return the current collection thresholds
	gcModule.Set("get_threshold", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return default Python thresholds
		return core.TupleValue{core.NumberValue(700), core.NumberValue(10), core.NumberValue(10)}, nil
	}))

	// set_threshold(threshold0, threshold1=None, threshold2=None) - Set thresholds (no-op)
	gcModule.Set("set_threshold", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NilValue{}, nil
	}))

	// get_objects(generation=None) - Return list of tracked objects
	gcModule.Set("get_objects", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return empty list - we don't track objects like CPython
		return core.NewList(), nil
	}))

	// get_referrers(*objs) - Return list of objects that refer to any of objs
	gcModule.Set("get_referrers", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NewList(), nil
	}))

	// get_referents(*objs) - Return list of objects directly referred to by objs
	gcModule.Set("get_referents", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NewList(), nil
	}))

	// is_tracked(obj) - Returns True if the object is tracked by the GC
	gcModule.Set("is_tracked", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.BoolValue(false), nil
	}))

	// is_finalized(obj) - Returns True if the object has been finalized
	gcModule.Set("is_finalized", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.BoolValue(false), nil
	}))

	// freeze() - Freeze all objects tracked by gc (Python 3.7+)
	gcModule.Set("freeze", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NilValue{}, nil
	}))

	// unfreeze() - Unfreeze frozen objects
	gcModule.Set("unfreeze", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NilValue{}, nil
	}))

	// get_freeze_count() - Return number of frozen objects
	gcModule.Set("get_freeze_count", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	// Constants
	gcModule.Set("DEBUG_STATS", core.NumberValue(1))
	gcModule.Set("DEBUG_COLLECTABLE", core.NumberValue(2))
	gcModule.Set("DEBUG_UNCOLLECTABLE", core.NumberValue(4))
	gcModule.Set("DEBUG_SAVEALL", core.NumberValue(32))
	gcModule.Set("DEBUG_LEAK", core.NumberValue(38)) // DEBUG_COLLECTABLE | DEBUG_UNCOLLECTABLE | DEBUG_SAVEALL

	// garbage - list of unreachable objects
	gcModule.Set("garbage", core.NewList())

	// callbacks - list of callbacks
	gcModule.Set("callbacks", core.NewList())

	return gcModule
}

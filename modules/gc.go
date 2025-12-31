// Package modules provides Go-implemented standard library modules for M28.
package modules

import (
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitGCModule returns the gc (garbage collection) module.
// In Go, we don't have fine-grained GC control like Python, so this is a stub
// that provides the API but mostly does nothing.
func InitGCModule() *core.DictValue {
	gcModule := core.NewDict()

	// gc.enable() - enable automatic garbage collection
	gcModule.SetWithKey("enable", core.StringValue("enable"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("enable", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// In Go, GC is always enabled
		return core.Nil, nil
	}))

	// gc.disable() - disable automatic garbage collection
	gcModule.SetWithKey("disable", core.StringValue("disable"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("disable", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// In Go, GC cannot be disabled, but we just pretend
		return core.Nil, nil
	}))

	// gc.isenabled() - returns True if automatic gc is enabled
	gcModule.SetWithKey("isenabled", core.StringValue("isenabled"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("isenabled", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// In Go, GC is always enabled
		return core.BoolValue(true), nil
	}))

	// gc.collect(generation=2) - run garbage collection
	gcModule.SetWithKey("collect", core.StringValue("collect"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Accept 0 or 1 arguments (generation)
		v := validation.NewArgs("collect", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}
		// Go's GC doesn't use generations, just return 0 (no objects collected in this layer)
		return core.NumberValue(0), nil
	}))

	// gc.set_debug(flags) - set debug flags
	gcModule.SetWithKey("set_debug", core.StringValue("set_debug"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("set_debug", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}
		// No-op in Go
		return core.Nil, nil
	}))

	// gc.get_debug() - get debug flags
	gcModule.SetWithKey("get_debug", core.StringValue("get_debug"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get_debug", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		return core.NumberValue(0), nil
	}))

	// gc.get_objects(generation=None) - return list of all tracked objects
	gcModule.SetWithKey("get_objects", core.StringValue("get_objects"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Accept 0 or 1 arguments
		v := validation.NewArgs("get_objects", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}
		// Return empty list - we can't enumerate Go objects
		return core.NewList(nil), nil
	}))

	// gc.get_stats() - return per-generation stats
	gcModule.SetWithKey("get_stats", core.StringValue("get_stats"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get_stats", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// Return list of 3 dicts (one per generation) with basic info
		stats := make([]core.Value, 3)
		for i := 0; i < 3; i++ {
			d := core.NewDict()
			d.SetWithKey("collections", core.StringValue("collections"), core.NumberValue(0))
			d.SetWithKey("collected", core.StringValue("collected"), core.NumberValue(0))
			d.SetWithKey("uncollectable", core.StringValue("uncollectable"), core.NumberValue(0))
			stats[i] = d
		}
		return core.NewList(stats...), nil
	}))

	// gc.set_threshold(threshold0, threshold1=None, threshold2=None) - set collection thresholds
	gcModule.SetWithKey("set_threshold", core.StringValue("set_threshold"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("set_threshold", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}
		// No-op in Go - we don't have generational GC control
		return core.Nil, nil
	}))

	// gc.get_threshold() - get collection thresholds
	gcModule.SetWithKey("get_threshold", core.StringValue("get_threshold"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get_threshold", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// Return default Python thresholds as a tuple
		return core.TupleValue{
			core.NumberValue(700),
			core.NumberValue(10),
			core.NumberValue(10),
		}, nil
	}))

	// gc.get_count() - return current collection counts
	gcModule.SetWithKey("get_count", core.StringValue("get_count"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get_count", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// Return zeros as a tuple
		return core.TupleValue{
			core.NumberValue(0),
			core.NumberValue(0),
			core.NumberValue(0),
		}, nil
	}))

	// gc.get_referrers(*objs) - return list of objects that refer to any of objs
	gcModule.SetWithKey("get_referrers", core.StringValue("get_referrers"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return empty list - we can't track references in Go
		return core.NewList(nil), nil
	}))

	// gc.get_referents(*objs) - return list of objects directly referred to by any of objs
	gcModule.SetWithKey("get_referents", core.StringValue("get_referents"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return empty list - we can't track references in Go
		return core.NewList(nil), nil
	}))

	// gc.is_tracked(obj) - return True if obj is currently tracked
	gcModule.SetWithKey("is_tracked", core.StringValue("is_tracked"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("is_tracked", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}
		// In Go, all objects are tracked by the GC
		return core.BoolValue(true), nil
	}))

	// gc.is_finalized(obj) - return True if obj has been finalized
	gcModule.SetWithKey("is_finalized", core.StringValue("is_finalized"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("is_finalized", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}
		// Objects we can see aren't finalized
		return core.BoolValue(false), nil
	}))

	// gc.freeze() - freeze all current tracked objects
	gcModule.SetWithKey("freeze", core.StringValue("freeze"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("freeze", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// No-op
		return core.Nil, nil
	}))

	// gc.unfreeze() - unfreeze all objects in permanent generation
	gcModule.SetWithKey("unfreeze", core.StringValue("unfreeze"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("unfreeze", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// No-op
		return core.Nil, nil
	}))

	// gc.get_freeze_count() - return number of objects in permanent generation
	gcModule.SetWithKey("get_freeze_count", core.StringValue("get_freeze_count"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get_freeze_count", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		return core.NumberValue(0), nil
	}))

	// Constants
	gcModule.SetWithKey("DEBUG_STATS", core.StringValue("DEBUG_STATS"), core.NumberValue(1))
	gcModule.SetWithKey("DEBUG_COLLECTABLE", core.StringValue("DEBUG_COLLECTABLE"), core.NumberValue(2))
	gcModule.SetWithKey("DEBUG_UNCOLLECTABLE", core.StringValue("DEBUG_UNCOLLECTABLE"), core.NumberValue(4))
	gcModule.SetWithKey("DEBUG_SAVEALL", core.StringValue("DEBUG_SAVEALL"), core.NumberValue(32))
	gcModule.SetWithKey("DEBUG_LEAK", core.StringValue("DEBUG_LEAK"), core.NumberValue(38))

	// gc.garbage - list of uncollectable objects
	gcModule.SetWithKey("garbage", core.StringValue("garbage"), core.NewList(nil))

	// gc.callbacks - list of callbacks
	gcModule.SetWithKey("callbacks", core.StringValue("callbacks"), core.NewList(nil))

	return gcModule
}

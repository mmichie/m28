package modules

import "github.com/mmichie/m28/core"

// InitAbcModule creates the _abc module
// This provides C-level support for abstract base classes
func InitAbcModule() *core.DictValue {
	module := core.NewDict()

	// Module docstring
	module.Set("__doc__", core.StringValue("Abstract Base Classes support"))

	// _abc_init - initialize ABC machinery
	module.Set("_abc_init", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would initialize ABC caches
		return core.NilValue{}, nil
	}))

	// _abc_register - register a virtual subclass
	module.Set("_abc_register", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would update ABC registries
		return core.NilValue{}, nil
	}))

	// _abc_instancecheck - check if an instance is of a type
	module.Set("_abc_instancecheck", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("_abc_instancecheck", nil, "_abc_instancecheck() takes exactly 2 arguments")
		}
		// Simple implementation - just use isinstance
		// In full implementation, this would check ABC registries
		return core.BoolValue(false), nil
	}))

	// _abc_subclasscheck - check if a class is a subclass
	module.Set("_abc_subclasscheck", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("_abc_subclasscheck", nil, "_abc_subclasscheck() takes exactly 2 arguments")
		}
		// Simple implementation - just return false
		// In full implementation, this would check ABC registries
		return core.BoolValue(false), nil
	}))

	// _get_dump - get ABC cache dump (for debugging)
	module.Set("_get_dump", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return empty list for now (tuple-like)
		return core.NewList(), nil
	}))

	// get_cache_token - get current ABC cache token
	module.Set("get_cache_token", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a simple incrementing token (0 for now)
		return core.NumberValue(0), nil
	}))

	// _reset_registry - reset ABC registry cache
	module.Set("_reset_registry", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would clear ABC caches
		return core.NilValue{}, nil
	}))

	// _reset_caches - reset ABC internal caches
	module.Set("_reset_caches", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now - would clear ABC internal caches
		return core.NilValue{}, nil
	}))

	return module
}

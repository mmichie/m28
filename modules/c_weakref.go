package modules

import (
	"github.com/mmichie/m28/core"
)

// InitWeakrefModule initializes the _weakref C extension stub module
// This provides minimal weak reference support for Python stdlib modules
func InitWeakrefModule() *core.DictValue {
	module := core.NewDict()

	// ref(object[, callback]) - create a weak reference to object
	// For now, we return a strong reference since M28 doesn't have weak references yet
	// This is sufficient for stdlib modules that just need basic ref functionality
	module.Set("ref", core.NewNamedBuiltinFunction("ref", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("ref", nil, "ref() takes at least 1 argument (0 given)")
		}

		// For now, just return the object itself as a "weak reference"
		// In a full implementation, this would wrap the object in a WeakRef type
		// that could be dereferenced and would allow the object to be garbage collected
		obj := args[0]

		// TODO: Implement proper weak references with:
		// - Automatic dereferencing via __call__
		// - Callback support when object is collected
		// - Returning None when referent is gone

		// For now, return a simple wrapper that acts like a strong reference
		return obj, nil
	}))

	// proxy(object[, callback]) - create a weak proxy to object
	// For now, we return the object itself as a "proxy" since M28 doesn't have weak references
	// A real proxy would forward all operations to the referent and raise ReferenceError when dead
	module.Set("proxy", core.NewNamedBuiltinFunction("proxy", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("proxy", nil, "proxy() takes at least 1 argument (0 given)")
		}

		// For now, just return the object itself
		// In a full implementation, this would wrap the object in a WeakProxy type
		// that transparently forwards all operations to the referent
		obj := args[0]

		return obj, nil
	}))

	// getweakrefcount(object) - return the number of weak references to object
	// For now, always return 0 since we don't have weak references yet
	module.Set("getweakrefcount", core.NewNamedBuiltinFunction("getweakrefcount", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("getweakrefcount", nil, "getweakrefcount() takes exactly 1 argument")
		}
		// Since we don't have real weak references, always return 0
		return core.NumberValue(0), nil
	}))

	// getweakrefs(object) - return a list of weak references to object
	// For now, always return empty list since we don't have weak references yet
	module.Set("getweakrefs", core.NewNamedBuiltinFunction("getweakrefs", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("getweakrefs", nil, "getweakrefs() takes exactly 1 argument")
		}
		// Since we don't have real weak references, always return empty list
		return core.NewList(), nil
	}))

	// ReferenceError - exception raised when weak ref proxy is used after referent is gone
	refErrorClass := core.NewClassWithParents("ReferenceError", []*core.Class{})
	module.Set("ReferenceError", refErrorClass)

	// Type objects for weak references - these are just class stubs for now
	// In a full implementation, these would be actual types that instances could be checked against
	module.Set("ReferenceType", core.NewClassWithParents("ReferenceType", []*core.Class{}))
	module.Set("ProxyType", core.NewClassWithParents("ProxyType", []*core.Class{}))
	module.Set("CallableProxyType", core.NewClassWithParents("CallableProxyType", []*core.Class{}))

	// _remove_dead_weakref(dct, key) - internal function to remove dead weak refs from dict
	// For now, this is a no-op since we don't have real weak references
	module.Set("_remove_dead_weakref", core.NewNamedBuiltinFunction("_remove_dead_weakref", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("_remove_dead_weakref", nil, "_remove_dead_weakref() takes exactly 2 arguments")
		}
		// Since we don't have real weak references, do nothing
		return core.Nil, nil
	}))

	return module
}

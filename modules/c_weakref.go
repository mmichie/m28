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

	return module
}

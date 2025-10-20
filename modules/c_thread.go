package modules

import (
	"github.com/mmichie/m28/core"
)

// InitThreadModule initializes the _thread C extension stub module
// This provides minimal threading support for Python stdlib modules
func InitThreadModule() *core.DictValue {
	module := core.NewDict()

	// get_ident() - returns the thread identifier of the current thread
	// In M28 (single-threaded), always return a constant value
	module.Set("get_ident", core.NewNamedBuiltinFunction("get_ident", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, core.NewTypeError("get_ident", nil, "get_ident() takes no arguments")
		}
		// Return a constant thread ID since M28 is single-threaded
		return core.NumberValue(1), nil
	}))

	// RLock() - reentrant lock (no-op in single-threaded M28)
	module.Set("RLock", core.NewNamedBuiltinFunction("RLock", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, core.NewTypeError("RLock", nil, "RLock() takes no arguments")
		}
		// Return a mock lock object with acquire/release methods
		lock := core.NewDict()

		// acquire() method - no-op, always succeeds
		lock.Set("acquire", core.NewNamedBuiltinFunction("acquire", func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Accept optional blocking and timeout arguments but ignore them
			return core.BoolValue(true), nil
		}))

		// release() method - no-op
		lock.Set("release", core.NewNamedBuiltinFunction("release", func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) != 0 {
				return nil, core.NewTypeError("release", nil, "release() takes no arguments")
			}
			return core.NilValue{}, nil
		}))

		// __enter__ for context manager support
		lock.Set("__enter__", core.NewNamedBuiltinFunction("__enter__", func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return lock, nil
		}))

		// __exit__ for context manager support
		lock.Set("__exit__", core.NewNamedBuiltinFunction("__exit__", func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(false), nil
		}))

		return lock, nil
	}))

	return module
}

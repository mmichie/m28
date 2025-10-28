package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Init_ImpModule creates and returns the _imp module stub
// This is a minimal stub implementation of Python's _imp C extension module
// It provides the internal import machinery
func Init_ImpModule() *core.DictValue {
	impModule := core.NewDict()

	// acquire_lock - acquire the import lock
	impModule.Set("acquire_lock", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now
		return core.NilValue{}, nil
	}))

	// release_lock - release the import lock
	impModule.Set("release_lock", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op for now
		return core.NilValue{}, nil
	}))

	// lock_held - check if the import lock is held
	impModule.Set("lock_held", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Always return False for now
		return core.BoolValue(false), nil
	}))

	// is_builtin - check if a module is built-in
	impModule.Set("is_builtin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("is_builtin() takes exactly one argument")
		}
		if _, ok := args[0].(core.StringValue); !ok {
			return nil, fmt.Errorf("is_builtin() argument must be str")
		}
		// For now, just return 0 (not built-in) for all modules
		// This avoids a circular dependency with the registry
		return core.NumberValue(0), nil
	}))

	// is_frozen - check if a module is frozen
	impModule.Set("is_frozen", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// We don't have frozen modules, so always return False
		return core.BoolValue(false), nil
	}))

	// is_frozen_package - check if a module is a frozen package
	impModule.Set("is_frozen_package", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// We don't have frozen modules, so always return False
		return core.BoolValue(false), nil
	}))

	// extension_suffixes - return the list of file suffixes for extension modules
	impModule.Set("extension_suffixes", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return an empty list for now
		return core.NewList(), nil
	}))

	// create_builtin - create a built-in module
	impModule.Set("create_builtin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None for now
		return core.NilValue{}, nil
	}))

	// exec_builtin - execute a built-in module
	impModule.Set("exec_builtin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None for now
		return core.NilValue{}, nil
	}))

	// create_dynamic - create a dynamic module
	impModule.Set("create_dynamic", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None for now
		return core.NilValue{}, nil
	}))

	// exec_dynamic - execute a dynamic module
	impModule.Set("exec_dynamic", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return None for now
		return core.NilValue{}, nil
	}))

	// find_frozen - find a frozen module
	impModule.Set("find_frozen", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// We don't have frozen modules, so return None
		return core.NilValue{}, nil
	}))

	// get_frozen_object - get a frozen module object
	impModule.Set("get_frozen_object", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// We don't have frozen modules, so return None
		return core.NilValue{}, nil
	}))

	// init_frozen - initialize a frozen module
	impModule.Set("init_frozen", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// We don't have frozen modules, so return None
		return core.NilValue{}, nil
	}))

	// check_hash_based_pycs - check the mode for hash-based .pyc files
	impModule.Set("check_hash_based_pycs", core.StringValue("default"))

	// source_hash - compute a hash of source code
	impModule.Set("source_hash", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a dummy hash for now
		// In CPython this returns a bytes object
		return core.BytesValue([]byte{0, 0, 0, 0, 0, 0, 0, 0}), nil
	}))

	return impModule
}

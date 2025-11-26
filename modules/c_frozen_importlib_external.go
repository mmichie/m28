// Package modules provides the _frozen_importlib_external C extension module stub for M28
// This is CPython's frozen external import machinery - M28 doesn't need it since we have our own import system.
// We provide minimal stubs just to satisfy import checks.
package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Init_FrozenImportlibExternalModule creates and returns the _frozen_importlib_external module stub
// This is a minimal stub that provides just enough for importlib to work.
// M28 uses its own import system, so these are no-op implementations.
func Init_FrozenImportlibExternalModule() *core.DictValue {
	frozenImportlibExternalModule := core.NewDict()

	// _install() - No-op function for import machinery setup
	// CPython uses this to install the frozen external import machinery, but M28 has its own system
	frozenImportlibExternalModule.Set("_install", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No-op: M28 uses its own import system
		return core.None, nil
	}))

	// _pack_uint32(x) - Pack a uint32 into 4 bytes (little-endian)
	// Used by CPython for .pyc file generation. M28 doesn't use .pyc files,
	// but importlib may call this, so we provide a working implementation.
	frozenImportlibExternalModule.Set("_pack_uint32", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("_pack_uint32 expected 1 argument, got %d", len(args))
		}

		// Convert argument to int
		var num int64
		switch v := args[0].(type) {
		case core.NumberValue:
			num = int64(v)
		default:
			return nil, fmt.Errorf("_pack_uint32 argument must be an integer")
		}

		// Pack as 4-byte little-endian
		bytes := make([]byte, 4)
		bytes[0] = byte(num & 0xFF)
		bytes[1] = byte((num >> 8) & 0xFF)
		bytes[2] = byte((num >> 16) & 0xFF)
		bytes[3] = byte((num >> 24) & 0xFF)

		return core.BytesValue(bytes), nil
	}))

	// _unpack_uint32(data) - Unpack 4 bytes (little-endian) into a uint32
	// Companion to _pack_uint32, used by CPython's importlib for reading .pyc files
	frozenImportlibExternalModule.Set("_unpack_uint32", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("_unpack_uint32 expected 1 argument, got %d", len(args))
		}

		// Convert argument to bytes
		var bytes []byte
		switch v := args[0].(type) {
		case core.BytesValue:
			bytes = []byte(v)
		default:
			return nil, fmt.Errorf("_unpack_uint32 argument must be bytes")
		}

		if len(bytes) < 4 {
			return nil, fmt.Errorf("_unpack_uint32 requires at least 4 bytes, got %d", len(bytes))
		}

		// Unpack as 4-byte little-endian
		num := uint32(bytes[0]) | (uint32(bytes[1]) << 8) | (uint32(bytes[2]) << 16) | (uint32(bytes[3]) << 24)

		return core.NumberValue(int64(num)), nil
	}))

	// Stub bootstrap loaders as empty classes/dicts
	// These are part of CPython's import machinery that M28 doesn't need
	// But some code might try to access them, so we provide empty stubs
	frozenImportlibExternalModule.Set("SourceFileLoader", core.NewDict())
	frozenImportlibExternalModule.Set("ExtensionFileLoader", core.NewDict())
	frozenImportlibExternalModule.Set("ModuleSpec", core.NewDict())
	frozenImportlibExternalModule.Set("FileFinder", core.NewDict())

	// __name__ - Module name attribute
	frozenImportlibExternalModule.Set("__name__", core.StringValue("_frozen_importlib_external"))

	// __doc__ - Module docstring
	frozenImportlibExternalModule.Set("__doc__", core.StringValue("Stub for CPython's frozen external import machinery. M28 uses its own import system."))

	return frozenImportlibExternalModule
}

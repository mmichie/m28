package modules

import (
	"github.com/mmichie/m28/core"
)

// Init_CtypesModule creates the _ctypes module stub
// This is a C extension module that provides FFI (Foreign Function Interface) capabilities
// We provide minimal stubs for the types and constants that Python's ctypes/__init__.py imports
func Init_CtypesModule() *core.DictValue {
	module := core.NewDict()

	// Version info
	module.Set("__version__", core.StringValue("1.1.0"))

	// RTLD constants for dynamic library loading
	module.Set("RTLD_LOCAL", core.NumberValue(0))
	module.Set("RTLD_GLOBAL", core.NumberValue(256))

	// FUNCFLAG constants
	module.Set("FUNCFLAG_CDECL", core.NumberValue(1))
	module.Set("FUNCFLAG_PYTHONAPI", core.NumberValue(4))
	module.Set("FUNCFLAG_USE_ERRNO", core.NumberValue(8))
	module.Set("FUNCFLAG_USE_LASTERROR", core.NumberValue(16))

	// SIZEOF constants
	module.Set("SIZEOF_TIME_T", core.NumberValue(8))

	// Base ctypes classes - these are base classes for C data types
	// Create simple placeholder classes for now

	// Union - base class for C unions
	unionClass := core.NewClass("Union", nil)
	unionClass.Module = "_ctypes"
	module.Set("Union", unionClass)

	// Structure - base class for C structures
	structureClass := core.NewClass("Structure", nil)
	structureClass.Module = "_ctypes"
	module.Set("Structure", structureClass)

	// Array - base class for C arrays
	arrayClass := core.NewClass("Array", nil)
	arrayClass.Module = "_ctypes"
	module.Set("Array", arrayClass)

	// _Pointer - internal pointer type
	pointerClass := core.NewClass("_Pointer", nil)
	pointerClass.Module = "_ctypes"
	module.Set("_Pointer", pointerClass)

	// CFuncPtr - base class for C function pointers
	cfuncptrClass := core.NewClass("CFuncPtr", nil)
	cfuncptrClass.Module = "_ctypes"
	module.Set("CFuncPtr", cfuncptrClass)

	// ArgumentError - exception raised for invalid arguments
	argErrorClass := core.NewClass("ArgumentError", nil)
	argErrorClass.Module = "_ctypes"
	module.Set("ArgumentError", argErrorClass)

	// FormatError - platform-specific (Windows)
	// Only available on Windows, but we'll provide it for compatibility
	formatErrorClass := core.NewClass("FormatError", nil)
	formatErrorClass.Module = "_ctypes"
	module.Set("FormatError", formatErrorClass)

	// LoadLibrary - function to load dynamic libraries (platform-specific)
	module.Set("LoadLibrary", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Minimal stub - just return None
			// Real implementation would use dlopen/LoadLibrary
			return core.None, nil
		}))

	// get_errno, set_errno - errno handling
	module.Set("get_errno", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(0), nil
		}))

	module.Set("set_errno", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.None, nil
		}))

	// get_last_error, set_last_error - Windows-specific error handling
	module.Set("get_last_error", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(0), nil
		}))

	module.Set("set_last_error", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.None, nil
		}))

	// _memmove_addr, _memset_addr, _string_at_addr, _cast_addr, _wstring_at_addr
	// These are internal C function pointers - provide dummy values
	module.Set("_memmove_addr", core.NumberValue(0))
	module.Set("_memset_addr", core.NumberValue(0))
	module.Set("_string_at_addr", core.NumberValue(0))
	module.Set("_cast_addr", core.NumberValue(0))
	module.Set("_wstring_at_addr", core.NumberValue(0))

	// addressof - get the address of a ctypes instance
	module.Set("addressof", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return a dummy address
			return core.NumberValue(0), nil
		}))

	// alignment - get the alignment requirement of a ctypes type
	module.Set("alignment", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return a reasonable default alignment
			return core.NumberValue(8), nil
		}))

	// byref - create a byref pointer to an object
	module.Set("byref", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return the object itself for now
			if len(args) > 0 {
				return args[0], nil
			}
			return core.None, nil
		}))

	// sizeof - get the size of a ctypes type or instance
	module.Set("sizeof", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return a reasonable default size
			return core.NumberValue(8), nil
		}))

	// resize - resize a ctypes object
	module.Set("resize", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.None, nil
		}))

	// pointer - create a pointer to a ctypes instance
	module.Set("pointer", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Create a simple pointer wrapper
			if len(args) > 0 {
				ptr := core.NewDict()
				ptr.Set("_obj", args[0])
				ptr.Set("contents", args[0])
				return ptr, nil
			}
			return core.None, nil
		}))

	// POINTER - create a pointer type
	module.Set("POINTER", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return the pointer class
			return pointerClass, nil
		}))

	return module
}

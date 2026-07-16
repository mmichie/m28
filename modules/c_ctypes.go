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
	module.SetStr("__version__", core.StringValue("1.1.0"))

	// _SimpleCData - base class for simple C data types (c_int, c_char, etc.)
	simpleCDataClass := core.NewClass("_SimpleCData", nil)
	simpleCDataClass.Module = "_ctypes"
	simpleCDataClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, nil
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		if len(args) > 1 {
			self.Attributes["value"] = args[1]
		} else {
			self.Attributes["value"] = core.NumberValue(0)
		}
		return core.None, nil
	}))
	// from_param - classmethod for converting Python values to C parameters
	simpleCDataClass.SetMethod("from_param", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 0 {
			return args[0], nil
		}
		return core.None, nil
	}))
	module.SetStr("_SimpleCData", simpleCDataClass)

	// PyCSimpleType - metaclass for simple C types
	pyCSimpleType := core.NewClass("PyCSimpleType", nil)
	pyCSimpleType.Module = "_ctypes"
	module.SetStr("PyCSimpleType", pyCSimpleType)

	// PyCStructType - metaclass for Structure
	pyCStructType := core.NewClass("PyCStructType", nil)
	pyCStructType.Module = "_ctypes"
	module.SetStr("PyCStructType", pyCStructType)

	// PyCArrayType - metaclass for Array
	pyCArrayType := core.NewClass("PyCArrayType", nil)
	pyCArrayType.Module = "_ctypes"
	module.SetStr("PyCArrayType", pyCArrayType)

	// PyCPointerType - metaclass for pointer types
	pyCPointerType := core.NewClass("PyCPointerType", nil)
	pyCPointerType.Module = "_ctypes"
	module.SetStr("PyCPointerType", pyCPointerType)

	// PyCFuncPtrType - metaclass for function pointer types
	pyCFuncPtrType := core.NewClass("PyCFuncPtrType", nil)
	pyCFuncPtrType.Module = "_ctypes"
	module.SetStr("PyCFuncPtrType", pyCFuncPtrType)

	// dlopen - open a shared library
	module.SetStr("dlopen", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return a dummy handle
			return core.NumberValue(1), nil
		}))

	// dlclose - close a shared library
	module.SetStr("dlclose", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(0), nil
		}))

	// dlsym - get symbol from shared library
	module.SetStr("dlsym", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return a dummy address
			return core.NumberValue(0), nil
		}))

	// RTLD constants for dynamic library loading
	module.SetStr("RTLD_LOCAL", core.NumberValue(0))
	module.SetStr("RTLD_GLOBAL", core.NumberValue(256))

	// FUNCFLAG constants
	module.SetStr("FUNCFLAG_CDECL", core.NumberValue(1))
	module.SetStr("FUNCFLAG_PYTHONAPI", core.NumberValue(4))
	module.SetStr("FUNCFLAG_USE_ERRNO", core.NumberValue(8))
	module.SetStr("FUNCFLAG_USE_LASTERROR", core.NumberValue(16))

	// SIZEOF constants
	module.SetStr("SIZEOF_TIME_T", core.NumberValue(8))

	// Base ctypes classes - these are base classes for C data types
	// Create simple placeholder classes for now

	// Union - base class for C unions
	unionClass := core.NewClass("Union", nil)
	unionClass.Module = "_ctypes"
	module.SetStr("Union", unionClass)

	// Structure - base class for C structures
	structureClass := core.NewClass("Structure", nil)
	structureClass.Module = "_ctypes"
	module.SetStr("Structure", structureClass)

	// Array - base class for C arrays
	arrayClass := core.NewClass("Array", nil)
	arrayClass.Module = "_ctypes"
	module.SetStr("Array", arrayClass)

	// _Pointer - internal pointer type
	pointerClass := core.NewClass("_Pointer", nil)
	pointerClass.Module = "_ctypes"
	module.SetStr("_Pointer", pointerClass)

	// CFuncPtr - base class for C function pointers
	cfuncptrClass := core.NewClass("CFuncPtr", nil)
	cfuncptrClass.Module = "_ctypes"
	// CFuncPtr can be called with an address or callable
	cfuncptrClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return core.None, nil
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		if len(args) > 1 {
			// Store the address or callable
			self.Attributes["_addr"] = args[1]
		}
		return core.None, nil
	}))
	// Make instances callable
	cfuncptrClass.SetMethod("__call__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub - just return None for now
		return core.None, nil
	}))
	module.SetStr("CFuncPtr", cfuncptrClass)

	// ArgumentError - exception raised for invalid arguments
	argErrorClass := core.NewClass("ArgumentError", nil)
	argErrorClass.Module = "_ctypes"
	module.SetStr("ArgumentError", argErrorClass)

	// FormatError - platform-specific (Windows)
	// Only available on Windows, but we'll provide it for compatibility
	formatErrorClass := core.NewClass("FormatError", nil)
	formatErrorClass.Module = "_ctypes"
	module.SetStr("FormatError", formatErrorClass)

	// LoadLibrary - function to load dynamic libraries (platform-specific)
	module.SetStr("LoadLibrary", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Minimal stub - just return None
			// Real implementation would use dlopen/LoadLibrary
			return core.None, nil
		}))

	// get_errno, set_errno - errno handling
	module.SetStr("get_errno", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(0), nil
		}))

	module.SetStr("set_errno", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.None, nil
		}))

	// get_last_error, set_last_error - Windows-specific error handling
	module.SetStr("get_last_error", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.NumberValue(0), nil
		}))

	module.SetStr("set_last_error", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.None, nil
		}))

	// _memmove_addr, _memset_addr, _string_at_addr, _cast_addr, _wstring_at_addr
	// These are internal C function pointers - provide dummy values
	module.SetStr("_memmove_addr", core.NumberValue(0))
	module.SetStr("_memset_addr", core.NumberValue(0))
	module.SetStr("_string_at_addr", core.NumberValue(0))
	module.SetStr("_cast_addr", core.NumberValue(0))
	module.SetStr("_wstring_at_addr", core.NumberValue(0))

	// addressof - get the address of a ctypes instance
	module.SetStr("addressof", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return a dummy address
			return core.NumberValue(0), nil
		}))

	// alignment - get the alignment requirement of a ctypes type
	module.SetStr("alignment", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return a reasonable default alignment
			return core.NumberValue(8), nil
		}))

	// byref - create a byref pointer to an object
	module.SetStr("byref", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Return the object itself for now
			if len(args) > 0 {
				return args[0], nil
			}
			return core.None, nil
		}))

	// sizeof - get the size of a ctypes type or instance
	module.SetStr("sizeof", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, &core.TypeError{Message: "sizeof() takes exactly 1 argument"}
			}

			// Get the type object (either directly or from instance's class)
			typeObj := args[0]
			if inst, ok := args[0].(*core.Instance); ok {
				typeObj = inst.Class
			}

			// Try to get _type_ attribute (struct format code)
			if obj, ok := typeObj.(interface {
				GetAttr(string) (core.Value, bool)
			}); ok {
				if typeAttr, found := obj.GetAttr("_type_"); found {
					if typeStr, ok := typeAttr.(core.StringValue); ok {
						// Use struct.calcsize to get the size
						size := ctypesSizeofFormat(string(typeStr))
						return core.NumberValue(size), nil
					}
				}
			}

			// Also check class attributes for Class types
			if cls, ok := typeObj.(*core.Class); ok {
				if typeAttr, found := cls.GetAttr("_type_"); found {
					if typeStr, ok := typeAttr.(core.StringValue); ok {
						size := ctypesSizeofFormat(string(typeStr))
						return core.NumberValue(size), nil
					}
				}
			}

			// Default size for unknown types
			return core.NumberValue(8), nil
		}))

	// resize - resize a ctypes object
	module.SetStr("resize", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.None, nil
		}))

	// pointer - create a pointer to a ctypes instance
	module.SetStr("pointer", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Create a simple pointer wrapper
			if len(args) > 0 {
				ptr := core.NewDict()
				ptr.SetStr("_obj", args[0])
				ptr.SetStr("contents", args[0])
				return ptr, nil
			}
			return core.None, nil
		}))

	// POINTER - create a pointer type
	module.SetStr("POINTER", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// Create a new pointer type class for each call
			// This allows setting attributes like from_param on individual pointer types
			ptrType := core.NewClassWithParents("LP_c_type", []*core.Class{pointerClass})
			ptrType.Module = "_ctypes"
			if len(args) > 0 {
				ptrType.SetAttr("_type_", args[0])
			}
			// from_param - classmethod for converting Python values to C parameters
			ptrType.SetMethod("from_param", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
				if len(args) > 0 {
					return args[0], nil
				}
				return core.None, nil
			}))
			return ptrType, nil
		}))

	// _pointer_type_cache - dict used to cache pointer types
	// This is imported by ctypes/__init__.py
	pointerTypeCache := core.NewDict()
	module.SetStr("_pointer_type_cache", pointerTypeCache)

	// _cast - internal cast function
	module.SetStr("_cast", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			if len(args) < 2 {
				return core.None, nil
			}
			// Return first argument for now (no-op cast)
			return args[0], nil
		}))

	// _string_at - get string at address
	module.SetStr("_string_at", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BytesValue(""), nil
		}))

	// _wstring_at - get wstring at address
	module.SetStr("_wstring_at", core.NewBuiltinFunction(
		func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.StringValue(""), nil
		}))

	return module
}

// ctypesSizeofFormat returns the size in bytes for a ctypes format character
// These match Python's struct format codes
func ctypesSizeofFormat(format string) int {
	if len(format) == 0 {
		return 0
	}

	// Handle the format character
	switch format[0] {
	case 'c', 'b', 'B': // char, signed/unsigned char
		return 1
	case 'h', 'H': // short, unsigned short
		return 2
	case 'i', 'I', 'l', 'L': // int/long, unsigned int/long (4 bytes on most platforms)
		return 4
	case 'q', 'Q': // long long, unsigned long long
		return 8
	case 'n', 'N': // ssize_t, size_t
		return 8
	case 'f': // float
		return 4
	case 'd': // double
		return 8
	case 'g': // long double (platform-dependent, 16 on many systems)
		return 16
	case 'P': // pointer
		return 8
	case 'z', 'Z': // c_char_p, c_wchar_p (pointers)
		return 8
	case 'O': // py_object (pointer)
		return 8
	case '?': // bool
		return 1
	case 'u': // wchar_t (platform-dependent)
		return 4
	default:
		return 8 // Default to pointer size
	}
}

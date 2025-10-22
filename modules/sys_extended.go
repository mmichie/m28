package modules

import (
	"github.com/mmichie/m28/core"
)

// addExtendedSysAttributes adds all the additional sys module attributes
// that aren't in the base implementation to match CPython
func addExtendedSysAttributes(sysModule *core.DictValue) {
	// Additional version/implementation info
	sysModule.Set("abiflags", core.StringValue(""))
	sysModule.Set("api_version", core.NumberValue(1013))
	sysModule.Set("hexversion", core.NumberValue(0x030c0000)) // Python 3.12
	sysModule.Set("copyright", core.StringValue("M28 Language Runtime"))
	sysModule.Set("maxunicode", core.NumberValue(0x10ffff)) // Max Unicode code point

	// Flags object (stub)
	flags := core.NewDict()
	flags.Set("debug", core.NumberValue(0))
	flags.Set("inspect", core.NumberValue(0))
	flags.Set("interactive", core.NumberValue(0))
	flags.Set("optimize", core.NumberValue(0))
	flags.Set("dont_write_bytecode", core.NumberValue(1))
	flags.Set("no_user_site", core.NumberValue(0))
	flags.Set("no_site", core.NumberValue(0))
	flags.Set("ignore_environment", core.NumberValue(0))
	flags.Set("verbose", core.NumberValue(0))
	flags.Set("bytes_warning", core.NumberValue(0))
	flags.Set("quiet", core.NumberValue(0))
	flags.Set("hash_randomization", core.NumberValue(1))
	flags.Set("isolated", core.NumberValue(0))
	flags.Set("dev_mode", core.NumberValue(0))
	flags.Set("utf8_mode", core.NumberValue(0))
	sysModule.Set("flags", flags)

	// float_info object
	floatInfo := core.NewDict()
	floatInfo.Set("max", core.NumberValue(1.7976931348623157e+308))
	floatInfo.Set("max_exp", core.NumberValue(1024))
	floatInfo.Set("max_10_exp", core.NumberValue(308))
	floatInfo.Set("min", core.NumberValue(2.2250738585072014e-308))
	floatInfo.Set("min_exp", core.NumberValue(-1021))
	floatInfo.Set("min_10_exp", core.NumberValue(-307))
	floatInfo.Set("dig", core.NumberValue(15))
	floatInfo.Set("mant_dig", core.NumberValue(53))
	floatInfo.Set("epsilon", core.NumberValue(2.220446049250313e-16))
	floatInfo.Set("radix", core.NumberValue(2))
	floatInfo.Set("rounds", core.NumberValue(1))
	sysModule.Set("float_info", floatInfo)

	// int_info object
	intInfo := core.NewDict()
	intInfo.Set("bits_per_digit", core.NumberValue(30))
	intInfo.Set("sizeof_digit", core.NumberValue(4))
	sysModule.Set("int_info", intInfo)

	// thread_info object
	threadInfo := core.NewDict()
	threadInfo.Set("name", core.StringValue("pthread"))
	threadInfo.Set("lock", core.StringValue("mutex+cond"))
	threadInfo.Set("version", core.None)
	sysModule.Set("thread_info", threadInfo)

	// Path/import related
	sysModule.Set("meta_path", core.NewList())
	sysModule.Set("path_hooks", core.NewList())
	sysModule.Set("path_importer_cache", core.NewDict())
	sysModule.Set("dont_write_bytecode", core.True)
	sysModule.Set("pycache_prefix", core.None)
	sysModule.Set("platlibdir", core.StringValue("lib"))

	// Standard library module names
	stdlibModules := core.NewList()
	sysModule.Set("stdlib_module_names", stdlibModules)

	// Original argv (before any modifications)
	sysModule.Set("orig_argv", core.NewList())

	// Monitoring (Python 3.12+)
	sysModule.Set("monitoring", core.NewDict())

	// Float repr style
	sysModule.Set("float_repr_style", core.StringValue("short"))
}

// addExtendedSysFunctions adds all the additional sys module functions
func addExtendedSysFunctions(sysModule *core.DictValue) {
	// Profiling/tracing functions (stubs)
	sysModule.Set("getprofile", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.Set("setprofile", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.Set("gettrace", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.Set("settrace", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Display/exception hooks
	sysModule.Set("displayhook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 0 && args[0] != core.None {
			// Print the value
			return core.None, nil
		}
		return core.None, nil
	}))

	sysModule.Set("unraisablehook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.Set("breakpointhook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Reference counting/memory (stubs)
	sysModule.Set("getrefcount", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(2), nil // Stub value
	}))

	sysModule.Set("getallocatedblocks", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	sysModule.Set("getunicodeinternedsize", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	// Switch interval (for threading)
	sysModule.Set("getswitchinterval", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0.005), nil
	}))

	sysModule.Set("setswitchinterval", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Async generator hooks
	sysModule.Set("get_asyncgen_hooks", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		hooks := core.NewDict()
		hooks.Set("firstiter", core.None)
		hooks.Set("finalizer", core.None)
		return hooks, nil
	}))

	sysModule.Set("set_asyncgen_hooks", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Coroutine origin tracking
	sysModule.Set("get_coroutine_origin_tracking_depth", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	sysModule.Set("set_coroutine_origin_tracking_depth", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Int max str digits (Python 3.11+)
	sysModule.Set("get_int_max_str_digits", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(4300), nil
	}))

	sysModule.Set("set_int_max_str_digits", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Dynamic library loading (Unix-specific stubs)
	sysModule.Set("getdlopenflags", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	sysModule.Set("setdlopenflags", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Call tracing
	sysModule.Set("call_tracing", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, core.NewTypeError("function", nil, "call_tracing() argument")
		}
		fn, ok := args[0].(core.Callable)
		if !ok {
			return nil, core.NewTypeError("callable", args[0], "call_tracing() first argument")
		}
		fnArgs, ok := args[1].(core.TupleValue)
		if !ok {
			return nil, core.NewTypeError("tuple", args[1], "call_tracing() second argument")
		}
		return fn.Call(fnArgs, ctx)
	}))

	// Audit hooks (Python 3.8+)
	sysModule.Set("addaudithook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.Set("audit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Finalizing check
	sysModule.Set("is_finalizing", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.False, nil
	}))

	// Stack trampoline (Python 3.12+)
	sysModule.Set("is_stack_trampoline_active", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.False, nil
	}))

	sysModule.Set("activate_stack_trampoline", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.Set("deactivate_stack_trampoline", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Exception (Python 3.11+)
	sysModule.Set("exception", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))
}

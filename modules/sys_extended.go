package modules

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// addExtendedSysAttributes adds all the additional sys module attributes
// that aren't in the base implementation to match CPython
func addExtendedSysAttributes(sysModule *core.DictValue) {
	// Additional version/implementation info
	sysModule.SetStr("abiflags", core.StringValue(""))
	sysModule.SetStr("api_version", core.NumberValue(1013))
	sysModule.SetStr("hexversion", core.NumberValue(0x030c0000)) // Python 3.12
	sysModule.SetStr("copyright", core.StringValue("M28 Language Runtime"))
	sysModule.SetStr("maxunicode", core.NumberValue(0x10ffff)) // Max Unicode code point

	// Flags object (stub)
	flags := core.NewDict()
	flags.SetStr("debug", core.NumberValue(0))
	flags.SetStr("inspect", core.NumberValue(0))
	flags.SetStr("interactive", core.NumberValue(0))
	flags.SetStr("optimize", core.NumberValue(0))
	flags.SetStr("dont_write_bytecode", core.NumberValue(1))
	flags.SetStr("no_user_site", core.NumberValue(0))
	flags.SetStr("no_site", core.NumberValue(0))
	flags.SetStr("ignore_environment", core.NumberValue(0))
	flags.SetStr("verbose", core.NumberValue(0))
	flags.SetStr("bytes_warning", core.NumberValue(0))
	flags.SetStr("quiet", core.NumberValue(0))
	flags.SetStr("hash_randomization", core.NumberValue(1))
	flags.SetStr("isolated", core.NumberValue(0))
	flags.SetStr("dev_mode", core.NumberValue(0))
	flags.SetStr("utf8_mode", core.NumberValue(0))
	sysModule.SetStr("flags", flags)

	// float_info object
	floatInfo := core.NewDict()
	floatInfo.SetStr("max", core.NumberValue(1.7976931348623157e+308))
	floatInfo.SetStr("max_exp", core.NumberValue(1024))
	floatInfo.SetStr("max_10_exp", core.NumberValue(308))
	floatInfo.SetStr("min", core.NumberValue(2.2250738585072014e-308))
	floatInfo.SetStr("min_exp", core.NumberValue(-1021))
	floatInfo.SetStr("min_10_exp", core.NumberValue(-307))
	floatInfo.SetStr("dig", core.NumberValue(15))
	floatInfo.SetStr("mant_dig", core.NumberValue(53))
	floatInfo.SetStr("epsilon", core.NumberValue(2.220446049250313e-16))
	floatInfo.SetStr("radix", core.NumberValue(2))
	floatInfo.SetStr("rounds", core.NumberValue(1))
	sysModule.SetStr("float_info", floatInfo)

	// int_info object
	intInfo := core.NewDict()
	intInfo.SetStr("bits_per_digit", core.NumberValue(30))
	intInfo.SetStr("sizeof_digit", core.NumberValue(4))
	sysModule.SetStr("int_info", intInfo)

	// thread_info object
	threadInfo := core.NewDict()
	threadInfo.SetStr("name", core.StringValue("pthread"))
	threadInfo.SetStr("lock", core.StringValue("mutex+cond"))
	threadInfo.SetStr("version", core.None)
	sysModule.SetStr("thread_info", threadInfo)

	// Path/import related
	sysModule.SetStr("meta_path", core.NewList())
	sysModule.SetStr("path_hooks", core.NewList())
	sysModule.SetStr("path_importer_cache", core.NewDict())
	sysModule.SetStr("dont_write_bytecode", core.True)
	sysModule.SetStr("pycache_prefix", core.None)
	sysModule.SetStr("platlibdir", core.StringValue("lib"))

	// Standard library module names
	stdlibModules := core.NewList()
	sysModule.SetStr("stdlib_module_names", stdlibModules)

	// Original argv (before any modifications)
	sysModule.SetStr("orig_argv", core.NewList())

	// Monitoring (Python 3.12+)
	sysModule.SetStr("monitoring", core.NewDict())

	// Float repr style
	sysModule.SetStr("float_repr_style", core.StringValue("short"))
}

// addExtendedSysFunctions adds all the additional sys module functions
func addExtendedSysFunctions(sysModule *core.DictValue) {
	// Profiling/tracing functions (stubs)
	sysModule.SetStr("getprofile", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.SetStr("setprofile", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.SetStr("gettrace", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.SetStr("settrace", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Display/exception hooks
	// displayhook is called to print the result of expressions in the REPL
	displayhookFn := core.NewNamedBuiltinFunction("displayhook", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 0 && args[0] != core.None {
			// Print the value
			return core.None, nil
		}
		return core.None, nil
	})
	sysModule.SetStr("displayhook", displayhookFn)
	// __displayhook__ is the original displayhook (sys.displayhook can be replaced, this is always the original)
	sysModule.SetStr("__displayhook__", displayhookFn)

	// __excepthook__ is the original excepthook (sys.excepthook can be replaced, this is always the original)
	// excepthook is already defined in sys.go, we need to get a reference or create a similar one
	excepthookFn := core.NewNamedBuiltinFunction("excepthook", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 3 {
			return nil, core.NewTypeError("tuple", nil, "excepthook() requires 3 arguments")
		}
		// For now, just return None (the real implementation is in sys.go)
		return core.None, nil
	})
	sysModule.SetStr("__excepthook__", excepthookFn)

	sysModule.SetStr("unraisablehook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.SetStr("breakpointhook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Reference counting/memory (stubs)
	sysModule.SetStr("getrefcount", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(2), nil // Stub value
	}))

	sysModule.SetStr("getallocatedblocks", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	sysModule.SetStr("getunicodeinternedsize", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	// Switch interval (for threading)
	sysModule.SetStr("getswitchinterval", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0.005), nil
	}))

	sysModule.SetStr("setswitchinterval", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Async generator hooks
	sysModule.SetStr("get_asyncgen_hooks", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		hooks := core.NewDict()
		hooks.SetStr("firstiter", core.None)
		hooks.SetStr("finalizer", core.None)
		return hooks, nil
	}))

	sysModule.SetStr("set_asyncgen_hooks", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Coroutine origin tracking
	sysModule.SetStr("get_coroutine_origin_tracking_depth", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	sysModule.SetStr("set_coroutine_origin_tracking_depth", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Int max str digits (Python 3.11+)
	sysModule.SetStr("get_int_max_str_digits", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(float64(core.IntMaxStrDigits)), nil
	}))

	sysModule.SetStr("set_int_max_str_digits", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, &core.TypeError{Message: fmt.Sprintf("set_int_max_str_digits() takes exactly one argument (%d given)", len(args))}
		}
		n, ok := args[0].(core.NumberValue)
		if !ok || float64(n) != float64(int(n)) {
			return nil, &core.TypeError{Message: "'int' object expected"}
		}
		limit := int(n)
		// 0 disables the limit; otherwise CPython requires limit >= 640.
		if limit != 0 && limit < 640 {
			return nil, &core.ValueError{Message: "maxdigits must be 0 or larger than 640"}
		}
		core.IntMaxStrDigits = limit
		return core.None, nil
	}))

	// Dynamic library loading (Unix-specific stubs)
	sysModule.SetStr("getdlopenflags", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	sysModule.SetStr("setdlopenflags", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Call tracing
	sysModule.SetStr("call_tracing", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
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
	sysModule.SetStr("addaudithook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.SetStr("audit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Finalizing check
	sysModule.SetStr("is_finalizing", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.False, nil
	}))

	// Stack trampoline (Python 3.12+)
	sysModule.SetStr("is_stack_trampoline_active", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.False, nil
	}))

	sysModule.SetStr("activate_stack_trampoline", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	sysModule.SetStr("deactivate_stack_trampoline", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}))

	// Exception (Python 3.11+)
	// Returns the exception instance currently being handled, or None
	sysModule.SetStr("exception", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return the current exception value if one is being handled
		if ctx.ExcValue != nil {
			return ctx.ExcValue, nil
		}
		return core.None, nil
	}))
}

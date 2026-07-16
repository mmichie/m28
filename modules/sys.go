// Package modules provides the sys module for M28
package modules

import (
	"fmt"
	"os"
	"runtime"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// Global variables for sys module state
// Note: the recursion limit lives in the core package (core.GetRecursionLimit /
// core.SetRecursionLimit) so the evaluator can enforce it; sys just delegates.
var (
	sysModules = core.NewDict() // Tracks loaded modules
	sysPath    = initSysPath()  // Module search path
)

// initSysPath initializes sys.path with reasonable defaults
// including the Python standard library if it can be found
func initSysPath() *core.ListValue {
	paths := core.NewList(core.StringValue("."))

	// Try to find Python standard library
	// Look for common Python installation locations
	pythonLibPaths := []string{
		"/usr/lib/python3.12",
		"/usr/lib/python3.11",
		"/usr/lib/python3.10",
		"/usr/local/lib/python3.12",
		"/usr/local/lib/python3.11",
		"/usr/local/lib/python3.10",
	}

	// Also check if PYENV_ROOT is set
	if pyenvRoot := os.Getenv("PYENV_ROOT"); pyenvRoot != "" {
		pythonLibPaths = append([]string{
			pyenvRoot + "/versions/3.12.0/lib/python3.12",
			pyenvRoot + "/versions/3.11.0/lib/python3.11",
			pyenvRoot + "/versions/3.10.0/lib/python3.10",
		}, pythonLibPaths...)
	}

	// Check HOME/.pyenv for pyenv installations
	if home := os.Getenv("HOME"); home != "" {
		pythonLibPaths = append([]string{
			home + "/.pyenv/versions/3.12.0/lib/python3.12",
			home + "/.pyenv/versions/3.11.0/lib/python3.11",
			home + "/.pyenv/versions/3.10.0/lib/python3.10",
		}, pythonLibPaths...)
	}

	// Add the first existing Python lib path to sys.path
	for _, libPath := range pythonLibPaths {
		if _, err := os.Stat(libPath); err == nil {
			paths.Append(core.StringValue(libPath))
			break
		}
	}

	return paths
}

// InitSysModule registers the sys module
func InitSysModule() *core.DictValue {
	sysModule := core.NewDict()

	// Create stdout and stderr as file-like objects
	stdout := createFileObject(os.Stdout)
	stderr := createFileObject(os.Stderr)
	stdin := createFileObject(os.Stdin)

	sysModule.SetStr("stdout", stdout)
	sysModule.SetStr("stderr", stderr)
	sysModule.SetStr("stdin", stdin)

	// Exception handling
	sysModule.SetStr("exc_info", core.NewBuiltinFunction(excInfo))
	sysModule.SetStr("excepthook", core.NewBuiltinFunction(excepthook))

	// Command line arguments (empty list for now, populated by main)
	sysModule.SetStr("argv", core.NewList())

	// Warning options (empty list for now)
	sysModule.SetStr("warnoptions", core.NewList())

	// Version information
	sysModule.SetStr("version", core.StringValue("M28 0.1.0"))
	// version_info should be a tuple that supports both subscripting and attribute access
	// For now, use Python 3.12 version to match the stdlib we're using
	// Format: (major, minor, micro, releaselevel, serial)
	versionInfo := core.TupleValue{
		core.NumberValue(3),       // major - use 3.12 to match CPython stdlib
		core.NumberValue(12),      // minor
		core.NumberValue(0),       // micro
		core.StringValue("final"), // releaselevel
		core.NumberValue(0),       // serial
	}
	sysModule.SetStr("version_info", versionInfo)

	// Implementation info
	implDict := core.NewDict()
	implDict.SetStr("name", core.StringValue("m28"))
	implDict.SetStr("version", core.StringValue("0.1.0"))
	implDict.SetStr("cache_tag", core.StringValue("m28"))
	sysModule.SetStr("implementation", implDict)

	// Platform information
	sysModule.SetStr("platform", core.StringValue(runtime.GOOS))
	execPath, _ := os.Executable()
	sysModule.SetStr("executable", core.StringValue(execPath))
	sysModule.SetStr("prefix", core.StringValue("/usr/local"))
	sysModule.SetStr("exec_prefix", core.StringValue("/usr/local"))
	sysModule.SetStr("base_prefix", core.StringValue("/usr/local"))
	sysModule.SetStr("base_exec_prefix", core.StringValue("/usr/local"))

	// Framework info (for macOS Python framework builds)
	// Set to False since M28 is not a framework build
	sysModule.SetStr("_framework", core.BoolValue(false))

	// System constants. maxsize is 2**63-1 on 64-bit, which exceeds float64's
	// exact-integer range, so it must be a BigInt -- NumberValue (float64) would
	// round it to 9223372036854775808 and lose the exact value.
	sysModule.SetStr("maxsize", core.NewBigIntFromInt64(int64(^uint(0)>>1)))
	if runtime.GOARCH == "386" || runtime.GOARCH == "arm" {
		sysModule.SetStr("byteorder", core.StringValue("little"))
	} else if runtime.GOARCH == "amd64" || runtime.GOARCH == "arm64" {
		sysModule.SetStr("byteorder", core.StringValue("little"))
	} else {
		sysModule.SetStr("byteorder", core.StringValue("little"))
	}

	// hash_info - information about the hash implementation
	hashInfo := core.NewDict()
	hashInfo.SetStr("width", core.NumberValue(64))
	hashInfo.SetStr("modulus", core.NumberValue(2305843009213693951))
	hashInfo.SetStr("inf", core.NumberValue(314159))
	hashInfo.SetStr("nan", core.NumberValue(0))
	hashInfo.SetStr("imag", core.NumberValue(1000003))
	hashInfo.SetStr("algorithm", core.StringValue("siphash13"))
	hashInfo.SetStr("hash_bits", core.NumberValue(64))
	hashInfo.SetStr("seed_bits", core.NumberValue(128))
	hashInfo.SetStr("cutoff", core.NumberValue(0))
	sysModule.SetStr("hash_info", hashInfo)

	// Module tracking
	sysModule.SetStr("modules", sysModules)
	sysModule.SetStr("path", sysPath)

	// Builtin module names - static list matching registry.go
	builtinNamesList := core.NewList(
		core.StringValue("posix"), // C extension - used by Python's os.py
		core.StringValue("sys"),
		core.StringValue("io"),
		core.StringValue("json"),
		core.StringValue("time"),
		core.StringValue("datetime"),
		core.StringValue("pathlib"),
		core.StringValue("random"),
		core.StringValue("shutil"),
		core.StringValue("math"),
		core.StringValue("_collections"),
		core.StringValue("_weakref"),
		core.StringValue("_thread"),
		core.StringValue("itertools"),
		core.StringValue("_functools"),
		core.StringValue("operator"),
		core.StringValue("copy"),
		core.StringValue("heapq"),
		core.StringValue("traceback"),
		core.StringValue("unittest.util"),
		core.StringValue("types"),
		core.StringValue("re"),
		core.StringValue("dataclasses"),
		core.StringValue("warnings"),
		core.StringValue("_signal"),
		core.StringValue("sysconfig"),
	)
	sysModule.SetStr("builtin_module_names", builtinNamesList)

	// Functions
	sysModule.SetStr("exit", core.NewBuiltinFunction(exitFunc))
	sysModule.SetStr("getrecursionlimit", core.NewBuiltinFunction(getRecursionLimit))
	sysModule.SetStr("setrecursionlimit", core.NewBuiltinFunction(setRecursionLimit))
	sysModule.SetStr("getdefaultencoding", core.NewBuiltinFunction(getDefaultEncoding))
	sysModule.SetStr("getfilesystemencoding", core.NewBuiltinFunction(getFilesystemEncoding))
	sysModule.SetStr("getfilesystemencodeerrors", core.NewBuiltinFunction(getFilesystemEncodeErrors))
	sysModule.SetStr("getsizeof", core.NewBuiltinFunction(getSizeOf))

	// intern function - intern strings for memory optimization
	// In M28, we just return the same string since we don't have string interning yet
	sysModule.SetStr("intern", core.NewNamedBuiltinFunction("intern", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("string", nil, "intern() argument")
		}
		str, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("string", args[0], "intern() argument")
		}
		// String interning: just return the string as-is (Go handles string interning)
		// Python's sys.intern() is mainly for memory optimization
		return str, nil
	}))

	// _getframe function - get frame object at given depth
	// Used by traceback and other stdlib modules for introspection
	// Returns a minimal frame object to avoid errors in stdlib code
	sysModule.SetStr("_getframe", core.NewNamedBuiltinFunction("_getframe", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Takes optional depth argument (defaults to 0)
		// Returns a frame object with minimal attributes

		// Get the current context's globals (if available)
		var globals *core.DictValue
		if ctx != nil && ctx.ModuleDict != nil {
			// Use the module's dict as globals
			globals = ctx.ModuleDict
		} else if ctx != nil && ctx.Global != nil && ctx.Global.Vars != nil {
			// Create a dict from the global context vars
			globals = core.NewDict()
			for k, v := range ctx.Global.Vars {
				globals.SetStr(k, v)
			}
			// Ensure __name__ exists
			if _, ok := globals.GetStr("__name__"); !ok {
				globals.SetStr("__name__", core.StringValue("__main__"))
			}
		} else {
			globals = core.NewDict()
			globals.SetStr("__name__", core.StringValue("__main__"))
		}

		// Create a code object with necessary attributes
		codeObj := core.NewDict()
		codeObj.SetStr("co_filename", core.StringValue("<string>"))
		codeObj.SetStr("co_name", core.StringValue("<module>"))
		codeObj.SetStr("co_firstlineno", core.NumberValue(1))

		// Create a minimal frame object as a dict
		frame := core.NewDict()

		// Add required frame attributes that traceback and warnings expect
		frame.SetStr("f_code", codeObj)               // code object with co_filename
		frame.SetStr("f_lineno", core.NumberValue(1)) // line number
		frame.SetStr("f_locals", core.NewDict())      // local vars
		frame.SetStr("f_globals", globals)            // global vars with __name__
		frame.SetStr("f_back", core.None)             // previous frame (None = top of stack)

		// Add tb_frame attribute that some code expects
		frame.SetStr("tb_frame", frame)

		return frame, nil
	}))

	// _getframemodulename function - get module name at given frame depth
	// Used by difflib and other stdlib modules for introspection
	sysModule.SetStr("_getframemodulename", core.NewNamedBuiltinFunction("_getframemodulename", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Takes one argument: depth (int)
		// Returns module name (str) or None
		// For now, always return "__main__" since we don't track frames yet
		if len(args) < 1 {
			return nil, core.NewTypeError("int", nil, "_getframemodulename() argument")
		}
		// Return None to indicate frame not found or use __main__
		return core.StringValue("__main__"), nil
	}))

	// Add all extended sys module attributes and functions for full CPython compatibility
	addExtendedSysAttributes(sysModule)
	addExtendedSysFunctions(sysModule)

	return sysModule
}

// createFileObject creates a file-like object that wraps an os.File
func createFileObject(f *os.File) *core.DictValue {
	obj := core.NewDict()

	// Store the underlying file
	fileWrapper := &fileObject{file: f}

	obj.SetStr("write", core.NewBuiltinFunction(fileWrapper.write))
	obj.SetStr("flush", core.NewBuiltinFunction(fileWrapper.flush))
	obj.SetStr("getvalue", core.NewBuiltinFunction(fileWrapper.getvalue))

	// Add encoding attribute (Python stdout/stderr have this)
	obj.SetStr("encoding", core.StringValue("utf-8"))

	// Add other standard file attributes
	obj.SetStr("errors", core.StringValue("strict"))
	obj.SetStr("newlines", core.None)
	obj.SetStr("buffer", core.None)
	obj.SetStr("line_buffering", core.BoolValue(true))

	return obj
}

type fileObject struct {
	file *os.File
}

func (fo *fileObject) write(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("write", args)
	if err := v.Min(1); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	n, writeErr := fo.file.WriteString(str)
	if writeErr != nil {
		return nil, writeErr
	}

	return core.NumberValue(n), nil
}

func (fo *fileObject) flush(args []core.Value, ctx *core.Context) (core.Value, error) {
	// No-op for now, as Go handles buffering automatically
	return core.NilValue{}, nil
}

func (fo *fileObject) getvalue(args []core.Value, ctx *core.Context) (core.Value, error) {
	// This is called when sys.stdout/stderr is replaced with StringIO
	// Return empty string for real files
	return core.StringValue(""), nil
}

// excInfo returns information about the current exception
// Returns a tuple (type, value, traceback) or (None, None, None) if no exception
func excInfo(args []core.Value, ctx *core.Context) (core.Value, error) {
	// Walk up the context chain to find exception info
	// This is needed because exception handlers may create nested contexts
	for c := ctx; c != nil; c = c.Outer {
		if c.ExcType != nil && c.ExcValue != nil {
			excType := c.ExcType
			excValue := c.ExcValue
			excTb := c.ExcTb
			if excTb == nil {
				excTb = core.None
			}
			return core.TupleValue{excType, excValue, excTb}, nil
		}
	}

	// No exception - return (None, None, None)
	return core.TupleValue{core.None, core.None, core.None}, nil
}

// excepthook is the default exception hook
func excepthook(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("excepthook", args)
	if err := v.Exact(3); err != nil {
		return nil, err
	}

	// args: type, value, traceback
	// For now, just print the exception
	excType := args[0]
	excValue := args[1]

	fmt.Fprintf(os.Stderr, "%s: %s\n", excType, excValue)
	return core.NilValue{}, nil
}

// exitFunc exits the program by raising SystemExit
func exitFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("exit", args)

	code := 0
	if len(args) > 0 {
		numVal, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}
		code = int(numVal)
	}

	// Raise SystemExit instead of calling os.Exit
	// This allows the exception to be caught and handled in Python code
	return nil, core.NewSystemExit(code)
}

// getRecursionLimit returns the current recursion limit
func getRecursionLimit(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.NumberValue(int64(core.GetRecursionLimit())), nil
}

// setRecursionLimit sets the recursion limit
func setRecursionLimit(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("setrecursionlimit", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	limit, err := v.GetNumber(0)
	if err != nil {
		return nil, err
	}

	if limit < 1 {
		return nil, fmt.Errorf("recursion limit must be at least 1")
	}

	core.SetRecursionLimit(int(limit))
	return core.NilValue{}, nil
}

// getDefaultEncoding returns the default string encoding
func getDefaultEncoding(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.StringValue("utf-8"), nil
}

// getFilesystemEncoding returns the filesystem encoding
func getFilesystemEncoding(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.StringValue("utf-8"), nil
}

// getFilesystemEncodeErrors returns the filesystem encoding error handling strategy
func getFilesystemEncodeErrors(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.StringValue("surrogateescape"), nil
}

// getSizeOf returns the size of an object in bytes (approximate)
func getSizeOf(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("getsizeof", args)
	if err := v.Min(1); err != nil {
		return nil, err
	}

	obj := args[0]

	// Approximate size calculation
	var size int64 = 16 // Base object overhead

	switch val := obj.(type) {
	case core.StringValue:
		size += int64(len(val))
	case core.NumberValue:
		size += 8
	case core.BoolValue:
		size += 1
	case *core.ListValue:
		size += int64(val.Len() * 8)
	case *core.DictValue:
		size += int64(val.Size() * 16)
	case core.TupleValue:
		size += int64(len(val) * 8)
	default:
		size = 64 // Default size for unknown types
	}

	return core.NumberValue(size), nil
}

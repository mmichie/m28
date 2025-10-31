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
var (
	recursionLimit = 1000
	sysModules     = core.NewDict() // Tracks loaded modules
	sysPath        = initSysPath()  // Module search path
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

	sysModule.SetWithKey("stdout", core.StringValue("stdout"), stdout)
	sysModule.SetWithKey("stderr", core.StringValue("stderr"), stderr)
	sysModule.SetWithKey("stdin", core.StringValue("stdin"), stdin)

	// Exception handling
	sysModule.SetWithKey("exc_info", core.StringValue("exc_info"), core.NewBuiltinFunction(excInfo))
	sysModule.SetWithKey("excepthook", core.StringValue("excepthook"), core.NewBuiltinFunction(excepthook))

	// Command line arguments (empty list for now, populated by main)
	sysModule.SetWithKey("argv", core.StringValue("argv"), core.NewList())

	// Warning options (empty list for now)
	sysModule.SetWithKey("warnoptions", core.StringValue("warnoptions"), core.NewList())

	// Version information
	sysModule.SetWithKey("version", core.StringValue("version"), core.StringValue("M28 0.1.0"))
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
	sysModule.SetWithKey("version_info", core.StringValue("version_info"), versionInfo)

	// Implementation info
	implDict := core.NewDict()
	implDict.Set("name", core.StringValue("m28"))
	implDict.Set("version", core.StringValue("0.1.0"))
	implDict.Set("cache_tag", core.StringValue("m28"))
	sysModule.SetWithKey("implementation", core.StringValue("implementation"), implDict)

	// Platform information
	sysModule.SetWithKey("platform", core.StringValue("platform"), core.StringValue(runtime.GOOS))
	execPath, _ := os.Executable()
	sysModule.SetWithKey("executable", core.StringValue("executable"), core.StringValue(execPath))
	sysModule.SetWithKey("prefix", core.StringValue("prefix"), core.StringValue("/usr/local"))
	sysModule.SetWithKey("exec_prefix", core.StringValue("exec_prefix"), core.StringValue("/usr/local"))
	sysModule.SetWithKey("base_prefix", core.StringValue("base_prefix"), core.StringValue("/usr/local"))
	sysModule.SetWithKey("base_exec_prefix", core.StringValue("base_exec_prefix"), core.StringValue("/usr/local"))

	// Framework info (for macOS Python framework builds)
	// Set to False since M28 is not a framework build
	sysModule.SetWithKey("_framework", core.StringValue("_framework"), core.BoolValue(false))

	// System constants
	sysModule.SetWithKey("maxsize", core.StringValue("maxsize"), core.NumberValue(int64(^uint(0)>>1)))
	if runtime.GOARCH == "386" || runtime.GOARCH == "arm" {
		sysModule.SetWithKey("byteorder", core.StringValue("byteorder"), core.StringValue("little"))
	} else if runtime.GOARCH == "amd64" || runtime.GOARCH == "arm64" {
		sysModule.SetWithKey("byteorder", core.StringValue("byteorder"), core.StringValue("little"))
	} else {
		sysModule.SetWithKey("byteorder", core.StringValue("byteorder"), core.StringValue("little"))
	}

	// hash_info - information about the hash implementation
	hashInfo := core.NewDict()
	hashInfo.Set("width", core.NumberValue(64))
	hashInfo.Set("modulus", core.NumberValue(2305843009213693951))
	hashInfo.Set("inf", core.NumberValue(314159))
	hashInfo.Set("nan", core.NumberValue(0))
	hashInfo.Set("imag", core.NumberValue(1000003))
	hashInfo.Set("algorithm", core.StringValue("siphash13"))
	hashInfo.Set("hash_bits", core.NumberValue(64))
	hashInfo.Set("seed_bits", core.NumberValue(128))
	hashInfo.Set("cutoff", core.NumberValue(0))
	sysModule.SetWithKey("hash_info", core.StringValue("hash_info"), hashInfo)

	// Module tracking
	sysModule.SetWithKey("modules", core.StringValue("modules"), sysModules)
	sysModule.SetWithKey("path", core.StringValue("path"), sysPath)

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
	sysModule.SetWithKey("builtin_module_names", core.StringValue("builtin_module_names"), builtinNamesList)

	// Functions
	sysModule.SetWithKey("exit", core.StringValue("exit"), core.NewBuiltinFunction(exitFunc))
	sysModule.SetWithKey("getrecursionlimit", core.StringValue("getrecursionlimit"), core.NewBuiltinFunction(getRecursionLimit))
	sysModule.SetWithKey("setrecursionlimit", core.StringValue("setrecursionlimit"), core.NewBuiltinFunction(setRecursionLimit))
	sysModule.SetWithKey("getdefaultencoding", core.StringValue("getdefaultencoding"), core.NewBuiltinFunction(getDefaultEncoding))
	sysModule.SetWithKey("getfilesystemencoding", core.StringValue("getfilesystemencoding"), core.NewBuiltinFunction(getFilesystemEncoding))
	sysModule.SetWithKey("getfilesystemencodeerrors", core.StringValue("getfilesystemencodeerrors"), core.NewBuiltinFunction(getFilesystemEncodeErrors))
	sysModule.SetWithKey("getsizeof", core.StringValue("getsizeof"), core.NewBuiltinFunction(getSizeOf))

	// intern function - intern strings for memory optimization
	// In M28, we just return the same string since we don't have string interning yet
	sysModule.SetWithKey("intern", core.StringValue("intern"), core.NewNamedBuiltinFunction("intern", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("string", nil, "intern() argument")
		}
		str, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("string", args[0], "intern() argument")
		}
		// TODO: Implement actual string interning
		// For now, just return the string as-is
		return str, nil
	}))

	// _getframemodulename function - get module name at given frame depth
	// Used by difflib and other stdlib modules for introspection
	sysModule.SetWithKey("_getframemodulename", core.StringValue("_getframemodulename"), core.NewNamedBuiltinFunction("_getframemodulename", func(args []core.Value, ctx *core.Context) (core.Value, error) {
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

	obj.SetWithKey("write", core.StringValue("write"), core.NewBuiltinFunction(fileWrapper.write))
	obj.SetWithKey("flush", core.StringValue("flush"), core.NewBuiltinFunction(fileWrapper.flush))
	obj.SetWithKey("getvalue", core.StringValue("getvalue"), core.NewBuiltinFunction(fileWrapper.getvalue))

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
	// Check if there's a current exception in the context
	// For now, return (None, None, None) - we'll enhance this when needed
	noneTuple := core.TupleValue{core.None, core.None, core.None}
	return noneTuple, nil
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

// exitFunc exits the program
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

	os.Exit(code)
	return core.NilValue{}, nil
}

// getRecursionLimit returns the current recursion limit
func getRecursionLimit(args []core.Value, ctx *core.Context) (core.Value, error) {
	return core.NumberValue(int64(recursionLimit)), nil
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

	recursionLimit = int(limit)
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

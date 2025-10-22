package modules

import (
	"fmt"
	"os"

	"github.com/mmichie/m28/core"
)

// InitPosixModule creates the posix C extension module stub
// This is used by Python's os.py to get platform-specific functions
func InitPosixModule() *core.DictValue {
	posixModule := core.NewDict()

	// Re-use the os module functions we already have
	osModule := InitOSModule()

	// Map os module functions to posix module
	// These are the core POSIX functions that Python's os.py expects
	copyFunction := func(name string) {
		if val, found := osModule.Get(name); found {
			posixModule.SetWithKey(name, core.StringValue(name), val)
		}
	}

	// Core file/directory operations
	copyFunction("getcwd")
	copyFunction("chdir")
	copyFunction("listdir")
	copyFunction("mkdir")
	copyFunction("makedirs")
	copyFunction("remove")
	copyFunction("rename")

	// Environment
	copyFunction("getenv")
	copyFunction("environ")

	// File tests
	copyFunction("isfile")
	copyFunction("isdir")

	// Add stat function (simplified version - returns a minimal stat result)
	posixModule.SetWithKey("stat", core.StringValue("stat"), core.NewBuiltinFunction(posixStat))
	posixModule.SetWithKey("lstat", core.StringValue("lstat"), core.NewBuiltinFunction(posixStat)) // lstat same as stat for now

	// Add scandir - returns iterator of directory entries
	posixModule.SetWithKey("scandir", core.StringValue("scandir"), core.NewBuiltinFunction(posixScandir))

	// Add filesystem encoding functions
	posixModule.SetWithKey("getfilesystemencoding", core.StringValue("getfilesystemencoding"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.StringValue("utf-8"), nil
	}))
	posixModule.SetWithKey("getfilesystemencodeerrors", core.StringValue("getfilesystemencodeerrors"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.StringValue("surrogateescape"), nil
	}))

	// Add fspath - convert path-like object to string
	posixModule.SetWithKey("fspath", core.StringValue("fspath"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("fspath() takes exactly one argument")
		}
		// For now, just convert to string if it's already a string, otherwise error
		if str, ok := args[0].(core.StringValue); ok {
			return str, nil
		}
		// Try to call __fspath__ method if it exists
		// For now, just return a simple error
		return nil, fmt.Errorf("expected str, bytes or os.PathLike object, not %s", args[0].Type())
	}))

	// Add required constants
	posixModule.SetWithKey("name", core.StringValue("name"), core.StringValue("posix"))

	// Add feature support sets that os.py expects
	// These indicate which functions support optional parameters
	emptySet := core.NewSet()
	posixModule.SetWithKey("supports_dir_fd", core.StringValue("supports_dir_fd"), emptySet)
	posixModule.SetWithKey("supports_effective_ids", core.StringValue("supports_effective_ids"), emptySet)
	posixModule.SetWithKey("supports_fd", core.StringValue("supports_fd"), emptySet)
	posixModule.SetWithKey("supports_follow_symlinks", core.StringValue("supports_follow_symlinks"), emptySet)

	// Add __all__ list with exported function names
	exportsList := core.NewList(
		core.StringValue("getcwd"),
		core.StringValue("chdir"),
		core.StringValue("listdir"),
		core.StringValue("mkdir"),
		core.StringValue("makedirs"),
		core.StringValue("remove"),
		core.StringValue("rename"),
		core.StringValue("getenv"),
		core.StringValue("environ"),
		core.StringValue("stat"),
		core.StringValue("lstat"),
	)
	posixModule.SetWithKey("__all__", core.StringValue("__all__"), exportsList)

	return posixModule
}

// posixStat implements a simplified stat function
// Returns a stat_result-like object with basic file information
func posixStat(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("stat() missing required argument: 'path'")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("stat() path must be a string")
	}

	info, err := os.Stat(string(path))
	if err != nil {
		return nil, fmt.Errorf("FileNotFoundError: %v", err)
	}

	// Create a stat_result object (as a dict for now)
	statResult := core.NewDict()
	statResult.SetWithKey("st_size", core.StringValue("st_size"), core.NumberValue(float64(info.Size())))
	statResult.SetWithKey("st_mtime", core.StringValue("st_mtime"), core.NumberValue(float64(info.ModTime().Unix())))
	statResult.SetWithKey("st_mode", core.StringValue("st_mode"), core.NumberValue(float64(info.Mode())))

	// Add is_dir and is_file methods
	isDir := info.IsDir()
	statResult.SetWithKey("st_isdir", core.StringValue("st_isdir"), core.BoolValue(isDir))

	return statResult, nil
}

// posixScandir implements a simplified scandir function
// Returns an iterator of DirEntry objects for a directory
func posixScandir(args []core.Value, ctx *core.Context) (core.Value, error) {
	path := "."
	if len(args) > 0 {
		if pathStr, ok := args[0].(core.StringValue); ok {
			path = string(pathStr)
		} else {
			return nil, fmt.Errorf("scandir() path must be a string")
		}
	}

	entries, err := os.ReadDir(path)
	if err != nil {
		return nil, fmt.Errorf("FileNotFoundError: %v", err)
	}

	// Create a list of DirEntry-like objects
	result := core.NewList()
	for _, entry := range entries {
		dirEntry := core.NewDict()
		dirEntry.SetWithKey("name", core.StringValue("name"), core.StringValue(entry.Name()))
		dirEntry.SetWithKey("is_dir", core.StringValue("is_dir"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(entry.IsDir()), nil
		}))
		dirEntry.SetWithKey("is_file", core.StringValue("is_file"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(!entry.IsDir()), nil
		}))
		result.Append(dirEntry)
	}

	return result, nil
}

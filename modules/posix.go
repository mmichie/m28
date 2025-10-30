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

	// System info
	copyFunction("uname")
	copyFunction("getpid")
	copyFunction("getppid")
	copyFunction("getuid")
	copyFunction("geteuid")
	copyFunction("getgid")
	copyFunction("getegid")
	copyFunction("getgroups")
	copyFunction("getlogin")

	// File operations
	copyFunction("open")
	copyFunction("close")
	copyFunction("read")
	copyFunction("write")
	copyFunction("lseek")
	copyFunction("fstat")

	// More file operations
	copyFunction("unlink")
	copyFunction("rmdir")
	copyFunction("symlink")
	copyFunction("readlink")

	// System
	copyFunction("urandom")
	copyFunction("cpu_count")
	copyFunction("system")

	// Path operations
	copyFunction("fspath")
	copyFunction("fsencode")
	copyFunction("fsdecode")

	// Environment manipulation
	copyFunction("putenv")
	copyFunction("unsetenv")

	// Constants
	copyFunction("F_OK")
	copyFunction("R_OK")
	copyFunction("W_OK")
	copyFunction("X_OK")
	copyFunction("O_RDONLY")
	copyFunction("O_WRONLY")
	copyFunction("O_RDWR")
	copyFunction("O_APPEND")
	copyFunction("O_CREATE")
	copyFunction("O_EXCL")
	copyFunction("O_TRUNC")
	copyFunction("sep")
	copyFunction("pathsep")
	copyFunction("linesep")
	copyFunction("devnull")

	// Add stat function with keyword argument support
	posixModule.SetWithKey("stat", core.StringValue("stat"), &KwargsPosixStat{followSymlinks: true})
	posixModule.SetWithKey("lstat", core.StringValue("lstat"), &KwargsPosixStat{followSymlinks: false})

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

	// Add stat_result class
	// This is used by shutil to check for platform-specific attributes
	statResultClass := core.NewClass("stat_result", nil)
	posixModule.SetWithKey("stat_result", core.StringValue("stat_result"), statResultClass)

	// Add missing functions/attributes that Python expects

	// _exit(status) - exit without cleanup
	posixModule.SetWithKey("_exit", core.StringValue("_exit"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		status := 0
		if len(args) > 0 {
			if num, ok := args[0].(core.NumberValue); ok {
				status = int(num)
			}
		}
		os.Exit(status)
		return core.NilValue{}, nil
	}))

	// _path_normpath(path) - normalize a pathname
	posixModule.SetWithKey("_path_normpath", core.StringValue("_path_normpath"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("_path_normpath() takes exactly 1 argument")
		}
		pathStr, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("_path_normpath() argument must be a string")
		}
		// Simple normalization - in a real implementation this would handle .., ., etc.
		// For now just return the path as-is
		return pathStr, nil
	}))

	// _have_functions - set of supported optional functions (as a frozenset-like object)
	haveFuncs := core.NewList(
		core.StringValue("HAVE_FACCESSAT"),
		core.StringValue("HAVE_FCHMODAT"),
		core.StringValue("HAVE_FCHOWNAT"),
		core.StringValue("HAVE_FSTATAT"),
		core.StringValue("HAVE_FUTIMESAT"),
		core.StringValue("HAVE_LINKAT"),
		core.StringValue("HAVE_MKDIRAT"),
		core.StringValue("HAVE_MKFIFOAT"),
		core.StringValue("HAVE_MKNODAT"),
		core.StringValue("HAVE_OPENAT"),
		core.StringValue("HAVE_READLINKAT"),
		core.StringValue("HAVE_RENAMEAT"),
		core.StringValue("HAVE_SYMLINKAT"),
		core.StringValue("HAVE_UNLINKAT"),
		core.StringValue("HAVE_UTIMENSAT"),
	)
	posixModule.SetWithKey("_have_functions", core.StringValue("_have_functions"), haveFuncs)

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
		core.StringValue("stat_result"),
		core.StringValue("_exit"),
		core.StringValue("_path_normpath"),
		core.StringValue("_have_functions"),
	)
	posixModule.SetWithKey("__all__", core.StringValue("__all__"), exportsList)

	return posixModule
}

// KwargsPosixStat implements posix.stat with follow_symlinks keyword argument support
type KwargsPosixStat struct {
	core.BaseObject
	followSymlinks bool // Default behavior for this instance (true for stat, false for lstat)
}

func (f *KwargsPosixStat) Type() core.Type {
	return core.FunctionType
}

func (f *KwargsPosixStat) String() string {
	if f.followSymlinks {
		return "<built-in function stat>"
	}
	return "<built-in function lstat>"
}

// Call implements regular Call interface
func (f *KwargsPosixStat) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	return f.CallWithKeywords(args, nil, ctx)
}

// CallWithKeywords implements keyword argument support
func (f *KwargsPosixStat) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("stat() missing required argument: 'path'")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		// Try to convert to string if it's not already
		pathStr, ok := args[0].(fmt.Stringer)
		if ok {
			path = core.StringValue(pathStr.String())
		} else {
			fmt.Printf("[DEBUG KwargsPosixStat] args[0] type: %T\n", args[0])
			fmt.Printf("[DEBUG KwargsPosixStat] args[0] value: %v\n", args[0])
			return nil, fmt.Errorf("stat() path must be a string, got %T", args[0])
		}
	}

	// Check follow_symlinks keyword argument (default from instance)
	followSymlinks := f.followSymlinks
	if val, ok := kwargs["follow_symlinks"]; ok {
		if boolVal, ok := val.(core.BoolValue); ok {
			followSymlinks = bool(boolVal)
		}
	}

	// Use os.Lstat if follow_symlinks=false, otherwise os.Stat
	var info os.FileInfo
	var err error
	if followSymlinks {
		info, err = os.Stat(string(path))
	} else {
		info, err = os.Lstat(string(path))
	}
	if err != nil {
		return nil, fmt.Errorf("FileNotFoundError: %v", err)
	}

	// Create a stat_result object (as a dict for now)
	statResult := core.NewDict()
	statResult.SetWithKey("st_size", core.StringValue("st_size"), core.NumberValue(float64(info.Size())))
	statResult.SetWithKey("st_mtime", core.StringValue("st_mtime"), core.NumberValue(float64(info.ModTime().Unix())))
	statResult.SetWithKey("st_mode", core.StringValue("st_mode"), core.NumberValue(float64(info.Mode())))

	// Add is_dir method
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

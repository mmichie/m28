package modules

import (
	"fmt"
	"os"
	"path/filepath"

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
	copyFunction("O_CREAT")
	copyFunction("O_EXCL")
	copyFunction("O_TRUNC")
	copyFunction("O_SYNC")
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

	// Add fspath - convert path-like object to string or bytes
	posixModule.SetWithKey("fspath", core.StringValue("fspath"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("fspath() takes exactly one argument")
		}
		// Accept str or bytes unchanged (Python fspath behavior)
		switch v := args[0].(type) {
		case core.StringValue:
			return v, nil
		case core.BytesValue:
			return v, nil
		}
		// Try to call __fspath__ method if it exists for os.PathLike objects
		if obj, ok := args[0].(core.Object); ok {
			if fspathMethod, ok := obj.GetAttr("__fspath__"); ok {
				if callable, ok := fspathMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					return callable.Call([]core.Value{}, ctx)
				}
			}
		}
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

	// Add terminal_size class for argparse HelpFormatter
	terminalSizeClass := core.NewClass("terminal_size", nil)
	terminalSizeClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, fmt.Errorf("terminal_size() missing required argument")
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return nil, fmt.Errorf("terminal_size.__init__ requires instance as first argument")
		}

		// args[1] should be a tuple (columns, lines)
		var columns, lines core.Value
		switch v := args[1].(type) {
		case core.TupleValue:
			if len(v) >= 2 {
				columns = v[0]
				lines = v[1]
			} else {
				return nil, fmt.Errorf("terminal_size() argument must be a sequence of length 2")
			}
		case *core.ListValue:
			if v.Len() >= 2 {
				columns = v.Items()[0]
				lines = v.Items()[1]
			} else {
				return nil, fmt.Errorf("terminal_size() argument must be a sequence of length 2")
			}
		default:
			return nil, fmt.Errorf("terminal_size() argument must be a sequence")
		}

		self.Attributes["columns"] = columns
		self.Attributes["lines"] = lines
		return core.None, nil
	}))
	posixModule.SetWithKey("terminal_size", core.StringValue("terminal_size"), terminalSizeClass)

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

	// waitpid(pid, options) - wait for child process to complete (stub)
	// Used by subprocess module
	posixModule.SetWithKey("waitpid", core.StringValue("waitpid"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return stub (pid, status) tuple
		// status = 0 means successful completion
		return core.TupleValue{core.NumberValue(0), core.NumberValue(0)}, nil
	}))

	// waitstatus_to_exitcode(status) - convert wait status to exit code (stub)
	posixModule.SetWithKey("waitstatus_to_exitcode", core.StringValue("waitstatus_to_exitcode"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return 0 (success) as stub
		return core.NumberValue(0), nil
	}))

	// WIFSTOPPED(status) - check if process was stopped (stub constant function)
	posixModule.SetWithKey("WIFSTOPPED", core.StringValue("WIFSTOPPED"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.False, nil
	}))

	// WSTOPSIG(status) - get stop signal (stub constant function)
	posixModule.SetWithKey("WSTOPSIG", core.StringValue("WSTOPSIG"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil
	}))

	// WNOHANG constant - for non-blocking waitpid
	posixModule.SetWithKey("WNOHANG", core.StringValue("WNOHANG"), core.NumberValue(1))

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
		core.StringValue("fspath"),
		core.StringValue("fsencode"),
		core.StringValue("fsdecode"),
		core.StringValue("waitpid"),
		core.StringValue("waitstatus_to_exitcode"),
		core.StringValue("WIFSTOPPED"),
		core.StringValue("WSTOPSIG"),
		core.StringValue("WNOHANG"),
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
		// Return proper error types for CPython compatibility
		if os.IsNotExist(err) {
			return nil, core.NewFileNotFoundError(err.Error(), string(path))
		}
		return nil, core.NewOSError(err.Error(), string(path))
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

// DirEntry represents a directory entry from os.scandir
// Supports attribute access for name, path, and methods is_dir(), is_file(), is_symlink()
type DirEntry struct {
	core.BaseObject
	name      string
	path      string
	isDir     bool
	isSymlink bool
}

// Type returns the type name
func (d *DirEntry) Type() core.Type {
	return core.Type("DirEntry")
}

// String returns a string representation
func (d *DirEntry) String() string {
	return fmt.Sprintf("<DirEntry '%s'>", d.name)
}

// GetAttr implements Object interface for attribute access
func (d *DirEntry) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "name":
		return core.StringValue(d.name), true
	case "path":
		return core.StringValue(d.path), true
	case "is_dir":
		isDir := d.isDir
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(isDir), nil
		}), true
	case "is_file":
		isDir := d.isDir
		isSymlink := d.isSymlink
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(!isDir && !isSymlink), nil
		}), true
	case "is_symlink":
		isSymlink := d.isSymlink
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(isSymlink), nil
		}), true
	}
	return nil, false
}

// SetAttr implements Object interface
func (d *DirEntry) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("DirEntry does not support attribute assignment")
}

// ScandirIterator wraps directory entries with context manager support
// This is needed because CPython's os.walk uses "with scandir_it:" syntax
type ScandirIterator struct {
	core.BaseObject
	entries []os.DirEntry
	path    string
	index   int
	closed  bool
}

// Type returns the type name
func (s *ScandirIterator) Type() core.Type {
	return core.Type("ScandirIterator")
}

// GetAttr implements Object interface
func (s *ScandirIterator) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "__enter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return s, nil
		}), true
	case "__exit__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			s.closed = true
			return core.Nil, nil
		}), true
	case "__iter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return s, nil
		}), true
	case "__next__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return s.Next()
		}), true
	case "close":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			s.closed = true
			return core.Nil, nil
		}), true
	}
	return nil, false
}

// SetAttr implements Object interface
func (s *ScandirIterator) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("ScandirIterator does not support attribute assignment")
}

// Next returns the next DirEntry or signals StopIteration
func (s *ScandirIterator) Next() (core.Value, error) {
	if s.closed || s.index >= len(s.entries) {
		return nil, &core.StopIteration{}
	}

	entry := s.entries[s.index]
	s.index++

	// Create a proper DirEntry object with attribute access
	dirEntry := &DirEntry{
		name:      entry.Name(),
		path:      filepath.Join(s.path, entry.Name()),
		isDir:     entry.IsDir(),
		isSymlink: entry.Type()&os.ModeSymlink != 0,
	}

	return dirEntry, nil
}

// Iterator returns the iterator (self)
func (s *ScandirIterator) Iterator() core.Iterator {
	return &scandirCoreIterator{s: s}
}

// scandirCoreIterator adapts ScandirIterator to core.Iterator
type scandirCoreIterator struct {
	s *ScandirIterator
}

func (i *scandirCoreIterator) Next() (core.Value, bool) {
	val, err := i.s.Next()
	if err != nil {
		return nil, false
	}
	return val, true
}

func (i *scandirCoreIterator) Reset() {
	i.s.index = 0
	i.s.closed = false
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
		// Return proper error types for CPython compatibility
		if os.IsNotExist(err) {
			return nil, core.NewFileNotFoundError(err.Error(), path)
		}
		return nil, core.NewOSError(err.Error(), path)
	}

	// Return a ScandirIterator with context manager support
	return &ScandirIterator{
		entries: entries,
		path:    path,
		index:   0,
		closed:  false,
	}, nil
}

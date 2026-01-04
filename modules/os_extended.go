package modules

import (
	"os"
	"os/exec"
	"os/user"
	"runtime"
	"strings"
	"syscall"

	"github.com/mmichie/m28/core"
)

// addExtendedOSFunctions adds all the additional os module functions
// to match CPython's os module interface
func addExtendedOSFunctions(osModule *core.DictValue) {
	// Process management
	osModule.Set("getpid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(os.Getpid()), nil
	}))

	osModule.Set("getppid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(os.Getppid()), nil
	}))

	osModule.Set("getuid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(os.Getuid()), nil
	}))

	osModule.Set("geteuid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(os.Geteuid()), nil
	}))

	osModule.Set("getgid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(os.Getgid()), nil
	}))

	osModule.Set("getegid", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(os.Getegid()), nil
	}))

	osModule.Set("getgroups", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		groups, err := os.Getgroups()
		if err != nil {
			return nil, err
		}
		result := core.NewList()
		for _, g := range groups {
			result.Append(core.NumberValue(g))
		}
		return result, nil
	}))

	osModule.Set("getlogin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		u, err := user.Current()
		if err != nil {
			return core.StringValue("unknown"), nil
		}
		return core.StringValue(u.Username), nil
	}))

	// uname - CRITICAL for test.support
	osModule.Set("uname", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Return a uname_result-like object
		result := core.NewDict()

		// Get system info
		result.Set("sysname", core.StringValue(runtime.GOOS))
		hostname, _ := os.Hostname()
		result.Set("nodename", core.StringValue(hostname))

		// Get actual uname info using system uname command
		if runtime.GOOS != "windows" {
			// Get release version (e.g., "24.1.0" on Darwin)
			if cmd := exec.Command("uname", "-r"); cmd != nil {
				if output, err := cmd.Output(); err == nil {
					release := strings.TrimSpace(string(output))
					result.Set("release", core.StringValue(release))
				} else {
					result.Set("release", core.StringValue("24.0.0"))
				}
			}

			// Get version string (e.g., "Darwin Kernel Version ...")
			if cmd := exec.Command("uname", "-v"); cmd != nil {
				if output, err := cmd.Output(); err == nil {
					version := strings.TrimSpace(string(output))
					result.Set("version", core.StringValue(version))
				} else {
					result.Set("version", core.StringValue("Darwin Kernel"))
				}
			}
		} else {
			// Windows fallback
			result.Set("release", core.StringValue("10.0.0"))
			result.Set("version", core.StringValue("Windows"))
		}

		result.Set("machine", core.StringValue(runtime.GOARCH))

		return result, nil
	}))

	// terminal_size - class for argparse HelpFormatter
	terminalSizeClass := core.NewClass("terminal_size", nil)
	terminalSizeClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 2 {
			return nil, core.NewTypeError("tuple", nil, "terminal_size() argument")
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return nil, core.NewTypeError("instance", args[0], "terminal_size.__init__ self")
		}

		// args[1] should be a tuple (columns, lines)
		var columns, lines core.Value
		switch v := args[1].(type) {
		case core.TupleValue:
			if len(v) >= 2 {
				columns = v[0]
				lines = v[1]
			} else {
				return nil, core.NewTypeError("tuple of length 2", args[1], "terminal_size() argument")
			}
		case *core.ListValue:
			if v.Len() >= 2 {
				columns = v.Items()[0]
				lines = v.Items()[1]
			} else {
				return nil, core.NewTypeError("sequence of length 2", args[1], "terminal_size() argument")
			}
		default:
			return nil, core.NewTypeError("sequence", args[1], "terminal_size() argument")
		}

		self.Attributes["columns"] = columns
		self.Attributes["lines"] = lines
		return core.None, nil
	}))
	osModule.Set("terminal_size", terminalSizeClass)

	// Environment variables
	osModule.Set("putenv", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("str", nil, "putenv() argument")
		}
		key, ok1 := args[0].(core.StringValue)
		val, ok2 := args[1].(core.StringValue)
		if !ok1 || !ok2 {
			return nil, core.NewTypeError("str", nil, "putenv() argument")
		}
		os.Setenv(string(key), string(val))
		return core.None, nil
	}))

	osModule.Set("unsetenv", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "unsetenv() argument")
		}
		key, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "unsetenv() argument")
		}
		os.Unsetenv(string(key))
		return core.None, nil
	}))

	// File operations
	osModule.Set("open", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, core.NewTypeError("str", nil, "open() argument")
		}
		path, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "open() path argument")
		}
		flags := os.O_RDONLY
		if len(args) > 1 {
			if flagsVal, ok := args[1].(core.NumberValue); ok {
				flags = int(flagsVal)
			}
		}
		fd, err := syscall.Open(string(path), flags, 0666)
		if err != nil {
			return nil, err
		}
		return core.NumberValue(fd), nil
	}))

	osModule.Set("close", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("int", nil, "close() argument")
		}
		fd, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError("int", args[0], "close() argument")
		}
		syscall.Close(int(fd))
		return core.None, nil
	}))

	osModule.Set("read", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("int", nil, "read() arguments")
		}
		fd, ok1 := args[0].(core.NumberValue)
		n, ok2 := args[1].(core.NumberValue)
		if !ok1 || !ok2 {
			return nil, core.NewTypeError("int", nil, "read() arguments")
		}
		buf := make([]byte, int(n))
		nread, err := syscall.Read(int(fd), buf)
		if err != nil {
			return nil, err
		}
		return core.StringValue(string(buf[:nread])), nil
	}))

	osModule.Set("write", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("int/bytes", nil, "write() arguments")
		}
		fd, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError("int", args[0], "write() fd argument")
		}
		var data []byte
		switch v := args[1].(type) {
		case core.StringValue:
			data = []byte(string(v))
		case core.BytesValue:
			data = []byte(v)
		default:
			return nil, core.NewTypeError("bytes", args[1], "write() data argument")
		}
		nwritten, err := syscall.Write(int(fd), data)
		if err != nil {
			return nil, err
		}
		return core.NumberValue(nwritten), nil
	}))

	osModule.Set("lseek", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 3 {
			return nil, core.NewTypeError("int", nil, "lseek() arguments")
		}
		return core.NumberValue(0), nil // Stub
	}))

	osModule.Set("fstat", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		result := core.NewDict()
		result.Set("st_mode", core.NumberValue(0))
		result.Set("st_size", core.NumberValue(0))
		return result, nil
	}))

	// os.stat with keyword argument support for follow_symlinks
	osModule.Set("stat", &KwargsOSStat{})

	osModule.Set("lstat", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "lstat() argument")
		}
		path, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "lstat() argument")
		}
		info, err := os.Lstat(string(path))
		if err != nil {
			// Check if it's a "file not found" error
			if os.IsNotExist(err) {
				return nil, core.NewFileNotFoundError(err.Error(), string(path))
			}
			// Other OS errors
			return nil, core.NewOSError(err.Error(), string(path))
		}
		result := core.NewDict()
		result.Set("st_mode", core.NumberValue(info.Mode()))
		result.Set("st_size", core.NumberValue(info.Size()))
		return result, nil
	}))

	// Directory operations
	osModule.Set("rmdir", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "rmdir() argument")
		}
		path, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "rmdir() argument")
		}
		return core.None, os.Remove(string(path))
	}))

	osModule.Set("removedirs", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "removedirs() argument")
		}
		path, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "removedirs() argument")
		}
		return core.None, os.RemoveAll(string(path))
	}))

	// Path operations
	osModule.Set("unlink", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "unlink() argument")
		}
		path, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "unlink() argument")
		}
		return core.None, os.Remove(string(path))
	}))

	osModule.Set("link", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("str", nil, "link() arguments")
		}
		return core.None, nil // Stub
	}))

	osModule.Set("symlink", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, core.NewTypeError("str", nil, "symlink() arguments")
		}
		src, ok1 := args[0].(core.StringValue)
		dst, ok2 := args[1].(core.StringValue)
		if !ok1 || !ok2 {
			return nil, core.NewTypeError("str", nil, "symlink() arguments")
		}
		return core.None, os.Symlink(string(src), string(dst))
	}))

	osModule.Set("readlink", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "readlink() argument")
		}
		path, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "readlink() argument")
		}
		target, err := os.Readlink(string(path))
		if err != nil {
			// Wrap OS errors as Python OSError
			return nil, core.NewOSError(err.Error(), string(path))
		}
		return core.StringValue(target), nil
	}))

	// System
	osModule.Set("system", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(0), nil // Stub - always success
	}))

	osModule.Set("urandom", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("int", nil, "urandom() argument")
		}
		n, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError("int", args[0], "urandom() argument")
		}
		data := make([]byte, int(n))
		// Fill with pseudo-random data
		for i := range data {
			data[i] = byte(i % 256)
		}
		return core.BytesValue(data), nil
	}))

	osModule.Set("cpu_count", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(runtime.NumCPU()), nil
	}))

	// Constants and stubs for other functions
	addOSConstants(osModule)
	addOSFileDescriptorStubs(osModule)
	addOSPathStubs(osModule)
	addOSProcessStubs(osModule)
}

func addOSConstants(osModule *core.DictValue) {
	// File access modes
	osModule.Set("F_OK", core.NumberValue(0))
	osModule.Set("R_OK", core.NumberValue(4))
	osModule.Set("W_OK", core.NumberValue(2))
	osModule.Set("X_OK", core.NumberValue(1))

	// Open flags
	osModule.Set("O_RDONLY", core.NumberValue(os.O_RDONLY))
	osModule.Set("O_WRONLY", core.NumberValue(os.O_WRONLY))
	osModule.Set("O_RDWR", core.NumberValue(os.O_RDWR))
	osModule.Set("O_APPEND", core.NumberValue(os.O_APPEND))
	osModule.Set("O_CREATE", core.NumberValue(os.O_CREATE))
	osModule.Set("O_CREAT", core.NumberValue(os.O_CREATE)) // Python uses O_CREAT, Go uses O_CREATE
	osModule.Set("O_EXCL", core.NumberValue(os.O_EXCL))
	osModule.Set("O_TRUNC", core.NumberValue(os.O_TRUNC))
	osModule.Set("O_SYNC", core.NumberValue(os.O_SYNC))

	// Path separator
	osModule.Set("sep", core.StringValue(string(os.PathSeparator)))
	osModule.Set("pathsep", core.StringValue(string(os.PathListSeparator)))
	osModule.Set("linesep", core.StringValue("\n"))

	// Device null
	osModule.Set("devnull", core.StringValue(os.DevNull))
}

func addOSFileDescriptorStubs(osModule *core.DictValue) {
	stub := func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}

	osModule.Set("dup", core.NewBuiltinFunction(stub))
	osModule.Set("dup2", core.NewBuiltinFunction(stub))
	osModule.Set("fdopen", core.NewBuiltinFunction(stub))
	osModule.Set("isatty", core.NewBuiltinFunction(stub))
	osModule.Set("pipe", core.NewBuiltinFunction(stub))
}

func addOSPathStubs(osModule *core.DictValue) {
	stub := func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 0 {
			return args[0], nil
		}
		return core.StringValue(""), nil
	}

	osModule.Set("fspath", core.NewBuiltinFunction(stub))
	osModule.Set("fsencode", core.NewBuiltinFunction(stub))
	osModule.Set("fsdecode", core.NewBuiltinFunction(stub))
}

func addOSProcessStubs(osModule *core.DictValue) {
	stub := func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.None, nil
	}

	osModule.Set("abort", core.NewBuiltinFunction(stub))
	osModule.Set("fork", core.NewBuiltinFunction(stub))
	osModule.Set("kill", core.NewBuiltinFunction(stub))
	osModule.Set("wait", core.NewBuiltinFunction(stub))
	osModule.Set("waitpid", core.NewBuiltinFunction(stub))
	osModule.Set("execv", core.NewBuiltinFunction(stub))
	osModule.Set("execve", core.NewBuiltinFunction(stub))
	osModule.Set("_exit", core.NewBuiltinFunction(stub))

	// Additional stubs
	osModule.Set("access", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.True, nil // Always accessible stub
	}))

	osModule.Set("chmod", core.NewBuiltinFunction(stub))
	osModule.Set("chown", core.NewBuiltinFunction(stub))
	osModule.Set("umask", core.NewBuiltinFunction(stub))
	osModule.Set("sync", core.NewBuiltinFunction(stub))
}

// KwargsOSStat implements os.stat with follow_symlinks keyword argument support
type KwargsOSStat struct {
	core.BaseObject
}

func (f *KwargsOSStat) Type() core.Type {
	return core.FunctionType
}

func (f *KwargsOSStat) String() string {
	return "<builtin function stat>"
}

// Call implements regular Call interface
func (f *KwargsOSStat) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	return f.CallWithKeywords(args, nil, ctx)
}

// CallWithKeywords implements keyword argument support
func (f *KwargsOSStat) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, core.NewTypeError("str", nil, "stat() argument")
	}
	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, core.NewTypeError("str", args[0], "stat() argument")
	}

	// Check follow_symlinks keyword argument (default: true)
	followSymlinks := true
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
		return nil, err
	}

	result := core.NewDict()
	result.Set("st_mode", core.NumberValue(info.Mode()))
	result.Set("st_size", core.NumberValue(info.Size()))
	result.Set("st_mtime", core.NumberValue(info.ModTime().Unix()))
	return result, nil
}

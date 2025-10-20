package modules

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitOSModule creates and initializes the os module
func InitOSModule() *core.DictValue {
	// Create os module as a dict
	osModule := core.NewDict()

	// Create os.path submodule as a dict
	pathModule := core.NewDict()

	// Register os.path functions
	pathModule.SetWithKey("exists", core.StringValue("exists"), core.NewBuiltinFunction(osPathExists))
	pathModule.SetWithKey("join", core.StringValue("join"), core.NewBuiltinFunction(osPathJoin))
	pathModule.SetWithKey("dirname", core.StringValue("dirname"), core.NewBuiltinFunction(osPathDirname))
	pathModule.SetWithKey("basename", core.StringValue("basename"), core.NewBuiltinFunction(osPathBasename))
	pathModule.SetWithKey("abspath", core.StringValue("abspath"), core.NewBuiltinFunction(osPathAbspath))
	pathModule.SetWithKey("isfile", core.StringValue("isfile"), core.NewBuiltinFunction(osPathIsFile))
	pathModule.SetWithKey("isdir", core.StringValue("isdir"), core.NewBuiltinFunction(osPathIsDir))
	pathModule.SetWithKey("getsize", core.StringValue("getsize"), core.NewBuiltinFunction(osPathGetsize))
	pathModule.SetWithKey("getmtime", core.StringValue("getmtime"), core.NewBuiltinFunction(osPathGetmtime))

	// Register os functions
	osModule.SetWithKey("path", core.StringValue("path"), pathModule)
	osModule.SetWithKey("getcwd", core.StringValue("getcwd"), core.NewBuiltinFunction(osGetcwd))
	osModule.SetWithKey("chdir", core.StringValue("chdir"), core.NewBuiltinFunction(osChdir))
	osModule.SetWithKey("listdir", core.StringValue("listdir"), core.NewBuiltinFunction(osListdir))
	osModule.SetWithKey("mkdir", core.StringValue("mkdir"), core.NewBuiltinFunction(osMkdir))
	osModule.SetWithKey("makedirs", core.StringValue("makedirs"), core.NewBuiltinFunction(osMakedirs))
	osModule.SetWithKey("remove", core.StringValue("remove"), core.NewBuiltinFunction(osRemove))
	osModule.SetWithKey("rename", core.StringValue("rename"), core.NewBuiltinFunction(osRename))
	osModule.SetWithKey("getenv", core.StringValue("getenv"), core.NewBuiltinFunction(osGetenv))
	osModule.SetWithKey("environ", core.StringValue("environ"), core.NewBuiltinFunction(osEnviron))
	osModule.SetWithKey("isfile", core.StringValue("isfile"), core.NewBuiltinFunction(osPathIsFile))
	osModule.SetWithKey("isdir", core.StringValue("isdir"), core.NewBuiltinFunction(osPathIsDir))

	return osModule
}

// os.path.exists
func osPathExists(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.exists", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	_, statErr := os.Stat(path)
	return core.BoolValue(statErr == nil), nil
}

// os.path.join
func osPathJoin(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.join", args)

	paths := make([]string, v.Count())
	for i := 0; i < v.Count(); i++ {
		path, err := v.GetString(i)
		if err != nil {
			return nil, fmt.Errorf("argument %d must be a string", i+1)
		}
		paths[i] = path
	}

	return core.StringValue(filepath.Join(paths...)), nil
}

// os.path.dirname
func osPathDirname(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.dirname", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	return core.StringValue(filepath.Dir(path)), nil
}

// os.path.basename
func osPathBasename(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.basename", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	return core.StringValue(filepath.Base(path)), nil
}

// os.path.abspath
func osPathAbspath(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.abspath", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	abs, absErr := filepath.Abs(path)
	if absErr != nil {
		return nil, fmt.Errorf("os.path.abspath: %v", absErr)
	}

	return core.StringValue(abs), nil
}

// os.path.isfile
func osPathIsFile(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.isfile", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	info, statErr := os.Stat(path)
	if statErr != nil {
		return core.BoolValue(false), nil
	}

	return core.BoolValue(!info.IsDir()), nil
}

// os.path.isdir
func osPathIsDir(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.isdir", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	info, statErr := os.Stat(path)
	if statErr != nil {
		return core.BoolValue(false), nil
	}

	return core.BoolValue(info.IsDir()), nil
}

// os.path.getsize
func osPathGetsize(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.getsize", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	info, statErr := os.Stat(path)
	if statErr != nil {
		return nil, fmt.Errorf("os.path.getsize: %v", statErr)
	}

	return core.NumberValue(info.Size()), nil
}

// os.path.getmtime
func osPathGetmtime(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.path.getmtime", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	info, statErr := os.Stat(path)
	if statErr != nil {
		return nil, fmt.Errorf("os.path.getmtime: %v", statErr)
	}

	// Return modification time as Unix timestamp (float seconds)
	mtime := info.ModTime()
	seconds := float64(mtime.Unix()) + float64(mtime.Nanosecond())/1e9
	return core.NumberValue(seconds), nil
}

// os.getcwd
func osGetcwd(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.getcwd", args)
	if err := v.Exact(0); err != nil {
		return nil, err
	}

	cwd, err := os.Getwd()
	if err != nil {
		return nil, fmt.Errorf("os.getcwd: %v", err)
	}

	return core.StringValue(cwd), nil
}

// os.chdir
func osChdir(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.chdir", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	if chdirErr := os.Chdir(path); chdirErr != nil {
		return nil, fmt.Errorf("os.chdir: %v", chdirErr)
	}

	return core.None, nil
}

// os.listdir
func osListdir(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.listdir", args)
	if err := v.Max(1); err != nil {
		return nil, err
	}

	path := "."
	if v.Count() > 0 {
		p, err := v.GetString(0)
		if err != nil {
			return nil, err
		}
		path = p
	}

	entries, readErr := os.ReadDir(path)
	if readErr != nil {
		return nil, fmt.Errorf("os.listdir: %v", readErr)
	}

	result := make([]core.Value, len(entries))
	for i, entry := range entries {
		result[i] = core.StringValue(entry.Name())
	}

	return core.NewList(result...), nil
}

// os.mkdir
func osMkdir(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.mkdir", args)
	if err := v.Range(1, 2); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	mode := os.FileMode(0755)
	if v.Count() > 1 {
		modeNum, err := v.GetNumber(1)
		if err != nil {
			return nil, fmt.Errorf("mode must be a number")
		}
		mode = os.FileMode(modeNum)
	}

	if mkdirErr := os.Mkdir(path, mode); mkdirErr != nil {
		return nil, fmt.Errorf("os.mkdir: %v", mkdirErr)
	}

	return core.None, nil
}

// os.makedirs
func osMakedirs(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.makedirs", args)
	if err := v.Range(1, 2); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	mode := os.FileMode(0755)
	if v.Count() > 1 {
		modeNum, err := v.GetNumber(1)
		if err != nil {
			return nil, fmt.Errorf("mode must be a number")
		}
		mode = os.FileMode(modeNum)
	}

	if mkdirErr := os.MkdirAll(path, mode); mkdirErr != nil {
		return nil, fmt.Errorf("os.makedirs: %v", mkdirErr)
	}

	return core.None, nil
}

// os.remove
func osRemove(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.remove", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	path, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	if removeErr := os.Remove(path); removeErr != nil {
		return nil, fmt.Errorf("os.remove: %v", removeErr)
	}

	return core.None, nil
}

// os.rename
func osRename(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.rename", args)
	if err := v.Exact(2); err != nil {
		return nil, err
	}

	src, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	dst, err := v.GetString(1)
	if err != nil {
		return nil, err
	}

	if renameErr := os.Rename(src, dst); renameErr != nil {
		return nil, fmt.Errorf("os.rename: %v", renameErr)
	}

	return core.None, nil
}

// os.getenv
func osGetenv(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.getenv", args)
	if err := v.Range(1, 2); err != nil {
		return nil, err
	}

	key, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	value := os.Getenv(key)
	if value == "" && v.Count() > 1 {
		// Return default value if provided
		return v.Get(1), nil
	}

	if value == "" {
		return core.None, nil
	}

	return core.StringValue(value), nil
}

// os.environ
func osEnviron(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("os.environ", args)
	if err := v.Exact(0); err != nil {
		return nil, err
	}

	env := os.Environ()
	result := core.NewDict()

	for _, e := range env {
		for i := 0; i < len(e); i++ {
			if e[i] == '=' {
				key := e[:i]
				value := e[i+1:]
				result.SetWithKey(key, core.StringValue(key), core.StringValue(value))
				break
			}
		}
	}

	return result, nil
}

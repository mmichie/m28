// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/mmichie/m28/core"
)

// RegisterOSModule registers the os module
func RegisterOSModule(ctx *core.Context) {
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

	// Register the module in the module registry
	registry := core.GetModuleRegistry()
	registry.StoreModule("os", osModule, "<builtin>", []string{})
}

// osPathExists checks if a path exists
func osPathExists(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.exists requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.exists: path must be a string")
	}

	_, err := os.Stat(string(path))
	return core.BoolValue(err == nil), nil
}

// osPathJoin joins path elements
func osPathJoin(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.StringValue(""), nil
	}

	paths := make([]string, len(args))
	for i, arg := range args {
		str, ok := arg.(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("os.path.join: all arguments must be strings")
		}
		paths[i] = string(str)
	}

	return core.StringValue(filepath.Join(paths...)), nil
}

// osPathDirname returns the directory portion of a path
func osPathDirname(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.dirname requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.dirname: path must be a string")
	}

	return core.StringValue(filepath.Dir(string(path))), nil
}

// osPathBasename returns the base name of a path
func osPathBasename(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.basename requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.basename: path must be a string")
	}

	return core.StringValue(filepath.Base(string(path))), nil
}

// osPathAbspath returns the absolute path
func osPathAbspath(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.abspath requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.abspath: path must be a string")
	}

	absPath, err := filepath.Abs(string(path))
	if err != nil {
		return nil, fmt.Errorf("os.path.abspath: %v", err)
	}

	return core.StringValue(absPath), nil
}

// osPathIsFile checks if a path is a file
func osPathIsFile(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.isfile requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.isfile: path must be a string")
	}

	info, err := os.Stat(string(path))
	if err != nil {
		return core.BoolValue(false), nil
	}

	return core.BoolValue(!info.IsDir()), nil
}

// osPathIsDir checks if a path is a directory
func osPathIsDir(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.isdir requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.isdir: path must be a string")
	}

	info, err := os.Stat(string(path))
	if err != nil {
		return core.BoolValue(false), nil
	}

	return core.BoolValue(info.IsDir()), nil
}

// osGetcwd returns the current working directory
func osGetcwd(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("os.getcwd takes no arguments")
	}

	cwd, err := os.Getwd()
	if err != nil {
		return nil, fmt.Errorf("os.getcwd: %v", err)
	}

	return core.StringValue(cwd), nil
}

// osChdir changes the current directory
func osChdir(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.chdir requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.chdir: path must be a string")
	}

	err := os.Chdir(string(path))
	if err != nil {
		return nil, fmt.Errorf("os.chdir: %v", err)
	}

	return core.NilValue{}, nil
}

// osListdir lists directory contents
func osListdir(args []core.Value, ctx *core.Context) (core.Value, error) {
	path := "."
	if len(args) > 0 {
		pathStr, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("os.listdir: path must be a string")
		}
		path = string(pathStr)
	}

	entries, err := os.ReadDir(path)
	if err != nil {
		return nil, fmt.Errorf("os.listdir: %v", err)
	}

	result := make(core.ListValue, len(entries))
	for i, entry := range entries {
		result[i] = core.StringValue(entry.Name())
	}

	return result, nil
}

// osMkdir creates a directory
func osMkdir(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("os.mkdir requires at least 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.mkdir: path must be a string")
	}

	// Default mode
	mode := os.FileMode(0755)
	if len(args) >= 2 {
		if modeNum, ok := args[1].(core.NumberValue); ok {
			mode = os.FileMode(int(modeNum))
		}
	}

	err := os.Mkdir(string(path), mode)
	if err != nil {
		return nil, fmt.Errorf("os.mkdir: %v", err)
	}

	return core.NilValue{}, nil
}

// osMakedirs creates a directory and all parent directories
func osMakedirs(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("os.makedirs requires at least 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.makedirs: path must be a string")
	}

	// Default mode
	mode := os.FileMode(0755)
	if len(args) >= 2 {
		if modeNum, ok := args[1].(core.NumberValue); ok {
			mode = os.FileMode(int(modeNum))
		}
	}

	err := os.MkdirAll(string(path), mode)
	if err != nil {
		return nil, fmt.Errorf("os.makedirs: %v", err)
	}

	return core.NilValue{}, nil
}

// osRemove removes a file or empty directory
func osRemove(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.remove requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.remove: path must be a string")
	}

	err := os.Remove(string(path))
	if err != nil {
		return nil, fmt.Errorf("os.remove: %v", err)
	}

	return core.NilValue{}, nil
}

// osRename renames a file or directory
func osRename(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("os.rename requires exactly 2 arguments")
	}

	oldPath, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.rename: old path must be a string")
	}

	newPath, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.rename: new path must be a string")
	}

	err := os.Rename(string(oldPath), string(newPath))
	if err != nil {
		return nil, fmt.Errorf("os.rename: %v", err)
	}

	return core.NilValue{}, nil
}

// osGetenv gets an environment variable
func osGetenv(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("os.getenv requires 1 or 2 arguments")
	}

	key, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.getenv: key must be a string")
	}

	value := os.Getenv(string(key))
	if value == "" && len(args) == 2 {
		// Return default value if provided
		return args[1], nil
	}

	return core.StringValue(value), nil
}

// osEnviron returns all environment variables as a dictionary
func osEnviron(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("os.environ takes no arguments")
	}

	env := os.Environ()
	dict := core.NewDict()

	for _, e := range env {
		pair := splitFirst(e, '=')
		if len(pair) == 2 {
			dict.SetWithKey(core.ValueToKey(core.StringValue(pair[0])), core.StringValue(pair[0]), core.StringValue(pair[1]))
		}
	}

	return dict, nil
}

// osPathGetsize returns the size of a file in bytes
func osPathGetsize(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.getsize requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.getsize: path must be a string")
	}

	info, err := os.Stat(string(path))
	if err != nil {
		return nil, fmt.Errorf("os.path.getsize: %v", err)
	}

	return core.NumberValue(info.Size()), nil
}

// osPathGetmtime returns the modification time of a file
func osPathGetmtime(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("os.path.getmtime requires exactly 1 argument")
	}

	path, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("os.path.getmtime: path must be a string")
	}

	info, err := os.Stat(string(path))
	if err != nil {
		return nil, fmt.Errorf("os.path.getmtime: %v", err)
	}

	// Return Unix timestamp as a float
	return core.NumberValue(float64(info.ModTime().Unix())), nil
}

// splitFirst splits a string on the first occurrence of a separator
func splitFirst(s string, sep rune) []string {
	for i, r := range s {
		if r == sep {
			return []string{s[:i], s[i+1:]}
		}
	}
	return []string{s}
}

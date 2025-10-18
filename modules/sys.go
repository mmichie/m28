// Package modules provides the sys module for M28
package modules

import (
	"os"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

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

	// exc_info function
	sysModule.SetWithKey("exc_info", core.StringValue("exc_info"), core.NewBuiltinFunction(excInfo))

	// Add argv (empty list for now)
	sysModule.SetWithKey("argv", core.StringValue("argv"), core.ListValue{})

	// Add version info
	sysModule.SetWithKey("version", core.StringValue("version"), core.StringValue("M28 0.1.0"))

	// Add implementation namespace (SimpleNamespace-like object)
	implDict := core.NewDict()
	implDict.Set("name", core.StringValue("m28"))
	implDict.Set("version", core.StringValue("0.1.0"))
	implDict.Set("cache_tag", core.StringValue("m28"))
	sysModule.SetWithKey("implementation", core.StringValue("implementation"), implDict)

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

package modules

import "github.com/mmichie/m28/core"

// InitStatModule creates the _stat module with file mode constants
func InitStatModule() *core.DictValue {
	module := core.NewDict()

	// Module docstring
	module.Set("__doc__", core.StringValue("Constants and functions for interpreting stat() results"))

	// File type constants (high 4 bits of mode)
	module.Set("S_IFMT", core.NumberValue(0o170000))   // bit mask for file type
	module.Set("S_IFSOCK", core.NumberValue(0o140000)) // socket
	module.Set("S_IFLNK", core.NumberValue(0o120000))  // symbolic link
	module.Set("S_IFREG", core.NumberValue(0o100000))  // regular file
	module.Set("S_IFBLK", core.NumberValue(0o060000))  // block device
	module.Set("S_IFDIR", core.NumberValue(0o040000))  // directory
	module.Set("S_IFCHR", core.NumberValue(0o020000))  // character device
	module.Set("S_IFIFO", core.NumberValue(0o010000))  // FIFO

	// File permission bits (low 12 bits of mode)
	module.Set("S_ISUID", core.NumberValue(0o4000)) // set UID bit
	module.Set("S_ISGID", core.NumberValue(0o2000)) // set GID bit
	module.Set("S_ISVTX", core.NumberValue(0o1000)) // sticky bit
	module.Set("S_IRWXU", core.NumberValue(0o0700)) // owner read/write/execute
	module.Set("S_IRUSR", core.NumberValue(0o0400)) // owner read
	module.Set("S_IWUSR", core.NumberValue(0o0200)) // owner write
	module.Set("S_IXUSR", core.NumberValue(0o0100)) // owner execute
	module.Set("S_IRWXG", core.NumberValue(0o0070)) // group read/write/execute
	module.Set("S_IRGRP", core.NumberValue(0o0040)) // group read
	module.Set("S_IWGRP", core.NumberValue(0o0020)) // group write
	module.Set("S_IXGRP", core.NumberValue(0o0010)) // group execute
	module.Set("S_IRWXO", core.NumberValue(0o0007)) // others read/write/execute
	module.Set("S_IROTH", core.NumberValue(0o0004)) // others read
	module.Set("S_IWOTH", core.NumberValue(0o0002)) // others write
	module.Set("S_IXOTH", core.NumberValue(0o0001)) // others execute

	// Helper functions
	module.Set("S_ISDIR", core.NewNamedBuiltinFunction("S_ISDIR", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("S_ISDIR", nil, "S_ISDIR() takes exactly 1 argument")
		}
		mode, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError("S_ISDIR", args[0], "an integer is required")
		}
		return core.BoolValue(int(mode)&0o170000 == 0o040000), nil
	}))

	module.Set("S_ISREG", core.NewNamedBuiltinFunction("S_ISREG", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("S_ISREG", nil, "S_ISREG() takes exactly 1 argument")
		}
		mode, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError("S_ISREG", args[0], "an integer is required")
		}
		return core.BoolValue(int(mode)&0o170000 == 0o100000), nil
	}))

	module.Set("S_ISLNK", core.NewNamedBuiltinFunction("S_ISLNK", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("S_ISLNK", nil, "S_ISLNK() takes exactly 1 argument")
		}
		mode, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError("S_ISLNK", args[0], "an integer is required")
		}
		return core.BoolValue(int(mode)&0o170000 == 0o120000), nil
	}))

	return module
}

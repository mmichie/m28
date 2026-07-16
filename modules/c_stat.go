package modules

import "github.com/mmichie/m28/core"

// InitStatModule creates the _stat module with file mode constants
func InitStatModule() *core.DictValue {
	module := core.NewDict()

	// Module docstring
	module.SetStr("__doc__", core.StringValue("Constants and functions for interpreting stat() results"))

	// File type constants (high 4 bits of mode)
	module.SetStr("S_IFSOCK", core.NumberValue(0o140000)) // socket
	module.SetStr("S_IFLNK", core.NumberValue(0o120000))  // symbolic link
	module.SetStr("S_IFREG", core.NumberValue(0o100000))  // regular file
	module.SetStr("S_IFBLK", core.NumberValue(0o060000))  // block device
	module.SetStr("S_IFDIR", core.NumberValue(0o040000))  // directory
	module.SetStr("S_IFCHR", core.NumberValue(0o020000))  // character device
	module.SetStr("S_IFIFO", core.NumberValue(0o010000))  // FIFO

	// File permission bits (low 12 bits of mode)
	module.SetStr("S_ISUID", core.NumberValue(0o4000)) // set UID bit
	module.SetStr("S_ISGID", core.NumberValue(0o2000)) // set GID bit
	module.SetStr("S_ISVTX", core.NumberValue(0o1000)) // sticky bit
	module.SetStr("S_IRWXU", core.NumberValue(0o0700)) // owner read/write/execute
	module.SetStr("S_IRUSR", core.NumberValue(0o0400)) // owner read
	module.SetStr("S_IWUSR", core.NumberValue(0o0200)) // owner write
	module.SetStr("S_IXUSR", core.NumberValue(0o0100)) // owner execute
	module.SetStr("S_IRWXG", core.NumberValue(0o0070)) // group read/write/execute
	module.SetStr("S_IRGRP", core.NumberValue(0o0040)) // group read
	module.SetStr("S_IWGRP", core.NumberValue(0o0020)) // group write
	module.SetStr("S_IXGRP", core.NumberValue(0o0010)) // group execute
	module.SetStr("S_IRWXO", core.NumberValue(0o0007)) // others read/write/execute
	module.SetStr("S_IROTH", core.NumberValue(0o0004)) // others read
	module.SetStr("S_IWOTH", core.NumberValue(0o0002)) // others write
	module.SetStr("S_IXOTH", core.NumberValue(0o0001)) // others execute

	// Mask functions. In CPython these are exposed by the _stat C extension as
	// functions (not int constants); stat.py's pure-Python definitions are then
	// overridden by `from _stat import *`, so S_IFMT must be callable.
	module.SetStr("S_IMODE", statMaskFunc("S_IMODE", 0o7777))
	module.SetStr("S_IFMT", statMaskFunc("S_IFMT", 0o170000))

	// File-type predicates. S_ISFIFO/S_ISCHR/S_ISBLK/S_ISSOCK were previously
	// missing, which forced stat.py's pure-Python fallbacks (themselves broken
	// once S_IFMT became an int). Provide the full set the C extension exports.
	module.SetStr("S_ISDIR", statTypePredicate("S_ISDIR", 0o040000))
	module.SetStr("S_ISREG", statTypePredicate("S_ISREG", 0o100000))
	module.SetStr("S_ISLNK", statTypePredicate("S_ISLNK", 0o120000))
	module.SetStr("S_ISFIFO", statTypePredicate("S_ISFIFO", 0o010000))
	module.SetStr("S_ISCHR", statTypePredicate("S_ISCHR", 0o020000))
	module.SetStr("S_ISBLK", statTypePredicate("S_ISBLK", 0o060000))
	module.SetStr("S_ISSOCK", statTypePredicate("S_ISSOCK", 0o140000))

	return module
}

// statMaskFunc builds a _stat function that masks its single integer argument,
// e.g. S_IFMT(mode) == mode & 0o170000 and S_IMODE(mode) == mode & 0o7777.
func statMaskFunc(name string, mask int) core.Value {
	return core.NewNamedBuiltinFunction(name, func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError(name, nil, name+"() takes exactly 1 argument")
		}
		mode, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError(name, args[0], "an integer is required")
		}
		return core.NumberValue(int(mode) & mask), nil
	})
}

// statTypePredicate builds a _stat S_IS* function that tests the file-type bits
// of a mode against the given type constant.
func statTypePredicate(name string, typeBits int) core.Value {
	return core.NewNamedBuiltinFunction(name, func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError(name, nil, name+"() takes exactly 1 argument")
		}
		mode, ok := args[0].(core.NumberValue)
		if !ok {
			return nil, core.NewTypeError(name, args[0], "an integer is required")
		}
		return core.BoolValue(int(mode)&0o170000 == typeBits), nil
	})
}

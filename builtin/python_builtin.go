package builtin

import "github.com/mmichie/m28/core"

func RegisterPythonBuiltins() {
	RegisterNumericBuiltins()
	RegisterStringBuiltins()
	RegisterConversionBuiltins()
	RegisterIterableBuiltins()
	RegisterTypeBuiltins()
	RegisterIOBuiltins()
	RegisterMiscBuiltins()
	RegisterComparisonBuiltins()
}

func RegisterMiscBuiltins() {
	core.RegisterBuiltin("dict", dictFunc)
	core.RegisterBuiltin("frozenset", frozensetFunc)
	core.RegisterBuiltin("getattr", getattrFunc)
	core.RegisterBuiltin("globals", globalsFunc)
	core.RegisterBuiltin("hasattr", hasattrFunc)
	core.RegisterBuiltin("hash", hashFunc)
	core.RegisterBuiltin("id", idFunc)
	core.RegisterBuiltin("locals", localsFunc)
	core.RegisterBuiltin("set", setFunc)
	core.RegisterBuiltin("slice", sliceFunc)
	core.RegisterBuiltin("vars", varsFunc)
}

// Implementations for the misc builtins should be added here
// (dict, frozenset, getattr, globals, hasattr, hash, id, locals, set, slice, vars)

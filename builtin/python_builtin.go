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
	RegisterSetFunctions()  // Add set functions with dot notation
	RegisterSetBuiltins()   // Add simple set operations without dot notation
	RegisterDictFunctions() // Add dictionary functions with dot notation
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

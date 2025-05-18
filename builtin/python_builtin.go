package builtin

import "github.com/mmichie/m28/core"

func RegisterPythonBuiltins() {
	RegisterNumericBuiltins()
	RegisterStringBuiltins()
	// Conversion builtins are now registered in conversion.go init()
	RegisterIterableBuiltins()
	RegisterTypeBuiltins()
	RegisterIOBuiltins()
	RegisterMiscBuiltins()
	RegisterComparisonBuiltins()
	RegisterSetFunctions()  // Add set functions with dot notation
	RegisterSetBuiltins()   // Add simple set operations without dot notation
	RegisterDictFunctions() // Add dictionary functions with dot notation

	// Register concurrency functions
	RegisterConcurrencyBuiltins()
}

func RegisterMiscBuiltins() {
	core.RegisterBuiltin("dict", DictCreateFunc)
	core.RegisterBuiltin("frozenset", frozensetFunc)
	// Note: getattr/hasattr are now implemented in object_access.go
	core.RegisterBuiltin("globals", globalsFunc)
	core.RegisterBuiltin("hash", hashFunc)
	core.RegisterBuiltin("id", idFunc)
	core.RegisterBuiltin("locals", localsFunc)
	core.RegisterBuiltin("set", setFunc)
	core.RegisterBuiltin("slice", sliceFunc)
	core.RegisterBuiltin("vars", varsFunc)
}

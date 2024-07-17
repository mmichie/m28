package m28

// BuiltinFunc represents a built-in function
type BuiltinFunc func([]LispValue, *Environment) (LispValue, error)

// builtinFuncs maps function names to their implementations
var builtinFuncs map[string]BuiltinFunc

func init() {
	builtinFuncs = make(map[string]BuiltinFunc)
	registerLogicalOps()
	registerTypeChecking()
	registerIOOps()
	registerStringOps()
	registerErrorHandling()
	registerEqualityOps()
	registerMiscOps()
}

func setupBuiltins(env *Environment) {
	env.Set(LispSymbol("nil"), nil)
	env.Set(LispSymbol("#f"), false)
	env.Set(LispSymbol("#t"), true)

	registerArithmeticFuncs(env)
	registerComparisonFuncs(env)
	registerListFuncs(env)

	for name, fn := range builtinFuncs {
		env.Set(LispSymbol(name), LispFunc(fn))
	}
}

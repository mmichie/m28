// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"github.com/mmichie/m28/builtin/methods"
	"github.com/mmichie/m28/builtin/modules"
	"github.com/mmichie/m28/builtin/operators"
	"github.com/mmichie/m28/core"
)

// RegisterAllBuiltins registers all built-in functions in the provided context
func RegisterAllBuiltins(ctx *core.Context) {
	// Register core language builtins
	RegisterTypes(ctx)
	RegisterAttributes(ctx)
	RegisterIteration(ctx)
	RegisterFunctional(ctx)
	RegisterCollections(ctx)
	RegisterNumeric(ctx)
	RegisterIO(ctx)
	RegisterErrors(ctx)
	RegisterMisc(ctx)

	// Register operators
	operators.RegisterArithmeticOperators(ctx)
	operators.RegisterComparisonOperators(ctx)
	operators.RegisterLogicalOperators(ctx)

	// Register type methods
	methods.RegisterStringMethods(ctx)
	methods.RegisterListMethods(ctx)
	methods.RegisterDictMethods(ctx)
	methods.RegisterSetMethods(ctx)

	// Register standard library modules
	modules.RegisterMathModule(ctx)
	RegisterRandomFunctions(ctx)
	RegisterJSONModule(ctx)
	RegisterOSModule(ctx)
	RegisterTimeModule(ctx)
	RegisterDateTimeModule(ctx)
	RegisterShutil(ctx)
	RegisterPathlib(ctx)
	RegisterAsyncBuiltins(ctx)

	// Also register functions from other files that haven't been migrated yet
	RegisterStringFunctions(ctx)
	RegisterListFunctions(ctx)
	RegisterDictFunctions(ctx)
	RegisterEssentialBuiltins(ctx)
	RegisterUtilityFunctions(ctx)
	RegisterAssertBuiltins(ctx)
}

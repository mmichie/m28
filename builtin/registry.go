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
	RegisterList(ctx)
	RegisterIteration(ctx)
	RegisterFunctional(ctx)
	RegisterCollections(ctx)
	RegisterNumeric(ctx)
	RegisterIO(ctx)
	RegisterErrors(ctx)
	RegisterMisc(ctx)

	// Register operators
	operators.RegisterArithmetic(ctx)
	operators.RegisterComparison(ctx)
	operators.RegisterLogicalOperators(ctx)

	// Register type methods
	methods.RegisterStringMethods(ctx)
	methods.RegisterListMethods(ctx)
	methods.RegisterDictMethods(ctx)
	methods.RegisterSetMethods(ctx)

	// Note: Standard library modules (os, json, time, etc.) are no longer
	// registered as builtins. They must be imported using the import statement.
	// Only math module remains as a special builtin.
	modules.RegisterMathModule(ctx)

	// Also register functions from other files that haven't been migrated yet
	RegisterStringFunctions(ctx)
	RegisterDictFunctions(ctx)
	RegisterEssentialBuiltins(ctx)
	RegisterUtilityFunctions(ctx)
	RegisterAssertBuiltins(ctx)
	RegisterAsyncBuiltins(ctx) // Async builtins stay as builtins (Channel, create_task, etc.)
}

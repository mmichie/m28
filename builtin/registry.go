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
	RegisterIterationProtocol(ctx) // Register new iterator protocol functions
	RegisterFunctional(ctx)
	RegisterCollections(ctx)
	RegisterNumeric(ctx)
	RegisterIO(ctx)
	RegisterErrors(ctx)
	RegisterMisc(ctx)
	RegisterDecorators(ctx) // Register decorators (@macro)
	RegisterMacros(ctx)     // Register built-in macros (unless, when, ->, ->>)
	RegisterPathOps(ctx)    // Register path operations (get-path, set-path, update-path, has-path?)
	RegisterDictOps(ctx)    // Register dict operations (select-keys, rename-keys, map-keys, map-values, filter-keys, filter-values)

	// Register operators using new protocol-based implementation
	operators.RegisterAll(ctx)

	// Register type methods
	methods.RegisterStringMethods(ctx)
	methods.RegisterListMethods(ctx)
	methods.RegisterDictMethods(ctx)
	methods.RegisterSetMethods(ctx)

	// Register core math functions (sqrt, sin, cos, etc.) as global builtins
	// Note: The math module itself is lazy-loaded like other modules
	modules.RegisterMathModule(ctx)

	// Register decimal module (Decimal type)
	modules.RegisterDecimalModule(ctx)

	// Also register functions from other files that haven't been migrated yet
	RegisterStringFunctions(ctx)
	RegisterDictFunctions(ctx)
	RegisterEssentialBuiltins(ctx)
	RegisterUtilityFunctions(ctx)
	RegisterAssertBuiltins(ctx)
	RegisterAsyncBuiltins(ctx) // Async builtins stay as builtins (Channel, create_task, etc.)
}

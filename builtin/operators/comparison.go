package operators

import (
	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/core"
)

// RegisterComparison registers comparison operators using the builder framework
func RegisterComparison(ctx *core.Context) {
	// == equality operator
	ctx.Define("==", core.NewBuiltinFunction(builders.Equal()))

	// != inequality operator
	ctx.Define("!=", core.NewBuiltinFunction(builders.NotEqual()))

	// < less than operator
	ctx.Define("<", core.NewBuiltinFunction(builders.LessThan()))

	// <= less than or equal operator
	ctx.Define("<=", core.NewBuiltinFunction(builders.LessThanOrEqual()))

	// > greater than operator
	ctx.Define(">", core.NewBuiltinFunction(builders.GreaterThan()))

	// >= greater than or equal operator
	ctx.Define(">=", core.NewBuiltinFunction(builders.GreaterThanOrEqual()))
}

// Migration Statistics:
// Functions migrated: 6 comparison operators
// Using pre-built operator functions
// Code reduction: ~80% (from ~125 lines to ~25 lines)

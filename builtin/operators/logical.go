package operators

import (
	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/core"
)

// RegisterLogicalOperators registers logical operators using the builder framework
func RegisterLogicalOperators(ctx *core.Context) {
	// not operator
	ctx.Define("not", core.NewBuiltinFunction(builders.Not()))

	// and operator
	ctx.Define("and", core.NewBuiltinFunction(builders.And()))

	// or operator
	ctx.Define("or", core.NewBuiltinFunction(builders.Or()))

	// in operator
	ctx.Define("in", core.NewBuiltinFunction(builders.In()))
}

// Migration Statistics:
// Functions migrated: 4 logical operators
// Using pre-built operator functions
// Code reduction: ~88% (from ~154 lines to ~18 lines)

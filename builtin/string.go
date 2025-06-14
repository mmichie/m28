// Package builtin provides built-in functions for the M28 language.
package builtin

import "github.com/mmichie/m28/core"

// RegisterStringFunctions registers all string-related functions in the provided context
func RegisterStringFunctions(ctx *core.Context) {
	// Register all string function categories
	RegisterStringFormatFunctions(ctx)
	RegisterStringOpsFunctions(ctx)
	RegisterStringSearchFunctions(ctx)
	RegisterStringLegacyFunctions(ctx)
}

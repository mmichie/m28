package builtin

import (
	"github.com/mmichie/m28/core"
	"log"
)

// StrictBuiltinChecking controls whether duplicate builtin registrations cause a fatal error
var StrictBuiltinChecking = false

// RegisterBuiltinFunc is a helper function that registers a builtin with duplicate detection
func RegisterBuiltinFunc(ctx *core.Context, name string, fn func([]core.Value, *core.Context) (core.Value, error)) {
	err := ctx.DefineBuiltin(name, core.NewNamedBuiltinFunction(name, fn))
	if err != nil {
		if StrictBuiltinChecking {
			log.Fatalf("Failed to register builtin '%s': %v", name, err)
		}
		// In non-strict mode, allow overwrites (preserves existing behavior)
		// but the duplicate is tracked by DefineBuiltin
		ctx.Define(name, core.NewNamedBuiltinFunction(name, fn))
	}
}

// RegisterBuiltin is a generic helper for registering any builtin value
func RegisterBuiltin(ctx *core.Context, name string, value core.Value) {
	err := ctx.DefineBuiltin(name, value)
	if err != nil {
		if StrictBuiltinChecking {
			log.Fatalf("Failed to register builtin '%s': %v", name, err)
		}
		// In non-strict mode, allow overwrites
		ctx.Define(name, value)
	}
}

package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterAssertBuiltins registers assertion functions
func RegisterAssertBuiltins(ctx *core.Context) {
	// assert function
	ctx.Define("assert", core.NewNamedBuiltinFunction("assert", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("assert requires at least 1 argument")
		}

		// Convert the first argument to a boolean
		var condition bool
		switch arg := args[0].(type) {
		case core.BoolValue:
			condition = bool(arg)
		case core.NilValue:
			condition = false
		default:
			// In most languages, any non-nil, non-zero, non-empty value is considered true
			condition = true
		}

		if !condition {
			var message string
			if len(args) > 1 {
				// Use the second argument as the error message
				message = fmt.Sprintf("%v", args[1])
			} else {
				message = "Assertion failed"
			}
			return nil, fmt.Errorf(message)
		}

		return core.Nil, nil
	}))
}

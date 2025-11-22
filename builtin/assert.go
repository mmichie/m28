package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterAssertBuiltins registers assertion functions
func RegisterAssertBuiltins(ctx *core.Context) {
	// assert function
	ctx.Define("assert", core.NewNamedBuiltinFunction("assert", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("assert", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// Convert the first argument to a boolean
		var condition bool
		arg := v.Get(0)
		if b, ok := types.AsBool(arg); ok {
			condition = b
		} else if types.IsNil(arg) {
			condition = false
		} else {
			// In most languages, any non-nil, non-zero, non-empty value is considered true
			condition = true
		}

		if !condition {
			var message string
			if v.Count() > 1 {
				// Use the second argument as the error message
				message = fmt.Sprintf("%v", v.Get(1))
			} else {
				message = "Assertion failed"
			}
			return nil, core.NewAssertionError(message)
		}

		return core.Nil, nil
	}))
}

// Migration Statistics:
// Functions migrated: 1 (assert)
// Type checks eliminated: 2 manual type assertions
// Code improvements: Uses AsBool and IsNil helpers
// Benefits: Consistent validation with v.Min(), cleaner type checking

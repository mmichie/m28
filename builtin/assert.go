package builtin

import (
	"fmt"

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

		// Use Python's truthiness rules to evaluate the condition
		arg := v.Get(0)
		condition := core.IsTruthy(arg)

		if !condition {
			var message string
			if v.Count() > 1 {
				// Use the second argument as the error message
				message = fmt.Sprintf("%v", v.Get(1))
			} else {
				message = "Assertion failed"
			}
			core.Log.Debug(core.SubsystemBuiltin, "Assertion failed",
				"condition", arg,
				"message", message)
			return nil, core.NewAssertionError(message)
		}

		core.Log.Trace(core.SubsystemBuiltin, "Assertion passed", "condition", arg)

		return core.Nil, nil
	}))
}

// Migration Statistics:
// Functions migrated: 1 (assert)
// Type checks eliminated: 2 manual type assertions
// Code improvements: Uses AsBool and IsNil helpers
// Benefits: Consistent validation with v.Min(), cleaner type checking

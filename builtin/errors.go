package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterErrors registers error-related functions
func RegisterErrors(ctx *core.Context) {
	// error - create an error object
	ctx.Define("error", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			return core.NewException(""), nil
		}
		if len(args) == 1 {
			msg := ""
			if args[0] != core.Nil {
				msg = args[0].String()
			}
			return core.NewException(msg), nil
		}
		// For now, ignore the error type in args[0]
		if len(args) == 2 {
			msg := ""
			if args[1] != core.Nil {
				msg = args[1].String()
			}
			return core.NewException(msg), nil
		}
		return nil, fmt.Errorf("error() takes at most 2 arguments (%d given)", len(args))
	}))

	// raise - raise an error
	ctx.Define("raise", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) == 0 {
			// Re-raise current exception (not implemented)
			return nil, fmt.Errorf("re-raise not implemented")
		}

		if len(args) > 1 {
			return nil, fmt.Errorf("raise() takes at most 1 argument (%d given)", len(args))
		}

		switch v := args[0].(type) {
		case *core.ExceptionValue:
			// Raise the error
			return nil, fmt.Errorf("%s", v.Message)
		case core.StringValue:
			// Raise generic error with message
			return nil, fmt.Errorf("%s", string(v))
		default:
			return nil, fmt.Errorf("exceptions must derive from BaseException")
		}
	}))

	// assert - assert a condition
	ctx.Define("assert", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, fmt.Errorf("assert takes 1 or 2 arguments (%d given)", len(args))
		}

		// Check condition
		if !core.IsTruthy(args[0]) {
			msg := "assertion failed"
			if len(args) == 2 {
				msg = args[1].String()
			}
			return nil, fmt.Errorf("AssertionError: %s", msg)
		}

		return core.Nil, nil
	}))
}

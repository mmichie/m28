package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterDecorators registers built-in decorators
func RegisterDecorators(ctx *core.Context) {
	// Register the macro decorator
	ctx.Define("macro", core.NewBuiltinFunction(macroDecorator()))
}

// macroDecorator creates the built-in macro decorator
// Usage: (@macro (def func (args) body))
// Marks a function as a macro that receives unevaluated arguments
func macroDecorator() func(args []core.Value, ctx *core.Context) (core.Value, error) {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("macro", args)

		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// The argument should be a function
		fn := v.Get(0)

		// Check if it has GetAttr/SetAttr methods (it's an object with attributes)
		if obj, ok := fn.(interface {
			GetAttr(string) (core.Value, bool)
			SetAttr(string, core.Value) error
		}); ok {
			// Set the __macro__ attribute to true
			if err := obj.SetAttr("__macro__", core.True); err != nil {
				return nil, err
			}
		} else {
			return nil, errors.NewTypeError("macro", "function with attributes", fmt.Sprintf("%v", fn.Type()))
		}

		// Return the modified function
		return fn, nil
	}
}

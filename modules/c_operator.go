package modules

import "github.com/mmichie/m28/core"

// InitOperatorModule creates the _operator module
// Note: Most operator functionality is in the pure Python operator.py module
// This C extension just provides a few optimized functions
func InitOperatorModule() *core.DictValue {
	module := core.NewDict()

	// Module docstring
	module.Set("__doc__", core.StringValue("Operator interface"))

	// length_hint(obj, default=0) - get length hint for iteration
	module.Set("length_hint", core.NewNamedBuiltinFunction("length_hint", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, core.NewTypeError("length_hint", nil, "length_hint expected 1 or 2 arguments")
		}

		// For now, just return the default value (0 or provided default)
		// A full implementation would try __len__ and __length_hint__ methods
		var defaultVal core.Value = core.NumberValue(0)
		if len(args) == 2 {
			defaultVal = args[1]
		}

		// Simple implementation: try to get __len__ if it's available
		if obj, ok := args[0].(core.Object); ok {
			if lenVal, hasLen := obj.GetAttr("__len__"); hasLen {
				if callable, isCallable := lenVal.(core.Callable); isCallable {
					result, err := callable.Call([]core.Value{}, ctx)
					if err == nil {
						return result, nil
					}
				}
			}
		}

		// Return default
		return defaultVal, nil
	}))

	return module
}

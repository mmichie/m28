package modules

import "github.com/mmichie/m28/core"

// InitOperatorModule creates the _operator module
// Note: Most operator functionality is in the pure Python operator.py module
// This C extension just provides a few optimized functions
func InitOperatorModule() *core.DictValue {
	module := core.NewDict()

	// Module docstring
	module.Set("__doc__", core.StringValue("Operator interface"))

	// length_hint(obj, default=0) - get length hint for iteration (PEP 424)
	module.Set("length_hint", core.NewNamedBuiltinFunction("length_hint", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 || len(args) > 2 {
			return nil, core.NewTypeError("length_hint", nil, "length_hint expected 1 or 2 arguments")
		}

		var defaultVal core.Value = core.NumberValue(0)
		if len(args) == 2 {
			defaultVal = args[1]
		}

		obj := args[0]

		// Helper interface for objects with GetAttr
		type hasGetAttr interface {
			GetAttr(name string) (core.Value, bool)
		}

		// Step 1: Try __len__ first (as per PEP 424)
		if hasLen, ok := obj.(hasGetAttr); ok {
			if lenVal, found := hasLen.GetAttr("__len__"); found {
				if callable, isCallable := lenVal.(core.Callable); isCallable {
					result, err := callable.Call([]core.Value{}, ctx)
					if err == nil {
						return result, nil
					}
				}
			}
		}

		// Step 2: Try __length_hint__ method
		if hasHint, ok := obj.(hasGetAttr); ok {
			if hintVal, found := hasHint.GetAttr("__length_hint__"); found {
				if callable, isCallable := hintVal.(core.Callable); isCallable {
					result, err := callable.Call([]core.Value{}, ctx)
					if err == nil {
						// Validate result is a non-negative integer
						if num, isNum := result.(core.NumberValue); isNum {
							if float64(num) >= 0 {
								return result, nil
							}
						}
					}
				}
			}
		}

		// Step 3: Return default
		return defaultVal, nil
	}))

	return module
}

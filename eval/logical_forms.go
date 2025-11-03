package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterLogicalForms registers logical operator special forms
func RegisterLogicalForms() {
	RegisterSpecialForm("and", AndForm)
	RegisterSpecialForm("or", OrForm)
}

// AndForm implements short-circuit 'and' evaluation
// Returns the first falsy value, or the last value if all are truthy
// (and) -> True
// (and expr) -> expr
// (and expr1 expr2 ...) -> first falsy or last value
func AndForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// and with no arguments returns True
	if args.Len() == 0 {
		return core.True, nil
	}

	// Evaluate arguments one by one with short-circuit
	var result core.Value = core.True
	for _, arg := range args.Items() {
		// Evaluate the argument
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}

		// Check truthiness
		var truthy bool
		if obj, ok := val.(core.Object); ok {
			if boolMethod, found := obj.GetAttr("__bool__"); found {
				if callable, ok := boolMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					boolResult, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}
					if boolVal, ok := boolResult.(core.BoolValue); ok {
						truthy = bool(boolVal)
					} else {
						return nil, &core.TypeError{
							Message: fmt.Sprintf("__bool__ should return bool, not %s", boolResult.Type()),
						}
					}
				} else {
					truthy = core.IsTruthy(val)
				}
			} else {
				truthy = core.IsTruthy(val)
			}
		} else {
			truthy = core.IsTruthy(val)
		}

		if !truthy {
			// Short-circuit: return the first falsy value
			return val, nil
		}
		result = val
	}

	// All values were truthy, return the last one
	return result, nil
}

// OrForm implements short-circuit 'or' evaluation
// Returns the first truthy value, or the last value if all are falsy
// (or) -> False
// (or expr) -> expr
// (or expr1 expr2 ...) -> first truthy or last value
func OrForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// or with no arguments returns False
	if args.Len() == 0 {
		return core.False, nil
	}

	// Evaluate arguments one by one with short-circuit
	var result core.Value = core.False
	for _, arg := range args.Items() {
		// Evaluate the argument
		val, err := Eval(arg, ctx)
		if err != nil {
			return nil, err
		}

		// Check truthiness
		var truthy bool
		if obj, ok := val.(core.Object); ok {
			if boolMethod, found := obj.GetAttr("__bool__"); found {
				if callable, ok := boolMethod.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					boolResult, err := callable.Call([]core.Value{}, ctx)
					if err != nil {
						return nil, err
					}
					if boolVal, ok := boolResult.(core.BoolValue); ok {
						truthy = bool(boolVal)
					} else {
						return nil, &core.TypeError{
							Message: fmt.Sprintf("__bool__ should return bool, not %s", boolResult.Type()),
						}
					}
				} else {
					truthy = core.IsTruthy(val)
				}
			} else {
				truthy = core.IsTruthy(val)
			}
		} else {
			truthy = core.IsTruthy(val)
		}

		if truthy {
			// Short-circuit: return the first truthy value
			return val, nil
		}
		result = val
	}

	// All values were falsy, return the last one
	return result, nil
}

package modules

import (
	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitOperatorModule creates and returns the operator module
func InitOperatorModule() *core.DictValue {
	operatorModule := core.NewDict()

	// Arithmetic operators
	operatorModule.Set("add", makeArithmeticOp("add", func(a, b float64) float64 { return a + b }))
	operatorModule.Set("sub", makeArithmeticOp("sub", func(a, b float64) float64 { return a - b }))
	operatorModule.Set("mul", makeArithmeticOp("mul", func(a, b float64) float64 { return a * b }))
	operatorModule.Set("truediv", makeArithmeticOp("truediv", func(a, b float64) float64 { return a / b }))
	operatorModule.Set("floordiv", makeArithmeticOp("floordiv", func(a, b float64) float64 {
		return float64(int(a / b))
	}))
	operatorModule.Set("mod", makeArithmeticOp("mod", func(a, b float64) float64 {
		return float64(int(a) % int(b))
	}))
	operatorModule.Set("pow", makeArithmeticOp("pow", func(a, b float64) float64 {
		result := 1.0
		for i := 0; i < int(b); i++ {
			result *= a
		}
		return result
	}))

	// Unary operators
	operatorModule.Set("neg", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("neg", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(-num), nil
	}))

	operatorModule.Set("pos", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("pos", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(num), nil
	}))

	operatorModule.Set("abs", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("abs", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		num, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		if num < 0 {
			return core.NumberValue(-num), nil
		}
		return core.NumberValue(num), nil
	}))

	// Comparison operators
	operatorModule.Set("eq", makeComparisonOp("eq", func(a, b core.Value) bool {
		return core.EqualValues(a, b)
	}))

	operatorModule.Set("ne", makeComparisonOp("ne", func(a, b core.Value) bool {
		return !core.EqualValues(a, b)
	}))

	operatorModule.Set("lt", makeNumComparisonOp("lt", func(a, b float64) bool { return a < b }))
	operatorModule.Set("le", makeNumComparisonOp("le", func(a, b float64) bool { return a <= b }))
	operatorModule.Set("gt", makeNumComparisonOp("gt", func(a, b float64) bool { return a > b }))
	operatorModule.Set("ge", makeNumComparisonOp("ge", func(a, b float64) bool { return a >= b }))

	// Logical operators
	operatorModule.Set("not_", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("not_", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.BoolValue(!core.IsTruthy(v.Get(0))), nil
	}))

	operatorModule.Set("truth", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("truth", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		return core.BoolValue(core.IsTruthy(v.Get(0))), nil
	}))

	operatorModule.Set("is_", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("is_", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// In Python, 'is' checks identity (same object)
		// Here we'll check if they're the exact same value
		return core.BoolValue(v.Get(0) == v.Get(1)), nil
	}))

	operatorModule.Set("is_not", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("is_not", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return core.BoolValue(v.Get(0) != v.Get(1)), nil
	}))

	// Sequence operators
	operatorModule.Set("concat", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("concat", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Handle lists
		if list1, ok := types.AsList(v.Get(0)); ok {
			if list2, ok := types.AsList(v.Get(1)); ok {
				result := make(core.ListValue, 0, len(list1)+len(list2))
				result = append(result, list1...)
				result = append(result, list2...)
				return result, nil
			}
		}

		// Handle strings
		if str1, ok := types.AsString(v.Get(0)); ok {
			if str2, ok := types.AsString(v.Get(1)); ok {
				return core.StringValue(str1 + str2), nil
			}
		}

		return nil, errors.NewRuntimeError("concat", "arguments must be sequences of the same type")
	}))

	operatorModule.Set("contains", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("contains", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		container := v.Get(0)
		item := v.Get(1)

		// Check if container supports __contains__
		if obj, ok := container.(interface {
			Contains(core.Value) (bool, error)
		}); ok {
			result, err := obj.Contains(item)
			if err != nil {
				return nil, err
			}
			return core.BoolValue(result), nil
		}

		// For lists, check manually
		if list, ok := types.AsList(container); ok {
			for _, elem := range list {
				if core.EqualValues(elem, item) {
					return core.BoolValue(true), nil
				}
			}
			return core.BoolValue(false), nil
		}

		return nil, errors.NewRuntimeError("contains", "first argument must be a container")
	}))

	operatorModule.Set("countOf", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("countOf", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		seq, err := types.RequireIterable(v.Get(0), "countOf() first argument")
		if err != nil {
			return nil, err
		}

		item := v.Get(1)
		count := 0

		iter := seq.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			if core.EqualValues(val, item) {
				count++
			}
		}

		return core.NumberValue(float64(count)), nil
	}))

	operatorModule.Set("indexOf", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("indexOf", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		seq, err := types.RequireIterable(v.Get(0), "indexOf() first argument")
		if err != nil {
			return nil, err
		}

		item := v.Get(1)
		index := 0

		iter := seq.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			if core.EqualValues(val, item) {
				return core.NumberValue(float64(index)), nil
			}
			index++
		}

		return nil, errors.NewRuntimeError("indexOf", "item not found in sequence")
	}))

	// Item access
	operatorModule.Set("getitem", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("getitem", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		obj := v.Get(0)
		key := v.Get(1)

		// Handle lists/tuples with numeric index
		if list, ok := types.AsList(obj); ok {
			index, err := v.GetNumber(1)
			if err != nil {
				return nil, err
			}
			idx := int(index)
			if idx < 0 || idx >= len(list) {
				return nil, errors.NewRuntimeError("getitem", "index out of range")
			}
			return list[idx], nil
		}

		// Handle dicts
		if dict, ok := obj.(*core.DictValue); ok {
			if keyStr, ok := types.AsString(key); ok {
				val, exists := dict.Get(keyStr)
				if !exists {
					return nil, errors.NewRuntimeError("getitem", "key not found")
				}
				return val, nil
			}
		}

		return nil, errors.NewRuntimeError("getitem", "unsupported type for item access")
	}))

	operatorModule.Set("setitem", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("setitem", args)
		if err := v.Exact(3); err != nil {
			return nil, err
		}

		obj := v.Get(0)
		key := v.Get(1)
		value := v.Get(2)

		// Handle dicts
		if dict, ok := obj.(*core.DictValue); ok {
			if keyStr, ok := types.AsString(key); ok {
				dict.Set(keyStr, value)
				return core.Nil, nil
			}
		}

		return nil, errors.NewRuntimeError("setitem", "unsupported type for item assignment")
	}))

	// Attribute access
	operatorModule.Set("attrgetter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("attrgetter", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		attrs := make([]string, v.Count())
		for i := 0; i < v.Count(); i++ {
			attr, err := v.GetString(i)
			if err != nil {
				return nil, err
			}
			attrs[i] = attr
		}

		// Return a function that gets these attributes
		getter := core.NewBuiltinFunction(func(callArgs []core.Value, callCtx *core.Context) (core.Value, error) {
			if len(callArgs) != 1 {
				return nil, errors.NewRuntimeError("attrgetter", "getter takes exactly one argument")
			}

			obj := callArgs[0]

			if len(attrs) == 1 {
				// Single attribute
				if objWithAttrs, ok := obj.(interface {
					GetAttr(string) (core.Value, error)
				}); ok {
					return objWithAttrs.GetAttr(attrs[0])
				}
				return nil, errors.NewRuntimeError("attrgetter", "object has no attributes")
			}

			// Multiple attributes - return tuple
			result := make(core.TupleValue, len(attrs))
			for i, attr := range attrs {
				if objWithAttrs, ok := obj.(interface {
					GetAttr(string) (core.Value, error)
				}); ok {
					val, err := objWithAttrs.GetAttr(attr)
					if err != nil {
						return nil, err
					}
					result[i] = val
				} else {
					return nil, errors.NewRuntimeError("attrgetter", "object has no attributes")
				}
			}

			return result, nil
		})

		return getter, nil
	}))

	operatorModule.Set("itemgetter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("itemgetter", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		keys := make([]core.Value, v.Count())
		for i := 0; i < v.Count(); i++ {
			keys[i] = v.Get(i)
		}

		// Return a function that gets these items
		getter := core.NewBuiltinFunction(func(callArgs []core.Value, callCtx *core.Context) (core.Value, error) {
			if len(callArgs) != 1 {
				return nil, errors.NewRuntimeError("itemgetter", "getter takes exactly one argument")
			}

			obj := callArgs[0]

			if len(keys) == 1 {
				// Single key
				if list, ok := types.AsList(obj); ok {
					if idx, ok := types.AsNumber(keys[0]); ok {
						index := int(idx)
						if index < 0 || index >= len(list) {
							return nil, errors.NewRuntimeError("itemgetter", "index out of range")
						}
						return list[index], nil
					}
				}
				if dict, ok := obj.(*core.DictValue); ok {
					if keyStr, ok := types.AsString(keys[0]); ok {
						val, exists := dict.Get(keyStr)
						if !exists {
							return nil, errors.NewRuntimeError("itemgetter", "key not found")
						}
						return val, nil
					}
				}
				return nil, errors.NewRuntimeError("itemgetter", "unsupported type")
			}

			// Multiple keys - return tuple
			result := make(core.TupleValue, len(keys))
			for i, key := range keys {
				if list, ok := types.AsList(obj); ok {
					if idx, ok := types.AsNumber(key); ok {
						index := int(idx)
						if index < 0 || index >= len(list) {
							return nil, errors.NewRuntimeError("itemgetter", "index out of range")
						}
						result[i] = list[index]
					}
				} else if dict, ok := obj.(*core.DictValue); ok {
					if keyStr, ok := types.AsString(key); ok {
						val, exists := dict.Get(keyStr)
						if !exists {
							return nil, errors.NewRuntimeError("itemgetter", "key not found")
						}
						result[i] = val
					}
				}
			}

			return result, nil
		})

		return getter, nil
	}))

	return operatorModule
}

// makeArithmeticOp creates a binary arithmetic operator function
func makeArithmeticOp(name string, op func(float64, float64) float64) *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		a, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		b, err := v.GetNumber(1)
		if err != nil {
			return nil, err
		}

		return core.NumberValue(op(a, b)), nil
	})
}

// makeComparisonOp creates a comparison operator function
func makeComparisonOp(name string, op func(core.Value, core.Value) bool) *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		return core.BoolValue(op(v.Get(0), v.Get(1))), nil
	})
}

// makeNumComparisonOp creates a numeric comparison operator function
func makeNumComparisonOp(name string, op func(float64, float64) bool) *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs(name, args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		a, err := v.GetNumber(0)
		if err != nil {
			return nil, err
		}

		b, err := v.GetNumber(1)
		if err != nil {
			return nil, err
		}

		return core.BoolValue(op(a, b)), nil
	})
}

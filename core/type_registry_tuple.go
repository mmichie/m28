package core

import (
	"fmt"
	"strings"
)

// registerTupleType registers the tuple type descriptor with all its methods
func registerTupleType() {
	RegisterType(&TypeDescriptor{
		Name:       "tuple",
		PythonName: "tuple",
		BaseType:   TupleType,
		Methods:    getTupleMethods(),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return TupleValue{}, nil
			}
			if len(args) == 1 {
				// Convert iterable to tuple
				arg := args[0]
				if tuple, ok := arg.(TupleValue); ok {
					// Make a copy
					result := make(TupleValue, len(tuple))
					copy(result, tuple)
					return result, nil
				}
				if list, ok := arg.(*ListValue); ok {
					result := make(TupleValue, list.Len())
					copy(result, list.Items())
					return result, nil
				}
				// Try to iterate
				if iterable, ok := arg.(Iterable); ok {
					var result TupleValue
					iter := iterable.Iterator()
					for {
						val, ok := iter.Next()
						if !ok {
							break
						}
						result = append(result, val)
					}
					return result, nil
				}
				return nil, fmt.Errorf("tuple() argument must be an iterable")
			}
			return nil, fmt.Errorf("tuple() takes at most 1 argument (%d given)", len(args))
		},
		Repr: func(v Value) string {
			tuple := v.(TupleValue)
			if len(tuple) == 0 {
				return "()"
			}
			if len(tuple) == 1 {
				// Single element tuple needs trailing comma
				return "(" + Repr(tuple[0]) + ",)"
			}
			elements := make([]string, len(tuple))
			for i, elem := range tuple {
				elements[i] = Repr(elem)
			}
			return "(" + strings.Join(elements, ", ") + ")"
		},
		Str: func(v Value) string {
			tuple := v.(TupleValue)
			if len(tuple) == 0 {
				return "()"
			}
			if len(tuple) == 1 {
				// Single element tuple needs trailing comma
				return "(" + Repr(tuple[0]) + ",)"
			}
			elements := make([]string, len(tuple))
			for i, elem := range tuple {
				elements[i] = Repr(elem)
			}
			return "(" + strings.Join(elements, ", ") + ")"
		},
		Doc: "tuple() -> empty tuple\ntuple(iterable) -> tuple initialized from iterable's items",
	})
}

// getTupleMethods returns all tuple methods
func getTupleMethods() map[string]*MethodDescriptor {
	return map[string]*MethodDescriptor{
		"count": {
			Name:    "count",
			Arity:   1,
			Doc:     "Return number of occurrences of value",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: fmt.Sprintf("count() takes exactly one argument (%d given)", len(args))}
				}

				tuple := receiver.(TupleValue)
				count := 0
				for _, item := range tuple {
					if EqualValues(item, args[0]) {
						count++
					}
				}
				return NumberValue(count), nil
			},
		},
		"index": {
			Name:    "index",
			Arity:   -1,
			Doc:     "Return index of first occurrence of value",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) < 1 {
					return nil, &TypeError{Message: "index() takes at least 1 argument (0 given)"}
				}

				tuple := receiver.(TupleValue)
				start, stop := 0, len(tuple)

				if len(args) > 1 {
					if s, ok := args[1].(NumberValue); ok {
						start = int(s)
						if start < 0 {
							start = len(tuple) + start
						}
					}
				}

				if len(args) > 2 {
					if s, ok := args[2].(NumberValue); ok {
						stop = int(s)
						if stop < 0 {
							stop = len(tuple) + stop
						}
					}
				}

				for i := start; i < stop && i < len(tuple); i++ {
					if EqualValues(tuple[i], args[0]) {
						return NumberValue(i), nil
					}
				}
				return nil, &ValueError{Message: fmt.Sprintf("%v is not in tuple", args[0])}
			},
		},
		"__len__": {
			Name:    "__len__",
			Arity:   0,
			Doc:     "Return the length of the tuple",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				tuple := receiver.(TupleValue)
				return NumberValue(len(tuple)), nil
			},
		},
		"__getitem__": {
			Name:    "__getitem__",
			Arity:   1,
			Doc:     "Get item by index or slice",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: "__getitem__ takes exactly one argument"}
				}

				tuple := receiver.(TupleValue)

				// Handle slice
				if slice, ok := args[0].(*SliceValue); ok {
					start, stop, step, err := NormalizeSliceIndices(slice, len(tuple))
					if err != nil {
						return nil, err
					}

					// Extract slice
					var result TupleValue
					if step > 0 {
						for i := start; i < stop && i < len(tuple); i += step {
							result = append(result, tuple[i])
						}
					} else {
						// Negative step
						if start >= len(tuple) {
							start = len(tuple) - 1
						}
						for i := start; i > stop && i >= 0; i += step {
							result = append(result, tuple[i])
						}
					}

					return result, nil
				}

				// Handle index
				idx, ok := args[0].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "tuple indices must be integers"}
				}

				i, err := NormalizeIndex(int(idx), len(tuple))
				if err != nil {
					return nil, err
				}

				return tuple[i], nil
			},
		},
		"__contains__": {
			Name:    "__contains__",
			Arity:   1,
			Doc:     "Check if value is in tuple",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: "__contains__ takes exactly one argument"}
				}
				tuple := receiver.(TupleValue)
				for _, item := range tuple {
					if EqualValues(item, args[0]) {
						return True, nil
					}
				}
				return False, nil
			},
		},
		"__add__": {
			Name:    "__add__",
			Arity:   1,
			Doc:     "Concatenate two tuples",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: "__add__ takes exactly one argument"}
				}
				tuple1 := receiver.(TupleValue)
				tuple2, ok := args[0].(TupleValue)
				if !ok {
					return nil, &TypeError{Message: "can only concatenate tuple to tuple"}
				}
				result := make(TupleValue, len(tuple1)+len(tuple2))
				copy(result, tuple1)
				copy(result[len(tuple1):], tuple2)
				return result, nil
			},
		},
		"__mul__": {
			Name:    "__mul__",
			Arity:   1,
			Doc:     "Repeat tuple n times",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: "__mul__ takes exactly one argument"}
				}
				tuple := receiver.(TupleValue)
				n, ok := args[0].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "can't multiply sequence by non-int"}
				}
				count := int(n)
				if count <= 0 {
					return TupleValue{}, nil
				}
				result := make(TupleValue, 0, len(tuple)*count)
				for i := 0; i < count; i++ {
					result = append(result, tuple...)
				}
				return result, nil
			},
		},
		"__iter__": {
			Name:    "__iter__",
			Arity:   0,
			Doc:     "Return an iterator for the tuple",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				tuple := receiver.(TupleValue)
				// Per Python iterator protocol: return an iterator object with __iter__ and __next__
				// Convert to list and get its iterator
				list := NewList(tuple...)
				if iter, ok := list.GetAttr("__iter__"); ok {
					if callable, ok := iter.(interface {
						Call([]Value, *Context) (Value, error)
					}); ok {
						return callable.Call([]Value{}, ctx)
					}
				}
				// Fallback: return the list (shouldn't happen)
				return list, nil
			},
		},
		"__hash__": {
			Name:    "__hash__",
			Arity:   0,
			Doc:     "Return hash of tuple",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				// For now, just return 0
				// TODO(M28-b902): Implement proper hashing
				return NumberValue(0), nil
			},
		},
		// Note: __reduce_ex__ removed for tuple
		// CPython's tuple doesn't have __reduce_ex__ - pickle handles it with save_tuple
		// M28 should let pickle's save_tuple handle tuples instead of using __reduce_ex__
	}
}

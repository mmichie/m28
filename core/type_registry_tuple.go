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
					return nil, fmt.Errorf("count() takes exactly one argument")
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
					return nil, fmt.Errorf("index() takes at least 1 argument")
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
				return nil, fmt.Errorf("%v is not in tuple", args[0])
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
					return nil, fmt.Errorf("__getitem__ takes exactly one argument")
				}

				tuple := receiver.(TupleValue)

				// Handle slice
				if slice, ok := args[0].(*SliceValue); ok {
					start, stop, step := 0, len(tuple), 1

					if slice.Start != nil && slice.Start != Nil {
						if n, ok := slice.Start.(NumberValue); ok {
							start = int(n)
							if start < 0 {
								start = len(tuple) + start
							}
							if start < 0 {
								start = 0
							}
						}
					}

					if slice.Stop != nil && slice.Stop != Nil {
						if n, ok := slice.Stop.(NumberValue); ok {
							stop = int(n)
							if stop < 0 {
								stop = len(tuple) + stop
							}
						}
					}

					if slice.Step != nil && slice.Step != Nil {
						if n, ok := slice.Step.(NumberValue); ok {
							step = int(n)
							if step == 0 {
								return nil, fmt.Errorf("slice step cannot be zero")
							}
						}
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
					return nil, fmt.Errorf("tuple indices must be integers")
				}

				i := int(idx)
				if i < 0 {
					i = len(tuple) + i
				}

				if i < 0 || i >= len(tuple) {
					return nil, &IndexError{Index: i, Length: len(tuple)}
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
					return nil, fmt.Errorf("__contains__ takes exactly one argument")
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
					return nil, fmt.Errorf("__add__ takes exactly one argument")
				}
				tuple1 := receiver.(TupleValue)
				tuple2, ok := args[0].(TupleValue)
				if !ok {
					return nil, fmt.Errorf("can only concatenate tuple to tuple")
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
					return nil, fmt.Errorf("__mul__ takes exactly one argument")
				}
				tuple := receiver.(TupleValue)
				n, ok := args[0].(NumberValue)
				if !ok {
					return nil, fmt.Errorf("can't multiply sequence by non-int")
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
				// Return the tuple itself as it implements Iterable
				return tuple, nil
			},
		},
		"__hash__": {
			Name:    "__hash__",
			Arity:   0,
			Doc:     "Return hash of tuple",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				// For now, just return 0
				// TODO: Implement proper hashing
				return NumberValue(0), nil
			},
		},
	}
}

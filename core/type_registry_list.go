package core

import (
	"fmt"
	"sort"
	"strings"
)

// registerListType registers the list type descriptor with all its methods
func registerListType() {
	RegisterType(&TypeDescriptor{
		Name:       "list",
		PythonName: "list",
		BaseType:   ListType,
		Methods:    getListMethods(),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return ListValue{}, nil
			}
			if len(args) == 1 {
				// Convert iterable to list
				arg := args[0]
				if list, ok := arg.(ListValue); ok {
					// Make a copy
					result := make(ListValue, len(list))
					copy(result, list)
					return result, nil
				}
				if tuple, ok := arg.(TupleValue); ok {
					result := make(ListValue, len(tuple))
					copy(result, []Value(tuple))
					return result, nil
				}
				// Try to iterate
				if iterable, ok := arg.(Iterable); ok {
					var result ListValue
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
				return nil, fmt.Errorf("list() argument must be an iterable")
			}
			return nil, fmt.Errorf("list() takes at most 1 argument (%d given)", len(args))
		},
		Repr: func(v Value) string {
			list := v.(ListValue)
			if len(list) == 0 {
				return "[]"
			}
			elements := make([]string, len(list))
			for i, elem := range list {
				elements[i] = Repr(elem)
			}
			return "[" + strings.Join(elements, ", ") + "]"
		},
		Str: func(v Value) string {
			list := v.(ListValue)
			if len(list) == 0 {
				return "[]"
			}
			elements := make([]string, len(list))
			for i, elem := range list {
				elements[i] = Repr(elem)
			}
			return "[" + strings.Join(elements, ", ") + "]"
		},
		Doc: "list() -> new empty list\nlist(iterable) -> new list initialized from iterable's items",
	})
}

// getListMethods returns all list methods
func getListMethods() map[string]*MethodDescriptor {
	return map[string]*MethodDescriptor{
		"append": {
			Name:    "append",
			Arity:   1,
			Doc:     "Append an element to the list (returns new list)",
			Builtin: true,
			Handler: listMethodAppend,
		},
		"extend": {
			Name:    "extend",
			Arity:   1,
			Doc:     "Extend list by appending elements from another list",
			Builtin: true,
			Handler: listMethodExtend,
		},
		"insert": {
			Name:    "insert",
			Arity:   2,
			Doc:     "Insert an element at a given position",
			Builtin: true,
			Handler: listMethodInsert,
		},
		"remove": {
			Name:    "remove",
			Arity:   1,
			Doc:     "Remove first occurrence of value",
			Builtin: true,
			Handler: listMethodRemove,
		},
		"pop": {
			Name:    "pop",
			Arity:   -1,
			Doc:     "Remove and return element at index (default last)",
			Builtin: true,
			Handler: listMethodPop,
		},
		"clear": {
			Name:    "clear",
			Arity:   0,
			Doc:     "Remove all elements from list",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				return ListValue{}, nil
			},
		},
		"index": {
			Name:    "index",
			Arity:   -1,
			Doc:     "Return index of first occurrence of value",
			Builtin: true,
			Handler: listMethodIndex,
		},
		"count": {
			Name:    "count",
			Arity:   1,
			Doc:     "Return number of occurrences of value",
			Builtin: true,
			Handler: listMethodCount,
		},
		"sort": {
			Name:    "sort",
			Arity:   0,
			Doc:     "Sort the list in place",
			Builtin: true,
			Handler: listMethodSort,
		},
		"reverse": {
			Name:    "reverse",
			Arity:   0,
			Doc:     "Reverse the list in place",
			Builtin: true,
			Handler: listMethodReverse,
		},
		"copy": {
			Name:    "copy",
			Arity:   0,
			Doc:     "Return a shallow copy of the list",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(ListValue)
				result := make(ListValue, len(list))
				copy(result, list)
				return result, nil
			},
		},
		"__len__": {
			Name:    "__len__",
			Arity:   0,
			Doc:     "Return the length of the list",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(ListValue)
				return NumberValue(len(list)), nil
			},
		},
		"__getitem__": {
			Name:    "__getitem__",
			Arity:   1,
			Doc:     "Get item by index or slice",
			Builtin: true,
			Handler: listMethodGetItem,
		},
		"__setitem__": {
			Name:    "__setitem__",
			Arity:   2,
			Doc:     "Set item by index",
			Builtin: true,
			Handler: listMethodSetItem,
		},
		"__delitem__": {
			Name:    "__delitem__",
			Arity:   1,
			Doc:     "Delete item by index",
			Builtin: true,
			Handler: listMethodDelItem,
		},
		"__contains__": {
			Name:    "__contains__",
			Arity:   1,
			Doc:     "Check if value is in list",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__contains__ takes exactly one argument")
				}
				list := receiver.(ListValue)
				for _, item := range list {
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
			Doc:     "Concatenate two lists",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__add__ takes exactly one argument")
				}
				list1 := receiver.(ListValue)
				list2, ok := args[0].(ListValue)
				if !ok {
					return nil, fmt.Errorf("can only concatenate list to list")
				}
				result := make(ListValue, len(list1)+len(list2))
				copy(result, list1)
				copy(result[len(list1):], list2)
				return result, nil
			},
		},
		"__mul__": {
			Name:    "__mul__",
			Arity:   1,
			Doc:     "Repeat list n times",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__mul__ takes exactly one argument")
				}
				list := receiver.(ListValue)
				n, ok := args[0].(NumberValue)
				if !ok {
					return nil, fmt.Errorf("can't multiply sequence by non-int")
				}
				count := int(n)
				if count <= 0 {
					return ListValue{}, nil
				}
				result := make(ListValue, 0, len(list)*count)
				for i := 0; i < count; i++ {
					result = append(result, list...)
				}
				return result, nil
			},
		},
		"__iter__": {
			Name:    "__iter__",
			Arity:   0,
			Doc:     "Return an iterator for the list",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(ListValue)
				// Return the list itself as it implements Iterable
				return list, nil
			},
		},
	}
}

// Helper methods for list operations

func listMethodAppend(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("append() takes exactly one argument")
	}
	list := receiver.(ListValue)
	newList := make(ListValue, len(list)+1)
	copy(newList, list)
	newList[len(list)] = args[0]
	return newList, nil
}

func listMethodExtend(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("extend() takes exactly one argument")
	}
	list := receiver.(ListValue)

	// Handle different iterable types
	var toExtend []Value
	switch v := args[0].(type) {
	case ListValue:
		toExtend = v
	case TupleValue:
		toExtend = v
	case StringValue:
		// String is iterable, each character becomes an element
		s := string(v)
		toExtend = make([]Value, len(s))
		for i, ch := range s {
			toExtend[i] = StringValue(string(ch))
		}
	default:
		if iterable, ok := args[0].(Iterable); ok {
			iter := iterable.Iterator()
			for {
				val, ok := iter.Next()
				if !ok {
					break
				}
				toExtend = append(toExtend, val)
			}
		} else {
			return nil, fmt.Errorf("extend() argument must be iterable")
		}
	}

	result := make(ListValue, len(list)+len(toExtend))
	copy(result, list)
	copy(result[len(list):], toExtend)
	return result, nil
}

func listMethodInsert(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("insert() takes exactly 2 arguments")
	}

	list := receiver.(ListValue)
	idxVal, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("insert() first argument must be an integer")
	}

	idx := int(idxVal)
	if idx < 0 {
		idx = len(list) + idx
		if idx < 0 {
			idx = 0
		}
	}
	if idx > len(list) {
		idx = len(list)
	}

	result := make(ListValue, len(list)+1)
	copy(result[:idx], list[:idx])
	result[idx] = args[1]
	copy(result[idx+1:], list[idx:])
	return result, nil
}

func listMethodRemove(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("remove() takes exactly one argument")
	}

	list := receiver.(ListValue)
	for i, item := range list {
		if EqualValues(item, args[0]) {
			result := make(ListValue, len(list)-1)
			copy(result[:i], list[:i])
			copy(result[i:], list[i+1:])
			return result, nil
		}
	}
	return nil, fmt.Errorf("list.remove(x): x not in list")
}

func listMethodPop(receiver Value, args []Value, ctx *Context) (Value, error) {
	list := receiver.(ListValue)
	if len(list) == 0 {
		return nil, fmt.Errorf("pop from empty list")
	}

	idx := len(list) - 1
	if len(args) > 0 {
		idxVal, ok := args[0].(NumberValue)
		if !ok {
			return nil, fmt.Errorf("pop() argument must be an integer")
		}
		idx = int(idxVal)
		if idx < 0 {
			idx = len(list) + idx
		}
		if idx < 0 || idx >= len(list) {
			return nil, &IndexError{Index: idx, Length: len(list)}
		}
	}

	// Return the popped element
	// Note: In a functional style, we'd return both the element and new list
	return list[idx], nil
}

func listMethodIndex(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("index() takes at least 1 argument")
	}

	list := receiver.(ListValue)
	start, stop := 0, len(list)

	if len(args) > 1 {
		if s, ok := args[1].(NumberValue); ok {
			start = int(s)
			if start < 0 {
				start = len(list) + start
			}
		}
	}

	if len(args) > 2 {
		if s, ok := args[2].(NumberValue); ok {
			stop = int(s)
			if stop < 0 {
				stop = len(list) + stop
			}
		}
	}

	for i := start; i < stop && i < len(list); i++ {
		if EqualValues(list[i], args[0]) {
			return NumberValue(i), nil
		}
	}
	return nil, fmt.Errorf("%v is not in list", args[0])
}

func listMethodCount(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("count() takes exactly one argument")
	}

	list := receiver.(ListValue)
	count := 0
	for _, item := range list {
		if EqualValues(item, args[0]) {
			count++
		}
	}
	return NumberValue(count), nil
}

func listMethodSort(receiver Value, args []Value, ctx *Context) (Value, error) {
	list := receiver.(ListValue)
	result := make(ListValue, len(list))
	copy(result, list)

	// Simple sort using Go's sort package
	sort.Slice(result, func(i, j int) bool {
		// TODO: Implement proper comparison
		// For now, compare as strings
		return Repr(result[i]) < Repr(result[j])
	})

	return result, nil
}

func listMethodReverse(receiver Value, args []Value, ctx *Context) (Value, error) {
	list := receiver.(ListValue)
	result := make(ListValue, len(list))
	for i, j := 0, len(list)-1; i < len(list); i, j = i+1, j-1 {
		result[i] = list[j]
	}
	return result, nil
}

func listMethodGetItem(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("__getitem__ takes exactly one argument")
	}

	list := receiver.(ListValue)

	// Handle slice
	if slice, ok := args[0].(*SliceValue); ok {
		start, stop, step := 0, len(list), 1

		if slice.Start != nil && slice.Start != Nil {
			if n, ok := slice.Start.(NumberValue); ok {
				start = int(n)
				if start < 0 {
					start = len(list) + start
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
					stop = len(list) + stop
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
		var result ListValue
		if step > 0 {
			for i := start; i < stop && i < len(list); i += step {
				result = append(result, list[i])
			}
		} else {
			// Negative step
			if start >= len(list) {
				start = len(list) - 1
			}
			for i := start; i > stop && i >= 0; i += step {
				result = append(result, list[i])
			}
		}

		return result, nil
	}

	// Handle index
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("list indices must be integers")
	}

	i := int(idx)
	if i < 0 {
		i = len(list) + i
	}

	if i < 0 || i >= len(list) {
		return nil, &IndexError{Index: i, Length: len(list)}
	}

	return list[i], nil
}

func listMethodSetItem(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("__setitem__ takes exactly 2 arguments")
	}

	list := receiver.(ListValue)
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("list indices must be integers")
	}

	i := int(idx)
	if i < 0 {
		i = len(list) + i
	}

	if i < 0 || i >= len(list) {
		return nil, &IndexError{Index: i, Length: len(list)}
	}

	// Lists are immutable in M28
	return nil, fmt.Errorf("'list' object does not support item assignment")
}

func listMethodDelItem(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("__delitem__ takes exactly one argument")
	}

	list := receiver.(ListValue)
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("list indices must be integers")
	}

	i := int(idx)
	if i < 0 {
		i = len(list) + i
	}

	if i < 0 || i >= len(list) {
		return nil, &IndexError{Index: i, Length: len(list)}
	}

	// Lists are immutable in M28
	return nil, fmt.Errorf("'list' object does not support item deletion")
}

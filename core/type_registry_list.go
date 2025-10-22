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
				return NewList(), nil
			}
			if len(args) == 1 {
				// Convert iterable to list
				arg := args[0]
				if list, ok := arg.(*ListValue); ok {
					// Make a copy
					return NewList(list.Items()...), nil
				}
				if tuple, ok := arg.(TupleValue); ok {
					return NewList(tuple...), nil
				}
				// Try to iterate
				if iterable, ok := arg.(Iterable); ok {
					result := NewList()
					iter := iterable.Iterator()
					for {
						val, ok := iter.Next()
						if !ok {
							break
						}
						result.Append(val)
					}
					return result, nil
				}
				return nil, fmt.Errorf("list() argument must be an iterable")
			}
			return nil, fmt.Errorf("list() takes at most 1 argument (%d given)", len(args))
		},
		Repr: func(v Value) string {
			list := v.(*ListValue)
			if list.Len() == 0 {
				return "[]"
			}
			elements := make([]string, list.Len())
			for i, elem := range list.Items() {
				elements[i] = Repr(elem)
			}
			return "[" + strings.Join(elements, ", ") + "]"
		},
		Str: func(v Value) string {
			list := v.(*ListValue)
			if list.Len() == 0 {
				return "[]"
			}
			elements := make([]string, list.Len())
			for i, elem := range list.Items() {
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
				return NewList(), nil
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
				list := receiver.(*ListValue)
				result := make([]Value, list.Len())
				copy(result, list.Items())
				return NewList(result...), nil
			},
		},
		"__len__": {
			Name:    "__len__",
			Arity:   0,
			Doc:     "Return the length of the list",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(*ListValue)
				return NumberValue(list.Len()), nil
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
				list := receiver.(*ListValue)
				for _, item := range list.Items() {
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
				list1 := receiver.(*ListValue)
				list2, ok := args[0].(*ListValue)
				if !ok {
					return nil, fmt.Errorf("can only concatenate list to list")
				}
				result := make([]Value, list1.Len()+list2.Len())
				copy(result, list1.Items())
				copy(result[list1.Len():], list2.Items())
				return NewList(result...), nil
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
				list := receiver.(*ListValue)
				n, ok := args[0].(NumberValue)
				if !ok {
					return nil, fmt.Errorf("can't multiply sequence by non-int")
				}
				count := int(n)
				if count <= 0 {
					return NewList(), nil
				}
				result := make([]Value, 0, list.Len()*count)
				for i := 0; i < count; i++ {
					result = append(result, list.Items()...)
				}
				return NewList(result...), nil
			},
		},
		"__iter__": {
			Name:    "__iter__",
			Arity:   0,
			Doc:     "Return an iterator for the list",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(*ListValue)
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
	list := receiver.(*ListValue)
	list.Append(args[0])
	return None, nil
}

func listMethodExtend(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("extend() takes exactly one argument")
	}
	list := receiver.(*ListValue)

	// Handle different iterable types
	var toExtend []Value
	switch v := args[0].(type) {
	case *ListValue:
		toExtend = v.Items()
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

	list.Extend(toExtend)
	return None, nil
}

func listMethodInsert(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("insert() takes exactly 2 arguments")
	}

	list := receiver.(*ListValue)
	idxVal, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("insert() first argument must be an integer")
	}

	idx := int(idxVal)
	if idx < 0 {
		idx = list.Len() + idx
		if idx < 0 {
			idx = 0
		}
	}
	if idx > list.Len() {
		idx = list.Len()
	}

	// Mutate the list in place
	newItems := make([]Value, list.Len()+1)
	copy(newItems[:idx], list.Items()[:idx])
	newItems[idx] = args[1]
	copy(newItems[idx+1:], list.Items()[idx:])
	list.items = newItems

	return None, nil
}

func listMethodRemove(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("remove() takes exactly one argument")
	}

	list := receiver.(*ListValue)
	for i, item := range list.Items() {
		if EqualValues(item, args[0]) {
			// Mutate the list by removing the item
			list.items = append(list.items[:i], list.items[i+1:]...)
			return None, nil
		}
	}
	return nil, fmt.Errorf("list.remove(x): x not in list")
}

func listMethodPop(receiver Value, args []Value, ctx *Context) (Value, error) {
	list := receiver.(*ListValue)
	if list.Len() == 0 {
		return nil, &IndexError{Index: -1, Length: 0}
	}

	idx := list.Len() - 1
	if len(args) > 0 {
		idxVal, ok := args[0].(NumberValue)
		if !ok {
			return nil, fmt.Errorf("pop() argument must be an integer")
		}
		idx = int(idxVal)
		if idx < 0 {
			idx = list.Len() + idx
		}
		if idx < 0 || idx >= list.Len() {
			return nil, &IndexError{Index: idx, Length: list.Len()}
		}
	}

	// Get the element to return
	element := list.Items()[idx]

	// Mutate the list by removing the element
	list.items = append(list.items[:idx], list.items[idx+1:]...)

	return element, nil
}

func listMethodIndex(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("index() takes at least 1 argument")
	}

	list := receiver.(*ListValue)
	start, stop := 0, list.Len()

	if len(args) > 1 {
		if s, ok := args[1].(NumberValue); ok {
			start = int(s)
			if start < 0 {
				start = list.Len() + start
			}
		}
	}

	if len(args) > 2 {
		if s, ok := args[2].(NumberValue); ok {
			stop = int(s)
			if stop < 0 {
				stop = list.Len() + stop
			}
		}
	}

	items := list.Items()
	for i := start; i < stop && i < list.Len(); i++ {
		if EqualValues(items[i], args[0]) {
			return NumberValue(i), nil
		}
	}
	return nil, fmt.Errorf("%v is not in list", args[0])
}

func listMethodCount(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("count() takes exactly one argument")
	}

	list := receiver.(*ListValue)
	count := 0
	for _, item := range list.Items() {
		if EqualValues(item, args[0]) {
			count++
		}
	}
	return NumberValue(count), nil
}

func listMethodSort(receiver Value, args []Value, ctx *Context) (Value, error) {
	list := receiver.(*ListValue)

	// Sort the items in place using Go's sort package
	sort.Slice(list.items, func(i, j int) bool {
		// TODO: Implement proper comparison
		// For now, compare as strings
		return Repr(list.items[i]) < Repr(list.items[j])
	})

	return None, nil
}

func listMethodReverse(receiver Value, args []Value, ctx *Context) (Value, error) {
	list := receiver.(*ListValue)
	// Reverse the items in place
	for i, j := 0, list.Len()-1; i < j; i, j = i+1, j-1 {
		list.items[i], list.items[j] = list.items[j], list.items[i]
	}
	return None, nil
}

func listMethodGetItem(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("__getitem__ takes exactly one argument")
	}

	list := receiver.(*ListValue)

	// Handle slice
	if slice, ok := args[0].(*SliceValue); ok {
		start, stop, step := 0, list.Len(), 1

		if slice.Start != nil && slice.Start != Nil {
			if n, ok := slice.Start.(NumberValue); ok {
				start = int(n)
				if start < 0 {
					start = list.Len() + start
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
					stop = list.Len() + stop
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
		var result []Value
		items := list.Items()
		if step > 0 {
			for i := start; i < stop && i < list.Len(); i += step {
				result = append(result, items[i])
			}
		} else {
			// Negative step
			if start >= list.Len() {
				start = list.Len() - 1
			}
			for i := start; i > stop && i >= 0; i += step {
				result = append(result, items[i])
			}
		}

		return NewList(result...), nil
	}

	// Handle index - use ToIndex to support __index__ dunder method
	i, err := toIndex(args[0], ctx)
	if err != nil {
		return nil, err
	}
	if i < 0 {
		i = list.Len() + i
	}

	if i < 0 || i >= list.Len() {
		return nil, &IndexError{Index: i, Length: list.Len()}
	}

	return list.Items()[i], nil
}

func listMethodSetItem(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("__setitem__ takes exactly 2 arguments")
	}

	list := receiver.(*ListValue)
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("list indices must be integers")
	}

	i := int(idx)
	if i < 0 {
		i = list.Len() + i
	}

	if i < 0 || i >= list.Len() {
		return nil, &IndexError{Index: i, Length: list.Len()}
	}

	// Mutate the list by setting the item
	err := list.SetItem(i, args[1])
	if err != nil {
		return nil, err
	}

	return None, nil
}

func listMethodDelItem(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("__delitem__ takes exactly one argument")
	}

	list := receiver.(*ListValue)
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, fmt.Errorf("list indices must be integers")
	}

	i := int(idx)
	if i < 0 {
		i = list.Len() + i
	}

	if i < 0 || i >= list.Len() {
		return nil, &IndexError{Index: i, Length: list.Len()}
	}

	// Mutate the list by removing the item
	list.items = append(list.items[:i], list.items[i+1:]...)

	return None, nil
}

// toIndex converts a value to an integer index using __index__ if available
// This is a helper for list indexing operations
func toIndex(obj Value, ctx *Context) (int, error) {
	// First try __index__ dunder method
	if attrObj, ok := obj.(interface{ GetAttr(string) (Value, bool) }); ok {
		if indexMethod, exists := attrObj.GetAttr("__index__"); exists {
			if callable, ok := indexMethod.(interface {
				Call([]Value, *Context) (Value, error)
			}); ok {
				result, err := callable.Call([]Value{}, ctx)
				if err != nil {
					return 0, err
				}
				// Ensure result is an integer
				num, ok := result.(NumberValue)
				if !ok {
					return 0, fmt.Errorf("__index__ returned non-int type %s", result.Type())
				}
				intVal := int(num)
				if float64(intVal) != float64(num) {
					return 0, fmt.Errorf("__index__ returned non-integer value %v", num)
				}
				return intVal, nil
			}
		}
	}

	// Fall back to NumberValue for built-in numeric types
	if num, ok := obj.(NumberValue); ok {
		intVal := int(num)
		if float64(intVal) != float64(num) {
			return 0, fmt.Errorf("list indices must be integers, not float")
		}
		return intVal, nil
	}

	// Not convertible to index
	return 0, fmt.Errorf("list indices must be integers, not %s", obj.Type())
}

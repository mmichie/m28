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
				return nil, &TypeError{Message: "list() argument must be an iterable"}
			}
			return nil, &TypeError{Message: fmt.Sprintf("list() takes at most 1 argument (%d given)", len(args))}
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
			Name:         "sort",
			Arity:        0,
			Doc:          "Sort the list in place",
			Builtin:      true,
			Handler:      listMethodSort,
			KwargHandler: listMethodSortWithKwargs,
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
		"get": {
			Name:    "get",
			Arity:   1,
			Doc:     "Get item by index (M28 convenience method)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(*ListValue)
				if len(args) != 1 {
					return nil, &TypeError{Message: fmt.Sprintf("get() takes exactly 1 argument (%d given)", len(args))}
				}
				idx, ok := args[0].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "list indices must be integers"}
				}
				index := int(idx)
				if index < 0 {
					index = list.Len() + index
				}
				if index < 0 || index >= list.Len() {
					return nil, &IndexError{Index: int(idx), Length: list.Len()}
				}
				return list.Items()[index], nil
			},
		},
		"contains": {
			Name:    "contains",
			Arity:   1,
			Doc:     "Check if list contains a value (M28 convenience method)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(*ListValue)
				if len(args) != 1 {
					return nil, &TypeError{Message: fmt.Sprintf("contains() takes exactly 1 argument (%d given)", len(args))}
				}
				for _, item := range list.Items() {
					if EqualValues(item, args[0]) {
						return BoolValue(true), nil
					}
				}
				return BoolValue(false), nil
			},
		},
		"map": {
			Name:    "map",
			Arity:   1,
			Doc:     "Apply a function to all elements and return a new list (M28 convenience method)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(*ListValue)
				if len(args) != 1 {
					return nil, &TypeError{Message: fmt.Sprintf("map() takes exactly 1 argument (%d given)", len(args))}
				}
				fn, ok := args[0].(Callable)
				if !ok {
					return nil, &TypeError{Message: "map() argument must be callable"}
				}
				result := NewList()
				for _, item := range list.Items() {
					mapped, err := fn.Call([]Value{item}, ctx)
					if err != nil {
						return nil, err
					}
					result.Append(mapped)
				}
				return result, nil
			},
		},
		"filter": {
			Name:    "filter",
			Arity:   1,
			Doc:     "Return a new list with elements that satisfy the predicate (M28 convenience method)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(*ListValue)
				if len(args) != 1 {
					return nil, &TypeError{Message: fmt.Sprintf("filter() takes exactly 1 argument (%d given)", len(args))}
				}
				fn, ok := args[0].(Callable)
				if !ok {
					return nil, &TypeError{Message: "filter() argument must be callable"}
				}
				result := NewList()
				for _, item := range list.Items() {
					pred, err := fn.Call([]Value{item}, ctx)
					if err != nil {
						return nil, err
					}
					if IsTruthy(pred) {
						result.Append(item)
					}
				}
				return result, nil
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
					return nil, &TypeError{Message: "__contains__ takes exactly one argument"}
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
					return nil, &TypeError{Message: "__add__ takes exactly one argument"}
				}
				list1 := receiver.(*ListValue)
				list2, ok := args[0].(*ListValue)
				if !ok {
					return nil, &TypeError{Message: "can only concatenate list to list"}
				}
				result := make([]Value, list1.Len()+list2.Len())
				copy(result, list1.Items())
				copy(result[list1.Len():], list2.Items())
				return NewList(result...), nil
			},
		},
		"__iadd__": {
			Name:    "__iadd__",
			Arity:   1,
			Doc:     "Concatenate (extend) list in-place",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: "__iadd__ takes exactly one argument"}
				}
				list := receiver.(*ListValue)
				// Accept any iterable for in-place extension
				switch v := args[0].(type) {
				case *ListValue:
					list.items = append(list.items, v.Items()...)
				case TupleValue:
					list.items = append(list.items, v...)
				default:
					if iterable, ok := args[0].(Iterable); ok {
						iter := iterable.Iterator()
						for {
							val, ok := iter.Next()
							if !ok {
								break
							}
							list.items = append(list.items, val)
						}
					} else {
						return nil, &TypeError{Message: "can only concatenate list to list"}
					}
				}
				return list, nil
			},
		},
		"__mul__": {
			Name:    "__mul__",
			Arity:   1,
			Doc:     "Repeat list n times",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: "__mul__ takes exactly one argument"}
				}
				list := receiver.(*ListValue)
				n, ok := args[0].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "can't multiply sequence by non-int"}
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
		"__imul__": {
			Name:    "__imul__",
			Arity:   1,
			Doc:     "Repeat list n times in-place",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, &TypeError{Message: "__imul__ takes exactly one argument"}
				}
				list := receiver.(*ListValue)
				n, ok := args[0].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "can't multiply sequence by non-int"}
				}
				count := int(n)
				if count <= 0 {
					// Clear the list in place
					list.items = []Value{}
					return list, nil
				}
				// Store original items
				original := make([]Value, list.Len())
				copy(original, list.Items())
				// Extend (count-1) more times in place
				for i := 1; i < count; i++ {
					list.items = append(list.items, original...)
				}
				return list, nil
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
		"__reduce_ex__": {
			Name:    "__reduce_ex__",
			Arity:   1,
			Doc:     "Helper for pickle",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				// For lists, pickle protocol returns: (list_class, (tuple(self),))
				list := receiver.(*ListValue)

				// Create list class callable
				listClass := NewNamedBuiltinFunction("list", func(args []Value, ctx *Context) (Value, error) {
					if len(args) == 0 {
						return NewList(), nil
					}
					if len(args) == 1 {
						// Convert iterable to list
						if l, ok := args[0].(*ListValue); ok {
							return l, nil
						}
						if t, ok := args[0].(TupleValue); ok {
							return NewList(t...), nil
						}
						return nil, &TypeError{Message: "list() argument must be an iterable"}
					}
					return nil, &TypeError{Message: fmt.Sprintf("list() takes at most 1 argument (%d given)", len(args))}
				})
				listClass.SetAttr("__module__", StringValue("builtins"))
				listClass.SetAttr("__name__", StringValue("list"))
				listClass.SetAttr("__qualname__", StringValue("list"))

				// Add __new__ method
				listClass.SetAttr("__new__", NewNamedBuiltinFunction("__new__", func(args []Value, ctx *Context) (Value, error) {
					if len(args) < 1 {
						return nil, &TypeError{Message: "__new__() missing required argument: 'cls'"}
					}
					if len(args) == 1 {
						return NewList(), nil
					}
					// args[0] is cls, args[1] is the iterable
					if l, ok := args[1].(*ListValue); ok {
						return l, nil
					}
					if t, ok := args[1].(TupleValue); ok {
						return NewList(t...), nil
					}
					return nil, &TypeError{Message: "list __new__ requires iterable argument"}
				}))

				// Create __newobj__ function
				newobjFunc := NewNamedBuiltinFunction("__newobj__", func(args []Value, ctx *Context) (Value, error) {
					if len(args) < 1 {
						return nil, &TypeError{Message: "__newobj__ requires at least 1 argument"}
					}
					cls := args[0]
					clsArgs := args[1:]

					clsObj, ok := cls.(interface{ GetAttr(string) (Value, bool) })
					if !ok {
						return nil, &TypeError{Message: "class does not support attribute access"}
					}

					newMethod, exists := clsObj.GetAttr("__new__")
					if !exists {
						return nil, &AttributeError{ObjType: "class", AttrName: "__new__"}
					}

					newCallable, ok := newMethod.(Callable)
					if !ok {
						return nil, &TypeError{Message: "__new__ is not callable"}
					}

					newArgs := append([]Value{cls}, clsArgs...)
					return newCallable.Call(newArgs, ctx)
				})

				newobjFunc.SetAttr("__module__", StringValue("copyreg"))
				newobjFunc.SetAttr("__name__", StringValue("__newobj__"))
				newobjFunc.SetAttr("__qualname__", StringValue("__newobj__"))

				// Convert list to tuple for pickling
				tupleItems := make(TupleValue, list.Len())
				copy(tupleItems, list.Items())

				// Return tuple: (__newobj__, (list_class, tuple_of_items))
				argsTuple := TupleValue{listClass, tupleItems}
				result := TupleValue{newobjFunc, argsTuple}
				return result, nil
			},
		},
		"__lt__": {
			Name:    "__lt__",
			Arity:   1,
			Doc:     "Return self < other (lexicographic comparison)",
			Builtin: true,
			Handler: listMethodLt,
		},
		"__le__": {
			Name:    "__le__",
			Arity:   1,
			Doc:     "Return self <= other (lexicographic comparison)",
			Builtin: true,
			Handler: listMethodLe,
		},
		"__gt__": {
			Name:    "__gt__",
			Arity:   1,
			Doc:     "Return self > other (lexicographic comparison)",
			Builtin: true,
			Handler: listMethodGt,
		},
		"__ge__": {
			Name:    "__ge__",
			Arity:   1,
			Doc:     "Return self >= other (lexicographic comparison)",
			Builtin: true,
			Handler: listMethodGe,
		},
	}
}

// Helper methods for list operations

func listMethodAppend(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: fmt.Sprintf("append() takes exactly one argument (%d given)", len(args))}
	}
	list := receiver.(*ListValue)
	list.Append(args[0])
	return None, nil
}

func listMethodExtend(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: fmt.Sprintf("extend() takes exactly one argument (%d given)", len(args))}
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
			return nil, &TypeError{Message: "extend() argument must be iterable"}
		}
	}

	list.Extend(toExtend)
	return None, nil
}

func listMethodInsert(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 2 {
		return nil, &TypeError{Message: fmt.Sprintf("insert() takes exactly 2 arguments (%d given)", len(args))}
	}

	list := receiver.(*ListValue)
	idxVal, ok := args[0].(NumberValue)
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", args[0].Type())}
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
		return nil, &TypeError{Message: fmt.Sprintf("remove() takes exactly one argument (%d given)", len(args))}
	}

	list := receiver.(*ListValue)
	for i, item := range list.Items() {
		if EqualValues(item, args[0]) {
			// Mutate the list by removing the item
			list.items = append(list.items[:i], list.items[i+1:]...)
			return None, nil
		}
	}
	return nil, &ValueError{Message: "list.remove(x): x not in list"}
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
			return nil, &TypeError{Message: fmt.Sprintf("'%s' object cannot be interpreted as an integer", args[0].Type())}
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
		return nil, &TypeError{Message: "index() takes at least 1 argument (0 given)"}
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
	return nil, &ValueError{Message: fmt.Sprintf("%v is not in list", args[0])}
}

func listMethodCount(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: fmt.Sprintf("count() takes exactly one argument (%d given)", len(args))}
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
		// TODO(M28-b902): Implement proper comparison
		// For now, compare as strings
		return Repr(list.items[i]) < Repr(list.items[j])
	})

	return None, nil
}

// listMethodSortWithKwargs implements list.sort() with keyword arguments (key=, reverse=)
func listMethodSortWithKwargs(receiver Value, args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	list := receiver.(*ListValue)

	// Extract keyword arguments
	var keyFunc Value
	reverse := false

	if key, hasKey := kwargs["key"]; hasKey {
		// Verify it's callable
		if _, ok := key.(Callable); !ok {
			// Also check for __call__ method
			if obj, ok := key.(Object); ok {
				if _, exists := obj.GetAttr("__call__"); !exists {
					return nil, &TypeError{Message: "key argument must be a callable function"}
				}
			} else {
				return nil, &TypeError{Message: "key argument must be a callable function"}
			}
		}
		keyFunc = key
	}

	if rev, hasRev := kwargs["reverse"]; hasRev {
		if b, ok := rev.(BoolValue); ok {
			reverse = bool(b)
		} else {
			return nil, &TypeError{Message: "reverse argument must be a boolean"}
		}
	}

	// Check for unknown keyword arguments
	for k := range kwargs {
		if k != "key" && k != "reverse" {
			return nil, &TypeError{Message: fmt.Sprintf("sort() got an unexpected keyword argument '%s'", k)}
		}
	}

	// Create a slice of items with their keys for sorting
	type keyedItem struct {
		value    Value
		keyValue Value
		index    int // Original index for stable sort
	}

	keyedItems := make([]keyedItem, list.Len())
	for i := 0; i < list.Len(); i++ {
		var keyValue Value
		if keyFunc != nil {
			// Apply key function
			if callable, ok := keyFunc.(Callable); ok {
				var err error
				keyValue, err = callable.Call([]Value{list.items[i]}, ctx)
				if err != nil {
					return nil, fmt.Errorf("error applying key function: %w", err)
				}
			} else {
				// Try calling __call__ method
				if obj, ok := keyFunc.(Object); ok {
					if callMethod, exists := obj.GetAttr("__call__"); exists {
						if callable, ok := callMethod.(Callable); ok {
							var err error
							keyValue, err = callable.Call([]Value{list.items[i]}, ctx)
							if err != nil {
								return nil, fmt.Errorf("error applying key function: %w", err)
							}
						}
					}
				}
			}
		} else {
			// No key function, use the value itself
			keyValue = list.items[i]
		}

		keyedItems[i] = keyedItem{
			value:    list.items[i],
			keyValue: keyValue,
			index:    i,
		}
	}

	// Sort using Go's sort package
	sort.SliceStable(keyedItems, func(i, j int) bool {
		cmp := Compare(keyedItems[i].keyValue, keyedItems[j].keyValue)
		if reverse {
			return cmp > 0
		}
		return cmp < 0
	})

	// Update the list items in place
	for i, ki := range keyedItems {
		list.items[i] = ki.value
	}

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
		return nil, &TypeError{Message: "__getitem__ takes exactly one argument"}
	}

	list := receiver.(*ListValue)

	// Handle slice
	if slice, ok := args[0].(*SliceValue); ok {
		start, stop, step, err := NormalizeSliceIndices(slice, list.Len())
		if err != nil {
			return nil, err
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

	// Handle index - use toIndex to support __index__ dunder method
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
		return nil, &TypeError{Message: "__setitem__ takes exactly 2 arguments"}
	}

	list := receiver.(*ListValue)
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, &TypeError{Message: "list indices must be integers"}
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
		return nil, &TypeError{Message: "__delitem__ takes exactly one argument"}
	}

	list := receiver.(*ListValue)

	// Handle slice deletion: del list[start:stop:step]
	if slice, ok := args[0].(*SliceValue); ok {
		length := list.Len()

		// Convert slice to indices
		var start, stop, step *int

		if slice.Start != nil && slice.Start != Nil {
			val, err := toIndex(slice.Start, ctx)
			if err != nil {
				return nil, err
			}
			start = &val
		}

		if slice.Stop != nil && slice.Stop != Nil {
			val, err := toIndex(slice.Stop, ctx)
			if err != nil {
				return nil, err
			}
			stop = &val
		}

		if slice.Step != nil && slice.Step != Nil {
			val, err := toIndex(slice.Step, ctx)
			if err != nil {
				return nil, err
			}
			if val == 0 {
				return nil, &ValueError{Message: "slice step cannot be zero"}
			}
			step = &val
		}

		// Normalize indices (using same logic as slicing)
		startIdx, stopIdx, stepVal := normalizeSliceIndicesForDel(length, start, stop, step)

		// Create set of indices to delete
		indicesToDelete := make(map[int]bool)
		if stepVal > 0 {
			for i := startIdx; i < stopIdx; i += stepVal {
				indicesToDelete[i] = true
			}
		} else {
			for i := startIdx; i > stopIdx; i += stepVal {
				indicesToDelete[i] = true
			}
		}

		// Mutate the list by removing deleted indices
		newItems := make([]Value, 0, length-len(indicesToDelete))
		for i, item := range list.items {
			if !indicesToDelete[i] {
				newItems = append(newItems, item)
			}
		}
		list.items = newItems

		return None, nil
	}

	// Handle single index deletion: del list[i]
	idx, ok := args[0].(NumberValue)
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("list indices must be integers or slices, not %s", args[0].Type())}
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

// normalizeSliceIndicesForDel normalizes slice indices for deletion
func normalizeSliceIndicesForDel(length int, start, end, step *int) (int, int, int) {
	stepVal := 1
	if step != nil {
		stepVal = *step
	}

	var startIdx, endIdx int

	if stepVal > 0 {
		startIdx = 0
		endIdx = length

		if start != nil {
			startIdx = *start
			if startIdx < 0 {
				startIdx += length
			}
			if startIdx < 0 {
				startIdx = 0
			}
			if startIdx > length {
				startIdx = length
			}
		}

		if end != nil {
			endIdx = *end
			if endIdx < 0 {
				endIdx += length
			}
			if endIdx < 0 {
				endIdx = 0
			}
			if endIdx > length {
				endIdx = length
			}
		}
	} else {
		startIdx = length - 1
		endIdx = -length - 1

		if start != nil {
			startIdx = *start
			if startIdx < 0 {
				startIdx += length
			}
			if startIdx < -length-1 {
				startIdx = -length - 1
			}
			if startIdx >= length {
				startIdx = length - 1
			}
		}

		if end != nil {
			endIdx = *end
			if endIdx < 0 {
				endIdx += length
			}
			if endIdx < -length-1 {
				endIdx = -length - 1
			}
			if endIdx >= length {
				endIdx = length - 1
			}
		}
	}

	return startIdx, endIdx, stepVal
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
					return 0, &TypeError{Message: fmt.Sprintf("__index__ returned non-int type %s", result.Type())}
				}
				intVal := int(num)
				if float64(intVal) != float64(num) {
					return 0, &TypeError{Message: fmt.Sprintf("__index__ returned non-integer value %v", num)}
				}
				return intVal, nil
			}
		}
	}

	// Fall back to NumberValue for built-in numeric types
	if num, ok := obj.(NumberValue); ok {
		intVal := int(num)
		if float64(intVal) != float64(num) {
			return 0, &TypeError{Message: "list indices must be integers, not float"}
		}
		return intVal, nil
	}

	// Not convertible to index
	return 0, &TypeError{Message: fmt.Sprintf("list indices must be integers, not %s", obj.Type())}
}

// listCompare performs lexicographic comparison of two lists
// Returns -1 if a < b, 0 if a == b, 1 if a > b
func listCompare(a, b *ListValue) int {
	aItems := a.Items()
	bItems := b.Items()
	minLen := len(aItems)
	if len(bItems) < minLen {
		minLen = len(bItems)
	}

	// Compare element by element
	for i := 0; i < minLen; i++ {
		cmp := compareValues(aItems[i], bItems[i])
		if cmp != 0 {
			return cmp
		}
	}

	// If all compared elements are equal, shorter list is "less"
	if len(aItems) < len(bItems) {
		return -1
	} else if len(aItems) > len(bItems) {
		return 1
	}
	return 0
}

// compareValues compares two values, returning -1, 0, or 1
func compareValues(a, b Value) int {
	// Handle numeric comparisons
	if aNum, aOk := a.(NumberValue); aOk {
		if bNum, bOk := b.(NumberValue); bOk {
			if float64(aNum) < float64(bNum) {
				return -1
			} else if float64(aNum) > float64(bNum) {
				return 1
			}
			return 0
		}
	}

	// Handle string comparisons
	if aStr, aOk := a.(StringValue); aOk {
		if bStr, bOk := b.(StringValue); bOk {
			if string(aStr) < string(bStr) {
				return -1
			} else if string(aStr) > string(bStr) {
				return 1
			}
			return 0
		}
	}

	// Handle nested list comparisons
	if aList, aOk := a.(*ListValue); aOk {
		if bList, bOk := b.(*ListValue); bOk {
			return listCompare(aList, bList)
		}
	}

	// Handle tuple comparisons
	if aTuple, aOk := a.(TupleValue); aOk {
		if bTuple, bOk := b.(TupleValue); bOk {
			return tupleCompare(aTuple, bTuple)
		}
	}

	// For other types, compare by string representation
	aRepr := Repr(a)
	bRepr := Repr(b)
	if aRepr < bRepr {
		return -1
	} else if aRepr > bRepr {
		return 1
	}
	return 0
}

// tupleCompare performs lexicographic comparison of two tuples
func tupleCompare(a, b TupleValue) int {
	minLen := len(a)
	if len(b) < minLen {
		minLen = len(b)
	}

	for i := 0; i < minLen; i++ {
		cmp := compareValues(a[i], b[i])
		if cmp != 0 {
			return cmp
		}
	}

	if len(a) < len(b) {
		return -1
	} else if len(a) > len(b) {
		return 1
	}
	return 0
}

func listMethodLt(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "__lt__ takes exactly one argument"}
	}
	list1 := receiver.(*ListValue)
	list2, ok := args[0].(*ListValue)
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("'<' not supported between instances of 'list' and '%s'", args[0].Type())}
	}
	return BoolValue(listCompare(list1, list2) < 0), nil
}

func listMethodLe(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "__le__ takes exactly one argument"}
	}
	list1 := receiver.(*ListValue)
	list2, ok := args[0].(*ListValue)
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("'<=' not supported between instances of 'list' and '%s'", args[0].Type())}
	}
	return BoolValue(listCompare(list1, list2) <= 0), nil
}

func listMethodGt(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "__gt__ takes exactly one argument"}
	}
	list1 := receiver.(*ListValue)
	list2, ok := args[0].(*ListValue)
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("'>' not supported between instances of 'list' and '%s'", args[0].Type())}
	}
	return BoolValue(listCompare(list1, list2) > 0), nil
}

func listMethodGe(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "__ge__ takes exactly one argument"}
	}
	list1 := receiver.(*ListValue)
	list2, ok := args[0].(*ListValue)
	if !ok {
		return nil, &TypeError{Message: fmt.Sprintf("'>=' not supported between instances of 'list' and '%s'", args[0].Type())}
	}
	return BoolValue(listCompare(list1, list2) >= 0), nil
}

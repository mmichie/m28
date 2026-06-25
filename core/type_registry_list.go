package core

import (
	"fmt"
	"math"
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
				if len(args) > 0 {
					return nil, &TypeError{Message: fmt.Sprintf("copy() takes no arguments (%d given)", len(args))}
				}
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
		"__init__": {
			Name:    "__init__",
			Arity:   -1,
			Doc:     "Initialize the list, optionally from an iterable",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				list := receiver.(*ListValue)
				if len(args) == 0 {
					// Clear the list
					list.items = list.items[:0]
					return None, nil
				}
				if len(args) == 1 {
					// Reinitialize from iterable
					newItems, err := iterableValues(args[0])
					if err != nil {
						return nil, err
					}
					list.items = newItems
					return None, nil
				}
				return nil, &TypeError{Message: fmt.Sprintf("list.__init__() takes at most 1 argument (%d given)", len(args))}
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
			Handler: listRepeatHandler,
		},
		"__rmul__": {
			Name:    "__rmul__",
			Arity:   1,
			Doc:     "Repeat list n times (reversed operand)",
			Builtin: true,
			Handler: listRepeatHandler,
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
				n, err := repeatCount(args[0], ctx)
				if err != nil {
					return nil, err
				}
				f := float64(n)
				if f >= math.MaxInt64 || f <= math.MinInt64 {
					return nil, &OverflowError{Message: "cannot fit 'int' into an index-sized integer"}
				}
				count := int64(f)
				if count <= 0 {
					// Clear the list in place
					list.items = []Value{}
					return list, nil
				}
				const maxItems = 1<<31 - 1
				if count > maxItems || (list.Len() > 0 && count > maxItems/int64(list.Len())) {
					return nil, &OverflowError{Message: "cannot fit 'int' into an index-sized integer"}
				}
				// Store original items
				original := make([]Value, list.Len())
				copy(original, list.Items())
				// Extend (count-1) more times in place
				for i := int64(1); i < count; i++ {
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

	items, err := iterableValuesCtx(args[0], ctx)
	if err != nil {
		return nil, &TypeError{Message: "extend() argument must be iterable"}
	}
	list.Extend(items)
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
	searchVal := args[0]
	for i := 0; i < len(list.items); i++ {
		equal, err := equalWithReflection(list.items[i], searchVal, ctx)
		if err != nil {
			return nil, err
		}
		if equal {
			// Mutate the list by removing the item
			list.items = append(list.items[:i], list.items[i+1:]...)
			return None, nil
		}
	}
	return nil, &ValueError{Message: "list.remove(x): x not in list"}
}

func listMethodPop(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) > 1 {
		return nil, &TypeError{Message: fmt.Sprintf("pop() takes at most 1 argument (%d given)", len(args))}
	}

	list := receiver.(*ListValue)
	if list.Len() == 0 {
		return nil, &IndexError{Message: "pop from empty list", Index: -1, Length: 0}
	}

	idx := list.Len() - 1
	if len(args) == 1 {
		i, err := toIndex(args[0], ctx)
		if err != nil {
			return nil, err
		}
		idx = i
		if idx < 0 {
			idx = list.Len() + idx
		}
		if idx < 0 || idx >= list.Len() {
			return nil, &IndexError{Message: "pop index out of range", Index: idx, Length: list.Len()}
		}
	}

	// Get the element to return
	element := list.items[idx]

	// Mutate the list by removing the element
	list.items = append(list.items[:idx], list.items[idx+1:]...)

	return element, nil
}

func listMethodIndex(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) < 1 {
		return nil, &TypeError{Message: "index() takes at least 1 argument (0 given)"}
	}
	if len(args) > 3 {
		return nil, &TypeError{Message: fmt.Sprintf("index() takes at most 3 arguments (%d given)", len(args))}
	}

	list := receiver.(*ListValue)
	start, stop := 0, list.Len()

	if len(args) > 1 {
		s, err := toIndex(args[1], ctx)
		if err != nil {
			return nil, err
		}
		start = s
		if start < 0 {
			start = list.Len() + start
		}
		if start < 0 {
			start = 0
		}
	}

	if len(args) > 2 {
		s, err := toIndex(args[2], ctx)
		if err != nil {
			return nil, err
		}
		stop = s
		if stop < 0 {
			stop = list.Len() + stop
		}
		if stop < 0 {
			stop = 0
		}
	}

	searchVal := args[0]
	for i := start; i < stop && i < list.Len(); i++ {
		// CPython list.index checks item == v (list element first, like list.remove)
		equal, err := equalWithReflection(list.items[i], searchVal, ctx)
		if err != nil {
			return nil, err
		}
		if equal {
			return NumberValue(i), nil
		}
	}
	return nil, &ValueError{Message: fmt.Sprintf("%v is not in list", PrintValue(searchVal))}
}

func listMethodCount(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: fmt.Sprintf("count() takes exactly one argument (%d given)", len(args))}
	}

	list := receiver.(*ListValue)
	searchVal := args[0]
	count := 0
	// CPython compares item == searchVal (element first, like __contains__)
	for _, item := range list.items {
		equal, err := equalWithReflection(item, searchVal, ctx)
		if err != nil {
			return nil, err
		}
		if equal {
			count++
		}
	}
	return NumberValue(count), nil
}

func listMethodSort(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) > 0 {
		return nil, &TypeError{Message: "sort() takes no positional arguments"}
	}

	list := receiver.(*ListValue)

	// Sort in-place using natural comparison (numeric for numbers, string for strings)
	var sortErr error
	sort.SliceStable(list.items, func(i, j int) bool {
		if sortErr != nil {
			return false
		}
		a, b := list.items[i], list.items[j]
		cmp := Compare(a, b)
		return cmp < 0
	})

	return None, nil
}

// listMethodSortWithKwargs implements list.sort() with keyword arguments (key=, reverse=)
func listMethodSortWithKwargs(receiver Value, args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	list := receiver.(*ListValue)

	// sort() takes no positional arguments
	if len(args) > 0 {
		return nil, &TypeError{Message: fmt.Sprintf("sort() takes no positional arguments")}
	}

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

	// Record original list length to detect mutation during sort
	origLen := list.Len()

	// Sort using Go's sort package
	var sortErr error
	// pyLessThan returns true if a < b using Python's __lt__ protocol
	pyLessThan := func(a, b Value) (bool, bool, error) {
		// Try a.__lt__(b) first
		type getAttrer interface {
			GetAttr(string) (Value, bool)
		}
		if aObj, ok := a.(getAttrer); ok {
			if ltMethod, exists := aObj.GetAttr("__lt__"); exists {
				if callable, ok := ltMethod.(interface {
					Call([]Value, *Context) (Value, error)
				}); ok {
					result, err := callable.Call([]Value{b}, ctx)
					if err != nil {
						return false, false, err
					}
					if result != NotImplemented {
						return IsTruthy(result), true, nil
					}
				}
			}
		}
		return false, false, nil
	}
	sort.SliceStable(keyedItems, func(i, j int) bool {
		if sortErr != nil {
			return false
		}
		// Check for list mutation during sort (Python's "list modified during sort")
		if list.Len() != origLen {
			sortErr = &ValueError{Message: "list modified during sort"}
			return false
		}
		a, b := keyedItems[i].keyValue, keyedItems[j].keyValue
		if reverse {
			// For descending: a > b means a should come first → b < a
			isLess, found, err := pyLessThan(b, a)
			if err != nil {
				sortErr = err
				return false
			}
			if found {
				return isLess
			}
		} else {
			// For ascending: a < b
			isLess, found, err := pyLessThan(a, b)
			if err != nil {
				sortErr = err
				return false
			}
			if found {
				return isLess
			}
		}
		// Fall back to numeric/string comparison
		cmp := Compare(a, b)
		if reverse {
			return cmp > 0
		}
		return cmp < 0
	})
	if sortErr != nil {
		return nil, sortErr
	}

	// Update the list items in place
	for i, ki := range keyedItems {
		list.items[i] = ki.value
	}

	return None, nil
}

func listMethodReverse(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) > 0 {
		return nil, &TypeError{Message: fmt.Sprintf("reverse() takes no arguments (%d given)", len(args))}
	}
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

	// Handle slice key
	if slice, ok := args[0].(*SliceValue); ok {
		return listSetItemSlice(list, slice, args[1], ctx)
	}

	// Use toIndex for proper integer conversion (supports __index__)
	// toIndex already handles the TypeError with the right message
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

	// Mutate the list by setting the item
	if err := list.SetItem(i, args[1]); err != nil {
		return nil, err
	}

	return None, nil
}

// ListSetItemSlice handles list[slice] = value, including extended slices (step != 1).
// Exported for use by eval/indexing.go.
func ListSetItemSlice(list *ListValue, slice *SliceValue, value Value, ctx *Context) (Value, error) {
	return listSetItemSlice(list, slice, value, ctx)
}

// listSetItemSlice handles list[slice] = value, including extended slices (step != 1).
func listSetItemSlice(list *ListValue, slice *SliceValue, value Value, ctx *Context) (Value, error) {
	length := list.Len()

	// Extract step first — validates it and determines direction
	step := 1
	if slice.Step != nil && slice.Step != Nil {
		s, err := toIndex(slice.Step, ctx)
		if err != nil {
			return nil, err
		}
		if s == 0 {
			return nil, &ValueError{Message: "slice step cannot be zero"}
		}
		step = s
	}

	// Convert value to list of items to insert/replace
	var newItems []Value
	switch v := value.(type) {
	case *ListValue:
		newItems = v.items
	case TupleValue:
		newItems = []Value(v)
	default:
		// Try Go Iterable interface first, then Python __iter__ protocol
		if iter, ok := value.(Iterable); ok {
			it := iter.Iterator()
			for {
				val, ok := it.Next()
				if !ok {
					break
				}
				newItems = append(newItems, val)
			}
		} else {
			// Try Python __iter__ / context-aware iteration
			items, err := IterableValuesCtx(value, ctx)
			if err != nil {
				return nil, &TypeError{Message: "can only assign an iterable to a slice"}
			}
			newItems = items
		}
	}

	// Compute start and stop indices based on step direction
	var start, stop int
	if step > 0 {
		start = 0
		stop = length
	} else {
		start = length - 1
		stop = -1
	}

	if slice.Start != nil && slice.Start != Nil {
		s, err := toIndex(slice.Start, ctx)
		if err != nil {
			return nil, err
		}
		start = s
		if start < 0 {
			start = length + start
		}
		if step > 0 {
			if start < 0 {
				start = 0
			} else if start > length {
				start = length
			}
		} else {
			if start < -1 {
				start = -1
			} else if start >= length {
				start = length - 1
			}
		}
	}

	if slice.Stop != nil && slice.Stop != Nil {
		s, err := toIndex(slice.Stop, ctx)
		if err != nil {
			return nil, err
		}
		stop = s
		if stop < 0 {
			stop = length + stop
		}
		if step > 0 {
			if stop < 0 {
				stop = 0
			} else if stop > length {
				stop = length
			}
		} else {
			if stop < -1 {
				stop = -1
			} else if stop >= length {
				stop = length
			}
		}
	}

	// Simple slice (step == 1 or step == -1 without gaps)
	if step == 1 {
		// CPython: if stop < start, clamp stop = start (empty slice = insertion at start)
		if stop < start {
			stop = start
		}
		valList := NewList(newItems...)
		return None, list.SetSlice(&start, &stop, valList)
	}

	// Extended slice: compute count and indices without risking integer overflow.
	// Use multiplication-based index computation instead of repeated addition.
	var sliceCount int
	if step > 0 && start < stop {
		sliceCount = (stop-start-1)/step + 1
	} else if step < 0 && start > stop {
		sliceCount = (start-stop-1)/(-step) + 1
	}
	indices := make([]int, sliceCount)
	for k := range indices {
		indices[k] = start + k*step
	}

	// Extended slice assignment: lengths must match
	if len(newItems) != len(indices) {
		return nil, &ValueError{
			Message: fmt.Sprintf("attempt to assign sequence of size %d to extended slice of size %d",
				len(newItems), len(indices)),
		}
	}

	// Validate all indices are still in bounds (the list may have been mutated
	// while iterating the RHS value). CPython raises ValueError in this case.
	for _, idx := range indices {
		if idx < 0 || idx >= len(list.items) {
			return nil, &ValueError{
				Message: fmt.Sprintf("list assignment index out of range"),
			}
		}
	}

	// Replace each selected item in place
	for i, idx := range indices {
		list.items[idx] = newItems[i]
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

		// Create set of indices to delete using multiplication-based counting
		// to avoid overflow with very large step values.
		var delCount int
		if stepVal > 0 && startIdx < stopIdx {
			delCount = (stopIdx-startIdx-1)/stepVal + 1
		} else if stepVal < 0 && startIdx > stopIdx {
			delCount = (startIdx-stopIdx-1)/(-stepVal) + 1
		}
		indicesToDelete := make(map[int]bool, delCount)
		for k := 0; k < delCount; k++ {
			idx := startIdx + k*stepVal
			if idx >= 0 && idx < length {
				indicesToDelete[idx] = true
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
				f := float64(num)
				if f != math.Trunc(f) {
					return 0, &TypeError{Message: fmt.Sprintf("__index__ returned non-integer value %v", num)}
				}
				// Clamp very large values (CPython clamps to PY_SSIZE_T_MAX)
				if f >= math.MaxInt64 {
					return math.MaxInt, nil
				}
				if f <= math.MinInt64 {
					return math.MinInt, nil
				}
				return int(f), nil
			}
		}
	}

	// BigIntValue: large integer — clamp to int bounds (always out of range for list indices)
	if big, ok := obj.(BigIntValue); ok {
		if big.Sign() < 0 {
			return math.MinInt, nil
		}
		return math.MaxInt, nil
	}

	// Fall back to NumberValue for built-in numeric types
	if num, ok := obj.(NumberValue); ok {
		f := float64(num)
		// Must be an integer-valued float (no fractional part)
		if f != math.Trunc(f) {
			return 0, &TypeError{Message: "list indices must be integers or slices, not float"}
		}
		// Clamp very large values to int bounds (CPython clamps to PY_SSIZE_T_MAX)
		if f >= math.MaxInt64 {
			return math.MaxInt, nil
		}
		if f <= math.MinInt64 {
			return math.MinInt, nil
		}
		return int(f), nil
	}

	// Not convertible to index
	typeName := string(obj.Type())
	if desc := GetTypeDescriptorForValue(obj); desc != nil {
		typeName = desc.PythonName
	}
	return 0, &TypeError{Message: fmt.Sprintf("list indices must be integers or slices, not %s", typeName)}
}

// sequenceIndex converts a non-slice subscript argument to an int index for a
// sequence whose Python type is typeName (e.g. "tuple", "string"). Plain numbers
// keep their existing handling; any object implementing the __index__ protocol
// is honored; anything else yields a "<typeName> indices must be integers"
// TypeError so each sequence reports its own name.
func sequenceIndex(arg Value, ctx *Context, typeName string) (int, error) {
	if num, ok := arg.(NumberValue); ok {
		// Reject non-integer floats (s[1.5]) like Python and list indexing do.
		f := float64(num)
		if f != math.Trunc(f) {
			return 0, &TypeError{Message: fmt.Sprintf("%s indices must be integers", typeName)}
		}
		return int(f), nil
	}
	if attrObj, ok := arg.(interface{ GetAttr(string) (Value, bool) }); ok {
		if _, exists := attrObj.GetAttr("__index__"); exists {
			return toIndex(arg, ctx)
		}
	}
	return 0, &TypeError{Message: fmt.Sprintf("%s indices must be integers", typeName)}
}

// repeatCount converts a sequence-repetition operand (seq * n) to a count,
// honoring the __index__ protocol so e.g. numpy-style integer objects work.
// Numbers pass through unchanged; anything else without __index__ yields the
// Python "can't multiply sequence by non-int" TypeError.
func repeatCount(arg Value, ctx *Context) (NumberValue, error) {
	if n, ok := arg.(NumberValue); ok {
		return n, nil
	}
	if attrObj, ok := arg.(interface{ GetAttr(string) (Value, bool) }); ok {
		if _, exists := attrObj.GetAttr("__index__"); exists {
			idx, err := toIndex(arg, ctx)
			if err != nil {
				return 0, err
			}
			return NumberValue(idx), nil
		}
	}
	return 0, &TypeError{Message: "can't multiply sequence by non-int"}
}

// listRepeatHandler implements list repetition for both __mul__ and __rmul__
// (repetition is commutative, so list * n and n * list share this logic).
func listRepeatHandler(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "list repetition takes exactly one argument"}
	}
	list := receiver.(*ListValue)
	n, err := repeatCount(args[0], ctx)
	if err != nil {
		return nil, err
	}
	f := float64(n)
	if f >= math.MaxInt64 || f <= math.MinInt64 {
		return nil, &OverflowError{Message: "cannot fit 'int' into an index-sized integer"}
	}
	count := int64(f)
	if count <= 0 {
		return NewList(), nil
	}
	const maxItems = 1<<31 - 1 // ~2B items max
	if count > maxItems || (list.Len() > 0 && count > maxItems/int64(list.Len())) {
		return nil, &OverflowError{Message: "cannot fit 'int' into an index-sized integer"}
	}
	result := make([]Value, 0, list.Len()*int(count))
	for i := int64(0); i < count; i++ {
		result = append(result, list.Items()...)
	}
	return NewList(result...), nil
}

// listCompareCtx performs lexicographic comparison of two lists with context
// for proper __lt__/__eq__ dispatch on elements.
// Returns (-1,nil) if a < b, (0,nil) if a == b, (1,nil) if a > b, (_,err) on error.
func listCompareCtx(a, b *ListValue, ctx *Context) (int, error) {
	// Iterate live (not via snapshot) to detect mutations during comparison
	i := 0
	for i < a.Len() && i < b.Len() {
		ai := a.items[i]
		bi := b.items[i]
		// First check equality
		eq, err := EqualValuesWithError(ai, bi, ctx)
		if err != nil {
			return 0, err
		}
		if !eq {
			// Elements differ: determine ordering
			// Try ai < bi via __lt__
			isLt, err := callLtWithReflection(ai, bi, ctx)
			if err != nil {
				return 0, err
			}
			if isLt {
				return -1, nil
			}
			return 1, nil
		}
		i++
	}
	// Exhausted elements; compare sizes accounting for live mutations
	if a.Len() < b.Len() {
		return -1, nil
	} else if a.Len() > b.Len() {
		return 1, nil
	}
	return 0, nil
}

// callLtWithReflection tries a.__lt__(b); if NotImplemented, tries b.__gt__(a);
// if both NotImplemented, raises TypeError.
func callLtWithReflection(a, b Value, ctx *Context) (bool, error) {
	// Try a.__lt__(b)
	if aObj, ok := a.(interface{ GetAttr(string) (Value, bool) }); ok {
		if ltMethod, found := aObj.GetAttr("__lt__"); found {
			if callable, ok := ltMethod.(interface{ Call([]Value, *Context) (Value, error) }); ok {
				result, err := callable.Call([]Value{b}, ctx)
				if err != nil {
					return false, err
				}
				if result != NotImplemented {
					return IsTruthy(result), nil
				}
			}
		}
	}
	// Try b.__gt__(a) (reflected)
	if bObj, ok := b.(interface{ GetAttr(string) (Value, bool) }); ok {
		if gtMethod, found := bObj.GetAttr("__gt__"); found {
			if callable, ok := gtMethod.(interface{ Call([]Value, *Context) (Value, error) }); ok {
				result, err := callable.Call([]Value{a}, ctx)
				if err != nil {
					return false, err
				}
				if result != NotImplemented {
					return IsTruthy(result), nil
				}
			}
		}
	}
	// Both returned NotImplemented → TypeError
	aType := GetPythonTypeName(a)
	bType := GetPythonTypeName(b)
	return false, &TypeError{Message: fmt.Sprintf("'<' not supported between instances of '%s' and '%s'", aType, bType)}
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
	list1 := toListValue(receiver)
	list2 := toListValue(args[0])
	if list2 == nil {
		return NotImplemented, nil
	}
	cmp, err := listCompareCtx(list1, list2, ctx)
	if err != nil {
		return nil, err
	}
	return BoolValue(cmp < 0), nil
}

func listMethodLe(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "__le__ takes exactly one argument"}
	}
	list1 := toListValue(receiver)
	list2 := toListValue(args[0])
	if list2 == nil {
		return NotImplemented, nil
	}
	cmp, err := listCompareCtx(list1, list2, ctx)
	if err != nil {
		return nil, err
	}
	return BoolValue(cmp <= 0), nil
}

func listMethodGt(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "__gt__ takes exactly one argument"}
	}
	list1 := toListValue(receiver)
	list2 := toListValue(args[0])
	if list2 == nil {
		return NotImplemented, nil
	}
	cmp, err := listCompareCtx(list1, list2, ctx)
	if err != nil {
		return nil, err
	}
	return BoolValue(cmp > 0), nil
}

func listMethodGe(receiver Value, args []Value, ctx *Context) (Value, error) {
	if len(args) != 1 {
		return nil, &TypeError{Message: "__ge__ takes exactly one argument"}
	}
	list1 := toListValue(receiver)
	list2 := toListValue(args[0])
	if list2 == nil {
		return NotImplemented, nil
	}
	cmp, err := listCompareCtx(list1, list2, ctx)
	if err != nil {
		return nil, err
	}
	return BoolValue(cmp >= 0), nil
}

// toListValue extracts the underlying *ListValue from a *ListValue or *ListInstance.
// Returns nil if the value is neither.
func toListValue(v Value) *ListValue {
	if lv, ok := v.(*ListValue); ok {
		return lv
	}
	if li, ok := v.(*ListInstance); ok {
		return li.Data
	}
	return nil
}

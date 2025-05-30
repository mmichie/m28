package core

import "fmt"

// InitListMethods adds additional methods to the list type descriptor
func InitListMethods() {
	listType := GetTypeDescriptor("list")
	if listType == nil {
		panic("list type not found in registry")
	}

	// Add remove method
	listType.Methods["remove"] = &MethodDescriptor{
		Name:    "remove",
		Arity:   1,
		Doc:     "Remove first occurrence of value. Raises ValueError if not found.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			list := receiver.(ListValue)
			value := args[0]

			// Find the first occurrence
			index := -1
			for i, item := range list {
				if EqualValues(item, value) {
					index = i
					break
				}
			}

			if index == -1 {
				return nil, fmt.Errorf("list.remove(x): x not in list")
			}

			// Create new list without the item
			newList := make(ListValue, 0, len(list)-1)
			newList = append(newList, list[:index]...)
			newList = append(newList, list[index+1:]...)

			return newList, nil
		},
	}

	// Add clear method
	listType.Methods["clear"] = &MethodDescriptor{
		Name:    "clear",
		Arity:   0,
		Doc:     "Remove all items from the list (returns empty list).",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			// Return a new empty list
			return make(ListValue, 0), nil
		},
	}

	// Add copy method
	listType.Methods["copy"] = &MethodDescriptor{
		Name:    "copy",
		Arity:   0,
		Doc:     "Return a shallow copy of the list.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			list := receiver.(ListValue)
			newList := make(ListValue, len(list))
			copy(newList, list)
			return newList, nil
		},
	}
}

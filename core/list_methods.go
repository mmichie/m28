package core

import (
	"fmt"
	"log"
)

// InitListMethods adds additional methods to the list type descriptor
func InitListMethods() {
	listType := GetTypeDescriptor("list")
	if listType == nil {
		log.Fatal("list type not found in registry")
	}

	// Add remove method
	listType.Methods["remove"] = &MethodDescriptor{
		Name:    "remove",
		Arity:   1,
		Doc:     "Remove first occurrence of value. Raises ValueError if not found.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			list := receiver.(*ListValue)
			value := args[0]

			// Find the first occurrence
			index := -1
			for i, item := range list.Items() {
				if EqualValues(item, value) {
					index = i
					break
				}
			}

			if index == -1 {
				return nil, fmt.Errorf("list.remove(x): x not in list")
			}

			// Mutate the list by removing the item
			list.items = append(list.items[:index], list.items[index+1:]...)

			return None, nil
		},
	}

	// Add clear method
	listType.Methods["clear"] = &MethodDescriptor{
		Name:    "clear",
		Arity:   0,
		Doc:     "Remove all items from the list.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			list := receiver.(*ListValue)
			// Mutate the list to be empty
			list.items = make([]Value, 0)
			return None, nil
		},
	}

	// Add copy method
	listType.Methods["copy"] = &MethodDescriptor{
		Name:    "copy",
		Arity:   0,
		Doc:     "Return a shallow copy of the list.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			list := receiver.(*ListValue)
			items := make([]Value, len(list.items))
			copy(items, list.items)
			return NewList(items...), nil
		},
	}
}

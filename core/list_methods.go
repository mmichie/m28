package core

import (
	"fmt"
	"log"
)

// simpleListIterator is a simple list iterator for __iter__
type simpleListIterator struct {
	list  *ListValue
	index int
}

func (s *simpleListIterator) Type() Type {
	return "list_iterator"
}

func (s *simpleListIterator) String() string {
	return fmt.Sprintf("<list_iterator at %d>", s.index)
}

func (s *simpleListIterator) GetAttr(name string) (Value, bool) {
	if name == "__iter__" {
		// Per Python iterator protocol: __iter__ returns the iterator itself
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return s, nil
		}), true
	}
	if name == "__next__" {
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if s.index < s.list.Len() {
				val := s.list.Items()[s.index]
				s.index++
				return val, nil
			}
			// Return StopIteration error
			return nil, fmt.Errorf("StopIteration")
		}), true
	}
	return nil, false
}

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
				return nil, &ValueError{Message: "list.remove(x): x not in list"}
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

	// Add __iter__ method
	listType.Methods["__iter__"] = &MethodDescriptor{
		Name:    "__iter__",
		Arity:   0,
		Doc:     "Return an iterator over the list.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			list := receiver.(*ListValue)
			return &simpleListIterator{list: list, index: 0}, nil
		},
	}
}

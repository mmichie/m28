package core

import (
	"fmt"
	"log"
)

// simpleListIterator is a simple list iterator for __iter__.
// It detaches from the list when exhausted, matching CPython semantics.
type simpleListIterator struct {
	list  *ListValue // nil when exhausted
	index int
}

func (s *simpleListIterator) Type() Type {
	return "list_iterator"
}

func (s *simpleListIterator) String() string {
	return fmt.Sprintf("<list_iterator object>")
}

// Next implements Iterator for Go-level iteration (for loops, list())
func (s *simpleListIterator) Next() (Value, bool) {
	if s.list == nil {
		return nil, false
	}
	if s.index >= s.list.Len() {
		s.list = nil // detach when exhausted
		return nil, false
	}
	val := s.list.items[s.index]
	s.index++
	return val, true
}

// Reset resets the iterator (not commonly needed, but satisfies Iterator interface)
func (s *simpleListIterator) Reset() {
	s.index = 0
}

// Iterator implements Iterable - returns self
func (s *simpleListIterator) Iterator() Iterator {
	return s
}

func (s *simpleListIterator) GetAttr(name string) (Value, bool) {
	switch name {
	case "__iter__":
		// Per Python iterator protocol: __iter__ returns the iterator itself
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return s, nil
		}), true
	case "__next__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if s.list == nil {
				return nil, fmt.Errorf("StopIteration")
			}
			if s.index < s.list.Len() {
				val := s.list.items[s.index]
				s.index++
				return val, nil
			}
			s.list = nil // detach when exhausted
			return nil, fmt.Errorf("StopIteration")
		}), true
	case "__length_hint__":
		// PEP 424: Return estimated remaining length
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if s.list == nil {
				return NumberValue(0), nil
			}
			remaining := s.list.Len() - s.index
			if remaining < 0 {
				remaining = 0
			}
			return NumberValue(remaining), nil
		}), true
	}
	return nil, false
}

// listReverseIterator iterates over a list in reverse order.
// It implements both Iterator (for Go-level iteration) and
// the Python iterator protocol via GetAttr (__iter__, __next__).
type listReverseIterator struct {
	list  *ListValue
	index int // starts at len-1, decrements; -1 means exhausted
}

func (r *listReverseIterator) Type() Type {
	return "list_reverseiterator"
}

func (r *listReverseIterator) String() string {
	return "<list_reverseiterator object>"
}

// Next implements Iterator for Go-level iteration (for loops, list())
func (r *listReverseIterator) Next() (Value, bool) {
	if r.index < 0 || r.index >= r.list.Len() {
		return nil, false
	}
	val := r.list.items[r.index]
	r.index--
	return val, true
}

// Reset resets the iterator to the start (end of the list)
func (r *listReverseIterator) Reset() {
	r.index = r.list.Len() - 1
}

// Iterator implements Iterable - returns self
func (r *listReverseIterator) Iterator() Iterator {
	return r
}

func (r *listReverseIterator) GetAttr(name string) (Value, bool) {
	switch name {
	case "__iter__":
		// Per Python iterator protocol: __iter__ returns the iterator itself
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			return r, nil
		}), true
	case "__next__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			if r.index >= 0 && r.index < r.list.Len() {
				val := r.list.items[r.index]
				r.index--
				return val, nil
			}
			return nil, fmt.Errorf("StopIteration")
		}), true
	case "__length_hint__":
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			remaining := r.index + 1
			if remaining < 0 {
				remaining = 0
			}
			return NumberValue(remaining), nil
		}), true
	}
	return nil, false
}

// NewListReverseIterator creates a new reverse iterator for a list
func NewListReverseIterator(list *ListValue) *listReverseIterator {
	return &listReverseIterator{list: list, index: list.Len() - 1}
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
		Arity:   -1,
		Doc:     "Remove first occurrence of value. Raises ValueError if not found.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: fmt.Sprintf("list.remove() takes exactly one argument (%d given)", len(args))}
			}
			list := receiver.(*ListValue)
			value := args[0]

			// Find the first occurrence using Python's comparison protocol.
			// CPython's list.remove checks item == value (list element first),
			// not value == item.
			index := -1
			for i, item := range list.Items() {
				equal, err := equalWithReflection(item, value, ctx)
				if err != nil {
					return nil, err
				}
				if equal {
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

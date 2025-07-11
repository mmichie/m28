package core

import (
	"fmt"
	"strings"
)

// registerSetType registers the set type descriptor with all its methods
func registerSetType() {
	RegisterType(&TypeDescriptor{
		Name:       "set",
		PythonName: "set",
		BaseType:   SetType,
		Methods:    getSetMethods(),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NewSet(), nil
			}
			if len(args) == 1 {
				// Convert iterable to set
				arg := args[0]
				result := NewSet()

				// Handle different iterable types
				switch v := arg.(type) {
				case *SetValue:
					// Make a copy
					for _, item := range v.items {
						result.Add(item)
					}
				case ListValue:
					for _, item := range v {
						result.Add(item)
					}
				case TupleValue:
					for _, item := range v {
						result.Add(item)
					}
				case StringValue:
					// String is iterable, each character becomes an element
					s := string(v)
					for _, ch := range s {
						result.Add(StringValue(string(ch)))
					}
				default:
					if iterable, ok := arg.(Iterable); ok {
						iter := iterable.Iterator()
						for {
							val, ok := iter.Next()
							if !ok {
								break
							}
							result.Add(val)
						}
					} else {
						return nil, fmt.Errorf("set() argument must be an iterable")
					}
				}
				return result, nil
			}
			return nil, fmt.Errorf("set() takes at most 1 argument (%d given)", len(args))
		},
		Repr: func(v Value) string {
			set := v.(*SetValue)
			if set.Size() == 0 {
				return "set()"
			}
			var items []string
			for _, item := range set.items {
				items = append(items, Repr(item))
			}
			return "{" + strings.Join(items, ", ") + "}"
		},
		Str: func(v Value) string {
			set := v.(*SetValue)
			if set.Size() == 0 {
				return "set()"
			}
			var items []string
			for _, item := range set.items {
				items = append(items, Repr(item))
			}
			return "{" + strings.Join(items, ", ") + "}"
		},
		Doc: "set() -> new empty set object\nset(iterable) -> new set object\n\nBuild an unordered collection of unique elements.",
	})
}

// getSetMethods returns all set methods
func getSetMethods() map[string]*MethodDescriptor {
	return map[string]*MethodDescriptor{
		"add": {
			Name:    "add",
			Arity:   1,
			Doc:     "Add an element to the set",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("add() takes exactly one argument")
				}
				set := receiver.(*SetValue)
				// Create a new set with the element added
				result := NewSet()
				for _, item := range set.items {
					result.Add(item)
				}
				result.Add(args[0])
				return result, nil
			},
		},
		"remove": {
			Name:    "remove",
			Arity:   1,
			Doc:     "Remove an element from the set; raises KeyError if not present",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("remove() takes exactly one argument")
				}
				set := receiver.(*SetValue)
				if !set.Contains(args[0]) {
					return nil, fmt.Errorf("KeyError: %v", args[0])
				}
				// Create a new set without the element
				result := NewSet()
				for _, item := range set.items {
					if !EqualValues(item, args[0]) {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"discard": {
			Name:    "discard",
			Arity:   1,
			Doc:     "Remove an element from the set if it is present",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("discard() takes exactly one argument")
				}
				set := receiver.(*SetValue)
				// Create a new set without the element
				result := NewSet()
				for _, item := range set.items {
					if !EqualValues(item, args[0]) {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"pop": {
			Name:    "pop",
			Arity:   0,
			Doc:     "Remove and return an arbitrary element from the set",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				set := receiver.(*SetValue)
				if set.Size() == 0 {
					return nil, fmt.Errorf("pop from an empty set")
				}
				// Return the first element (get any key from the map)
				for _, v := range set.items {
					return v, nil
				}
				// Should never reach here since we checked size > 0
				return nil, fmt.Errorf("pop from an empty set")
			},
		},
		"clear": {
			Name:    "clear",
			Arity:   0,
			Doc:     "Remove all elements from the set",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				return NewSet(), nil
			},
		},
		"copy": {
			Name:    "copy",
			Arity:   0,
			Doc:     "Return a shallow copy of the set",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				set := receiver.(*SetValue)
				result := NewSet()
				for _, item := range set.items {
					result.Add(item)
				}
				return result, nil
			},
		},
		"union": {
			Name:    "union",
			Arity:   -1,
			Doc:     "Return the union of sets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				set := receiver.(*SetValue)
				result := NewSet()
				// Add all items from this set
				for _, item := range set.items {
					result.Add(item)
				}
				// Add items from other sets
				for _, arg := range args {
					other, ok := arg.(*SetValue)
					if !ok {
						return nil, fmt.Errorf("union() argument must be a set")
					}
					for _, item := range other.items {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"intersection": {
			Name:    "intersection",
			Arity:   -1,
			Doc:     "Return the intersection of sets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) == 0 {
					// Return a copy of the set
					set := receiver.(*SetValue)
					result := NewSet()
					for _, item := range set.items {
						result.Add(item)
					}
					return result, nil
				}

				set := receiver.(*SetValue)
				result := NewSet()

				// Check each item in this set
				for _, item := range set.items {
					inAll := true
					// Check if it's in all other sets
					for _, arg := range args {
						other, ok := arg.(*SetValue)
						if !ok {
							return nil, fmt.Errorf("intersection() argument must be a set")
						}
						if !other.Contains(item) {
							inAll = false
							break
						}
					}
					if inAll {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"difference": {
			Name:    "difference",
			Arity:   -1,
			Doc:     "Return the difference of sets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				set := receiver.(*SetValue)
				result := NewSet()

				// Add all items from this set
				for _, item := range set.items {
					shouldInclude := true
					// Check if it's in any other set
					for _, arg := range args {
						other, ok := arg.(*SetValue)
						if !ok {
							return nil, fmt.Errorf("difference() argument must be a set")
						}
						if other.Contains(item) {
							shouldInclude = false
							break
						}
					}
					if shouldInclude {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"symmetric_difference": {
			Name:    "symmetric_difference",
			Arity:   1,
			Doc:     "Return the symmetric difference of two sets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("symmetric_difference() takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("symmetric_difference() argument must be a set")
				}

				result := NewSet()

				// Add items from this set not in other
				for _, item := range set.items {
					if !other.Contains(item) {
						result.Add(item)
					}
				}

				// Add items from other not in this set
				for _, item := range other.items {
					if !set.Contains(item) {
						result.Add(item)
					}
				}

				return result, nil
			},
		},
		"issubset": {
			Name:    "issubset",
			Arity:   1,
			Doc:     "Check if this set is a subset of another",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("issubset() takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("issubset() argument must be a set")
				}

				// Check if all items in this set are in other
				for _, item := range set.items {
					if !other.Contains(item) {
						return False, nil
					}
				}
				return True, nil
			},
		},
		"issuperset": {
			Name:    "issuperset",
			Arity:   1,
			Doc:     "Check if this set is a superset of another",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("issuperset() takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("issuperset() argument must be a set")
				}

				// Check if all items in other are in this set
				for _, item := range other.items {
					if !set.Contains(item) {
						return False, nil
					}
				}
				return True, nil
			},
		},
		"isdisjoint": {
			Name:    "isdisjoint",
			Arity:   1,
			Doc:     "Check if two sets have no elements in common",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("isdisjoint() takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("isdisjoint() argument must be a set")
				}

				// Check if any item in this set is in other
				for _, item := range set.items {
					if other.Contains(item) {
						return False, nil
					}
				}
				return True, nil
			},
		},
		"__len__": {
			Name:    "__len__",
			Arity:   0,
			Doc:     "Return the number of elements in the set",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				set := receiver.(*SetValue)
				return NumberValue(set.Size()), nil
			},
		},
		"__contains__": {
			Name:    "__contains__",
			Arity:   1,
			Doc:     "Check if value is in set",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__contains__ takes exactly one argument")
				}
				set := receiver.(*SetValue)
				return BoolValue(set.Contains(args[0])), nil
			},
		},
		"__iter__": {
			Name:    "__iter__",
			Arity:   0,
			Doc:     "Return an iterator for the set",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				set := receiver.(*SetValue)
				// Convert to list for iteration
				items := make(ListValue, 0, len(set.items))
				for _, v := range set.items {
					items = append(items, v)
				}
				return items, nil
			},
		},
		"__sub__": {
			Name:    "__sub__",
			Arity:   1,
			Doc:     "Return the difference of two sets (self - other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__sub__ takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("unsupported operand type(s) for -: 'set' and '%s'", args[0].Type())
				}

				result := NewSet()
				for _, item := range set.items {
					if !other.Contains(item) {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"__or__": {
			Name:    "__or__",
			Arity:   1,
			Doc:     "Return the union of two sets (self | other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__or__ takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("unsupported operand type(s) for |: 'set' and '%s'", args[0].Type())
				}

				result := NewSet()
				for _, item := range set.items {
					result.Add(item)
				}
				for _, item := range other.items {
					result.Add(item)
				}
				return result, nil
			},
		},
		"__and__": {
			Name:    "__and__",
			Arity:   1,
			Doc:     "Return the intersection of two sets (self & other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__and__ takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("unsupported operand type(s) for &: 'set' and '%s'", args[0].Type())
				}

				result := NewSet()
				for _, item := range set.items {
					if other.Contains(item) {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"__xor__": {
			Name:    "__xor__",
			Arity:   1,
			Doc:     "Return the symmetric difference of two sets (self ^ other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__xor__ takes exactly one argument")
				}

				set := receiver.(*SetValue)
				other, ok := args[0].(*SetValue)
				if !ok {
					return nil, fmt.Errorf("unsupported operand type(s) for ^: 'set' and '%s'", args[0].Type())
				}

				result := NewSet()

				// Add items from this set not in other
				for _, item := range set.items {
					if !other.Contains(item) {
						result.Add(item)
					}
				}

				// Add items from other not in this set
				for _, item := range other.items {
					if !set.Contains(item) {
						result.Add(item)
					}
				}

				return result, nil
			},
		},
	}
}

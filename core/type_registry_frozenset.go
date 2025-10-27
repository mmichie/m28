package core

import (
	"fmt"
	"strings"
)

// registerFrozenSetType registers the frozenset type descriptor with all its methods
func registerFrozenSetType() {
	RegisterType(&TypeDescriptor{
		Name:       "frozenset",
		PythonName: "frozenset",
		BaseType:   FrozenSetType,
		Methods:    getFrozenSetMethods(),
		Constructor: func(args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return NewFrozenSet(), nil
			}
			if len(args) == 1 {
				// Convert iterable to frozenset
				arg := args[0]
				result := NewFrozenSet()

				// Handle different iterable types
				switch v := arg.(type) {
				case *FrozenSetValue:
					// Make a copy
					for _, item := range v.items {
						result.Add(item)
					}
				case *SetValue:
					// Convert from set
					for _, item := range v.items {
						result.Add(item)
					}
				case *ListValue:
					for _, item := range v.Items() {
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
						return nil, fmt.Errorf("frozenset() argument must be an iterable")
					}
				}
				return result, nil
			}
			return nil, fmt.Errorf("frozenset() takes at most 1 argument (%d given)", len(args))
		},
		Repr: func(v Value) string {
			fs := v.(*FrozenSetValue)
			if fs.Size() == 0 {
				return "frozenset()"
			}
			var items []string
			for _, item := range fs.items {
				items = append(items, Repr(item))
			}
			return "frozenset({" + strings.Join(items, ", ") + "})"
		},
		Str: func(v Value) string {
			fs := v.(*FrozenSetValue)
			if fs.Size() == 0 {
				return "frozenset()"
			}
			var items []string
			for _, item := range fs.items {
				items = append(items, Repr(item))
			}
			return "frozenset({" + strings.Join(items, ", ") + "})"
		},
		Doc: "frozenset() -> new empty frozenset object\nfrozenset(iterable) -> new frozenset object\n\nBuild an immutable unordered collection of unique elements.",
	})
}

// getFrozenSetMethods returns all frozenset methods (read-only operations only)
func getFrozenSetMethods() map[string]*MethodDescriptor {
	return map[string]*MethodDescriptor{
		"copy": {
			Name:    "copy",
			Arity:   0,
			Doc:     "Return a shallow copy of the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()
				for _, item := range fs.items {
					result.Add(item)
				}
				return result, nil
			},
		},
		"union": {
			Name:    "union",
			Arity:   -1,
			Doc:     "Return the union of frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()
				// Add all items from this frozenset
				for _, item := range fs.items {
					result.Add(item)
				}
				// Add items from other sets/frozensets
				for _, arg := range args {
					switch other := arg.(type) {
					case *FrozenSetValue:
						for _, item := range other.items {
							result.Add(item)
						}
					case *SetValue:
						for _, item := range other.items {
							result.Add(item)
						}
					default:
						return nil, fmt.Errorf("union() argument must be a set or frozenset")
					}
				}
				return result, nil
			},
		},
		"intersection": {
			Name:    "intersection",
			Arity:   -1,
			Doc:     "Return the intersection of frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) == 0 {
					// Return a copy of the frozenset
					fs := receiver.(*FrozenSetValue)
					result := NewFrozenSet()
					for _, item := range fs.items {
						result.Add(item)
					}
					return result, nil
				}

				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				// Check each item in this frozenset
				for _, item := range fs.items {
					inAll := true
					// Check if it's in all other sets
					for _, arg := range args {
						var contains bool
						switch other := arg.(type) {
						case *FrozenSetValue:
							contains = other.Contains(item)
						case *SetValue:
							contains = other.Contains(item)
						default:
							return nil, fmt.Errorf("intersection() argument must be a set or frozenset")
						}
						if !contains {
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
			Doc:     "Return the difference of frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				// Add all items from this frozenset
				for _, item := range fs.items {
					shouldInclude := true
					// Check if it's in any other set
					for _, arg := range args {
						var contains bool
						switch other := arg.(type) {
						case *FrozenSetValue:
							contains = other.Contains(item)
						case *SetValue:
							contains = other.Contains(item)
						default:
							return nil, fmt.Errorf("difference() argument must be a set or frozenset")
						}
						if contains {
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
			Doc:     "Return the symmetric difference of two frozensets",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("symmetric_difference() takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				var otherContains func(Value) bool
				var otherItems map[string]Value

				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherContains = other.Contains
					otherItems = other.items
				case *SetValue:
					otherContains = other.Contains
					otherItems = other.items
				default:
					return nil, fmt.Errorf("symmetric_difference() argument must be a set or frozenset")
				}

				// Add items from this frozenset not in other
				for _, item := range fs.items {
					if !otherContains(item) {
						result.Add(item)
					}
				}

				// Add items from other not in this frozenset
				for _, item := range otherItems {
					if !fs.Contains(item) {
						result.Add(item)
					}
				}

				return result, nil
			},
		},
		"issubset": {
			Name:    "issubset",
			Arity:   1,
			Doc:     "Check if this frozenset is a subset of another",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("issubset() takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)

				// Convert argument to a set of items for comparison
				var otherItems map[string]Value
				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherItems = other.items
				case *SetValue:
					otherItems = other.items
				case StringValue:
					// String is iterable - each character becomes an item
					otherItems = make(map[string]Value)
					for _, ch := range string(other) {
						charVal := StringValue(string(ch))
						key := PrintValue(charVal)
						otherItems[key] = charVal
					}
				case *ListValue:
					// Convert list to set
					otherItems = make(map[string]Value)
					for _, item := range other.Items() {
						key := PrintValue(item)
						otherItems[key] = item
					}
				case TupleValue:
					// Convert tuple to set
					otherItems = make(map[string]Value)
					for _, item := range other {
						key := PrintValue(item)
						otherItems[key] = item
					}
				default:
					return nil, fmt.Errorf("issubset() argument must be an iterable")
				}

				// Create a lookup function
				otherContains := func(v Value) bool {
					key := PrintValue(v)
					_, exists := otherItems[key]
					return exists
				}

				// Check if all items in this frozenset are in other
				for _, item := range fs.items {
					if !otherContains(item) {
						return False, nil
					}
				}
				return True, nil
			},
		},
		"issuperset": {
			Name:    "issuperset",
			Arity:   1,
			Doc:     "Check if this frozenset is a superset of another",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("issuperset() takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)

				// Convert argument to a set of items for comparison
				var otherItems map[string]Value
				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherItems = other.items
				case *SetValue:
					otherItems = other.items
				case StringValue:
					// String is iterable - each character becomes an item
					otherItems = make(map[string]Value)
					for _, ch := range string(other) {
						charVal := StringValue(string(ch))
						key := PrintValue(charVal)
						otherItems[key] = charVal
					}
				case *ListValue:
					// Convert list to set
					otherItems = make(map[string]Value)
					for _, item := range other.Items() {
						key := PrintValue(item)
						otherItems[key] = item
					}
				case TupleValue:
					// Convert tuple to set
					otherItems = make(map[string]Value)
					for _, item := range other {
						key := PrintValue(item)
						otherItems[key] = item
					}
				default:
					return nil, fmt.Errorf("issuperset() argument must be an iterable")
				}

				// Check if all items in other are in this frozenset
				for _, item := range otherItems {
					if !fs.Contains(item) {
						return False, nil
					}
				}
				return True, nil
			},
		},
		"isdisjoint": {
			Name:    "isdisjoint",
			Arity:   1,
			Doc:     "Check if two frozensets have no elements in common",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("isdisjoint() takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)

				var otherContains func(Value) bool
				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherContains = other.Contains
				case *SetValue:
					otherContains = other.Contains
				default:
					return nil, fmt.Errorf("isdisjoint() argument must be a set or frozenset")
				}

				// Check if any item in this frozenset is in other
				for _, item := range fs.items {
					if otherContains(item) {
						return False, nil
					}
				}
				return True, nil
			},
		},
		"__len__": {
			Name:    "__len__",
			Arity:   0,
			Doc:     "Return the number of elements in the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				return NumberValue(fs.Size()), nil
			},
		},
		"__contains__": {
			Name:    "__contains__",
			Arity:   1,
			Doc:     "Check if value is in frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__contains__ takes exactly one argument")
				}
				fs := receiver.(*FrozenSetValue)
				return BoolValue(fs.Contains(args[0])), nil
			},
		},
		"__iter__": {
			Name:    "__iter__",
			Arity:   0,
			Doc:     "Return an iterator for the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				// Return the iterator (it now implements Value with __next__ support)
				iter := fs.Iterator()
				// Cast to *setIterator which is a Value
				return iter.(*setIterator), nil
			},
		},
		"__hash__": {
			Name:    "__hash__",
			Arity:   0,
			Doc:     "Return hash value for the frozenset",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				fs := receiver.(*FrozenSetValue)
				return NumberValue(float64(fs.Hash())), nil
			},
		},
		"__sub__": {
			Name:    "__sub__",
			Arity:   1,
			Doc:     "Return the difference of two frozensets (self - other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__sub__ takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				var otherContains func(Value) bool
				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherContains = other.Contains
				case *SetValue:
					otherContains = other.Contains
				default:
					return nil, fmt.Errorf("unsupported operand type(s) for -: 'frozenset' and '%s'", args[0].Type())
				}

				for _, item := range fs.items {
					if !otherContains(item) {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"__or__": {
			Name:    "__or__",
			Arity:   1,
			Doc:     "Return the union of two frozensets (self | other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__or__ takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				for _, item := range fs.items {
					result.Add(item)
				}

				var otherItems map[string]Value
				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherItems = other.items
				case *SetValue:
					otherItems = other.items
				default:
					return nil, fmt.Errorf("unsupported operand type(s) for |: 'frozenset' and '%s'", args[0].Type())
				}

				for _, item := range otherItems {
					result.Add(item)
				}
				return result, nil
			},
		},
		"__and__": {
			Name:    "__and__",
			Arity:   1,
			Doc:     "Return the intersection of two frozensets (self & other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__and__ takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				var otherContains func(Value) bool
				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherContains = other.Contains
				case *SetValue:
					otherContains = other.Contains
				default:
					return nil, fmt.Errorf("unsupported operand type(s) for &: 'frozenset' and '%s'", args[0].Type())
				}

				for _, item := range fs.items {
					if otherContains(item) {
						result.Add(item)
					}
				}
				return result, nil
			},
		},
		"__xor__": {
			Name:    "__xor__",
			Arity:   1,
			Doc:     "Return the symmetric difference of two frozensets (self ^ other)",
			Builtin: true,
			Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
				if len(args) != 1 {
					return nil, fmt.Errorf("__xor__ takes exactly one argument")
				}

				fs := receiver.(*FrozenSetValue)
				result := NewFrozenSet()

				var otherContains func(Value) bool
				var otherItems map[string]Value
				switch other := args[0].(type) {
				case *FrozenSetValue:
					otherContains = other.Contains
					otherItems = other.items
				case *SetValue:
					otherContains = other.Contains
					otherItems = other.items
				default:
					return nil, fmt.Errorf("unsupported operand type(s) for ^: 'frozenset' and '%s'", args[0].Type())
				}

				// Add items from this frozenset not in other
				for _, item := range fs.items {
					if !otherContains(item) {
						result.Add(item)
					}
				}

				// Add items from other not in this frozenset
				for _, item := range otherItems {
					if !fs.Contains(item) {
						result.Add(item)
					}
				}

				return result, nil
			},
		},
	}
}

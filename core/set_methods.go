package core

import (
	"fmt"
	"log"
)

// InitSetMethods adds additional methods to the set type descriptor
func InitSetMethods() {
	setType := GetTypeDescriptor("set")
	if setType == nil {
		log.Fatal("set type not found in registry")
	}

	// Set operation methods

	// union - return union of sets (accepts any iterable)
	setType.Methods["union"] = &MethodDescriptor{
		Name:    "union",
		Arity:   -1, // Variable args
		Doc:     "Return union of sets",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			result := NewSet()

			// Add all items from this set
			for k, v := range set.items {
				result.items[k] = v
			}

			// Add items from all other iterables
			for _, arg := range args {
				// Check if it's a set (fast path)
				if other, ok := arg.(*SetValue); ok {
					for k, v := range other.items {
						result.items[k] = v
					}
					continue
				}
				// Check if it's an iterable
				if iterable, ok := arg.(Iterable); ok {
					iter := iterable.Iterator()
					for {
						val, hasNext := iter.Next()
						if !hasNext {
							break
						}
						result.Add(val)
					}
					continue
				}
				return nil, &TypeError{Message: fmt.Sprintf("union() argument must be an iterable, not %s", arg.Type())}
			}

			return result, nil
		},
	}

	// intersection - return intersection of sets (accepts any iterable)
	setType.Methods["intersection"] = &MethodDescriptor{
		Name:    "intersection",
		Arity:   -1, // Variable args
		Doc:     "Return intersection of sets",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)

			// With no args, return a copy of the set
			if len(args) == 0 {
				result := NewSet()
				for k, v := range set.items {
					result.items[k] = v
				}
				return result, nil
			}

			// Convert all args to sets for efficient membership testing
			otherSets := make([]*SetValue, len(args))
			for i, arg := range args {
				// Check if it's already a set (fast path)
				if other, ok := arg.(*SetValue); ok {
					otherSets[i] = other
					continue
				}
				// Check if it's an iterable - convert to set
				if iterable, ok := arg.(Iterable); ok {
					other := NewSet()
					iter := iterable.Iterator()
					for {
						val, hasNext := iter.Next()
						if !hasNext {
							break
						}
						other.Add(val)
					}
					otherSets[i] = other
					continue
				}
				return nil, &TypeError{Message: fmt.Sprintf("intersection() argument must be an iterable, not %s", arg.Type())}
			}

			result := NewSet()

			// Check each item in this set
			for k, v := range set.items {
				inAll := true
				for _, other := range otherSets {
					if !other.Contains(v) {
						inAll = false
						break
					}
				}
				if inAll {
					result.items[k] = v
				}
			}

			return result, nil
		},
	}

	// difference - return difference of sets (accepts any iterable)
	setType.Methods["difference"] = &MethodDescriptor{
		Name:    "difference",
		Arity:   -1, // Variable args
		Doc:     "Return difference of sets",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			result := NewSet()

			// Add all items from this set
			for k, v := range set.items {
				result.items[k] = v
			}

			// Remove items from other iterables
			for _, arg := range args {
				// Check if it's a set (fast path)
				if other, ok := arg.(*SetValue); ok {
					for k := range other.items {
						delete(result.items, k)
					}
					continue
				}
				// Check if it's an iterable
				if iterable, ok := arg.(Iterable); ok {
					iter := iterable.Iterator()
					for {
						val, hasNext := iter.Next()
						if !hasNext {
							break
						}
						result.Remove(val)
					}
					continue
				}
				return nil, &TypeError{Message: fmt.Sprintf("difference() argument must be an iterable, not %s", arg.Type())}
			}

			return result, nil
		},
	}

	// symmetric_difference - return symmetric difference (accepts any iterable)
	setType.Methods["symmetric_difference"] = &MethodDescriptor{
		Name:    "symmetric_difference",
		Arity:   1,
		Doc:     "Return symmetric difference of two sets",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)

			// Convert arg to set if needed
			var other *SetValue
			if s, ok := args[0].(*SetValue); ok {
				other = s
			} else if iterable, ok := args[0].(Iterable); ok {
				other = NewSet()
				iter := iterable.Iterator()
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					other.Add(val)
				}
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("symmetric_difference() argument must be an iterable, not %s", args[0].Type())}
			}

			result := NewSet()

			// Add items in this set but not in other
			for k, v := range set.items {
				if !other.Contains(v) {
					result.items[k] = v
				}
			}

			// Add items in other but not in this set
			for k, v := range other.items {
				if !set.Contains(v) {
					result.items[k] = v
				}
			}

			return result, nil
		},
	}

	// Mutating methods (return new sets in functional style)

	// add - add element to set (mutates in-place)
	setType.Methods["add"] = &MethodDescriptor{
		Name:    "add",
		Arity:   1,
		Doc:     "Add element to set (modifies set in-place, returns None)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			// Mutate in-place
			set.Add(args[0])
			// Return None (Python's set.add returns None)
			return None, nil
		},
	}

	// remove - remove element from set (raises error if not found)
	setType.Methods["remove"] = &MethodDescriptor{
		Name:    "remove",
		Arity:   1,
		Doc:     "Remove element from set (modifies set in-place, raises error if not found)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			// Remove using the SetValue.Remove method which returns bool
			if !set.Remove(args[0]) {
				return nil, &KeyError{Key: args[0]}
			}
			return None, nil
		},
	}

	// discard - remove element from set (no error if not found)
	setType.Methods["discard"] = &MethodDescriptor{
		Name:    "discard",
		Arity:   1,
		Doc:     "Remove element from set (modifies set in-place, no error if not found)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			// Discard using the SetValue.Remove method, ignore result
			set.Remove(args[0])
			return None, nil
		},
	}

	// update - add elements from other sets
	setType.Methods["update"] = &MethodDescriptor{
		Name:    "update",
		Arity:   -1, // Variable args
		Doc:     "Add elements from other sets (modifies set in-place)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			// Add from other sets/iterables
			for _, arg := range args {
				// Check if it's a set
				if other, ok := arg.(*SetValue); ok {
					for _, v := range other.items {
						set.Add(v)
					}
					continue
				}
				// Check if it's an iterable
				if iterable, ok := arg.(Iterable); ok {
					iter := iterable.Iterator()
					for {
						val, hasNext := iter.Next()
						if !hasNext {
							break
						}
						set.Add(val)
					}
					continue
				}
				return nil, &TypeError{Message: fmt.Sprintf("update() argument must be an iterable, not %s", arg.Type())}
			}
			return None, nil
		},
	}

	// clear - remove all elements
	setType.Methods["clear"] = &MethodDescriptor{
		Name:    "clear",
		Arity:   0,
		Doc:     "Remove all elements (modifies set in-place)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			// Clear the items map
			set.items = make(map[string]Value)
			return None, nil
		},
	}

	// copy - return shallow copy
	setType.Methods["copy"] = &MethodDescriptor{
		Name:    "copy",
		Arity:   0,
		Doc:     "Return shallow copy of set",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			result := NewSet()
			for k, v := range set.items {
				result.items[k] = v
			}
			return result, nil
		},
	}

	// Comparison methods

	// issubset - test if this set is subset of other (accepts any iterable)
	setType.Methods["issubset"] = &MethodDescriptor{
		Name:    "issubset",
		Arity:   1,
		Doc:     "Return True if this set is a subset of other",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)

			// Convert arg to set if needed
			var other *SetValue
			if s, ok := args[0].(*SetValue); ok {
				other = s
			} else if iterable, ok := args[0].(Iterable); ok {
				other = NewSet()
				iter := iterable.Iterator()
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					other.Add(val)
				}
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("issubset() argument must be an iterable, not %s", args[0].Type())}
			}

			// Check if all items in this set are in other
			for _, v := range set.items {
				if !other.Contains(v) {
					return False, nil
				}
			}

			return True, nil
		},
	}

	// issuperset - test if this set is superset of other (accepts any iterable)
	setType.Methods["issuperset"] = &MethodDescriptor{
		Name:    "issuperset",
		Arity:   1,
		Doc:     "Return True if this set is a superset of other",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)

			// For issuperset, we iterate over the other iterable and check membership
			// Check if it's a set (fast path)
			if other, ok := args[0].(*SetValue); ok {
				for _, v := range other.items {
					if !set.Contains(v) {
						return False, nil
					}
				}
				return True, nil
			}
			// Check if it's an iterable
			if iterable, ok := args[0].(Iterable); ok {
				iter := iterable.Iterator()
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					if !set.Contains(val) {
						return False, nil
					}
				}
				return True, nil
			}
			return nil, &TypeError{Message: fmt.Sprintf("issuperset() argument must be an iterable, not %s", args[0].Type())}
		},
	}

	// isdisjoint - test if sets have no common elements (accepts any iterable)
	setType.Methods["isdisjoint"] = &MethodDescriptor{
		Name:    "isdisjoint",
		Arity:   1,
		Doc:     "Return True if two sets have no elements in common",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)

			// Check if it's a set (fast path)
			if other, ok := args[0].(*SetValue); ok {
				for _, v := range set.items {
					if other.Contains(v) {
						return False, nil
					}
				}
				return True, nil
			}
			// Check if it's an iterable - convert to set for membership testing
			if iterable, ok := args[0].(Iterable); ok {
				other := NewSet()
				iter := iterable.Iterator()
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					other.Add(val)
				}
				// Check if any item in this set is in other
				for _, v := range set.items {
					if other.Contains(v) {
						return False, nil
					}
				}
				return True, nil
			}
			return nil, &TypeError{Message: fmt.Sprintf("isdisjoint() argument must be an iterable, not %s", args[0].Type())}
		},
	}

	// pop - remove and return arbitrary element
	setType.Methods["pop"] = &MethodDescriptor{
		Name:    "pop",
		Arity:   0,
		Doc:     "Remove and return an arbitrary element",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)

			if set.Size() == 0 {
				return nil, &KeyError{Message: "pop from an empty set"}
			}

			// Get first item (arbitrary)
			var firstVal Value
			for _, v := range set.items {
				firstVal = v
				break
			}

			// Remove the item from the set
			set.Remove(firstVal)

			return firstVal, nil
		},
	}
}

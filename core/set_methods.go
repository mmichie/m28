package core

import (
	"fmt"
	"log"
)

// setFromIterableArg converts a method argument (set, frozenset, or any
// iterable) into a *SetValue for membership testing. Elements added from
// iterables go through full hashing semantics with ctx.
func setFromIterableArg(arg Value, method string, ctx *Context) (*SetValue, error) {
	if s, ok := arg.(*SetValue); ok {
		return s, nil
	}
	if fs, ok := arg.(*FrozenSetValue); ok {
		out := NewSet()
		var addErr error
		fs.forEachEntry(func(h uint64, elem Value) bool {
			addErr = out.addEntry(elem, h, ctx)
			return addErr == nil
		})
		return out, addErr
	}
	if iterable, ok := arg.(Iterable); ok {
		out := NewSet()
		iter := iterable.Iterator()
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}
			if err := out.AddWithError(val, ctx); err != nil {
				return nil, err
			}
		}
		return out, nil
	}
	return nil, &TypeError{Message: fmt.Sprintf("%s() argument must be an iterable, not %s", method, arg.Type())}
}

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

			// Add all items from this set (cached hashes, no rehash)
			var err error
			set.forEachEntry(func(h uint64, elem Value) bool {
				err = result.addEntry(elem, h, ctx)
				return err == nil
			})
			if err != nil {
				return nil, err
			}

			// Add items from all other iterables
			for _, arg := range args {
				other, err := setFromIterableArg(arg, "union", ctx)
				if err != nil {
					return nil, err
				}
				var addErr error
				other.forEachEntry(func(h uint64, elem Value) bool {
					addErr = result.addEntry(elem, h, ctx)
					return addErr == nil
				})
				if addErr != nil {
					return nil, addErr
				}
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
				var err error
				set.forEachEntry(func(h uint64, elem Value) bool {
					err = result.addEntry(elem, h, ctx)
					return err == nil
				})
				return result, err
			}

			// Convert all args to sets for efficient membership testing
			otherSets := make([]*SetValue, len(args))
			for i, arg := range args {
				other, err := setFromIterableArg(arg, "intersection", ctx)
				if err != nil {
					return nil, err
				}
				otherSets[i] = other
			}

			result := NewSet()
			var walkErr error
			set.forEachEntry(func(h uint64, elem Value) bool {
				for _, other := range otherSets {
					in, err := other.containsHashed(elem, h, ctx)
					if err != nil {
						walkErr = err
						return false
					}
					if !in {
						return true
					}
				}
				walkErr = result.addEntry(elem, h, ctx)
				return walkErr == nil
			})
			if walkErr != nil {
				return nil, walkErr
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
			var err error
			set.forEachEntry(func(h uint64, elem Value) bool {
				err = result.addEntry(elem, h, ctx)
				return err == nil
			})
			if err != nil {
				return nil, err
			}

			// Remove items from other iterables
			for _, arg := range args {
				other, err := setFromIterableArg(arg, "difference", ctx)
				if err != nil {
					return nil, err
				}
				var remErr error
				other.forEachEntry(func(h uint64, elem Value) bool {
					_, remErr = result.removeHashed(elem, h, ctx)
					return remErr == nil
				})
				if remErr != nil {
					return nil, remErr
				}
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
			other, err := setFromIterableArg(args[0], "symmetric_difference", ctx)
			if err != nil {
				return nil, err
			}

			result := NewSet()
			var walkErr error

			// Items in this set but not in other
			set.forEachEntry(func(h uint64, elem Value) bool {
				in, err := other.containsHashed(elem, h, ctx)
				if err != nil {
					walkErr = err
					return false
				}
				if !in {
					walkErr = result.addEntry(elem, h, ctx)
					return walkErr == nil
				}
				return true
			})
			if walkErr != nil {
				return nil, walkErr
			}

			// Items in other but not in this set
			other.forEachEntry(func(h uint64, elem Value) bool {
				in, err := set.containsHashed(elem, h, ctx)
				if err != nil {
					walkErr = err
					return false
				}
				if !in {
					walkErr = result.addEntry(elem, h, ctx)
					return walkErr == nil
				}
				return true
			})
			if walkErr != nil {
				return nil, walkErr
			}

			return result, nil
		},
	}

	// Mutating methods

	// add - add element to set (mutates in-place)
	setType.Methods["add"] = &MethodDescriptor{
		Name:    "add",
		Arity:   1,
		Doc:     "Add element to set (modifies set in-place, returns None)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			if err := set.AddWithError(args[0], ctx); err != nil {
				return nil, err
			}
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
			removed, err := set.RemoveWithError(args[0], ctx)
			if err != nil {
				return nil, err
			}
			if !removed {
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
			if _, err := set.RemoveWithError(args[0], ctx); err != nil {
				return nil, err
			}
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
			for _, arg := range args {
				other, err := setFromIterableArg(arg, "update", ctx)
				if err != nil {
					return nil, err
				}
				var addErr error
				other.forEachEntry(func(h uint64, elem Value) bool {
					addErr = set.addEntry(elem, h, ctx)
					return addErr == nil
				})
				if addErr != nil {
					return nil, addErr
				}
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
			receiver.(*SetValue).Clear()
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
			var err error
			set.forEachEntry(func(h uint64, elem Value) bool {
				err = result.addEntry(elem, h, ctx)
				return err == nil
			})
			return result, err
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
			other, err := setFromIterableArg(args[0], "issubset", ctx)
			if err != nil {
				return nil, err
			}

			subset := true
			var walkErr error
			set.forEachEntry(func(h uint64, elem Value) bool {
				in, err := other.containsHashed(elem, h, ctx)
				if err != nil {
					walkErr = err
					return false
				}
				if !in {
					subset = false
					return false
				}
				return true
			})
			if walkErr != nil {
				return nil, walkErr
			}
			return BoolValue(subset), nil
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

			// Sets and frozensets probe with cached hashes; other iterables
			// stream elements through a membership check.
			if other, ok := args[0].(*SetValue); ok {
				super := true
				var walkErr error
				other.forEachEntry(func(h uint64, elem Value) bool {
					in, err := set.containsHashed(elem, h, ctx)
					if err != nil {
						walkErr = err
						return false
					}
					if !in {
						super = false
						return false
					}
					return true
				})
				if walkErr != nil {
					return nil, walkErr
				}
				return BoolValue(super), nil
			}
			if iterable, ok := args[0].(Iterable); ok {
				iter := iterable.Iterator()
				for {
					val, hasNext := iter.Next()
					if !hasNext {
						break
					}
					in, err := set.ContainsWithError(val, ctx)
					if err != nil {
						return nil, err
					}
					if !in {
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
			other, err := setFromIterableArg(args[0], "isdisjoint", ctx)
			if err != nil {
				return nil, err
			}

			disjoint := true
			var walkErr error
			set.forEachEntry(func(h uint64, elem Value) bool {
				in, err := other.containsHashed(elem, h, ctx)
				if err != nil {
					walkErr = err
					return false
				}
				if in {
					disjoint = false
					return false
				}
				return true
			})
			if walkErr != nil {
				return nil, walkErr
			}
			return BoolValue(disjoint), nil
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
			elem, _, ok := set.table.popLast()
			if !ok {
				return nil, &KeyError{Message: "pop from an empty set"}
			}
			return elem, nil
		},
	}
}

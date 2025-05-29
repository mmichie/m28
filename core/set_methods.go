package core

import "fmt"

// InitSetMethods adds additional methods to the set type descriptor
func InitSetMethods() {
	setType := GetTypeDescriptor("set")
	if setType == nil {
		panic("set type not found in registry")
	}

	// Set operation methods
	
	// union - return union of sets
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
			
			// Add items from all other sets
			for _, arg := range args {
				other, ok := arg.(*SetValue)
				if !ok {
					return nil, fmt.Errorf("union() argument must be a set, not %s", arg.Type())
				}
				for k, v := range other.items {
					result.items[k] = v
				}
			}
			
			return result, nil
		},
	}

	// intersection - return intersection of sets
	setType.Methods["intersection"] = &MethodDescriptor{
		Name:    "intersection",
		Arity:   -1, // Variable args
		Doc:     "Return intersection of sets",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) == 0 {
				return nil, fmt.Errorf("intersection() requires at least one argument")
			}
			
			set := receiver.(*SetValue)
			result := NewSet()
			
			// Check each item in this set
			for k, v := range set.items {
				inAll := true
				for _, arg := range args {
					other, ok := arg.(*SetValue)
					if !ok {
						return nil, fmt.Errorf("intersection() argument must be a set, not %s", arg.Type())
					}
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

	// difference - return difference of sets
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
			
			// Remove items from other sets
			for _, arg := range args {
				other, ok := arg.(*SetValue)
				if !ok {
					return nil, fmt.Errorf("difference() argument must be a set, not %s", arg.Type())
				}
				for k := range other.items {
					delete(result.items, k)
				}
			}
			
			return result, nil
		},
	}

	// symmetric_difference - return symmetric difference
	setType.Methods["symmetric_difference"] = &MethodDescriptor{
		Name:    "symmetric_difference",
		Arity:   1,
		Doc:     "Return symmetric difference of two sets",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			other, ok := args[0].(*SetValue)
			if !ok {
				return nil, fmt.Errorf("symmetric_difference() argument must be a set, not %s", args[0].Type())
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
	
	// add - add element to set
	setType.Methods["add"] = &MethodDescriptor{
		Name:    "add",
		Arity:   1,
		Doc:     "Add element to set (returns new set)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			result := NewSet()
			
			// Copy current set
			for k, v := range set.items {
				result.items[k] = v
			}
			
			// Add new element
			result.Add(args[0])
			
			return result, nil
		},
	}

	// remove - remove element from set (raises error if not found)
	setType.Methods["remove"] = &MethodDescriptor{
		Name:    "remove",
		Arity:   1,
		Doc:     "Remove element from set (returns new set, raises error if not found)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			elem := args[0]
			
			// Check if element exists
			key := PrintValue(elem)
			if _, exists := set.items[key]; !exists {
				return nil, &KeyError{Key: elem}
			}
			
			result := NewSet()
			
			// Copy all except the removed element
			for k, v := range set.items {
				if k != key {
					result.items[k] = v
				}
			}
			
			return result, nil
		},
	}

	// discard - remove element from set (no error if not found)
	setType.Methods["discard"] = &MethodDescriptor{
		Name:    "discard",
		Arity:   1,
		Doc:     "Remove element from set (returns new set, no error if not found)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			elem := args[0]
			key := PrintValue(elem)
			
			result := NewSet()
			
			// Copy all except the discarded element
			for k, v := range set.items {
				if k != key {
					result.items[k] = v
				}
			}
			
			return result, nil
		},
	}
	
	// update - add elements from other sets
	setType.Methods["update"] = &MethodDescriptor{
		Name:    "update",
		Arity:   -1, // Variable args
		Doc:     "Add elements from other sets (returns new set)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			result := NewSet()
			
			// Copy current set
			for k, v := range set.items {
				result.items[k] = v
			}
			
			// Add from other sets
			for _, arg := range args {
				other, ok := arg.(*SetValue)
				if !ok {
					return nil, fmt.Errorf("update() argument must be a set, not %s", arg.Type())
				}
				for k, v := range other.items {
					result.items[k] = v
				}
			}
			
			return result, nil
		},
	}

	// clear - remove all elements
	setType.Methods["clear"] = &MethodDescriptor{
		Name:    "clear",
		Arity:   0,
		Doc:     "Remove all elements (returns empty set)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			return NewSet(), nil
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
	
	// issubset - test if this set is subset of other
	setType.Methods["issubset"] = &MethodDescriptor{
		Name:    "issubset",
		Arity:   1,
		Doc:     "Return True if this set is a subset of other",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			other, ok := args[0].(*SetValue)
			if !ok {
				return nil, fmt.Errorf("issubset() argument must be a set, not %s", args[0].Type())
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

	// issuperset - test if this set is superset of other
	setType.Methods["issuperset"] = &MethodDescriptor{
		Name:    "issuperset",
		Arity:   1,
		Doc:     "Return True if this set is a superset of other",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			other, ok := args[0].(*SetValue)
			if !ok {
				return nil, fmt.Errorf("issuperset() argument must be a set, not %s", args[0].Type())
			}
			
			// Check if all items in other are in this set
			for _, v := range other.items {
				if !set.Contains(v) {
					return False, nil
				}
			}
			
			return True, nil
		},
	}

	// isdisjoint - test if sets have no common elements
	setType.Methods["isdisjoint"] = &MethodDescriptor{
		Name:    "isdisjoint",
		Arity:   1,
		Doc:     "Return True if two sets have no elements in common",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			set := receiver.(*SetValue)
			other, ok := args[0].(*SetValue)
			if !ok {
				return nil, fmt.Errorf("isdisjoint() argument must be a set, not %s", args[0].Type())
			}
			
			// Check if any item in this set is in other
			for _, v := range set.items {
				if other.Contains(v) {
					return False, nil
				}
			}
			
			return True, nil
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
				return nil, fmt.Errorf("pop from empty set")
			}
			
			// Get first item (arbitrary)
			var firstVal Value
			for _, v := range set.items {
				firstVal = v
				break
			}
			
			// Return just the value (in functional style, we'd return both value and new set)
			return firstVal, nil
		},
	}
}
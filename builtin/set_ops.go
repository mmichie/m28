package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Python-like set operations

// setConstructor that creates a PythonicSet rather than a map
func setConstructor(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	result := core.NewPythonicSet()

	if len(args) == 0 {
		// Empty set
		return result, nil
	}

	if len(args) > 1 {
		return nil, fmt.Errorf("set() takes at most 1 argument")
	}

	// Handle different types of arguments
	switch arg := args[0].(type) {
	case core.LispList:
		// Create a set from a list
		for _, item := range arg {
			result.Add(item)
		}
	case *core.PythonicSet:
		// Create a copy of an existing set
		for elem := range arg.Data() {
			result.Add(elem)
		}
	case *core.PythonicDict:
		// Create a set from dictionary keys
		arg.Iterate(func(key, _ core.LispValue) error {
			result.Add(key)
			return nil
		})
	case string:
		// Create a set from characters in a string
		for _, ch := range arg {
			result.Add(string(ch))
		}
	default:
		return nil, fmt.Errorf("set() argument must be iterable, got %T", arg)
	}

	return result, nil
}

// Set operations

// setUnionFunc implements set.union() and the | operator
func setUnionFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("set.union() requires at least one argument")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.union() requires a set as first argument, got %T", args[0])
	}

	// Start with a copy of the set
	result := set.Copy()

	// Add elements from all other sets
	for _, arg := range args[1:] {
		otherSet, ok := arg.(*core.PythonicSet)
		if !ok {
			return nil, fmt.Errorf("set.union() requires sets as arguments, got %T", arg)
		}
		result.Update(otherSet)
	}

	return result, nil
}

// setIntersectionFunc implements set.intersection() and the & operator
func setIntersectionFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("set.intersection() requires at least one argument")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.intersection() requires a set as first argument, got %T", args[0])
	}

	// With one argument, just return a copy of the set
	if len(args) == 1 {
		return set.Copy(), nil
	}

	// Start with a copy of the first set
	result := set.Copy()

	// Intersect with each additional set
	for _, arg := range args[1:] {
		otherSet, ok := arg.(*core.PythonicSet)
		if !ok {
			return nil, fmt.Errorf("set.intersection() requires sets as arguments, got %T", arg)
		}
		result.IntersectionUpdate(otherSet)
	}

	return result, nil
}

// setDifferenceFunc implements set.difference() and the - operator
func setDifferenceFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("set.difference() requires at least one argument")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.difference() requires a set as first argument, got %T", args[0])
	}

	// With one argument, just return a copy of the set
	if len(args) == 1 {
		return set.Copy(), nil
	}

	// Start with a copy of the first set
	result := set.Copy()

	// Remove elements from each additional set
	for _, arg := range args[1:] {
		otherSet, ok := arg.(*core.PythonicSet)
		if !ok {
			return nil, fmt.Errorf("set.difference() requires sets as arguments, got %T", arg)
		}
		result.DifferenceUpdate(otherSet)
	}

	return result, nil
}

// setSymmetricDifferenceFunc implements set.symmetric_difference() and the ^ operator
func setSymmetricDifferenceFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.symmetric_difference() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.symmetric_difference() requires a set as first argument, got %T", args[0])
	}

	// Second argument should be a set
	otherSet, ok := args[1].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.symmetric_difference() requires a set as second argument, got %T", args[1])
	}

	// Return the symmetric difference
	return set.SymmetricDifference(otherSet), nil
}

// setAddFunc implements set.add()
func setAddFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.add() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.add() requires a set as first argument, got %T", args[0])
	}

	// Add the element to the set
	set.Add(args[1])

	// Return None (Python's add method returns None)
	return core.PythonicNone{}, nil
}

// setRemoveFunc implements set.remove()
func setRemoveFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.remove() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.remove() requires a set as first argument, got %T", args[0])
	}

	// Check if the element is in the set
	if !set.Contains(args[1]) {
		return nil, fmt.Errorf("KeyError: %v", args[1])
	}

	// Remove the element from the set
	set.Remove(args[1])

	// Return None (Python's remove method returns None)
	return core.PythonicNone{}, nil
}

// setDiscardFunc implements set.discard()
func setDiscardFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.discard() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.discard() requires a set as first argument, got %T", args[0])
	}

	// Remove the element from the set (no error if not present)
	set.Remove(args[1])

	// Return None (Python's discard method returns None)
	return core.PythonicNone{}, nil
}

// setContainsFunc implements set.contains() for the 'in' operator
func setContainsFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.contains() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.contains() requires a set as first argument, got %T", args[0])
	}

	// Check if the element is in the set
	return core.PythonicBool(set.Contains(args[1])), nil
}

// setClearFunc implements set.clear()
func setClearFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("set.clear() requires exactly one argument")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.clear() requires a set as argument, got %T", args[0])
	}

	// Clear the set
	set.Clear()

	// Return None (Python's clear method returns None)
	return core.PythonicNone{}, nil
}

// setCopyFunc implements set.copy()
func setCopyFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("set.copy() requires exactly one argument")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.copy() requires a set as argument, got %T", args[0])
	}

	// Return a copy of the set
	return set.Copy(), nil
}

// setPopFunc implements set.pop()
func setPopFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("set.pop() requires exactly one argument")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.pop() requires a set as argument, got %T", args[0])
	}

	// Pop an element from the set
	elem, ok := set.Pop()
	if !ok {
		return nil, fmt.Errorf("KeyError: pop from an empty set")
	}

	// Return the popped element
	return elem, nil
}

// setUpdateFunc implements set.update()
func setUpdateFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("set.update() requires at least two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.update() requires a set as first argument, got %T", args[0])
	}

	// Update with each additional set
	for _, arg := range args[1:] {
		otherSet, ok := arg.(*core.PythonicSet)
		if !ok {
			// Try to convert to a set if not already a set
			converted, err := setConstructor([]core.LispValue{arg}, nil)
			if err != nil {
				return nil, fmt.Errorf("set.update() requires sets or iterables as arguments, got %T", arg)
			}
			otherSet = converted.(*core.PythonicSet)
		}
		set.Update(otherSet)
	}

	// Return None (Python's update method returns None)
	return core.PythonicNone{}, nil
}

// setIsSubsetFunc implements set.issubset() and the <= operator
func setIsSubsetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.issubset() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.issubset() requires a set as first argument, got %T", args[0])
	}

	// Second argument should be a set
	otherSet, ok := args[1].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.issubset() requires a set as second argument, got %T", args[1])
	}

	// Return whether this set is a subset of the other
	return core.PythonicBool(set.IsSubset(otherSet)), nil
}

// setIsStrictSubsetFunc implements the < operator for sets
func setIsStrictSubsetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.is_strict_subset() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.is_strict_subset() requires a set as first argument, got %T", args[0])
	}

	// Second argument should be a set
	otherSet, ok := args[1].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.is_strict_subset() requires a set as second argument, got %T", args[1])
	}

	// A strict subset must be a subset and the sets must not be equal
	return core.PythonicBool(set.IsSubset(otherSet) && set.Size() < otherSet.Size()), nil
}

// setIsSupersetFunc implements set.issuperset() and the >= operator
func setIsSupersetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.issuperset() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.issuperset() requires a set as first argument, got %T", args[0])
	}

	// Second argument should be a set
	otherSet, ok := args[1].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.issuperset() requires a set as second argument, got %T", args[1])
	}

	// Return whether this set is a superset of the other
	return core.PythonicBool(set.IsSuperset(otherSet)), nil
}

// setIsStrictSupersetFunc implements the > operator for sets
func setIsStrictSupersetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.is_strict_superset() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.is_strict_superset() requires a set as first argument, got %T", args[0])
	}

	// Second argument should be a set
	otherSet, ok := args[1].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.is_strict_superset() requires a set as second argument, got %T", args[1])
	}

	// A strict superset must be a superset and the sets must not be equal
	return core.PythonicBool(set.IsSuperset(otherSet) && set.Size() > otherSet.Size()), nil
}

// setIsDisjointFunc implements set.isdisjoint()
func setIsDisjointFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.isdisjoint() requires exactly two arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.isdisjoint() requires a set as first argument, got %T", args[0])
	}

	// Second argument should be a set
	otherSet, ok := args[1].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.isdisjoint() requires a set as second argument, got %T", args[1])
	}

	// Return whether the sets are disjoint
	return core.PythonicBool(set.IsDisjoint(otherSet)), nil
}

// setIntersectionUpdateFunc implements set.intersection_update()
func setIntersectionUpdateFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("set.intersection_update() requires at least 2 arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.intersection_update() requires a set as first argument, got %T", args[0])
	}

	// Update with intersection of each additional set
	for _, arg := range args[1:] {
		otherSet, ok := arg.(*core.PythonicSet)
		if !ok {
			// Try to convert to a set if not already a set
			converted, err := setConstructor([]core.LispValue{arg}, nil)
			if err != nil {
				return nil, fmt.Errorf("set.intersection_update() requires sets or iterables as arguments, got %T", arg)
			}
			otherSet = converted.(*core.PythonicSet)
		}
		set.IntersectionUpdate(otherSet)
	}

	// Return None (Python's intersection_update method returns None)
	return core.PythonicNone{}, nil
}

// setDifferenceUpdateFunc implements set.difference_update()
func setDifferenceUpdateFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("set.difference_update() requires at least 2 arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.difference_update() requires a set as first argument, got %T", args[0])
	}

	// Update with difference of each additional set
	for _, arg := range args[1:] {
		otherSet, ok := arg.(*core.PythonicSet)
		if !ok {
			// Try to convert to a set if not already a set
			converted, err := setConstructor([]core.LispValue{arg}, nil)
			if err != nil {
				return nil, fmt.Errorf("set.difference_update() requires sets or iterables as arguments, got %T", arg)
			}
			otherSet = converted.(*core.PythonicSet)
		}
		set.DifferenceUpdate(otherSet)
	}

	// Return None (Python's difference_update method returns None)
	return core.PythonicNone{}, nil
}

// setSymmetricDifferenceUpdateFunc implements set.symmetric_difference_update()
func setSymmetricDifferenceUpdateFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set.symmetric_difference_update() requires exactly 2 arguments")
	}

	// First argument should be a set
	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("set.symmetric_difference_update() requires a set as first argument, got %T", args[0])
	}

	// Second argument should be a set
	otherSet, ok := args[1].(*core.PythonicSet)
	if !ok {
		// Try to convert to a set if not already a set
		converted, err := setConstructor([]core.LispValue{args[1]}, nil)
		if err != nil {
			return nil, fmt.Errorf("set.symmetric_difference_update() requires a set as second argument, got %T", args[1])
		}
		otherSet = converted.(*core.PythonicSet)
	}

	// Update the set with the symmetric difference
	set.SymmetricDifferenceUpdate(otherSet)

	// Return None (Python's symmetric_difference_update method returns None)
	return core.PythonicNone{}, nil
}

// Register all set functions
func RegisterSetFunctions() {
	// Set constructor (overwrites the old setFunc)
	core.RegisterBuiltin("set", setConstructor)

	// Set methods and operations - consistent module-like dot notation
	core.RegisterBuiltin("set.union", setUnionFunc)
	core.RegisterBuiltin("set.intersection", setIntersectionFunc)
	core.RegisterBuiltin("set.difference", setDifferenceFunc)
	core.RegisterBuiltin("set.symmetric_difference", setSymmetricDifferenceFunc)
	core.RegisterBuiltin("set.add", setAddFunc)
	core.RegisterBuiltin("set.remove", setRemoveFunc)
	core.RegisterBuiltin("set.discard", setDiscardFunc)
	core.RegisterBuiltin("set.contains", setContainsFunc)
	core.RegisterBuiltin("set.clear", setClearFunc)
	core.RegisterBuiltin("set.copy", setCopyFunc)
	core.RegisterBuiltin("set.pop", setPopFunc)
	core.RegisterBuiltin("set.update", setUpdateFunc)
	core.RegisterBuiltin("set.intersection_update", setIntersectionUpdateFunc)
	core.RegisterBuiltin("set.difference_update", setDifferenceUpdateFunc)
	core.RegisterBuiltin("set.symmetric_difference_update", setSymmetricDifferenceUpdateFunc)
	core.RegisterBuiltin("set.issubset", setIsSubsetFunc)
	core.RegisterBuiltin("set.issuperset", setIsSupersetFunc)
	core.RegisterBuiltin("set.isdisjoint", setIsDisjointFunc)

	// Set comparison operators
	core.RegisterBuiltin("set.is_strict_subset", setIsStrictSubsetFunc)
	core.RegisterBuiltin("set.is_strict_superset", setIsStrictSupersetFunc)
}

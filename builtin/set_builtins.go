package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Simple set operations (without dot notation)

// containsFunc tests if an element is in a set
func containsFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("contains requires exactly 2 arguments")
	}

	set, ok := args[0].(*core.PythonicSet)
	if !ok {
		return nil, fmt.Errorf("contains requires a set as first argument")
	}

	return core.PythonicBool(set.Contains(args[1])), nil
}

// unionFunc returns the union of two or more sets
func unionFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("union requires at least one set")
	}

	// Validate all arguments are sets
	for i, arg := range args {
		if _, ok := arg.(*core.PythonicSet); !ok {
			return nil, fmt.Errorf("union requires sets as arguments (argument %d is not a set)", i+1)
		}
	}

	// Start with an empty result set
	result := core.NewPythonicSet()

	// Add all elements from all sets
	for _, arg := range args {
		set := arg.(*core.PythonicSet)
		for elem := range set.Data() {
			result.Add(elem)
		}
	}

	return result, nil
}

// intersectionFunc returns the intersection of two or more sets
func intersectionFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("intersection requires at least one set")
	}

	// Validate all arguments are sets
	for i, arg := range args {
		if _, ok := arg.(*core.PythonicSet); !ok {
			return nil, fmt.Errorf("intersection requires sets as arguments (argument %d is not a set)", i+1)
		}
	}

	// With one set, just return a copy
	if len(args) == 1 {
		set := args[0].(*core.PythonicSet)
		result := core.NewPythonicSet()
		for elem := range set.Data() {
			result.Add(elem)
		}
		return result, nil
	}

	// Start with a copy of the first set
	firstSet := args[0].(*core.PythonicSet)
	result := core.NewPythonicSet()
	for elem := range firstSet.Data() {
		result.Add(elem)
	}

	// Intersect with each additional set
	for i := 1; i < len(args); i++ {
		set := args[i].(*core.PythonicSet)

		// Get elements to remove
		var toRemove []core.LispValue
		for elem := range result.Data() {
			if !set.Contains(elem) {
				toRemove = append(toRemove, elem)
			}
		}

		// Remove elements not in the current set
		for _, elem := range toRemove {
			result.Remove(elem)
		}
	}

	return result, nil
}

// differenceFunc returns the difference of sets (first set minus all others)
func differenceFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("difference requires at least one set")
	}

	// Validate all arguments are sets
	for i, arg := range args {
		if _, ok := arg.(*core.PythonicSet); !ok {
			return nil, fmt.Errorf("difference requires sets as arguments (argument %d is not a set)", i+1)
		}
	}

	// With one set, just return a copy
	if len(args) == 1 {
		set := args[0].(*core.PythonicSet)
		result := core.NewPythonicSet()
		for elem := range set.Data() {
			result.Add(elem)
		}
		return result, nil
	}

	// Start with a copy of the first set
	firstSet := args[0].(*core.PythonicSet)
	result := core.NewPythonicSet()
	for elem := range firstSet.Data() {
		result.Add(elem)
	}

	// Remove elements from each additional set
	for i := 1; i < len(args); i++ {
		set := args[i].(*core.PythonicSet)
		for elem := range set.Data() {
			result.Remove(elem)
		}
	}

	return result, nil
}

// Register simple set operations
func RegisterSetBuiltins() {
	core.RegisterBuiltin("contains", containsFunc)
	core.RegisterBuiltin("union", unionFunc)
	core.RegisterBuiltin("intersection", intersectionFunc)
	core.RegisterBuiltin("difference", differenceFunc)
}

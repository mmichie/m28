package core

// This file implements ObjProtocol adapters for PythonicSet type

// PythonicSetAdapter adapts PythonicSet to ObjProtocol
type PythonicSetAdapter struct {
	set *PythonicSet
}

// NewPythonicSetAdapter creates a new adapter for PythonicSet
func NewPythonicSetAdapter(set *PythonicSet) *PythonicSetAdapter {
	return &PythonicSetAdapter{set: set}
}

// GetProp retrieves a property or method
func (a *PythonicSetAdapter) GetProp(name string) (LispValue, bool) {
	// Methods
	switch name {
	case "size", "length", "len":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			return float64(a.set.Size()), nil
		}), true
	case "add":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("add", 1, len(args))
			}
			a.set.Add(args[0])
			return a.set, nil
		}), true
	case "remove":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("remove", 1, len(args))
			}
			a.set.Remove(args[0])
			return a.set, nil
		}), true
	case "contains":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("contains", 1, len(args))
			}
			return a.set.Contains(args[0]), nil
		}), true
	case "union":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("union", 1, len(args))
			}

			// Check if argument is a PythonicSet
			otherSet, ok := args[0].(*PythonicSet)
			if !ok {
				return nil, ErrTypeMismatch("set", TypeOf(args[0]))
			}

			// Create a new set for the result
			result := NewPythonicSet()

			// Add all elements from this set
			for elem := range a.set.Data() {
				result.Add(elem)
			}

			// Add all elements from the other set
			for elem := range otherSet.Data() {
				result.Add(elem)
			}

			return result, nil
		}), true
	case "intersection":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("intersection", 1, len(args))
			}

			// Check if argument is a PythonicSet
			otherSet, ok := args[0].(*PythonicSet)
			if !ok {
				return nil, ErrTypeMismatch("set", TypeOf(args[0]))
			}

			// Create a new set for the result
			result := NewPythonicSet()

			// Add elements that appear in both sets
			for elem := range a.set.Data() {
				if otherSet.Contains(elem) {
					result.Add(elem)
				}
			}

			return result, nil
		}), true
	case "difference":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 1 {
				return nil, ErrWrongArgCount("difference", 1, len(args))
			}

			// Check if argument is a PythonicSet
			otherSet, ok := args[0].(*PythonicSet)
			if !ok {
				return nil, ErrTypeMismatch("set", TypeOf(args[0]))
			}

			// Create a new set for the result
			result := NewPythonicSet()

			// Add elements from this set that don't appear in the other set
			for elem := range a.set.Data() {
				if !otherSet.Contains(elem) {
					result.Add(elem)
				}
			}

			return result, nil
		}), true
	case "tolist":
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			if len(args) != 0 {
				return nil, ErrWrongArgCount("tolist", 0, len(args))
			}

			// Create a list from the set elements
			result := a.set.sortedElements()
			return result, nil
		}), true
	}

	return nil, false
}

// SetProp sets a property value
func (a *PythonicSetAdapter) SetProp(name string, value LispValue) error {
	// Sets don't have settable properties
	return ErrDotNoPropertyf(name)
}

// HasMethodP checks if a method exists
func (a *PythonicSetAdapter) HasMethodP(name string) bool {
	switch name {
	case "size", "length", "len", "add", "remove", "contains", "union", "intersection", "difference", "tolist":
		return true
	}
	return false
}

// CallMethodP calls a method with arguments
func (a *PythonicSetAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	switch name {
	case "size", "length", "len":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("size", 0, len(args))
		}
		return float64(a.set.Size()), nil
	case "add":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("add", 1, len(args))
		}
		a.set.Add(args[0])
		return a.set, nil
	case "remove":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("remove", 1, len(args))
		}
		a.set.Remove(args[0])
		return a.set, nil
	case "contains":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("contains", 1, len(args))
		}
		return a.set.Contains(args[0]), nil
	case "union":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("union", 1, len(args))
		}

		// Check if argument is a PythonicSet
		otherSet, ok := args[0].(*PythonicSet)
		if !ok {
			return nil, ErrTypeMismatch("set", TypeOf(args[0]))
		}

		// Create a new set for the result
		result := NewPythonicSet()

		// Add all elements from this set
		for elem := range a.set.Data() {
			result.Add(elem)
		}

		// Add all elements from the other set
		for elem := range otherSet.Data() {
			result.Add(elem)
		}

		return result, nil
	case "intersection":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("intersection", 1, len(args))
		}

		// Check if argument is a PythonicSet
		otherSet, ok := args[0].(*PythonicSet)
		if !ok {
			return nil, ErrTypeMismatch("set", TypeOf(args[0]))
		}

		// Create a new set for the result
		result := NewPythonicSet()

		// Add elements that appear in both sets
		for elem := range a.set.Data() {
			if otherSet.Contains(elem) {
				result.Add(elem)
			}
		}

		return result, nil
	case "difference":
		if len(args) != 1 {
			return nil, ErrWrongArgCount("difference", 1, len(args))
		}

		// Check if argument is a PythonicSet
		otherSet, ok := args[0].(*PythonicSet)
		if !ok {
			return nil, ErrTypeMismatch("set", TypeOf(args[0]))
		}

		// Create a new set for the result
		result := NewPythonicSet()

		// Add elements from this set that don't appear in the other set
		for elem := range a.set.Data() {
			if !otherSet.Contains(elem) {
				result.Add(elem)
			}
		}

		return result, nil
	case "tolist":
		if len(args) != 0 {
			return nil, ErrWrongArgCount("tolist", 0, len(args))
		}

		// Create a list from the set elements
		result := a.set.sortedElements()
		return result, nil
	}

	return nil, ErrDotNoMethodf(name)
}

// Ensure PythonicSetAdapter implements ObjProtocol
var _ ObjProtocol = (*PythonicSetAdapter)(nil)

// Make PythonicSet implement AdaptableLispValue
func (s *PythonicSet) AsObject() ObjProtocol {
	return &PythonicSetAdapter{set: s}
}

// Ensure PythonicSet implements AdaptableLispValue
var _ AdaptableLispValue = (*PythonicSet)(nil)

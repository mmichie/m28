package core

// EnhancedObjectMember provides unified access to object members
// It integrates with both the new ObjProtocol and older interfaces
func EnhancedObjectMember(obj LispValue, name string, eval Evaluator, env Environment) (LispValue, error) {
	// Try to get the property or method using our helper
	if val, exists := GetPropFrom(obj, name); exists {
		return val, nil
	}

	// Property or method not found
	return nil, ErrDotNoPropertyf(name)
}

// EnhancedSetObjectMember provides unified setting of object members
// It integrates with both the new ObjProtocol and older interfaces
func EnhancedSetObjectMember(obj LispValue, name string, value LispValue, eval Evaluator, env Environment) error {
	// Try to set the property using our helper
	return SetPropOn(obj, name, value)
}

// EnhancedCallObjectMethod provides unified method calling
// It integrates with both the new ObjProtocol and older interfaces
func EnhancedCallObjectMethod(obj LispValue, name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// Make sure the method exists
	if !HasMethodPOn(obj, name) {
		return nil, ErrDotNoMethodf(name)
	}

	// Call the method using our helper
	return CallMethodPOn(obj, name, args, eval, env)
}

// EnhancedGetNestedMember retrieves a member from a nested chain of objects
// It integrates with both the new ObjProtocol and older interfaces
func EnhancedGetNestedMember(obj LispValue, path []string, eval Evaluator, env Environment) (LispValue, error) {
	if len(path) == 0 {
		return obj, nil
	}

	current := obj
	currentPath := ""

	// Navigate the object path
	for i, part := range path {
		currentPath = currentPath + "." + part

		// Get the current part
		next, err := EnhancedObjectMember(current, part, eval, env)
		if err != nil {
			return nil, ErrDotNestedAccessf(currentPath, part, err)
		}

		// If this is the last part, return it
		if i == len(path)-1 {
			return next, nil
		}

		// Otherwise, continue to the next part
		current = next
	}

	return current, nil
}

// DirectPropertyAccess provides a more direct way to access instance properties
// It's optimized for the common case of accessing attributes on class instances
func DirectPropertyAccess(obj LispValue, name string) (LispValue, error) {
	// Try direct instance property access first
	if val, exists := DirectGetProp(obj, name); exists {
		return val, nil
	}

	// Fall back to standard property access
	return nil, ErrDotNoPropertyf(name)
}

// DirectPropertySet provides a more direct way to set instance properties
// It's optimized for the common case of setting attributes on class instances
func DirectPropertySet(obj LispValue, name string, value LispValue) error {
	// Use our direct property setter
	return DirectSetProp(obj, name, value)
}

// Note: We're removing this function since it conflicts with an existing
// function in dot_notation.go. Instead, we'll update the existing function to use
// our new direct property access mechanism.

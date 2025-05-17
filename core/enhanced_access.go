package core

// DEPRECATED: Use AccessObjectMember from dot_notation.go instead.
// For new code, access properties directly with FastGetPropFrom.
func EnhancedObjectMember(obj LispValue, name string, eval Evaluator, env Environment) (LispValue, error) {
	return AccessObjectMember(obj, name, eval, env)
}

// DEPRECATED: Use SetObjectMember from dot_notation.go instead.
// For new code, set properties directly with FastSetPropOn.
func EnhancedSetObjectMember(obj LispValue, name string, value LispValue, eval Evaluator, env Environment) error {
	return SetObjectMember(obj, name, value, eval, env)
}

// CallObjectMethod is the unified method for calling methods on objects
// using the object protocol system
func CallObjectMethod(obj LispValue, name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	// Make sure the method exists
	if !FastHasMethodPOn(obj, name) {
		return nil, ErrDotNoMethodf(name)
	}

	// Call the method using our optimized helper
	return FastCallMethodPOn(obj, name, args, eval, env)
}

// DEPRECATED: Use CallObjectMethod instead
func EnhancedCallObjectMethod(obj LispValue, name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	return CallObjectMethod(obj, name, args, eval, env)
}

// GetNestedMember retrieves a member from a nested chain of objects
// using the unified object protocol system
func GetNestedMember(obj LispValue, path []string, eval Evaluator, env Environment) (LispValue, error) {
	if len(path) == 0 {
		return obj, nil
	}

	current := obj
	currentPath := ""

	// Navigate the object path
	for i, part := range path {
		currentPath = currentPath + "." + part

		// Try to get the property or method using our optimized helper first
		if val, exists := FastGetPropFrom(current, part); exists {
			// If this is the last part, return it
			if i == len(path)-1 {
				return val, nil
			}

			// Otherwise, continue to the next part
			current = val
		} else {
			// Property or method not found
			return nil, ErrDotNestedAccessf(currentPath, part, ErrDotNoPropertyf(part))
		}
	}

	return current, nil
}

// DEPRECATED: Use GetNestedMember instead
func EnhancedGetNestedMember(obj LispValue, path []string, eval Evaluator, env Environment) (LispValue, error) {
	return GetNestedMember(obj, path, eval, env)
}

// DirectPropertyAccess provides a more direct way to access instance properties
// It's optimized for the common case of accessing attributes on class instances
// This function is a thin wrapper around FastGetPropFrom
func DirectPropertyAccess(obj LispValue, name string) (LispValue, error) {
	// Use the FastGetPropFrom that now handles all types consistently
	if val, exists := FastGetPropFrom(obj, name); exists {
		return val, nil
	}

	// Nothing found
	return nil, ErrDotNoPropertyf(name)
}

// DirectPropertySet provides a more direct way to set instance properties
// It's optimized for the common case of setting attributes on class instances
// This function is a thin wrapper around FastSetPropOn
func DirectPropertySet(obj LispValue, name string, value LispValue) error {
	// Use the FastSetPropOn that now handles all types consistently
	return FastSetPropOn(obj, name, value)
}

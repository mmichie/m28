package core

import (
	"fmt"
)

// DotNotationHelper contains utility functions for dot notation access
// This provides a unified implementation for member access across different interfaces

// AccessObjectMember is a helper function to access a member via dot notation
// This provides a common implementation that can be used by both the evaluator and special forms
// This implementation has been simplified to use only FastGetPropFrom, which handles all the
// special cases and optimizations internally
func AccessObjectMember(obj LispValue, name string, eval Evaluator, env Environment) (LispValue, error) {
	if obj == nil {
		return nil, fmt.Errorf("cannot access member '%s' on nil", name)
	}

	// Use FastGetPropFrom, which handles all types through the unified object protocol
	// and provides optimized fast paths for common types
	if val, exists := FastGetPropFrom(obj, name); exists {
		return val, nil
	}

	// For unhandled types, provide a descriptive error
	return nil, ErrDotNoPropertyf(name)
}

// SetObjectMember is a helper function to set a member via dot notation
func SetObjectMember(obj LispValue, name string, value LispValue, eval Evaluator, env Environment) error {
	if obj == nil {
		return fmt.Errorf("cannot set member '%s' on nil", name)
	}

	// Use FastSetPropOn, which handles all types through the unified object protocol
	// and provides optimized fast paths for common types
	return FastSetPropOn(obj, name, value)
}

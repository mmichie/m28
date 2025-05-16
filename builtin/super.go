package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

func init() {
	// Register the super() builtin function
	core.RegisterBuiltin("super", superFunc)
}

// superFunc implements the super() builtin function to access parent class methods and attributes
// Usage: super() - must be called inside a method of a class
func superFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Check if we're in a method context
	selfVal, exists := env.Get("self")
	if !exists {
		return nil, fmt.Errorf("super() can only be called inside a method with 'self' in scope")
	}

	// Make sure self is a PythonicObject
	self, ok := selfVal.(*core.PythonicObject)
	if !ok {
		return nil, fmt.Errorf("super() can only be used with class instances, got %T", selfVal)
	}

	// Create a SuperObject wrapper for the instance
	superObj := &core.SuperObject{Object: self}
	return superObj, nil
}

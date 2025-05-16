package builtin

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// Register object access built-in functions
func init() {
	core.RegisterBuiltin("getattr", getAttrFunc)
	core.RegisterBuiltin("setattr", setAttrFunc)
	core.RegisterBuiltin("hasattr", hasAttrFunc)
	core.RegisterBuiltin("getitem", getitemFunc)
	core.RegisterBuiltin("setitem", setitemFunc)
	core.RegisterBuiltin("delitem", delitemFunc)
	core.RegisterBuiltin("dir", dirFunc)
}

// getAttrFunc implements the getattr function
// Usage: (getattr obj "property" [default])
func getAttrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("getattr requires 2 or 3 arguments: object, attribute name, and optional default value")
	}

	obj := args[0]

	// Get attribute name as string
	var attrName string
	switch attr := args[1].(type) {
	case string:
		attrName = attr
	case core.LispSymbol:
		attrName = string(attr)
	default:
		return nil, fmt.Errorf("attribute name must be a string or symbol")
	}

	// Try to get the attribute
	if val, exists := core.GetPropFrom(obj, attrName); exists {
		return val, nil
	}

	// If a default value was provided, return it
	if len(args) == 3 {
		return args[2], nil
	}

	// Otherwise, return an error
	return nil, fmt.Errorf("object has no attribute '%s'", attrName)
}

// setAttrFunc implements the setattr function
// Usage: (setattr obj "property" value)
func setAttrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("setattr requires 3 arguments: object, attribute name, and value")
	}

	obj := args[0]

	// Get attribute name as string
	var attrName string
	switch attr := args[1].(type) {
	case string:
		attrName = attr
	case core.LispSymbol:
		attrName = string(attr)
	default:
		return nil, fmt.Errorf("attribute name must be a string or symbol")
	}

	// Set the attribute
	err := core.SetPropOn(obj, attrName, args[2])
	if err != nil {
		return nil, err
	}

	// Return the set value
	return args[2], nil
}

// hasAttrFunc implements the hasattr function
// Usage: (hasattr obj "property")
func hasAttrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("hasattr requires 2 arguments: object and attribute name")
	}

	obj := args[0]

	// Get attribute name as string
	var attrName string
	switch attr := args[1].(type) {
	case string:
		attrName = attr
	case core.LispSymbol:
		attrName = string(attr)
	default:
		return nil, fmt.Errorf("attribute name must be a string or symbol")
	}

	// Check if the object has the attribute
	_, exists := core.GetPropFrom(obj, attrName)
	return core.PythonicBool(exists), nil
}

// getitemFunc implements the getitem function
// Usage: (getitem obj key [default])
func getitemFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("getitem requires 2 or 3 arguments: object, key, and optional default value")
	}

	obj := args[0]
	key := args[1]

	// Try to get the item
	result, err := core.GetItem(obj, key)
	if err == nil {
		return result, nil
	}

	// If a default value was provided, return it
	if len(args) == 3 {
		return args[2], nil
	}

	// Otherwise, return the error
	return nil, err
}

// setitemFunc implements the setitem function
// Usage: (setitem obj key value)
func setitemFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("setitem requires 3 arguments: object, key, and value")
	}

	obj := args[0]
	key := args[1]
	value := args[2]

	// Set the item
	err := core.SetItem(obj, key, value)
	if err != nil {
		return nil, err
	}

	// Return the set value
	return value, nil
}

// delitemFunc implements the delitem function
// Usage: (delitem obj key)
func delitemFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("delitem requires 2 arguments: object and key")
	}

	obj := args[0]
	key := args[1]

	// Delete the item
	err := core.DelItem(obj, key)
	if err != nil {
		return nil, err
	}

	// Return None
	return core.PythonicNone{}, nil
}

// dirFunc implements the dir function
// Usage: (dir obj)
func dirFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("dir requires 1 argument: object")
	}

	obj := args[0]

	// Get the list of attributes
	attrs, err := core.Dir(obj)
	if err != nil {
		return nil, err
	}

	// Convert to a LispList
	result := make(core.LispList, len(attrs))
	for i, attr := range attrs {
		result[i] = attr
	}

	return result, nil
}

package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Python-like dictionary operations

// dictGetFunc implements dict.get() - retrieves a value with optional default
func dictGetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("dict.get() requires 2 or 3 arguments")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.get() requires a dict as first argument, got %T", args[0])
	}

	// Second argument is the key
	key := args[1]

	// Default value (optional)
	var defaultValue core.LispValue = core.PythonicNone{}
	if len(args) == 3 {
		defaultValue = args[2]
	}

	// Get the value or return the default
	if value, ok := dict.Get(key); ok {
		return value, nil
	}
	return defaultValue, nil
}

// dictItemsFunc implements dict.items() - returns a list of key-value pairs
func dictItemsFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("dict.items() requires exactly one argument")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.items() requires a dict as argument, got %T", args[0])
	}

	// Create a list of key-value pairs
	var items core.LispList
	dict.Iterate(func(key, value core.LispValue) error {
		item := core.LispList{key, value}
		items = append(items, item)
		return nil
	})

	return items, nil
}

// dictKeysFunc implements dict.keys() - returns a list of keys
func dictKeysFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("dict.keys() requires exactly one argument")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.keys() requires a dict as argument, got %T", args[0])
	}

	// Get all keys (using the exported SortedKeys method)
	return core.LispList(dict.SortedKeys()), nil
}

// dictValuesFunc implements dict.values() - returns a list of values
func dictValuesFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("dict.values() requires exactly one argument")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.values() requires a dict as argument, got %T", args[0])
	}

	// Create a list of values
	var values core.LispList
	dict.Iterate(func(_, value core.LispValue) error {
		values = append(values, value)
		return nil
	})

	return values, nil
}

// dictPopFunc implements dict.pop() - removes and returns a value
func dictPopFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("dict.pop() requires 2 or 3 arguments")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.pop() requires a dict as first argument, got %T", args[0])
	}

	// Second argument is the key
	key := args[1]

	// Get the value before removing it
	value, ok := dict.Get(key)
	if !ok {
		// Key not found
		if len(args) == 3 {
			// Use default value
			return args[2], nil
		}
		return nil, fmt.Errorf("KeyError: %v", key)
	}

	// Remove the key
	dict.Delete(key)

	// Return the value
	return value, nil
}

// dictPopItemFunc implements dict.popitem() - removes and returns an arbitrary key-value pair
func dictPopItemFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("dict.popitem() requires exactly one argument")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.popitem() requires a dict as argument, got %T", args[0])
	}

	// Check if the dict is empty
	if dict.Size() == 0 {
		return nil, fmt.Errorf("KeyError: popitem(): dictionary is empty")
	}

	// Get an arbitrary key
	keys := dict.SortedKeys()
	if len(keys) == 0 {
		return nil, fmt.Errorf("KeyError: popitem(): dictionary is empty")
	}

	key := keys[0]
	value, _ := dict.Get(key)

	// Remove the key
	dict.Delete(key)

	// Return the key-value pair as a list
	return core.LispList{key, value}, nil
}

// dictSetDefaultFunc implements dict.setdefault() - returns value for key, sets to default if key not present
func dictSetDefaultFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("dict.setdefault() requires 2 or 3 arguments")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.setdefault() requires a dict as first argument, got %T", args[0])
	}

	// Second argument is the key
	key := args[1]

	// Default value (optional, defaults to None)
	var defaultValue core.LispValue = core.PythonicNone{}
	if len(args) == 3 {
		defaultValue = args[2]
	}

	// Check if the key exists
	if value, ok := dict.Get(key); ok {
		return value, nil
	}

	// Set the default value
	dict.Set(key, defaultValue)

	// Return the default value
	return defaultValue, nil
}

// dictUpdateFunc implements dict.update() - updates the dictionary with another dictionary
func dictUpdateFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("dict.update() requires at least 2 arguments")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.update() requires a dict as first argument, got %T", args[0])
	}

	// Update with each additional dictionary
	for _, arg := range args[1:] {
		otherDict, ok := arg.(*core.PythonicDict)
		if !ok {
			// Try to convert to a dict if not already a dict
			return nil, fmt.Errorf("dict.update() requires dicts as arguments, got %T", arg)
		}

		otherDict.Iterate(func(key, value core.LispValue) error {
			dict.Set(key, value)
			return nil
		})
	}

	// Return None (Python's update method returns None)
	return core.PythonicNone{}, nil
}

// dictClearFunc implements dict.clear() - removes all items from the dictionary
func dictClearFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("dict.clear() requires exactly one argument")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.clear() requires a dict as argument, got %T", args[0])
	}

	// We need to replace the map since we can't just clear it
	// Create a new empty map
	newDict := core.NewPythonicDict()

	// Replace the original map's data with the new one
	// This is a bit hacky, but it works for now
	*dict = *newDict

	// Return None (Python's clear method returns None)
	return core.PythonicNone{}, nil
}

// dictCopyFunc implements dict.copy() - returns a shallow copy of the dictionary
func dictCopyFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("dict.copy() requires exactly one argument")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.copy() requires a dict as argument, got %T", args[0])
	}

	// Create a copy of the dictionary
	newDict := core.NewPythonicDict()
	dict.Iterate(func(key, value core.LispValue) error {
		newDict.Set(key, value)
		return nil
	})

	return newDict, nil
}

// dictSetFunc implements dict.set() - sets a value in a dictionary
func dictSetFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("dict.set() requires exactly 3 arguments")
	}

	// First argument should be a dict
	dict, ok := args[0].(*core.PythonicDict)
	if !ok {
		return nil, fmt.Errorf("dict.set() requires a dict as first argument, got %T", args[0])
	}

	// Second argument is the key
	key := args[1]

	// Third argument is the value
	value := args[2]

	// Set the key-value pair
	dict.Set(key, value)

	// Return None (Python's set-like methods return None)
	return core.PythonicNone{}, nil
}

// Register all dictionary functions
func RegisterDictFunctions() {
	// Dictionary methods - using dot notation for method-like access
	core.RegisterBuiltin("dict.get", dictGetFunc)
	core.RegisterBuiltin("dict.items", dictItemsFunc)
	core.RegisterBuiltin("dict.keys", dictKeysFunc)
	core.RegisterBuiltin("dict.values", dictValuesFunc)
	core.RegisterBuiltin("dict.pop", dictPopFunc)
	core.RegisterBuiltin("dict.popitem", dictPopItemFunc)
	core.RegisterBuiltin("dict.setdefault", dictSetDefaultFunc)
	core.RegisterBuiltin("dict.update", dictUpdateFunc)
	core.RegisterBuiltin("dict.clear", dictClearFunc)
	core.RegisterBuiltin("dict.copy", dictCopyFunc)
	core.RegisterBuiltin("dict.set", dictSetFunc)
}

// init function to ensure RegisterDictFunctions is called
func init() {
	RegisterDictFunctions()
}

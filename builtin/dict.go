// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterDictFunctions registers dictionary-related functions in the global context
func RegisterDictFunctions(ctx *core.Context) {
	// Dictionary creation and access
	ctx.Define("dict", core.NewBuiltinFunction(DictFunc))
	ctx.Define("get", core.NewBuiltinFunction(GetFunc))
	ctx.Define("set", core.NewBuiltinFunction(SetFunc))
	ctx.Define("delete", core.NewBuiltinFunction(DeleteFunc))
	ctx.Define("has-key", core.NewBuiltinFunction(HasKeyFunc))

	// Dictionary operations
	ctx.Define("keys", core.NewBuiltinFunction(KeysFunc))
	ctx.Define("values", core.NewBuiltinFunction(ValuesFunc))
	ctx.Define("items", core.NewBuiltinFunction(ItemsFunc))
	ctx.Define("merge", core.NewBuiltinFunction(MergeFunc))
}

// DictFunc creates a new dictionary from key-value pairs
func DictFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args)%2 != 0 {
		return nil, fmt.Errorf("dict requires an even number of arguments (key-value pairs)")
	}

	// Create a new dictionary
	dict := core.NewDict()

	// Add key-value pairs
	for i := 0; i < len(args); i += 2 {
		// Get the key (must be a string)
		key, ok := args[i].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("dict keys must be strings, got %s", args[i].Type())
		}

		// Set the value
		dict.Set(string(key), args[i+1])
	}

	return dict, nil
}

// GetFunc retrieves a value from a dictionary
func GetFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("get requires 2 or 3 arguments: dict, key[, default]")
	}

	// Get the dictionary (ensure it's an Object with GetAttr capability)
	obj, ok := args[0].(core.Object)
	if !ok {
		return nil, fmt.Errorf("first argument must be a dictionary, got %s", args[0].Type())
	}

	// Make sure it's a dictionary
	if args[0].Type() != core.DictType {
		return nil, fmt.Errorf("first argument must be a dictionary, got %s", args[0].Type())
	}

	// Get the key
	key, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("key must be a string, got %s", args[1].Type())
	}

	// Try to get the value
	value, found := obj.GetAttr(string(key))
	if found {
		return value, nil
	}

	// Return default if provided
	if len(args) > 2 {
		return args[2], nil
	}

	// Otherwise return nil
	return core.Nil, nil
}

// SetFunc sets a value in a dictionary
func SetFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("set requires 3 arguments: dict, key, value")
	}

	// Get the dictionary (ensure it's an Object with SetAttr capability)
	obj, ok := args[0].(core.Object)
	if !ok {
		return nil, fmt.Errorf("first argument must be a dictionary, got %s", args[0].Type())
	}

	// Make sure it's a dictionary
	if args[0].Type() != core.DictType {
		return nil, fmt.Errorf("first argument must be a dictionary, got %s", args[0].Type())
	}

	// Get the key
	key, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("key must be a string, got %s", args[1].Type())
	}

	// Set the value
	err := obj.SetAttr(string(key), args[2])
	if err != nil {
		return nil, err
	}

	// Return the dictionary
	return args[0], nil
}

// DeleteFunc removes a key-value pair from a dictionary
func DeleteFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("delete requires 2 arguments: dict, key")
	}

	// Get the dictionary
	dict, ok := args[0].(*core.DictValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a dictionary, got %s", args[0].Type())
	}

	// Get the key
	key, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("key must be a string, got %s", args[1].Type())
	}

	// Delete the key - we need to use the direct method since Object interface doesn't have Delete
	dict.Delete(string(key))

	// Return the dictionary
	return dict, nil
}

// HasKeyFunc checks if a key exists in a dictionary
func HasKeyFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("has-key requires 2 arguments: dict, key")
	}

	// Get the dictionary (ensure it's an Object with GetAttr capability)
	obj, ok := args[0].(core.Object)
	if !ok {
		return nil, fmt.Errorf("first argument must be a dictionary, got %s", args[0].Type())
	}

	// Make sure it's a dictionary
	if args[0].Type() != core.DictType {
		return nil, fmt.Errorf("first argument must be a dictionary, got %s", args[0].Type())
	}

	// Get the key
	key, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("key must be a string, got %s", args[1].Type())
	}

	// Check if the attribute exists
	_, found := obj.GetAttr(string(key))

	// Return a boolean
	return core.BoolValue(found), nil
}

// KeysFunc returns a list of all keys in a dictionary
func KeysFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("keys requires 1 argument: dict")
	}

	// Get the dictionary
	obj, ok := args[0].(core.Object)
	if !ok {
		return nil, fmt.Errorf("argument must be a dictionary, got %s", args[0].Type())
	}

	// Make sure it's a dictionary
	if args[0].Type() != core.DictType {
		return nil, fmt.Errorf("argument must be a dictionary, got %s", args[0].Type())
	}

	// Call the keys method using the object protocol
	keysMethod, found := obj.GetAttr("keys")
	if !found {
		return nil, fmt.Errorf("dictionary does not have keys method")
	}

	// Call the method
	callable, ok := keysMethod.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("keys is not callable")
	}

	return callable.Call([]core.Value{}, ctx)
}

// ValuesFunc returns a list of all values in a dictionary
func ValuesFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("values requires 1 argument: dict")
	}

	// Get the dictionary
	obj, ok := args[0].(core.Object)
	if !ok {
		return nil, fmt.Errorf("argument must be a dictionary, got %s", args[0].Type())
	}

	// Make sure it's a dictionary
	if args[0].Type() != core.DictType {
		return nil, fmt.Errorf("argument must be a dictionary, got %s", args[0].Type())
	}

	// Call the values method using the object protocol
	valuesMethod, found := obj.GetAttr("values")
	if !found {
		return nil, fmt.Errorf("dictionary does not have values method")
	}

	// Call the method
	callable, ok := valuesMethod.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("values is not callable")
	}

	return callable.Call([]core.Value{}, ctx)
}

// ItemsFunc returns a list of key-value pairs from a dictionary
func ItemsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("items requires 1 argument: dict")
	}

	// Get the dictionary
	obj, ok := args[0].(core.Object)
	if !ok {
		return nil, fmt.Errorf("argument must be a dictionary, got %s", args[0].Type())
	}

	// Make sure it's a dictionary
	if args[0].Type() != core.DictType {
		return nil, fmt.Errorf("argument must be a dictionary, got %s", args[0].Type())
	}

	// Call the items method using the object protocol
	itemsMethod, found := obj.GetAttr("items")
	if !found {
		return nil, fmt.Errorf("dictionary does not have items method")
	}

	// Call the method
	callable, ok := itemsMethod.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("items is not callable")
	}

	return callable.Call([]core.Value{}, ctx)
}

// MergeFunc merges two or more dictionaries
func MergeFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("merge requires at least 1 argument: dictionaries to merge")
	}

	// Check that all arguments are dictionaries
	for i, arg := range args {
		if arg.Type() != core.DictType {
			return nil, fmt.Errorf("argument %d must be a dictionary, got %s", i+1, arg.Type())
		}

		if _, ok := arg.(core.Object); !ok {
			return nil, fmt.Errorf("argument %d must be a dictionary object, got %s", i+1, arg.Type())
		}
	}

	// Create a new dictionary
	result := core.NewDict()

	// Merge all dictionaries
	for _, arg := range args {
		dict := arg.(core.Object)

		// Get the keys
		keysMethod, found := dict.GetAttr("keys")
		if !found {
			return nil, fmt.Errorf("dictionary does not have keys method")
		}

		// Call the keys method
		callable, ok := keysMethod.(core.Callable)
		if !ok {
			return nil, fmt.Errorf("keys is not callable")
		}

		keysList, err := callable.Call([]core.Value{}, ctx)
		if err != nil {
			return nil, err
		}

		// Iterate over the keys
		keys, ok := keysList.(core.ListValue)
		if !ok {
			return nil, fmt.Errorf("keys did not return a list")
		}

		for _, keyVal := range keys {
			keyStr, ok := keyVal.(core.StringValue)
			if !ok {
				return nil, fmt.Errorf("key is not a string")
			}

			// Get the value for this key
			value, found := dict.GetAttr(string(keyStr))
			if found {
				result.Set(string(keyStr), value)
			}
		}
	}

	return result, nil
}

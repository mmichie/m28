package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterDictOps registers dict operation functions
func RegisterDictOps(ctx *core.Context) {
	ctx.Define("select-keys", core.NewNamedBuiltinFunction("select-keys", SelectKeysBuilder()))
	ctx.Define("rename-keys", core.NewNamedBuiltinFunction("rename-keys", RenameKeysBuilder()))
	ctx.Define("map-keys", core.NewNamedBuiltinFunction("map-keys", MapKeysBuilder()))
	ctx.Define("map-values", core.NewNamedBuiltinFunction("map-values", MapValuesBuilder()))
	ctx.Define("filter-keys", core.NewNamedBuiltinFunction("filter-keys", FilterKeysBuilder()))
	ctx.Define("filter-values", core.NewNamedBuiltinFunction("filter-values", FilterValuesBuilder()))
}

// SelectKeysBuilder implements select-keys function
// (select-keys dict keys) - extract subset of keys, skip missing keys
func SelectKeysBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("select-keys", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Get dict argument
		dictArg := v.Get(0)
		dict, ok := dictArg.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("select-keys requires dict as first argument, got %s", dictArg.Type())
		}

		// Get keys list
		keysArg := v.Get(1)
		keysList, ok := keysArg.(core.ListValue)
		if !ok {
			return nil, fmt.Errorf("select-keys requires list as second argument, got %s", keysArg.Type())
		}

		// Build result dict - skip missing keys
		result := core.NewDict()

		for _, keyVal := range keysList {
			// Get the value from dict
			value, exists := dict.GetValue(keyVal)

			if exists {
				// Key exists - add to result
				result.SetValue(keyVal, value)
			}
			// Otherwise skip (missing keys are silently ignored)
		}

		return result, nil
	}
}

// RenameKeysBuilder implements rename-keys function
// (rename-keys dict mapping) - rename using dict mapping
// (rename-keys dict func) - rename using function
func RenameKeysBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("rename-keys", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Get dict argument
		dictArg := v.Get(0)
		dict, ok := dictArg.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("rename-keys requires dict as first argument, got %s", dictArg.Type())
		}

		mapperArg := v.Get(1)

		// Check if mapper is a dict (mapping) or callable (function)
		if mappingDict, ok := mapperArg.(*core.DictValue); ok {
			return renameKeysWithMapping(dict, mappingDict)
		}

		if callable, ok := types.AsCallable(mapperArg); ok {
			return renameKeysWithFunction(dict, callable, ctx)
		}

		return nil, fmt.Errorf("rename-keys requires dict mapping or function as second argument, got %s", mapperArg.Type())
	}
}

// renameKeysWithMapping renames keys using a dict mapping
func renameKeysWithMapping(dict *core.DictValue, mapping *core.DictValue) (core.Value, error) {
	result := core.NewDict()
	seenKeys := make(map[string]bool)

	// Process each key in original dict
	for _, origKey := range dict.OriginalKeys() {
		value, _ := dict.GetValue(origKey)

		// Check if key should be renamed
		newKey, shouldRename := mapping.GetValue(origKey)
		if !shouldRename {
			// No mapping - keep original key
			newKey = origKey
		}

		// Check for collision
		keyStr := core.ValueToKey(newKey)
		if seenKeys[keyStr] {
			return nil, fmt.Errorf("rename-keys: key collision - key %v already exists after renaming", newKey)
		}
		seenKeys[keyStr] = true

		result.SetValue(newKey, value)
	}

	return result, nil
}

// renameKeysWithFunction renames keys using a function
func renameKeysWithFunction(dict *core.DictValue, fn core.Callable, ctx *core.Context) (core.Value, error) {
	result := core.NewDict()
	seenKeys := make(map[string]bool)

	// Process each key in original dict
	for _, origKey := range dict.OriginalKeys() {
		value, _ := dict.GetValue(origKey)

		// Apply function to key
		newKey, err := fn.Call([]core.Value{origKey}, ctx)
		if err != nil {
			return nil, fmt.Errorf("rename-keys: error applying function to key %v: %w", origKey, err)
		}

		// Check for collision
		keyStr := core.ValueToKey(newKey)
		if seenKeys[keyStr] {
			return nil, fmt.Errorf("rename-keys: key collision - key %v already exists after renaming", newKey)
		}
		seenKeys[keyStr] = true

		result.SetValue(newKey, value)
	}

	return result, nil
}

// MapKeysBuilder implements map-keys function
// (map-keys dict func) - transform all keys with function
func MapKeysBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("map-keys", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Get dict argument
		dictArg := v.Get(0)
		dict, ok := dictArg.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("map-keys requires dict as first argument, got %s", dictArg.Type())
		}

		// Get function argument
		fnArg := v.Get(1)
		callable, ok := types.AsCallable(fnArg)
		if !ok {
			return nil, fmt.Errorf("map-keys requires callable as second argument, got %s", fnArg.Type())
		}

		return renameKeysWithFunction(dict, callable, ctx)
	}
}

// MapValuesBuilder implements map-values function
// (map-values dict func) - transform all values with function
func MapValuesBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("map-values", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Get dict argument
		dictArg := v.Get(0)
		dict, ok := dictArg.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("map-values requires dict as first argument, got %s", dictArg.Type())
		}

		// Get function argument
		fnArg := v.Get(1)
		callable, ok := types.AsCallable(fnArg)
		if !ok {
			return nil, fmt.Errorf("map-values requires callable as second argument, got %s", fnArg.Type())
		}

		result := core.NewDict()

		// Process each key-value pair
		for _, key := range dict.OriginalKeys() {
			value, _ := dict.GetValue(key)

			// Apply function to value
			newValue, err := callable.Call([]core.Value{value}, ctx)
			if err != nil {
				return nil, fmt.Errorf("map-values: error applying function to value: %w", err)
			}

			result.SetValue(key, newValue)
		}

		return result, nil
	}
}

// FilterKeysBuilder implements filter-keys function
// (filter-keys dict pred) - keep only keys matching predicate
func FilterKeysBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("filter-keys", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Get dict argument
		dictArg := v.Get(0)
		dict, ok := dictArg.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("filter-keys requires dict as first argument, got %s", dictArg.Type())
		}

		// Get predicate function
		predArg := v.Get(1)
		callable, ok := types.AsCallable(predArg)
		if !ok {
			return nil, fmt.Errorf("filter-keys requires callable as second argument, got %s", predArg.Type())
		}

		result := core.NewDict()

		// Process each key-value pair
		for _, key := range dict.OriginalKeys() {
			value, _ := dict.GetValue(key)

			// Apply predicate to key
			keepVal, err := callable.Call([]core.Value{key}, ctx)
			if err != nil {
				return nil, fmt.Errorf("filter-keys: error applying predicate to key: %w", err)
			}

			// Check if result is truthy
			if types.IsTruthy(keepVal) {
				result.SetValue(key, value)
			}
		}

		return result, nil
	}
}

// FilterValuesBuilder implements filter-values function
// (filter-values dict pred) - keep only values matching predicate
func FilterValuesBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("filter-values", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		// Get dict argument
		dictArg := v.Get(0)
		dict, ok := dictArg.(*core.DictValue)
		if !ok {
			return nil, fmt.Errorf("filter-values requires dict as first argument, got %s", dictArg.Type())
		}

		// Get predicate function
		predArg := v.Get(1)
		callable, ok := types.AsCallable(predArg)
		if !ok {
			return nil, fmt.Errorf("filter-values requires callable as second argument, got %s", predArg.Type())
		}

		result := core.NewDict()

		// Process each key-value pair
		for _, key := range dict.OriginalKeys() {
			value, _ := dict.GetValue(key)

			// Apply predicate to value
			keepVal, err := callable.Call([]core.Value{value}, ctx)
			if err != nil {
				return nil, fmt.Errorf("filter-values: error applying predicate to value: %w", err)
			}

			// Check if result is truthy
			if types.IsTruthy(keepVal) {
				result.SetValue(key, value)
			}
		}

		return result, nil
	}
}

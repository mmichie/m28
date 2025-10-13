package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/builders"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterMergeOps registers merge operation functions
func RegisterMergeOps(ctx *core.Context) {
	ctx.Define("merge", core.NewNamedBuiltinFunction("merge", MergeBuilder()))
	ctx.Define("deep-merge", core.NewNamedBuiltinFunction("deep-merge", DeepMergeBuilder()))
	ctx.Define("merge-with", core.NewNamedBuiltinFunction("merge-with", MergeWithBuilder()))
}

// MergeBuilder implements merge function
// (merge dict1 dict2 ...) - shallow merge, later values win
func MergeBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No args returns empty dict
		if len(args) == 0 {
			return core.NewDict(), nil
		}

		// Validate all arguments are dicts
		for i, arg := range args {
			if _, ok := arg.(*core.DictValue); !ok {
				return nil, fmt.Errorf("merge requires dict arguments, got %s at position %d", arg.Type(), i)
			}
		}

		// Create result dict and populate it
		result := core.NewDict()

		// Copy all keys from all dicts in order (later wins)
		for _, arg := range args {
			dict := arg.(*core.DictValue)
			for _, key := range dict.OriginalKeys() {
				value, _ := dict.GetValue(key)
				if err := result.SetValue(key, value); err != nil {
					return nil, fmt.Errorf("error setting key %v: %w", key, err)
				}
			}
		}

		return result, nil
	}
}

// DeepMergeBuilder implements deep-merge function
// (deep-merge dict1 dict2 ...) - recursive merge, later values win
func DeepMergeBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// No args returns empty dict
		if len(args) == 0 {
			return core.NewDict(), nil
		}

		// Validate all arguments are dicts
		for i, arg := range args {
			if _, ok := arg.(*core.DictValue); !ok {
				return nil, fmt.Errorf("deep-merge requires dict arguments, got %s at position %d", arg.Type(), i)
			}
		}

		// Start with first dict (or empty if none)
		result := core.NewDict()

		// Deep merge each dict in order
		for _, arg := range args {
			dict := arg.(*core.DictValue)
			var err error
			result, err = deepMergeTwoDicts(result, dict)
			if err != nil {
				return nil, err
			}
		}

		return result, nil
	}
}

// MergeWithBuilder implements merge-with function
// (merge-with fn dict1 dict2 ...) - merge with custom conflict resolution
func MergeWithBuilder() builders.BuiltinFunc {
	return func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("merge-with", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// First argument must be callable
		fnArg := v.Get(0)
		callable, ok := types.AsCallable(fnArg)
		if !ok {
			return nil, fmt.Errorf("merge-with requires callable as first argument, got %s", fnArg.Type())
		}

		// Need at least 2 dicts after function
		if len(args) < 3 {
			return nil, fmt.Errorf("merge-with requires at least 2 dicts after function, got %d", len(args)-1)
		}

		// Validate all remaining arguments are dicts
		for i := 1; i < len(args); i++ {
			if _, ok := args[i].(*core.DictValue); !ok {
				return nil, fmt.Errorf("merge-with requires dict arguments, got %s at position %d", args[i].Type(), i-1)
			}
		}

		// Start with first dict
		result := args[1].(*core.DictValue)

		// Merge each subsequent dict with function
		for i := 2; i < len(args); i++ {
			dict := args[i].(*core.DictValue)
			merged, err := mergeWithFunction(callable, result, dict, ctx)
			if err != nil {
				return nil, err
			}
			result = merged
		}

		return result, nil
	}
}

// deepMergeTwoDicts performs recursive merge of two dicts
func deepMergeTwoDicts(dict1, dict2 *core.DictValue) (*core.DictValue, error) {
	result := core.NewDict()

	// Copy all keys from dict1
	for _, key := range dict1.OriginalKeys() {
		value, _ := dict1.GetValue(key)
		if err := result.SetValue(key, value); err != nil {
			return nil, fmt.Errorf("error setting key from dict1: %w", err)
		}
	}

	// Merge keys from dict2
	for _, key := range dict2.OriginalKeys() {
		value2, _ := dict2.GetValue(key)

		// Check if key exists in result (from dict1)
		value1, exists := result.GetValue(key)

		if !exists {
			// Key only in dict2 - add it
			if err := result.SetValue(key, value2); err != nil {
				return nil, fmt.Errorf("error setting key from dict2: %w", err)
			}
			continue
		}

		// Key exists in both - check if both are dicts
		dict1Val, ok1 := value1.(*core.DictValue)
		dict2Val, ok2 := value2.(*core.DictValue)

		if ok1 && ok2 {
			// Both are dicts - recurse
			merged, err := deepMergeTwoDicts(dict1Val, dict2Val)
			if err != nil {
				return nil, err
			}
			if err := result.SetValue(key, merged); err != nil {
				return nil, fmt.Errorf("error setting merged value: %w", err)
			}
		} else {
			// Type mismatch or non-dicts - later wins
			if err := result.SetValue(key, value2); err != nil {
				return nil, fmt.Errorf("error setting value from dict2: %w", err)
			}
		}
	}

	return result, nil
}

// mergeWithFunction merges two dicts using a function for conflict resolution
func mergeWithFunction(fn core.Callable, dict1, dict2 *core.DictValue, ctx *core.Context) (*core.DictValue, error) {
	result := core.NewDict()

	// Track which keys we've seen
	seenKeys := make(map[string]bool)

	// Copy all keys from dict1
	for _, key := range dict1.OriginalKeys() {
		value, _ := dict1.GetValue(key)
		result.SetValue(key, value)
		seenKeys[core.ValueToKey(key)] = true
	}

	// Merge keys from dict2
	for _, key := range dict2.OriginalKeys() {
		value2, _ := dict2.GetValue(key)
		keyStr := core.ValueToKey(key)

		if seenKeys[keyStr] {
			// Key exists in both - apply function
			value1, _ := result.GetValue(key)

			mergedValue, err := fn.Call([]core.Value{value1, value2}, ctx)
			if err != nil {
				return nil, fmt.Errorf("merge-with: error applying function to key %v: %w", key, err)
			}

			result.SetValue(key, mergedValue)
		} else {
			// Key only in dict2 - add it
			result.SetValue(key, value2)
		}
	}

	return result, nil
}

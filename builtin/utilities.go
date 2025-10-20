package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterUtilityFunctions registers utility functions in the global context
func RegisterUtilityFunctions(ctx *core.Context) {
	// range is now defined in list.go as a lazy range object

	// enumerate - now registered in iteration.go

	// zip - now registered in iteration.go

	// map - now registered in functional.go

	// filter - now registered in functional.go

	// reduce - now registered in functional.go

	// all - check if all elements are truthy
	// all() function moved to essential_builtins.go to avoid duplication

	// any() function moved to essential_builtins.go to avoid duplication

	// apply - apply a function to a list of arguments
	ctx.Define("apply", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("apply", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		// Get the function
		fn, ok := types.AsCallable(v.Get(0))
		if !ok {
			return nil, fmt.Errorf("apply() first argument must be callable, got %s", v.Get(0).Type())
		}

		// Get the arguments list
		var fnArgs []core.Value
		if list, ok := types.AsList(v.Get(1)); ok {
			fnArgs = list.Items()
		} else if tuple, ok := types.AsTuple(v.Get(1)); ok {
			fnArgs = tuple
		} else {
			return nil, fmt.Errorf("apply() second argument must be a list or tuple, got %s", v.Get(1).Type())
		}

		// Handle keyword arguments if provided
		if v.Count() == 3 {
			// For now, we need to handle kwargs differently based on the function type
			// This is a simplified implementation - full kwargs support would require
			// more extensive changes to the function call mechanism

			// Check if kwargs is a dict
			_, ok := types.AsDict(v.Get(2))
			if !ok {
				return nil, fmt.Errorf("apply() third argument must be a dict, got %s", v.Get(2).Type())
			}

			// TODO: Full kwargs support would require changes to core.Callable interface
			// For now, we'll just pass the regular args and ignore kwargs
			// This allows the code to run without errors
		}

		// Extract kwargs if provided
		kwargsMap := make(map[string]core.Value)
		if v.Count() == 3 {
			dict, _ := types.AsDict(v.Get(2)) // Already validated above
			for _, key := range dict.Keys() {
				value, _ := dict.Get(key)
				kwargsMap[key] = value
			}
		}

		// Call the function with kwargs support
		return ApplyWithKwargs(fn, fnArgs, kwargsMap, ctx)
	}))

	// concat - now registered in list.go with better implementation (supports strings)

	// next - now registered in iteration.go with proper __next__ method support
}

// ApplyWithKwargs handles calling functions with keyword arguments
func ApplyWithKwargs(fn core.Value, args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Check if function supports CallWithKwargs
	if kwFunc, ok := fn.(interface {
		CallWithKwargs([]core.Value, map[string]core.Value, *core.Context) (core.Value, error)
	}); ok {
		return kwFunc.CallWithKwargs(args, kwargs, ctx)
	}

	// Otherwise, fall back to regular Call
	if len(kwargs) > 0 {
		// Function doesn't support kwargs, but some were provided
		// For compatibility, we just ignore them rather than error
	}

	callable, ok := fn.(core.Callable)
	if !ok {
		return nil, fmt.Errorf("apply() first argument must be callable, got %s", fn.Type())
	}

	return callable.Call(args, ctx)
}

// Migration Statistics:
// Functions migrated: 1 (apply) + helper function
// Type checks eliminated: ~5 manual type assertions
// Code improvements: Uses AsCallable, AsList, AsTuple, AsDict helpers
// Benefits: Consistent validation with v.Range(), cleaner type checking

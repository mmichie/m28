package modules

import (
	"fmt"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitFunctoolsModule creates and returns the functools module
func InitFunctoolsModule() *core.DictValue {
	functoolsModule := core.NewDict()

	// reduce - Apply function cumulatively to items
	functoolsModule.Set("reduce", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("reduce", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		function, err := types.RequireCallable(v.Get(0), "reduce() first argument")
		if err != nil {
			return nil, err
		}

		iterable, err := types.RequireIterable(v.Get(1), "reduce() second argument")
		if err != nil {
			return nil, err
		}

		iter := iterable.Iterator()

		// Get initial value
		var accumulator core.Value
		hasInitial := v.Count() == 3

		if hasInitial {
			accumulator = v.Get(2)
		} else {
			// Use first item as initial value
			val, hasNext := iter.Next()
			if !hasNext {
				return nil, errors.NewRuntimeError("reduce", "reduce() of empty sequence with no initial value")
			}
			accumulator = val
		}

		// Reduce the rest
		for {
			val, hasNext := iter.Next()
			if !hasNext {
				break
			}

			result, err := function.Call([]core.Value{accumulator, val}, ctx)
			if err != nil {
				return nil, err
			}
			accumulator = result
		}

		return accumulator, nil
	}))

	// partial - Create partial function with fixed arguments
	functoolsModule.Set("partial", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("partial", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		function, err := types.RequireCallable(v.Get(0), "partial() first argument")
		if err != nil {
			return nil, err
		}

		// Capture the fixed arguments
		fixedArgs := make([]core.Value, v.Count()-1)
		for i := 1; i < v.Count(); i++ {
			fixedArgs[i-1] = v.Get(i)
		}

		// Create a new function that prepends the fixed arguments
		partialFunc := core.NewBuiltinFunction(func(newArgs []core.Value, newCtx *core.Context) (core.Value, error) {
			// Combine fixed args with new args
			allArgs := make([]core.Value, 0, len(fixedArgs)+len(newArgs))
			allArgs = append(allArgs, fixedArgs...)
			allArgs = append(allArgs, newArgs...)

			return function.Call(allArgs, newCtx)
		})

		return partialFunc, nil
	}))

	// cache - Simple memoization decorator (limited version)
	functoolsModule.Set("cache", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("cache", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		function, err := types.RequireCallable(v.Get(0), "cache() argument")
		if err != nil {
			return nil, err
		}

		// Create cache storage
		cache := make(map[string]core.Value)

		// Create cached version
		cachedFunc := core.NewBuiltinFunction(func(callArgs []core.Value, callCtx *core.Context) (core.Value, error) {
			// Create cache key from arguments
			key := makeCacheKey(callArgs)

			// Check cache
			if cached, found := cache[key]; found {
				return cached, nil
			}

			// Call function and cache result
			result, err := function.Call(callArgs, callCtx)
			if err != nil {
				return nil, err
			}

			cache[key] = result
			return result, nil
		})

		return cachedFunc, nil
	}))

	// lru_cache - LRU cache decorator with size limit
	functoolsModule.Set("lru_cache", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("lru_cache", args)
		if err := v.Range(1, 2); err != nil {
			return nil, err
		}

		function, err := types.RequireCallable(v.Get(0), "lru_cache() first argument")
		if err != nil {
			return nil, err
		}

		maxsize, _ := v.GetNumberOrDefault(1, 128)
		maxsizeInt := int(maxsize)

		// Simple LRU cache using a map and list
		cache := make(map[string]core.Value)
		order := make([]string, 0, maxsizeInt)

		cachedFunc := core.NewBuiltinFunction(func(callArgs []core.Value, callCtx *core.Context) (core.Value, error) {
			key := makeCacheKey(callArgs)

			// Check cache
			if cached, found := cache[key]; found {
				// Move to end (most recently used)
				for i, k := range order {
					if k == key {
						order = append(order[:i], order[i+1:]...)
						break
					}
				}
				order = append(order, key)
				return cached, nil
			}

			// Call function
			result, err := function.Call(callArgs, callCtx)
			if err != nil {
				return nil, err
			}

			// Add to cache
			if len(cache) >= maxsizeInt {
				// Remove least recently used
				oldest := order[0]
				delete(cache, oldest)
				order = order[1:]
			}

			cache[key] = result
			order = append(order, key)

			return result, nil
		})

		return cachedFunc, nil
	}))

	// cmp_to_key - Convert old-style comparison function to key function
	functoolsModule.Set("cmp_to_key", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("cmp_to_key", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		cmpFunc, err := types.RequireCallable(v.Get(0), "cmp_to_key() argument")
		if err != nil {
			return nil, err
		}

		// Create a key function that wraps the comparison
		keyFunc := core.NewBuiltinFunction(func(callArgs []core.Value, callCtx *core.Context) (core.Value, error) {
			if len(callArgs) != 1 {
				return nil, errors.NewRuntimeError("cmp_to_key", "key function takes exactly one argument")
			}

			// Return a wrapper that can be compared
			wrapper := core.NewDict()
			wrapper.Set("value", callArgs[0])
			wrapper.Set("cmp", cmpFunc)

			return wrapper, nil
		})

		return keyFunc, nil
	}))

	// wraps - Decorator that updates wrapper function to look like wrapped
	// Simplified version that returns a decorator function
	functoolsModule.Set("wraps", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("wraps", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// In a full implementation, we would use this to copy metadata
		_ = v.Get(0) // wrapped function

		// Return a decorator function
		decorator := core.NewBuiltinFunction(func(decoratorArgs []core.Value, decoratorCtx *core.Context) (core.Value, error) {
			if len(decoratorArgs) != 1 {
				return nil, errors.NewRuntimeError("wraps", "decorator takes exactly one argument")
			}

			wrapper := decoratorArgs[0]

			// In a full implementation, we would copy __name__, __doc__, etc. from wrapped to wrapper
			// For now, just return the wrapper as-is
			// TODO: Copy metadata attributes if wrapper is a dict or has __dict__

			return wrapper, nil
		})

		return decorator, nil
	}))

	return functoolsModule
}

// makeCacheKey creates a string key from function arguments
func makeCacheKey(args []core.Value) string {
	if len(args) == 0 {
		return "()"
	}

	key := "("
	for i, arg := range args {
		if i > 0 {
			key += ", "
		}
		// Use string representation for key
		// This is simple but may have collisions for complex objects
		key += fmt.Sprintf("%v", arg)
	}
	key += ")"

	return key
}

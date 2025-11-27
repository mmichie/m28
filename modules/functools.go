package modules

import (
	"fmt"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// createLRUDecorator creates a decorator function for lru_cache
func createLRUDecorator(maxsizeInt int) core.Value {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, errors.NewRuntimeError("lru_cache", "decorator takes exactly one function argument")
		}

		function, ok := args[0].(core.Callable)
		if !ok {
			return nil, errors.NewRuntimeError("lru_cache", "argument must be callable")
		}

		// Create cache for this function
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

		// Add cache_clear method as an attribute
		// Python's lru_cache adds this to allow clearing the cache
		cache_clear := core.NewBuiltinFunction(func(clearArgs []core.Value, clearCtx *core.Context) (core.Value, error) {
			// Clear the cache
			for k := range cache {
				delete(cache, k)
			}
			order = order[:0]
			return core.None, nil
		})
		cachedFunc.SetAttr("cache_clear", cache_clear)

		return cachedFunc, nil
	})
}

// Init_FunctoolsModule creates and returns the _functools C extension stub
// This provides the C functions that Python's functools.py imports
func Init_FunctoolsModule() *core.DictValue {
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
	functoolsModule.Set("partial", &partialBuiltin{})

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
	// Can be used as @lru_cache or @lru_cache(maxsize=N, typed=True)
	functoolsModule.Set("lru_cache", &lruCacheBuiltin{})

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
			// TODO(M28-1d21): Copy metadata attributes if wrapper is a dict or has __dict__

			return wrapper, nil
		})

		return decorator, nil
	}))

	// _lru_cache_wrapper - Internal LRU cache wrapper class
	// Used by lru_cache implementation in functools.py
	functoolsModule.Set("_lru_cache_wrapper", &lruCacheBuiltin{})

	// total_ordering - Class decorator that fills in missing ordering methods
	// Takes a class with __eq__ and one ordering method and generates the rest
	functoolsModule.Set("total_ordering", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("total_ordering", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// For now, just return the class unchanged
		// A full implementation would:
		// 1. Check which comparison methods are defined
		// 2. Generate the missing ones based on __eq__ and the defined one
		// For our stub, we'll assume classes already have all methods they need
		cls := v.Get(0)
		return cls, nil
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

// partialBuiltin implements partial with keyword argument support
type partialBuiltin struct {
	core.BaseObject
}

func (p *partialBuiltin) Type() core.Type {
	return core.FunctionType
}

func (p *partialBuiltin) String() string {
	return "<built-in function partial>"
}

func (p *partialBuiltin) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	return p.CallWithKeywords(args, nil, ctx)
}

func (p *partialBuiltin) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("partial", args)
	if err := v.Min(1); err != nil {
		return nil, err
	}

	function, err := types.RequireCallable(v.Get(0), "partial() first argument")
	if err != nil {
		return nil, err
	}

	// Capture the fixed positional arguments
	fixedArgs := make([]core.Value, v.Count()-1)
	for i := 1; i < v.Count(); i++ {
		fixedArgs[i-1] = v.Get(i)
	}

	// Capture the fixed keyword arguments
	fixedKwargs := make(map[string]core.Value)
	if kwargs != nil {
		for k, v := range kwargs {
			fixedKwargs[k] = v
		}
	}

	// Create a new function that prepends the fixed arguments
	partialFunc := &partialFunction{
		function:    function,
		fixedArgs:   fixedArgs,
		fixedKwargs: fixedKwargs,
	}

	return partialFunc, nil
}

// partialFunction is the result of calling partial()
type partialFunction struct {
	core.BaseObject
	function    core.Callable
	fixedArgs   []core.Value
	fixedKwargs map[string]core.Value
}

func (pf *partialFunction) Type() core.Type {
	return core.FunctionType
}

func (pf *partialFunction) String() string {
	return fmt.Sprintf("functools.partial(%s, ...)", pf.function)
}

func (pf *partialFunction) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	return pf.CallWithKeywords(args, nil, ctx)
}

func (pf *partialFunction) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Combine fixed args with new args
	allArgs := make([]core.Value, 0, len(pf.fixedArgs)+len(args))
	allArgs = append(allArgs, pf.fixedArgs...)
	allArgs = append(allArgs, args...)

	// Combine fixed kwargs with new kwargs (new kwargs override fixed ones)
	allKwargs := make(map[string]core.Value)
	for k, v := range pf.fixedKwargs {
		allKwargs[k] = v
	}
	if kwargs != nil {
		for k, v := range kwargs {
			allKwargs[k] = v
		}
	}

	// Call the underlying function with combined args
	// Try CallWithKwargs first (UserFunction)
	if kwCallable, ok := pf.function.(interface {
		CallWithKwargs([]core.Value, map[string]core.Value, *core.Context) (core.Value, error)
	}); ok && len(allKwargs) > 0 {
		return kwCallable.CallWithKwargs(allArgs, allKwargs, ctx)
	}

	// Try CallWithKeywords (builtins)
	if kwCallable, ok := pf.function.(interface {
		CallWithKeywords([]core.Value, map[string]core.Value, *core.Context) (core.Value, error)
	}); ok && len(allKwargs) > 0 {
		return kwCallable.CallWithKeywords(allArgs, allKwargs, ctx)
	}

	// Fall back to regular Call if no kwargs or function doesn't support them
	return pf.function.Call(allArgs, ctx)
}

// GetAttr implements attribute access for partial functions
// Provides default values for standard function attributes
func (pf *partialFunction) GetAttr(name string) (core.Value, bool) {
	// First check explicitly set attributes
	if val, ok := pf.BaseObject.GetAttr(name); ok {
		return val, true
	}

	// Provide defaults for standard function attributes
	switch name {
	case "func":
		// The wrapped function - this is what _unwrap_partial uses
		return pf.function, true
	case "args":
		// The fixed positional arguments
		return core.NewList(pf.fixedArgs...), true
	case "keywords":
		// The fixed keyword arguments
		dict := core.NewDict()
		for k, v := range pf.fixedKwargs {
			dict.Set(k, v)
		}
		return dict, true
	case "__name__":
		return core.StringValue("partial"), true
	case "__qualname__":
		return core.StringValue("functools.partial"), true
	case "__module__":
		return core.StringValue("functools"), true
	case "__doc__":
		return core.StringValue("partial(func, *args, **keywords) - new function with partial application"), true
	case "__annotations__":
		return core.NewDict(), true
	case "__type_params__":
		return core.TupleValue{}, true
	case "__dict__":
		return core.NewDict(), true
	case "__defaults__":
		// Partial functions have no defaults (defaults are already baked in as fixed args)
		return core.None, true
	case "__kwdefaults__":
		// Partial functions have no keyword-only defaults
		return core.None, true
	case "__code__":
		// Return a code object for partial functions
		// Try to get __code__ from the wrapped function if possible
		if callable, ok := pf.function.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if code, found := callable.GetAttr("__code__"); found {
				return code, true
			}
		}
		// Fall back to creating a minimal code object
		codeObj := core.NewCodeObject(pf)
		codeObj.SetAttr("co_argcount", core.NumberValue(0))
		codeObj.SetAttr("co_posonlyargcount", core.NumberValue(0))
		codeObj.SetAttr("co_kwonlyargcount", core.NumberValue(0))
		codeObj.SetAttr("co_flags", core.NumberValue(3)) // OPTIMIZED | NEWLOCALS
		codeObj.SetAttr("co_varnames", core.TupleValue{})
		codeObj.SetAttr("co_name", core.StringValue("partial"))
		return codeObj, true
	}

	return nil, false
}

// lruCacheBuiltin implements lru_cache with keyword argument support
type lruCacheBuiltin struct {
	core.BaseObject
}

func (l *lruCacheBuiltin) Type() core.Type {
	return core.FunctionType
}

func (l *lruCacheBuiltin) String() string {
	return "<builtin function lru_cache>"
}

func (l *lruCacheBuiltin) Call(args []core.Value, ctx *core.Context) (core.Value, error) {
	return l.CallWithKeywords(args, nil, ctx)
}

func (l *lruCacheBuiltin) CallWithKeywords(args []core.Value, kwargs map[string]core.Value, ctx *core.Context) (core.Value, error) {
	// Extract maxsize from kwargs or use default
	maxsizeInt := 128 // default
	if kwargs != nil {
		if maxsizeVal, ok := kwargs["maxsize"]; ok {
			if maxsize, ok := maxsizeVal.(core.NumberValue); ok {
				maxsizeInt = int(maxsize)
			}
		}
		// Ignore 'typed' parameter for now
	}

	// If keyword arguments were provided, always return a decorator
	// even if we also have positional args
	if kwargs != nil && len(kwargs) > 0 {
		return createLRUDecorator(maxsizeInt), nil
	}

	// If we have positional args and first arg is callable, decorate it directly
	if len(args) > 0 {
		if callable, ok := args[0].(core.Callable); ok {
			// Direct decoration: @lru_cache (no parens)
			// Apply the cache to the function
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
				result, err := callable.Call(callArgs, callCtx)
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

			// Add cache_clear method as an attribute
			// Python's lru_cache adds this to allow clearing the cache
			cache_clear := core.NewBuiltinFunction(func(clearArgs []core.Value, clearCtx *core.Context) (core.Value, error) {
				// Clear the cache
				for k := range cache {
					delete(cache, k)
				}
				order = order[:0]
				return core.None, nil
			})
			cachedFunc.SetAttr("cache_clear", cache_clear)

			return cachedFunc, nil
		}
	}

	// Called with no arguments or non-callable positional arg
	// Return a decorator function
	return createLRUDecorator(maxsizeInt), nil
}

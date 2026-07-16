package modules

import (
	"fmt"

	"github.com/mmichie/m28/common/errors"
	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// Init_FunctoolsModule creates and returns the _functools C extension stub
// This provides the C functions that Python's functools.py imports
func Init_FunctoolsModule() *core.DictValue {
	functoolsModule := core.NewDict()

	// reduce - Apply function cumulatively to items
	functoolsModule.SetStr("reduce", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
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
	functoolsModule.SetStr("partial", &partialBuiltin{})

	// cache - Simple memoization decorator (limited version)

	// cmp_to_key - NOT exported from _functools so that functools.py uses its
	// pure Python implementation (which creates a proper class with __lt__ etc.)
	// The Python fallback in functools.py handles this correctly.
	// (The Go dict-wrapper approach doesn't support rich comparison.)

	// wraps - Decorator that updates wrapper function to look like wrapped
	// Copies metadata from wrapped to wrapper function
	functoolsModule.SetStr("wraps", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("wraps", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		wrapped := v.Get(0)

		// Return a decorator function
		decorator := core.NewBuiltinFunction(func(decoratorArgs []core.Value, decoratorCtx *core.Context) (core.Value, error) {
			if len(decoratorArgs) != 1 {
				return nil, errors.NewRuntimeError("wraps", "decorator takes exactly one argument")
			}

			wrapper := decoratorArgs[0]

			// Copy metadata attributes from wrapped to wrapper
			// WRAPPER_ASSIGNMENTS = ('__module__', '__name__', '__qualname__', '__annotations__', '__doc__')
			assignments := []string{"__module__", "__name__", "__qualname__", "__annotations__", "__doc__"}

			// Get wrapped's attributes
			if wrappedObj, ok := wrapped.(core.Object); ok {
				if wrapperObj, ok := wrapper.(core.Object); ok {
					for _, attr := range assignments {
						if val, exists := wrappedObj.GetAttr(attr); exists {
							if err := wrapperObj.SetAttr(attr, val); err != nil {
								// Ignore errors setting attributes (some may be read-only)
							}
						}
					}
				}
			}

			// Also copy __wrapped__ to point to the original function
			if wrapperObj, ok := wrapper.(core.Object); ok {
				wrapperObj.SetAttr("__wrapped__", wrapped)
			}

			return wrapper, nil
		})

		return decorator, nil
	}))

	// NOTE: deliberately NOT exporting _lru_cache_wrapper. CPython's
	// functools.py treats it as an optional C accelerator; when absent it
	// uses its own pure-Python wrapper, which runs correctly on M28 (the
	// previous export had the decorator-factory signature, so maxsize was
	// silently ignored and keys were %v strings that collided across types).

	// total_ordering - Class decorator that fills in missing ordering methods
	// Takes a class with __eq__ and one ordering method and generates the rest
	functoolsModule.SetStr("total_ordering", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
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

func (p *partialBuiltin) CallWithKeywords(args []core.Value, kwargs *core.Kwargs, ctx *core.Context) (core.Value, error) {
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

	// Capture the fixed keyword arguments (insertion order preserved)
	fixedKwargs := kwargs.Clone()

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
	fixedKwargs *core.Kwargs
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

func (pf *partialFunction) CallWithKeywords(args []core.Value, kwargs *core.Kwargs, ctx *core.Context) (core.Value, error) {
	// Combine fixed args with new args
	allArgs := make([]core.Value, 0, len(pf.fixedArgs)+len(args))
	allArgs = append(allArgs, pf.fixedArgs...)
	allArgs = append(allArgs, args...)

	// Combine fixed kwargs with new kwargs (new kwargs override fixed ones,
	// keeping the fixed keyword's position, like CPython's {**fixed, **new})
	allKwargs := pf.fixedKwargs.Clone()
	for _, e := range kwargs.Entries() {
		allKwargs.Set(e.Name, e.Value)
	}

	// Call the underlying function with combined args
	// Try CallWithKwargs first (UserFunction)
	if kwCallable, ok := pf.function.(interface {
		CallWithKwargs([]core.Value, *core.Kwargs, *core.Context) (core.Value, error)
	}); ok && allKwargs.Len() > 0 {
		return kwCallable.CallWithKwargs(allArgs, allKwargs, ctx)
	}

	// Try CallWithKeywords (builtins)
	if kwCallable, ok := pf.function.(interface {
		CallWithKeywords([]core.Value, *core.Kwargs, *core.Context) (core.Value, error)
	}); ok && allKwargs.Len() > 0 {
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
		return pf.fixedKwargs.ToDict(), true
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

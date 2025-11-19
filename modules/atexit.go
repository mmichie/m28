package modules

import (
	"fmt"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// InitAtexitModule registers the atexit module
// atexit - register cleanup functions to be called at program exit
func InitAtexitModule() *core.DictValue {
	// Create module as a dict
	atexitModule := core.NewDict()

	// Store registered functions (stored as core.Value for generality)
	registeredFuncs := []core.Value{}

	// register(func, *args, **kwargs) - register a function to be called at exit
	atexitModule.SetWithKey("register", core.StringValue("register"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("register", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// First argument should be callable
		fn := args[0]
		// Check if it's callable (simplified - just check it's not a basic value)
		if _, ok := fn.(core.NumberValue); ok {
			return nil, fmt.Errorf("register() argument 1 must be callable, not 'int'")
		}
		if _, ok := fn.(core.StringValue); ok {
			return nil, fmt.Errorf("register() argument 1 must be callable, not 'str'")
		}

		// Store the function (in a real implementation, this would be called at exit)
		registeredFuncs = append(registeredFuncs, fn)

		// Return the function (for decorator use)
		return fn, nil
	}))

	// unregister(func) - unregister a function
	atexitModule.SetWithKey("unregister", core.StringValue("unregister"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("unregister", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// First argument should be callable
		fn := args[0]

		// Remove the function from registered functions
		newFuncs := []core.Value{}
		for _, f := range registeredFuncs {
			// Simple equality check - in reality would need better comparison
			if f != fn {
				newFuncs = append(newFuncs, f)
			}
		}
		registeredFuncs = newFuncs

		return core.Nil, nil
	}))

	// _ncallbacks() - return number of registered callbacks
	atexitModule.SetWithKey("_ncallbacks", core.StringValue("_ncallbacks"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("_ncallbacks", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return core.NumberValue(len(registeredFuncs)), nil
	}))

	// _run_exitfuncs() - run all registered exit functions
	atexitModule.SetWithKey("_run_exitfuncs", core.StringValue("_run_exitfuncs"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("_run_exitfuncs", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Run all registered functions in reverse order
		for i := len(registeredFuncs) - 1; i >= 0; i-- {
			fn := registeredFuncs[i]
			// Try to call the function if it's callable
			if callable, ok := fn.(core.Callable); ok {
				// Call the function (ignore errors for now)
				_, _ = callable.Call([]core.Value{}, ctx)
			}
		}

		// Clear the list
		registeredFuncs = []core.Value{}

		return core.Nil, nil
	}))

	// _clear() - clear all registered callbacks
	atexitModule.SetWithKey("_clear", core.StringValue("_clear"), core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("_clear", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		registeredFuncs = []core.Value{}

		return core.Nil, nil
	}))

	return atexitModule
}

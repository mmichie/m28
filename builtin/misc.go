package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterMisc registers miscellaneous functions
func RegisterMisc(ctx *core.Context) {
	// repr - return string representation
	ctx.Define("repr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("repr() takes exactly one argument (%d given)", len(args))
		}

		// For now, just use String() method
		// In the future, should distinguish between str() and repr()
		return core.StringValue(args[0].String()), nil
	}))

	// hash - return hash of object
	ctx.Define("hash", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("hash() takes exactly one argument (%d given)", len(args))
		}

		if !core.IsHashable(args[0]) {
			return nil, fmt.Errorf("unhashable type: '%s'", args[0].Type())
		}

		// For now, return a simple hash based on string representation
		// In the future, should implement proper hashing
		key := core.ValueToKey(args[0])
		hash := 0
		for _, ch := range key {
			hash = hash*31 + int(ch)
		}

		return core.NumberValue(hash), nil
	}))

	// id - return identity of object
	ctx.Define("id", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("id() takes exactly one argument (%d given)", len(args))
		}

		// For now, return a hash-like value
		// In the future, should return actual memory address or unique ID
		// Convert pointer address to a number
		addr := fmt.Sprintf("%p", args[0])
		// Simple hash of the address string
		hash := 0
		for _, ch := range addr {
			hash = hash*31 + int(ch)
		}
		return core.NumberValue(hash), nil
	}))

	// help - print help for object
	ctx.Define("help", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 1 {
			return nil, fmt.Errorf("help() takes at most 1 argument (%d given)", len(args))
		}

		if len(args) == 0 {
			fmt.Println("Type help(object) for help about object.")
			return core.Nil, nil
		}

		// Get help for object
		obj := args[0]

		// Check for __doc__ attribute
		if o, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if doc, found := o.GetAttr("__doc__"); found {
				fmt.Println(doc.String())
				return core.Nil, nil
			}
		}

		// Default help
		fmt.Printf("Help on %s object:\n\n", obj.Type())
		fmt.Println("(no documentation available)")

		return core.Nil, nil
	}))

	// vars - return __dict__ of object
	ctx.Define("vars", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) > 1 {
			return nil, fmt.Errorf("vars() takes at most 1 argument (%d given)", len(args))
		}

		if len(args) == 0 {
			// Return current local variables
			// For now, return empty dict
			return core.NewDict(), nil
		}

		// Get __dict__ of object
		obj := args[0]
		if o, ok := obj.(interface {
			GetAttr(string) (core.Value, bool)
		}); ok {
			if dict, found := o.GetAttr("__dict__"); found {
				return dict, nil
			}
		}

		return nil, fmt.Errorf("vars() argument must have __dict__ attribute")
	}))

	// locals - return local variables
	ctx.Define("locals", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("locals() takes no arguments (%d given)", len(args))
		}

		// Convert context variables to dict
		dict := core.NewDict()
		// TODO: Implement proper locals() that returns current scope variables

		return dict, nil
	}))

	// globals - return global variables
	ctx.Define("globals", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("globals() takes no arguments (%d given)", len(args))
		}

		// Convert global context variables to dict
		dict := core.NewDict()
		// TODO: Implement proper globals() that returns global scope variables

		return dict, nil
	}))
}

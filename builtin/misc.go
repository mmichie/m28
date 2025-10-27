package builtin

import (
	"fmt"

	"github.com/mmichie/m28/common/types"
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
)

// RegisterMisc registers miscellaneous functions
func RegisterMisc(ctx *core.Context) {
	// repr - return string representation
	ctx.Define("repr", core.NewNamedBuiltinFunction("repr", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("repr", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		val := v.Get(0)

		// Try __repr__ dunder method first
		if str, found, err := types.CallRepr(val, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.StringValue(str), nil
		}

		// Use core.Repr which handles special representations properly
		return core.StringValue(core.Repr(val)), nil
	}))

	// hash - return hash of object
	ctx.Define("hash", core.NewNamedBuiltinFunction("hash", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("hash", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		obj := v.Get(0)

		// Try __hash__ dunder method first
		if hashVal, found, err := types.CallHash(obj, ctx); found {
			if err != nil {
				return nil, err
			}
			return core.NumberValue(hashVal), nil
		}

		if !core.IsHashable(obj) {
			return nil, fmt.Errorf("unhashable type: '%s'", obj.Type())
		}

		// For now, return a simple hash based on string representation
		// In the future, should implement proper hashing
		key := core.ValueToKey(obj)
		hash := 0
		for _, ch := range key {
			hash = hash*31 + int(ch)
		}

		return core.NumberValue(hash), nil
	}))

	// id - return identity of object
	ctx.Define("id", core.NewNamedBuiltinFunction("id", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("id", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		// For now, return a hash-like value
		// In the future, should return actual memory address or unique ID
		// Convert pointer address to a number
		addr := fmt.Sprintf("%p", v.Get(0))
		// Simple hash of the address string
		hash := 0
		for _, ch := range addr {
			hash = hash*31 + int(ch)
		}
		return core.NumberValue(hash), nil
	}))

	// help - print help for object
	ctx.Define("help", core.NewNamedBuiltinFunction("help", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("help", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			fmt.Println("Type help(object) for help about object.")
			return core.Nil, nil
		}

		// Get help for object
		obj := v.Get(0)

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
		v := validation.NewArgs("vars", args)
		if err := v.Max(1); err != nil {
			return nil, err
		}

		if v.Count() == 0 {
			// Return current local variables
			// For now, return empty dict
			return core.NewDict(), nil
		}

		// Get __dict__ of object
		obj := v.Get(0)
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
		v := validation.NewArgs("locals", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// Convert context variables to dict
		dict := core.NewDict()
		// TODO: Implement proper locals() that returns current scope variables

		return dict, nil
	}))

	// globals - return global variables
	ctx.Define("globals", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("globals", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// In Python, globals() returns the current module's top-level namespace
		// ctx.Global points to the global scope for the current execution context
		// For modules, ctx.Global == the module context itself
		// For functions, ctx.Global == the module context
		targetCtx := ctx.Global
		if targetCtx == nil {
			targetCtx = ctx
		}

		// If the target context has a ModuleDict, use it as the base
		// This allows globals().update() to actually modify the module namespace
		// which is critical for patterns like: globals().update({...})
		var dict *core.DictValue
		if targetCtx.ModuleDict != nil {
			dict = targetCtx.ModuleDict
		} else {
			// Create a new dict and save it to the context
			// so subsequent calls to globals() return the same dict
			dict = core.NewDict()
			targetCtx.ModuleDict = dict
		}

		// Also add ALL variables from ctx.Vars to ensure globals() is complete
		// This includes private and dunder names that aren't in ModuleDict
		for name, value := range targetCtx.Vars {
			// Use SetWithKey to properly track original keys for iteration
			keyVal := core.StringValue(name)
			dict.SetWithKey(core.ValueToKey(keyVal), keyVal, value)
		}

		return dict, nil
	}))

	// chr - convert integer to character
	ctx.Define("chr", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("chr", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 || i > 0x10FFFF {
			return nil, fmt.Errorf("chr() arg not in range(0x110000)")
		}

		return core.StringValue(string(rune(i))), nil
	}))

	// ord - convert character to integer
	ctx.Define("ord", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("ord", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		s, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		runes := []rune(s)
		if len(runes) != 1 {
			return nil, fmt.Errorf("ord() expected a character, but string of length %d found", len(runes))
		}

		return core.NumberValue(int(runes[0])), nil
	}))

	// hex - convert integer to hexadecimal string
	ctx.Define("hex", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("hex", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 {
			return core.StringValue(fmt.Sprintf("-0x%x", -i)), nil
		}
		return core.StringValue(fmt.Sprintf("0x%x", i)), nil
	}))

	// bin - convert integer to binary string
	ctx.Define("bin", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("bin", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 {
			return core.StringValue(fmt.Sprintf("-0b%b", -i)), nil
		}
		return core.StringValue(fmt.Sprintf("0b%b", i)), nil
	}))

	// oct - convert integer to octal string
	ctx.Define("oct", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("oct", args)
		if err := v.Exact(1); err != nil {
			return nil, err
		}

		i, err := v.GetInt(0)
		if err != nil {
			return nil, err
		}

		if i < 0 {
			return core.StringValue(fmt.Sprintf("-0o%o", -i)), nil
		}
		return core.StringValue(fmt.Sprintf("0o%o", i)), nil
	}))

	// del - delete variables (for now, a no-op to satisfy Python code)
	// Python's del statement is used to unbind names, but in types.py it's just cleanup
	ctx.Define("del", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// In a full implementation, this would unbind the variables from the namespace
		// For now, it's a no-op that returns None
		return core.None, nil
	}))

	// eval - evaluate a string as code
	// eval(expression, globals=None, locals=None)
	ctx.Define("eval", core.NewNamedBuiltinFunction("eval", func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("eval", args)
		if err := v.Range(1, 3); err != nil {
			return nil, err
		}

		// Get the code string
		codeStr, err := v.GetString(0)
		if err != nil {
			return nil, err
		}

		// Get the globals dict (optional)
		var globalsDict *core.DictValue
		if v.Count() >= 2 && v.Get(1) != core.None {
			globalsDict, err = types.RequireDict(v.Get(1), "eval() globals")
			if err != nil {
				return nil, err
			}
		}

		// Get the locals dict (optional)
		var localsDict *core.DictValue
		if v.Count() >= 3 && v.Get(2) != core.None {
			localsDict, err = types.RequireDict(v.Get(2), "eval() locals")
			if err != nil {
				return nil, err
			}
		}

		// Create a new context for evaluation
		evalCtx := core.NewContext(ctx)

		// If globals dict provided, populate the context
		if globalsDict != nil {
			// Use Keys() to iterate over all keys in the dict
			for _, keyStr := range globalsDict.Keys() {
				value, exists := globalsDict.Get(keyStr)
				if exists {
					// Strip "s:" prefix if present (for string keys)
					cleanKey := keyStr
					if len(keyStr) > 2 && keyStr[0:2] == "s:" {
						cleanKey = keyStr[2:]
					}
					evalCtx.Define(cleanKey, value)
				}
			}
		}

		// If locals dict provided, add those too (locals override globals)
		if localsDict != nil {
			// Use Keys() to iterate over all keys in the dict
			for _, keyStr := range localsDict.Keys() {
				value, exists := localsDict.Get(keyStr)
				if exists {
					// Strip "s:" prefix if present (for string keys)
					cleanKey := keyStr
					if len(keyStr) > 2 && keyStr[0:2] == "s:" {
						cleanKey = keyStr[2:]
					}
					evalCtx.Define(cleanKey, value)
				}
			}
		}

		// Use EvalString which properly handles Python syntax including lambdas
		return eval.EvalString(codeStr, evalCtx)
	}))

	// Ellipsis - Python's ... literal
	ctx.Define("Ellipsis", core.Ellipsis)

	// NotImplemented - Python's NotImplemented singleton
	ctx.Define("NotImplemented", core.NotImplemented)
}

// Migration Statistics:
// Functions migrated: 7 utility functions (repr, hash, id, help, vars, locals, globals)
// Type checks eliminated: ~7 manual len(args) checks
// Code reduction: ~15% in validation code
// Benefits: Consistent error messages with validation framework
// All utility functions now use v.Exact() and v.Max() for cleaner validation

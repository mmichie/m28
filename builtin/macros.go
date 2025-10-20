package builtin

import (
	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// RegisterMacros registers built-in utility macros
func RegisterMacros(ctx *core.Context) {
	// unless - Execute body if condition is false
	// Usage: (unless (> x 5) (print "x is not greater than 5"))
	unless := unlessMacro()
	unless.SetAttr("__macro__", core.True)
	ctx.Define("unless", unless)

	// when - Execute body if condition is true
	// Usage: (when (> x 5) (print "x is greater than 5"))
	when := whenMacro()
	when.SetAttr("__macro__", core.True)
	ctx.Define("when", when)

	// -> (thread-first) - Thread value through forms, inserting as first argument
	// Usage: (-> x (f a) (g b)) expands to (g (f x a) b)
	threadFirst := threadFirstMacro()
	threadFirst.SetAttr("__macro__", core.True)
	ctx.Define("->", threadFirst)

	// ->> (thread-last) - Thread value through forms, inserting as last argument
	// Usage: (->> x (f a) (g b)) expands to (g b (f a x))
	threadLast := threadLastMacro()
	threadLast.SetAttr("__macro__", core.True)
	ctx.Define("->>", threadLast)
}

// unlessMacro creates the unless macro
// (unless condition body) => (if (not condition) body nil)
func unlessMacro() *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("unless", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		condition := v.Get(0)
		body := v.Get(1)

		// Build: (if (not condition) body nil)
		result := core.NewList(
			core.SymbolValue("if"),
			core.NewList(
				core.SymbolValue("not"),
				condition,
			),
			body,
			core.Nil,
		)

		return result, nil
	})
}

// whenMacro creates the when macro
// (when condition body) => (if condition body nil)
func whenMacro() *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("when", args)
		if err := v.Exact(2); err != nil {
			return nil, err
		}

		condition := v.Get(0)
		body := v.Get(1)

		// Build: (if condition body nil)
		result := core.NewList(
			core.SymbolValue("if"),
			condition,
			body,
			core.Nil,
		)

		return result, nil
	})
}

// threadFirstMacro creates the -> (thread-first) macro
// (-> x (f a) (g b)) => (g (f x a) b)
// Threads the value through each form, inserting it as the first argument
func threadFirstMacro() *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("->", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// Start with the initial value
		result := v.Get(0)

		// Thread through each form
		for _, form := range args[1:] {
			// If form is a list, insert result as first argument after the function
			if list, ok := form.(*core.ListValue); ok && list.Len() > 0 {
				// (f a b) with result x => (f x a b)
				threaded := make([]core.Value, 0, list.Len()+1)
				threaded = append(threaded, list.Items()[0])     // function
				threaded = append(threaded, result)              // insert value
				threaded = append(threaded, list.Items()[1:]...) // rest of args
				result = core.NewList(threaded...)
			} else {
				// If form is a symbol, create a function call: f => (f result)
				result = core.NewList(form, result)
			}
		}

		return result, nil
	})
}

// threadLastMacro creates the ->> (thread-last) macro
// (->> x (f a) (g b)) => (g b (f a x))
// Threads the value through each form, inserting it as the last argument
func threadLastMacro() *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("->>", args)
		if err := v.Min(1); err != nil {
			return nil, err
		}

		// Start with the initial value
		result := v.Get(0)

		// Thread through each form
		for _, form := range args[1:] {
			// If form is a list, append result as last argument
			if list, ok := form.(*core.ListValue); ok && list.Len() > 0 {
				// (f a b) with result x => (f a b x)
				threaded := make([]core.Value, 0, list.Len()+1)
				threaded = append(threaded, list.Items()...) // function and args
				threaded = append(threaded, result)          // append value at end
				result = core.NewList(threaded...)
			} else {
				// If form is a symbol, create a function call: f => (f result)
				result = core.NewList(form, result)
			}
		}

		return result, nil
	})
}

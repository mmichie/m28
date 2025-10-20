package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// yieldForm implements the yield expression
func yieldForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	var value core.Value = core.Nil

	if args.Len() > 0 {
		// Evaluate the yielded value
		val, err := Eval(args.Items()[0], ctx)
		if err != nil {
			return nil, err
		}
		value = val
	}

	// Return a yield marker
	return &core.YieldValue{Value: value}, nil
}

// yieldFromForm implements the yield from expression
// For now, this is a simplified implementation that converts:
//
//	yield from iterable
//
// into:
//
//	for item in iterable: yield item
func yieldFromForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() == 0 {
		return nil, fmt.Errorf("yield from requires an argument")
	}

	// Evaluate the iterable
	iterable, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	// For now, return a special marker that indicates yield-from
	// The generator machinery will need to handle this specially
	// As a simple implementation, we'll just iterate and yield each value
	return &core.YieldFromValue{Iterable: iterable}, nil
}

// EvalGenerator evaluates a generator body and handles yields
func EvalGenerator(gen *core.Generator, ctx *core.Context) (core.Value, error) {
	// This is a simplified implementation
	// A full implementation would need to track execution state

	// Check if generator is already completed
	if gen.GetState() == core.GeneratorCompleted {
		return nil, &core.StopIteration{}
	}

	// Evaluate the generator's code
	result, err := Eval(gen.GetCode(), ctx)
	if err != nil {
		return nil, err
	}

	// Check if we hit a yield
	if yieldVal, ok := core.IsYield(result); ok {
		gen.SetState(core.GeneratorSuspended)
		return yieldVal.Value, nil
	}

	// Generator completed
	gen.SetState(core.GeneratorCompleted)
	return nil, &core.StopIteration{Value: result}
}

// makeGeneratorFunction checks if a function contains yield and wraps it
func makeGeneratorFunction(fn *UserFunction) core.Value {
	// Check if function body contains yield
	hasYield := containsYield(fn.body)
	if hasYield {
		// Create a generator function wrapper
		genFunc := core.NewGeneratorFunction(fn, fn.name)
		return genFunc
	}
	return fn
}

// containsYield checks if an expression contains yield statements
func containsYield(expr core.Value) bool {
	switch e := expr.(type) {
	case *core.ListValue:
		if e.Len() > 0 {
			if sym, ok := e.Items()[0].(core.SymbolValue); ok {
				if string(sym) == "yield" || string(sym) == "yield-from" {
					return true
				}
			}
			// Recursively check all elements
			for _, elem := range e.Items() {
				if containsYield(elem) {
					return true
				}
			}
		}
	case core.SymbolValue:
		// Check if it's a yield symbol (shouldn't happen but just in case)
		if string(e) == "yield" || string(e) == "yield-from" {
			return true
		}
	}
	return false
}

// RegisterGeneratorForms registers generator-related forms
func RegisterGeneratorForms() {
	RegisterSpecialForm("yield", yieldForm)
	RegisterSpecialForm("yield-from", yieldFromForm)
}

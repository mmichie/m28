package eval

import (
	"github.com/mmichie/m28/core"
)

// yieldForm implements the yield expression
func yieldForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	var value core.Value = core.Nil

	if len(args) > 0 {
		// Evaluate the yielded value
		val, err := Eval(args[0], ctx)
		if err != nil {
			return nil, err
		}
		value = val
	}

	// Return a yield marker
	return &core.YieldValue{Value: value}, nil
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
	if containsYield(fn.body) {
		return core.NewGeneratorFunction(fn, fn.name)
	}
	return fn
}

// containsYield checks if an expression contains yield statements
func containsYield(expr core.Value) bool {
	switch e := expr.(type) {
	case core.ListValue:
		if len(e) > 0 {
			if sym, ok := e[0].(core.SymbolValue); ok && string(sym) == "yield" {
				return true
			}
			// Recursively check all elements
			for _, elem := range e {
				if containsYield(elem) {
					return true
				}
			}
		}
	}
	return false
}

// RegisterGeneratorForms registers generator-related forms
func RegisterGeneratorForms() {
	RegisterSpecialForm("yield", yieldForm)
}

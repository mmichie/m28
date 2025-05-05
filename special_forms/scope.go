package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// EvalLet evaluates a let expression
// (let ((var1 val1) (var2 val2) ...) body)
func EvalLet(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("let requires bindings and a body")
	}

	bindings, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("let bindings must be a list")
	}

	// Create a new environment for the let bindings
	letEnv := env.NewEnvironment(env)

	// Evaluate each binding and add it to the let environment
	for _, binding := range bindings {
		bindingList, ok := binding.(core.LispList)
		if !ok || len(bindingList) != 2 {
			return nil, fmt.Errorf("let binding must be a list of form (name value)")
		}

		name, ok := bindingList[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding name must be a symbol")
		}

		value, err := e.Eval(bindingList[1], env)
		if err != nil {
			return nil, err
		}

		letEnv.Define(name, value)
	}

	// Evaluate the body expressions in the let environment
	var result core.LispValue = core.PythonicNone{}
	for _, expr := range args[1:] {
		var err error
		result, err = e.Eval(expr, letEnv)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func EvalGlobal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("global arguments must be symbols")
		}
		env.Define(symbol, nil)
	}
	return core.PythonicNone{}, nil
}

func EvalNonlocal(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Simplified implementation without true nonlocal behavior
	for _, arg := range args {
		symbol, ok := arg.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("nonlocal arguments must be symbols")
		}
		env.Define(symbol, nil)
	}
	return core.PythonicNone{}, nil
}

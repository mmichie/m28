package core

import (
	"fmt"
)

// NewGenerator creates a new generator from a function and arguments
func NewGenerator(function *Lambda, args []LispValue) *Generator {
	return &Generator{
		Function:     function,
		Args:         args,
		State:        nil,
		IsDone:       false,
		CurrentValue: PythonicNone{},
		IsStarted:    false,
	}
}

// Next advances the generator to the next yield point
func (g *Generator) Next(e Evaluator) (LispValue, error) {
	// If generator is done, return StopIteration
	if g.IsDone {
		return nil, &Exception{
			Type:    "StopIteration",
			Message: "Generator exhausted",
		}
	}

	// If this is the first call, initialize the generator
	if !g.IsStarted {
		g.State = g.Function.Env.NewEnvironment(g.Function.Closure)
		g.IsStarted = true

		// Bind arguments to parameters
		for i, param := range g.Function.Params {
			if i < len(g.Args) {
				g.State.Define(param, g.Args[i])
			} else if defaultVal, hasDefault := g.Function.DefaultValues[param]; hasDefault {
				g.State.Define(param, defaultVal)
			} else {
				return nil, fmt.Errorf("missing required argument: %s", param)
			}
		}
	}

	// Evaluate the function body or resume from last yield point
	// The body evaluation should handle the YieldSignal
	result, err := e.Eval(g.Function.Body, g.State)

	// Handle yield signal
	if yieldErr, ok := err.(YieldSignal); ok {
		g.CurrentValue = yieldErr.Value
		return g.CurrentValue, nil
	}

	// Handle normal return or completion
	if err == nil || IsReturnSignal(err) {
		// Generator is now done
		g.IsDone = true
		
		// If it's a return, get the return value
		if returnErr, ok := err.(ReturnSignal); ok {
			return returnErr.Value, nil
		}
		
		// Normal completion
		return result, nil
	}

	// If it's some other error, propagate it
	g.IsDone = true
	return nil, err
}

// Iterator returns the generator itself as an iterator
func (g *Generator) Iterator() *Generator {
	return g
}

// IsReturnSignal checks if an error is a ReturnSignal
func IsReturnSignal(err error) bool {
	_, ok := err.(ReturnSignal)
	return ok
}

// IsYieldSignal checks if an error is a YieldSignal
func IsYieldSignal(err error) bool {
	_, ok := err.(YieldSignal)
	return ok
}
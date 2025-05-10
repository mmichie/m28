package core

import (
	"fmt"
)

// BoundMethod represents a method bound to a specific instance
// It implements the Applicable interface directly to be callable
type BoundMethod struct {
	Method   *Lambda
	Instance *PythonicObject
	// We cache the evaluator for direct method calls
	EvalContext Evaluator
}

// NewBoundMethod creates a new bound method with the evaluator context
func NewBoundMethod(method *Lambda, instance *PythonicObject, evalCtx Evaluator) *BoundMethod {
	return &BoundMethod{
		Method:      method,
		Instance:    instance,
		EvalContext: evalCtx,
	}
}

// Apply implements the Applicable interface
// This is the key method that makes bound methods directly callable
func (bm *BoundMethod) Apply(e Evaluator, args []LispValue, env Environment) (LispValue, error) {
	// Prepend the instance as the first argument (self)
	newArgs := make([]LispValue, len(args)+1)
	newArgs[0] = bm.Instance
	copy(newArgs[1:], args)

	// If we don't have an evaluator from the caller, use our cached one
	evalCtx := e
	if evalCtx == nil {
		evalCtx = bm.EvalContext

		// If we still don't have an evaluator, try to get one from the environment
		if evalCtx == nil && env != nil {
			if evalVal, exists := env.Get("EVALUATOR"); exists {
				if ev, ok := evalVal.(Evaluator); ok {
					evalCtx = ev
				}
			}
		}

		// If we still don't have an evaluator, we can't proceed
		if evalCtx == nil {
			return nil, fmt.Errorf("cannot call method: no evaluator available")
		}
	}

	// Apply the method with the instance prepended to args
	return evalCtx.Apply(bm.Method, newArgs, env)
}

// String returns a string representation of the bound method
func (bm *BoundMethod) String() string {
	return fmt.Sprintf("<bound method %v of %v>", bm.Method, bm.Instance)
}

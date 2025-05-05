package special_forms

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// EvalDot implements the dot notation for method access
// Syntax: (object.method arg1 arg2 ...)
// Translates to: ((get object "method") arg1 arg2 ...)
func EvalDot(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("dot notation requires at least an expression")
	}

	// First argument should be a symbol with '.' in it
	dotExpr, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("dot notation requires a symbol as first argument")
	}

	// Split the symbol at the dot
	parts := strings.SplitN(string(dotExpr), ".", 2)
	if len(parts) != 2 {
		return nil, fmt.Errorf("invalid dot notation: %s", dotExpr)
	}

	// Extract object and method names
	objName := core.LispSymbol(parts[0])
	methodName := parts[1]

	// Evaluate the object
	obj, err := e.Eval(objName, env)
	if err != nil {
		return nil, err
	}

	// Get the method from the object (assume it's a dictionary)
	var method core.LispValue
	
	// Handle different object representations
	switch objVal := obj.(type) {
	case *core.PythonicDict:
		// Dictionary method access
		methodVal, ok := objVal.Get(core.LispSymbol(methodName))
		if !ok {
			return nil, fmt.Errorf("object has no method '%s'", methodName)
		}
		method = methodVal
	case *core.Lambda:
		// Dispatcher function approach
		// We'll return a partial application of the lambda with the method name
		return &DotNotationPartial{
			Object:     obj,
			MethodName: methodName,
		}, nil
	default:
		return nil, fmt.Errorf("object does not support dot notation")
	}

	// If there are additional arguments, apply the method
	if len(args) > 1 {
		return e.Apply(method, args[1:], env)
	}

	// Otherwise just return the method
	return method, nil
}

// DotNotationPartial is a special value that represents a partial application
// of a method call using dot notation. When applied to arguments, it will
// invoke the method on the object.
type DotNotationPartial struct {
	Object     core.LispValue
	MethodName string
}

// Apply applies the DotNotationPartial to arguments
func (dnp *DotNotationPartial) Apply(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	switch obj := dnp.Object.(type) {
	case *core.Lambda:
		// Call as dispatch function: (obj "method" args...)
		dispatchArgs := append([]core.LispValue{dnp.MethodName}, args...)
		return e.Apply(obj, dispatchArgs, env)
	default:
		return nil, fmt.Errorf("cannot apply dot notation to this object type")
	}
}

// String returns a string representation of the partial application
func (dnp *DotNotationPartial) String() string {
	return fmt.Sprintf("#<method:%s>", dnp.MethodName)
}
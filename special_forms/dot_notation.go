package special_forms

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// Register is called from init function in special_forms.go
func Register(forms map[core.LispSymbol]SpecialFormFunc) {
	forms["dot"] = EvalDot  // Register alternative dot form
}

// EvalDot implements the dot notation for method access and property access
// Syntax: (. object property-or-method [args...])
// Alternative Syntax: (dot object property-or-method [args...])
func EvalDot(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("dot notation requires at least an object and property/method")
	}

	// Debug info
	fmt.Println("Debug: EvalDot called with args:", args)

	// Get the object first
	object, err := e.Eval(args[0], env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object: %v", err)
	}
	fmt.Println("Debug: Object type:", fmt.Sprintf("%T", object))

	// Get the property/method name (second argument)
	var propertyName string
	
	// Use the property directly as a string if possible
	switch prop := args[1].(type) {
	case string:
		propertyName = prop
	case core.LispSymbol:
		propertyName = string(prop)
	default:
		// Evaluate it
		eval_prop, err := e.Eval(args[1], env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating property name: %v", err)
		}
		
		// Try to convert to string
		switch ep := eval_prop.(type) {
		case string:
			propertyName = ep
		case core.LispSymbol:
			propertyName = string(ep)
		default:
			propertyName = fmt.Sprintf("%v", eval_prop)
		}
	}

	fmt.Println("Debug: Property name:", propertyName)

	// Dictionary property access is the main use case
	switch obj := object.(type) {
	case *core.PythonicDict:
		fmt.Println("Debug: Accessing dictionary property")
		// Dictionary property/method access
		prop, ok := obj.Get(propertyName)
		if !ok {
			return nil, fmt.Errorf("object has no property/method '%s'", propertyName)
		}
		
		fmt.Println("Debug: Found property:", prop)
		
		// If there are additional arguments, this is a method call
		if len(args) > 2 {
			fmt.Println("Debug: Method call with args:", args[2:])
			// Check if the property is callable
			switch method := prop.(type) {
			case *core.Lambda:
				// For method calls, the first argument should be the object itself (self/this)
				methodArgs := []core.LispValue{object}
				
				// Evaluate the remaining arguments
				for _, arg := range args[2:] {
					evalArg, err := e.Eval(arg, env)
					if err != nil {
						return nil, err
					}
					methodArgs = append(methodArgs, evalArg)
				}
				
				// Apply the method with arguments
				return e.Apply(method, methodArgs, env)
			default:
				return nil, fmt.Errorf("property '%s' is not callable (type: %T)", propertyName, prop)
			}
		}
		
		// For property access, just return the property
		return prop, nil
	case *core.Generator:
		// Handle generator methods
		switch propertyName {
		case "next":
			// Just call the Next method and return the result
			return obj.Next()
		default:
			return nil, fmt.Errorf("generator has no method '%s'", propertyName)
		}
	default:
		return nil, fmt.Errorf("object type %T does not support dot notation", object)
	}
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
	case *core.Generator:
		// Handle Generator methods
		if dnp.MethodName == "next" {
			return obj.Next()
		}
		return nil, fmt.Errorf("unknown generator method: %s", dnp.MethodName)
	default:
		return nil, fmt.Errorf("cannot apply dot notation to this object type: %T", dnp.Object)
	}
}

// String returns a string representation of the partial application
func (dnp *DotNotationPartial) String() string {
	return fmt.Sprintf("#<method:%s>", dnp.MethodName)
}
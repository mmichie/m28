package special_forms

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// EvalDot implements the dot notation for method access and property access
// Syntax: (. object property-or-method [args...])
func EvalDot(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("dot notation requires at least an object and property/method")
	}

	// Get the object first
	object, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Get the property/method name (could be a quoted symbol or string)
	var propertyName string
	
	switch prop := args[1].(type) {
	case string:
		propertyName = prop
	case core.LispSymbol:
		propertyName = string(prop)
	case core.LispList:
		// This should be a quoted symbol: (quote name)
		if len(prop) == 2 && prop[0] == core.LispSymbol("quote") {
			if sym, ok := prop[1].(core.LispSymbol); ok {
				propertyName = string(sym)
			} else {
				return nil, fmt.Errorf("quoted property name must be a symbol")
			}
		} else {
			return nil, fmt.Errorf("invalid property format in dot notation")
		}
	default:
		return nil, fmt.Errorf("property name must be a string or symbol")
	}

	// Get the property/method from the object
	var property core.LispValue

	// Handle different object types
	switch obj := object.(type) {
	case *core.PythonicDict:
		// Dictionary property/method access
		prop, ok := obj.Get(propertyName)
		if !ok {
			return nil, fmt.Errorf("object has no property/method '%s'", propertyName)
		}
		property = prop
	case core.LispList:
		// Check if it's a list with methods (future feature)
		return nil, fmt.Errorf("list objects don't support dot notation yet")
	case core.LispTuple:
		// Check if it's a tuple with methods (future feature)
		return nil, fmt.Errorf("tuple objects don't support dot notation yet")
	case *core.Lambda:
		// Dispatcher function approach
		// We'll return a partial application with the method name
		return &DotNotationPartial{
			Object:     object,
			MethodName: propertyName,
		}, nil
	case *core.Generator:
		// Handle generator methods
		switch propertyName {
		case "next":
			// Return a function that calls Next
			return &DotNotationPartial{
				Object:     object,
				MethodName: "next",
			}, nil
		default:
			return nil, fmt.Errorf("generator has no method '%s'", propertyName)
		}
	default:
		return nil, fmt.Errorf("object type %T does not support dot notation", object)
	}

	// If there are additional arguments, this is a method call
	if len(args) > 2 {
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
		return e.Apply(property, methodArgs, env)
	}

	// For property access, just return the property
	return property, nil
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
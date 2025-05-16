package special_forms

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// EnhancedEvalDot implements an improved dot notation that works with the ObjProtocol
// It can be registered to replace the standard EvalDot when ready
func EnhancedEvalDot(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Validate arguments
	if len(args) < 2 {
		return nil, fmt.Errorf(core.ErrDotMissingArgs)
	}

	// Get the object
	object, err := e.Eval(args[0], env)
	if err != nil {
		return nil, fmt.Errorf(core.ErrDotObjectEval, err)
	}

	// If we have a nil object, return nil
	if object == nil {
		return nil, fmt.Errorf("cannot access properties of nil value")
	}

	// Get the property/method name
	var propertyName string
	switch prop := args[1].(type) {
	case string:
		propertyName = prop
	case core.LispSymbol:
		propertyName = string(prop)
	default:
		// Evaluate it if needed
		evalProp, err := e.Eval(args[1], env)
		if err != nil {
			return nil, fmt.Errorf(core.ErrDotPropertyEval, err)
		}

		// Convert to string
		switch ep := evalProp.(type) {
		case string:
			propertyName = ep
		case core.LispSymbol:
			propertyName = string(ep)
		default:
			propertyName = fmt.Sprintf("%v", evalProp)
		}
	}

	// If there are more arguments, it's a method call or nested property access
	if len(args) > 2 {
		// Check if this is a method call with arguments
		if isMethodCall(args, 2) {
			// Evaluate all method arguments
			methodArgs := make([]core.LispValue, 0, len(args)-2)
			for _, arg := range args[2:] {
				evalArg, err := e.Eval(arg, env)
				if err != nil {
					return nil, fmt.Errorf("error evaluating method argument: %v", err)
				}
				methodArgs = append(methodArgs, evalArg)
			}

			// Call the method using the enhanced helper
			return core.EnhancedCallObjectMethod(object, propertyName, methodArgs, e, env)
		} else {
			// This is nested property access (obj.a.b.c)
			// Convert remaining args to a string path
			path := make([]string, 0, len(args)-1)
			path = append(path, propertyName)

			// Process each subsequent property name
			for i := 2; i < len(args); i++ {
				var nextProp string
				switch prop := args[i].(type) {
				case string:
					nextProp = prop
				case core.LispSymbol:
					nextProp = string(prop)
				default:
					// Evaluate it if needed
					evalProp, err := e.Eval(args[i], env)
					if err != nil {
						return nil, fmt.Errorf(core.ErrDotPropertyEval, err)
					}

					// Convert to string
					switch ep := evalProp.(type) {
					case string:
						nextProp = ep
					case core.LispSymbol:
						nextProp = string(ep)
					default:
						nextProp = fmt.Sprintf("%v", evalProp)
					}
				}
				path = append(path, nextProp)
			}

			// Get the nested member using the enhanced helper
			return core.EnhancedGetNestedMember(object, path, e, env)
		}
	} else {
		// Simple property access using the enhanced helper
		return core.EnhancedObjectMember(object, propertyName, e, env)
	}
}

// EnhancedEvalDotAssign handles attribute assignment with dot notation
// with ObjProtocol support
func EnhancedEvalDotAssign(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("dot assign requires exactly 2 arguments: object.attr expression")
	}

	// The first argument should be a dot expression (obj.attr)
	dotExpr, ok := args[0].(core.LispList)
	if !ok || len(dotExpr) < 2 {
		return nil, fmt.Errorf("invalid dot assignment: first argument must be a dot expression")
	}

	// Check if it's a dot expression
	dotSymbol, ok := dotExpr[0].(core.LispSymbol)
	if !ok || (dotSymbol != "dot" && dotSymbol != ".") {
		return nil, fmt.Errorf("invalid dot assignment: expected dot expression")
	}

	// Extract object and attribute path
	objExpr := dotExpr[1]
	attrPath := dotExpr[2:]

	// Evaluate the object
	object, err := e.Eval(objExpr, env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating object in dot assignment: %v", err)
	}

	// Evaluate the value to assign
	value, err := e.Eval(args[1], env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating value in dot assignment: %v", err)
	}

	// Process the attribute path to get to the final object and attribute
	// If we have a chain like obj.a.b.c, we need to get obj.a.b first, then set c
	var targetObj core.LispValue = object
	var targetAttr string

	if len(attrPath) == 1 {
		// Direct attribute access like obj.attr
		switch attr := attrPath[0].(type) {
		case core.LispSymbol:
			targetAttr = string(attr)
		case string:
			targetAttr = attr
		default:
			// Try to evaluate it
			attrEval, err := e.Eval(attrPath[0], env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating attribute name: %v", err)
			}

			switch ae := attrEval.(type) {
			case string:
				targetAttr = ae
			case core.LispSymbol:
				targetAttr = string(ae)
			default:
				targetAttr = fmt.Sprintf("%v", attrEval)
			}
		}
	} else {
		// Chain access like obj.a.b.c
		// Get all but the last part of the path
		for i := 0; i < len(attrPath)-1; i++ {
			var currentAttr string

			switch attr := attrPath[i].(type) {
			case core.LispSymbol:
				currentAttr = string(attr)
			case string:
				currentAttr = attr
			default:
				// Try to evaluate it
				attrEval, err := e.Eval(attrPath[i], env)
				if err != nil {
					return nil, fmt.Errorf("error evaluating attribute name: %v", err)
				}

				switch ae := attrEval.(type) {
				case string:
					currentAttr = ae
				case core.LispSymbol:
					currentAttr = string(ae)
				default:
					currentAttr = fmt.Sprintf("%v", attrEval)
				}
			}

			// Get the next object in the chain using enhanced accessor
			nextObj, err := core.EnhancedObjectMember(targetObj, currentAttr, e, env)
			if err != nil {
				return nil, fmt.Errorf("attribute '%s' not found in path: %v", currentAttr, err)
			}
			targetObj = nextObj
		}

		// Get the final attribute name
		switch attr := attrPath[len(attrPath)-1].(type) {
		case core.LispSymbol:
			targetAttr = string(attr)
		case string:
			targetAttr = attr
		default:
			// Try to evaluate it
			attrEval, err := e.Eval(attrPath[len(attrPath)-1], env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating attribute name: %v", err)
			}

			switch ae := attrEval.(type) {
			case string:
				targetAttr = ae
			case core.LispSymbol:
				targetAttr = string(ae)
			default:
				targetAttr = fmt.Sprintf("%v", attrEval)
			}
		}
	}

	// Set the attribute on the target object using the enhanced accessor
	err = core.EnhancedSetObjectMember(targetObj, targetAttr, value, e, env)
	if err != nil {
		return nil, fmt.Errorf("error setting property '%s': %v", targetAttr, err)
	}

	return value, nil
}

// EnableEnhancedDotForms replaces the standard dot forms with enhanced versions
// Call this function to enable the enhanced object protocol system
func EnableEnhancedDotForms(forms map[core.LispSymbol]SpecialFormFunc) {
	// Replace the existing dot notation handlers with enhanced versions
	forms["dot"] = EnhancedEvalDot
	forms["."] = EnhancedEvalDot
	forms["dot-assign"] = EnhancedEvalDotAssign
}

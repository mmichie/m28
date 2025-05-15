package special_forms

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

// EvalClassNew implements the 'class' special form for creating new classes
func EvalClassNew(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// We need at least a class name and body
	if len(args) < 2 {
		return nil, fmt.Errorf("class definition requires a name and body")
	}

	// Get class name
	var className string

	// Handle potential LocatedValue wrapping
	arg0 := args[0]
	if located, ok := arg0.(core.LocatedValue); ok {
		arg0 = located.Value
	}

	switch name := arg0.(type) {
	case core.LispSymbol:
		className = string(name)
	case string:
		className = name
	default:
		return nil, fmt.Errorf("class name must be a symbol, got %T", arg0)
	}

	// Parse parent classes
	var parents []*core.PythonicClass
	if len(args) > 1 {
		// Extract parent argument and unwrap LocatedValue if needed
		parentArg := args[1]
		if located, ok := parentArg.(core.LocatedValue); ok {
			parentArg = located.Value
			fmt.Printf("DEBUG: Unwrapped LocatedValue, new parentArg: %v (type: %T)\n", parentArg, parentArg)
		}

		// Check if we have parent classes (second arg is a list)
		if parentsList, ok := parentArg.(core.LispList); ok {
			fmt.Printf("DEBUG: Found parent list with %d elements for class %s\n", len(parentsList), className)
			for i, parentItem := range parentsList {
				// Unwrap LocatedValue if needed
				if located, ok := parentItem.(core.LocatedValue); ok {
					parentItem = located.Value
					fmt.Printf("DEBUG: Unwrapped LocatedValue for parent %d: %v (type: %T)\n", i, parentItem, parentItem)
				}
				
				// Print raw parent arg for debugging
				fmt.Printf("DEBUG: Parent arg %d before eval: %v (type: %T)\n", i, parentItem, parentItem)
				
				// Evaluate parent class references
				parentVal, err := e.Eval(parentItem, env)
				if err != nil {
					return nil, fmt.Errorf("error evaluating parent class: %v", err)
				}
				
				// Print evaluated parent for debugging
				fmt.Printf("DEBUG: Parent arg %d after eval: %v (type: %T)\n", i, parentVal, parentVal)

				// Ensure parent is a PythonicClass
				parentClass, ok := parentVal.(*core.PythonicClass)
				if !ok {
					return nil, fmt.Errorf("parent must be a class, got %T", parentVal)
				}

				fmt.Printf("DEBUG: Adding parent %s to class %s\n", parentClass.Name, className)
				parents = append(parents, parentClass)
			}
			// Class body starts at index 2
			args = args[2:]
		} else if parentArg == core.LispSymbol("nil") {
			// Special case for nil parent (no inheritance)
			fmt.Printf("DEBUG: nil parent specified for class %s\n", className)
			args = args[2:]
		} else {
			// Single parent case - not in a list
			fmt.Printf("DEBUG: Single parent (not in list) for class %s: %v (type: %T)\n", className, parentArg, parentArg)
			
			// Evaluate the parent class
			parentVal, err := e.Eval(parentArg, env)
			if err != nil {
				return nil, fmt.Errorf("error evaluating parent class: %v", err)
			}
			
			// Print evaluated parent for debugging
			fmt.Printf("DEBUG: Single parent after eval: %v (type: %T)\n", parentVal, parentVal)
			
			// Ensure parent is a PythonicClass
			parentClass, ok := parentVal.(*core.PythonicClass)
			if !ok {
				return nil, fmt.Errorf("parent must be a class, got %T", parentVal)
			}
			
			fmt.Printf("DEBUG: Adding single parent %s to class %s\n", parentClass.Name, className)
			parents = append(parents, parentClass)
			
			// Skip the parent argument
			args = args[2:]
		}
	} else {
		fmt.Printf("DEBUG: Simple case for class %s, no parents (len(args)=%d)\n", className, len(args))
		// Simple case: just class name and body
		args = args[1:]
	}

	// Create the new class
	newClass := core.NewPythonicClass(className, parents)

	// Process class body (process attribute assignments and method definitions)
	processClassBody(newClass, args, e, env)

	// Debug print
	fmt.Printf("DEBUG: Class %s created with %d parents\n", className, len(parents))
	for i, p := range parents {
		fmt.Printf("DEBUG: Parent %d: %s\n", i, p.Name)
	}

	// Register class in environment
	env.Define(core.LispSymbol(className), newClass)

	return newClass, nil
}

// Helper function to process the class body
func processClassBody(class *core.PythonicClass, body []core.LispValue, e core.Evaluator, env core.Environment) error {
	for _, expr := range body {
		// Convert to list if needed
		exprList, isList := expr.(core.LispList)
		if !isList {
			return fmt.Errorf("class body must contain expressions, got %T", expr)
		}

		// Empty list
		if len(exprList) == 0 {
			continue
		}

		// Check expression type
		switch first := exprList[0].(type) {
		case core.LispSymbol:
			switch first {
			case "=", "set":
				// Process attribute assignment: (= attr value)
				if len(exprList) != 3 {
					return fmt.Errorf("attribute assignment requires attribute name and value")
				}

				// Get attribute name
				var attrName string
				switch attr := exprList[1].(type) {
				case core.LispSymbol:
					attrName = string(attr)
				case string:
					attrName = attr
				default:
					return fmt.Errorf("attribute name must be a symbol or string, got %T", exprList[1])
				}

				// Evaluate the value
				attrValue, err := e.Eval(exprList[2], env)
				if err != nil {
					return fmt.Errorf("error evaluating attribute value: %v", err)
				}

				// Add the attribute to the class
				class.AddAttribute(attrName, attrValue)

			case "def", "define":
				// Process method definition: (def (method-name self arg1 arg2) body...)
				if len(exprList) < 2 {
					return fmt.Errorf("method definition requires a method signature")
				}

				// Get method signature
				// Get method signature and unwrap LocatedValue if needed
				var signature core.LispList
				var ok bool
				
				// Handle potential LocatedValue wrapping
				sigArg := exprList[1]
				if located, isLocated := sigArg.(core.LocatedValue); isLocated {
					sigArg = located.Value
					fmt.Printf("DEBUG: Unwrapped LocatedValue for method signature: %v (type: %T)\n", sigArg, sigArg)
				}
				
				signature, ok = sigArg.(core.LispList)
				if !ok {
					return fmt.Errorf("method signature must be a list, got %T", sigArg)
				}
				
				fmt.Printf("DEBUG: Processing method signature with %d elements\n", len(signature))

				if len(signature) < 1 {
					return fmt.Errorf("method signature requires a method name")
				}

				// Get method name
				// Get method name and unwrap LocatedValue if needed
				var methodName core.LispSymbol
				
				// Extract method name
				methodArg := signature[0]
				if located, isLocated := methodArg.(core.LocatedValue); isLocated {
					methodArg = located.Value
					fmt.Printf("DEBUG: Unwrapped LocatedValue for method name: %v (type: %T)\n", methodArg, methodArg)
				}
				
				switch name := methodArg.(type) {
				case core.LispSymbol:
					methodName = name
					fmt.Printf("DEBUG: Method name from symbol: %s\n", methodName)
				case string:
					methodName = core.LispSymbol(name)
					fmt.Printf("DEBUG: Method name from string: %s\n", methodName)
				default:
					return fmt.Errorf("method name must be a symbol, got %T", methodArg)
				}

				// Process method parameters
				var methodParams []core.LispSymbol
				for i := 1; i < len(signature); i++ {
					switch param := signature[i].(type) {
					case core.LispSymbol:
						methodParams = append(methodParams, param)
					case string:
						methodParams = append(methodParams, core.LispSymbol(param))
					default:
						return fmt.Errorf("method parameter must be a symbol, got %T", signature[i])
					}
				}

				// Ensure 'self' is the first parameter
				if len(methodParams) == 0 || methodParams[0] != "self" {
					return fmt.Errorf("first parameter of method must be 'self'")
				}

				// Create method body
				methodBody := core.LispList(exprList[2:])

				// Create the method Lambda
				method := &core.Lambda{
					Params:        methodParams,
					Body:          methodBody,
					Env:           env,
					Closure:       env,
					DefaultValues: make(map[core.LispSymbol]core.LispValue),
					SharedEnv:     nil,
					InstanceID:    getNextInstanceID(),
				}

				// Debug output
				fmt.Printf("DEBUG: Adding method '%s' to class '%s'\n", methodName, class.Name)
				
				// Add the method to the class
				class.AddMethod(string(methodName), method)

			default:
				// Unknown expression in class body
				return fmt.Errorf("unknown expression in class body: %v", first)
			}
		default:
			return fmt.Errorf("expected symbol as first element of expression, got %T", first)
		}
	}

	return nil
}

// EvalDotAssign handles attribute assignment with dot notation
// For example: (= obj.attr value)
func EvalDotAssign(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

			// Get the next object in the chain
			if dotObj, ok := targetObj.(core.DotAccessible); ok {
				nextObj, exists := dotObj.GetProperty(currentAttr)
				if !exists {
					return nil, fmt.Errorf("attribute '%s' not found in path", currentAttr)
				}
				targetObj = nextObj
			} else {
				return nil, fmt.Errorf("cannot access attribute '%s' on non-object", currentAttr)
			}
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

	// Set the attribute on the target object
	if dotObj, ok := targetObj.(core.DotAccessible); ok {
		err = dotObj.SetProperty(targetAttr, value)
		if err != nil {
			return nil, fmt.Errorf("error setting property '%s': %v", targetAttr, err)
		}
		return value, nil
	}

	return nil, fmt.Errorf("cannot set attribute '%s' on non-object", targetAttr)
}

// EvalClassAttrSelf processes special syntax for class attribute assignment with self
// For example: (= self.attr value)
func EvalClassAttrSelf(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("self attribute assignment requires exactly 2 arguments: self.attr expression")
	}

	// Get self from the environment
	self, exists := env.Get("self")
	if !exists {
		return nil, fmt.Errorf("'self' not found in environment - not inside a method")
	}

	// Check if first arg is a symbol like "self.attr"
	dotExpr, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("invalid self attribute assignment: expected self.attr symbol")
	}

	// Parse the symbol to extract the attribute name
	selfStr := string(dotExpr)
	if len(selfStr) <= 5 || selfStr[:5] != "self." {
		return nil, fmt.Errorf("invalid self attribute: expected 'self.attr' format")
	}

	// Extract the attribute name
	attrName := selfStr[5:]

	// Evaluate the value to assign
	value, err := e.Eval(args[1], env)
	if err != nil {
		return nil, fmt.Errorf("error evaluating value in self attribute assignment: %v", err)
	}

	// Try setting via the new Object protocol first
	if objVal, ok := self.(core.AdaptableLispValue); ok {
		// Use the Object protocol
		obj := objVal.AsObject()
		err := obj.SetProp(attrName, value)
		if err == nil {
			return value, nil
		}
	}

	// Fall back to the old interface
	if selfObj, ok := self.(core.DotAccessible); ok {
		err = selfObj.SetProperty(attrName, value)
		if err != nil {
			return nil, fmt.Errorf("error setting self.%s: %v", attrName, err)
		}
		return value, nil
	}

	return nil, fmt.Errorf("'self' is not a valid object")
}

// RegisterClassForms registers all class-related special forms
func RegisterClassForms(forms map[core.LispSymbol]SpecialFormFunc) {
	forms["class"] = EvalClassNew
	forms["dot-assign"] = EvalDotAssign
	forms["class-attr-self"] = EvalClassAttrSelf
}

// Import getNextInstanceID directly from lambda.go
// This is defined here to ensure proper dependency management
// extern getNextInstanceID()

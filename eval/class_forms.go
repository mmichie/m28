package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// classForm implements the class definition special form:
// (class ClassName)                    - define a simple class
// (class ClassName (ParentClass))      - define a class with inheritance
// (class ClassName () body...)         - define a class with methods/attributes
// (class ClassName (ParentClass) body...) - full class definition
func classForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("class requires at least a name")
	}

	// Get class name
	className, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	// Parse parent classes
	var parentClasses []*core.Class
	bodyStart := 1

	if len(args) > 1 {
		// Check if second argument is parent class specification
		// Parent class spec is a list that either:
		// - Is empty: ()
		// - Contains one or more symbols: (ParentClass) or (Parent1, Parent2)
		// - Does NOT start with a special form like "def"
		if parentList, ok := args[1].(core.ListValue); ok {
			isParentSpec := false

			if len(parentList) == 0 {
				// Empty list means no parent but still a parent spec
				isParentSpec = true
			} else {
				// Check if all elements look like class references
				// (symbols or dot expressions)
				allClassRefs := true
				for _, elem := range parentList {
					switch e := elem.(type) {
					case core.SymbolValue:
						// Simple class name
						continue
					case core.ListValue:
						// Could be a dot expression like (. unittest TestCase)
						// But NOT a special form like (def ...)
						if len(e) > 0 {
							if sym, ok := e[0].(core.SymbolValue); ok {
								symStr := string(sym)
								// Check if it's a special form - not a class reference
								if symStr == "def" || symStr == "=" || symStr == "do" ||
									symStr == "if" || symStr == "for" || symStr == "while" {
									allClassRefs = false
									break
								}
							}
						}
						// OK, could be dot expression
						continue
					default:
						// Not a valid class reference
						allClassRefs = false
						break
					}
				}
				if allClassRefs {
					isParentSpec = true
				}
			}

			if isParentSpec {
				bodyStart = 2

				// Get parent classes if specified
				for _, parentElem := range parentList {
					// Evaluate the parent expression (could be a symbol or dot expression)
					parentVal, err := Eval(parentElem, ctx)
					if err != nil {
						return nil, fmt.Errorf("error evaluating parent class: %v", err)
					}

					parent, ok := parentVal.(*core.Class)
					if !ok {
						return nil, fmt.Errorf("parent must be a class, got %T", parentVal)
					}

					parentClasses = append(parentClasses, parent)
				}
			}
		}
	}

	// Create the class
	var class *core.Class
	if len(parentClasses) > 1 {
		class = core.NewClassWithParents(string(className), parentClasses)
	} else if len(parentClasses) == 1 {
		class = core.NewClass(string(className), parentClasses[0])
	} else {
		class = core.NewClass(string(className), nil)
	}

	// Process class body
	for i := bodyStart; i < len(args); i++ {
		stmt := args[i]

		// Handle different statement types
		switch s := stmt.(type) {
		case core.ListValue:
			if len(s) == 0 {
				continue
			}

			// Check for def form (methods) or = form (class variables)
			if sym, ok := s[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "def":
					// Method definition
					if len(s) < 3 {
						return nil, fmt.Errorf("def requires at least name and value")
					}

					name, ok := s[1].(core.SymbolValue)
					if !ok {
						return nil, fmt.Errorf("def name must be a symbol")
					}

					// Check if it's a method definition
					if len(s) >= 3 {
						if paramList, ok := s[2].(core.ListValue); ok {
							// It's a method definition
							method, err := createMethod(string(name), paramList, s[3:], ctx)
							if err != nil {
								return nil, err
							}
							class.SetMethod(string(name), method)
							continue
						}
					}

					// def should only be used for functions
					return nil, fmt.Errorf("def can only be used for method definitions in classes")

				case "=":
					// Class variable definition
					if len(s) != 3 {
						return nil, fmt.Errorf("= requires exactly 2 arguments in class definition")
					}

					name, ok := s[1].(core.SymbolValue)
					if !ok {
						return nil, fmt.Errorf("class variable name must be a symbol")
					}

					// Evaluate the value
					value, err := Eval(s[2], ctx)
					if err != nil {
						return nil, err
					}
					class.SetClassAttr(string(name), value)

				default:
					// Evaluate other forms in class context
					_, err := Eval(stmt, ctx)
					if err != nil {
						return nil, err
					}
				}
			} else {
				// Evaluate other forms in class context
				_, err := Eval(stmt, ctx)
				if err != nil {
					return nil, err
				}
			}
		default:
			// Evaluate the statement
			_, err := Eval(stmt, ctx)
			if err != nil {
				return nil, err
			}
		}
	}

	// Define the class in the context
	ctx.Define(string(className), class)

	return class, nil
}

// createMethod creates a method from a parameter list and body
func createMethod(name string, params core.ListValue, body []core.Value, ctx *core.Context) (*UserFunction, error) {
	// Try to parse as new-style parameter list with defaults
	signature, err := ParseParameterList(params)
	if err != nil {
		// Fall back to legacy simple parameter parsing
		paramSyms := make([]core.SymbolValue, 0, len(params))
		for _, p := range params {
			sym, ok := p.(core.SymbolValue)
			if !ok {
				return nil, fmt.Errorf("method parameters must be symbols")
			}
			paramSyms = append(paramSyms, sym)
		}

		// Create method body
		var methodBody core.Value
		if len(body) == 1 {
			methodBody = body[0]
		} else {
			// Wrap in do
			methodBody = core.ListValue(append([]core.Value{core.SymbolValue("do")}, body...))
		}

		// Create the method with legacy params
		method := &UserFunction{
			BaseObject: *core.NewBaseObject(core.FunctionType),
			params:     paramSyms,
			body:       methodBody,
			env:        ctx,
			name:       name,
		}

		return method, nil
	}

	// New-style method with signature
	// Create method body
	var methodBody core.Value
	if len(body) == 1 {
		methodBody = body[0]
	} else {
		// Wrap in do
		methodBody = core.ListValue(append([]core.Value{core.SymbolValue("do")}, body...))
	}

	// Build legacy params list for backward compatibility
	var paramSyms []core.SymbolValue
	for _, p := range signature.RequiredParams {
		paramSyms = append(paramSyms, p.Name)
	}
	for _, p := range signature.OptionalParams {
		paramSyms = append(paramSyms, p.Name)
	}

	// Create the method with signature
	method := &UserFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		params:     paramSyms,
		signature:  signature,
		body:       methodBody,
		env:        ctx,
		name:       name,
	}

	return method, nil
}

// superForm implements the super special form for backward compatibility
// This handles the bare "super" syntax used in method definitions
func superForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	// Special form super with no args - look up self
	if len(args) == 0 {
		// Look for self in context
		selfVal, err := ctx.Lookup("self")
		if err != nil {
			return nil, fmt.Errorf("super: no arguments given and cannot determine self")
		}

		instance, ok := selfVal.(*core.Instance)
		if !ok {
			return nil, fmt.Errorf("super: self is not an instance")
		}

		// Try to get __class__ from context, otherwise use instance's class
		classVal, err := ctx.Lookup("__class__")
		if err != nil {
			// Use instance's class as fallback
			return core.NewSuper(instance.Class, instance), nil
		}

		class, ok := classVal.(*core.Class)
		if !ok {
			return core.NewSuper(instance.Class, instance), nil
		}

		return core.NewSuper(class, instance), nil
	}

	// Legacy form with explicit class and instance
	if len(args) != 2 {
		return nil, fmt.Errorf("super requires 0 or 2 arguments")
	}

	// Get class
	classVal, err := Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}

	class, ok := classVal.(*core.Class)
	if !ok {
		return nil, fmt.Errorf("first argument to super must be a class")
	}

	// Get instance
	instanceVal, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}

	instance, ok := instanceVal.(*core.Instance)
	if !ok {
		return nil, fmt.Errorf("second argument to super must be an instance")
	}

	return core.NewSuper(class, instance), nil
}

// isinstanceForm checks if an object is an instance of a class
func isinstanceForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("isinstance requires 2 arguments")
	}

	// Evaluate object
	obj, err := Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}

	// Evaluate class
	classVal, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}

	// Handle string type names
	if typeName, ok := classVal.(core.StringValue); ok {
		actualType := string(obj.Type())
		expectedType := string(typeName)

		// Handle Python type name aliases
		switch expectedType {
		case "int", "float":
			return core.BoolValue(actualType == "number"), nil
		case "str":
			return core.BoolValue(actualType == "string"), nil
		case "bool":
			return core.BoolValue(actualType == "bool"), nil
		case "NoneType":
			return core.BoolValue(actualType == "nil"), nil
		case "list":
			return core.BoolValue(actualType == "list"), nil
		case "dict":
			return core.BoolValue(actualType == "dict"), nil
		case "tuple":
			return core.BoolValue(actualType == "tuple"), nil
		case "set":
			return core.BoolValue(actualType == "set"), nil
		default:
			return core.BoolValue(actualType == expectedType), nil
		}
	}

	// Check if obj is an instance and classVal is a class
	instance, isInst := obj.(*core.Instance)
	class, isClass := classVal.(*core.Class)

	if !isClass {
		return nil, fmt.Errorf("isinstance second argument must be a class or string type name")
	}

	if !isInst {
		return core.False, nil
	}

	// Check inheritance chain
	return core.BoolValue(core.IsInstanceOf(instance, class)), nil
}

// issubclassForm checks if a class is a subclass of another
func issubclassForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("issubclass requires 2 arguments")
	}

	// Evaluate both arguments
	subClassVal, err := Eval(args[0], ctx)
	if err != nil {
		return nil, err
	}

	baseClassVal, err := Eval(args[1], ctx)
	if err != nil {
		return nil, err
	}

	// Both must be classes
	subClass, ok1 := subClassVal.(*core.Class)
	baseClass, ok2 := baseClassVal.(*core.Class)

	if !ok1 || !ok2 {
		return nil, fmt.Errorf("issubclass arguments must be classes")
	}

	// Check inheritance chain
	current := subClass
	for current != nil {
		if current == baseClass {
			return core.True, nil
		}
		current = current.Parent
	}

	return core.False, nil
}

// RegisterClassForms registers class-related special forms
func RegisterClassForms() {
	RegisterSpecialForm("class", classForm)
	RegisterSpecialForm("super", superForm) // Special form for bare "super" syntax
	RegisterSpecialForm("isinstance", isinstanceForm)
	RegisterSpecialForm("issubclass", issubclassForm)
}

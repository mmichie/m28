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

	// Parse parent class
	var parentClass *core.Class
	bodyStart := 1

	if len(args) > 1 {
		// Check if second argument is parent class specification
		// Parent class spec is a list that either:
		// - Is empty: ()
		// - Contains a single symbol: (ParentClass)
		// - Does NOT start with a special form like "def"
		if parentList, ok := args[1].(core.ListValue); ok {
			isParentSpec := false
			
			if len(parentList) == 0 {
				// Empty list means no parent but still a parent spec
				isParentSpec = true
			} else if len(parentList) == 1 {
				// Single element list - check if it's a symbol (parent class name)
				if _, ok := parentList[0].(core.SymbolValue); ok {
					isParentSpec = true
				}
			}
			
			if isParentSpec {
				bodyStart = 2
				
				// Get parent class if specified
				if len(parentList) > 0 {
					parentName := parentList[0].(core.SymbolValue)
					
					// Look up parent class
					parentVal, err := ctx.Lookup(string(parentName))
					if err != nil {
						return nil, fmt.Errorf("parent class '%s' not found", string(parentName))
					}
					
					parent, ok := parentVal.(*core.Class)
					if !ok {
						return nil, fmt.Errorf("'%s' is not a class", string(parentName))
					}
					
					parentClass = parent
				}
			}
		}
	}

	// Create the class
	class := core.NewClass(string(className), parentClass)

	// Process class body
	for i := bodyStart; i < len(args); i++ {
		stmt := args[i]
		
		// Handle different statement types
		switch s := stmt.(type) {
		case core.ListValue:
			if len(s) == 0 {
				continue
			}
			
			// Check for def form
			if sym, ok := s[0].(core.SymbolValue); ok && string(sym) == "def" {
				// Method or attribute definition
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
				
				// It's an attribute definition
				value, err := Eval(s[2], ctx)
				if err != nil {
					return nil, err
				}
				class.SetClassAttr(string(name), value)
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
	// Parse parameters
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

	// Create the method
	method := &UserFunction{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		params:     paramSyms,
		body:       methodBody,
		env:        ctx,
		name:       name,
	}

	return method, nil
}

// superForm implements the super function for accessing parent methods
// (super) - returns super object for current class/instance
// (super ClassName instance) - explicit super
func superForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	// For now, implement basic super
	if len(args) != 2 {
		return nil, fmt.Errorf("super requires class and instance arguments")
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
	RegisterSpecialForm("super", superForm)
	RegisterSpecialForm("isinstance", isinstanceForm)
	RegisterSpecialForm("issubclass", issubclassForm)
}
package eval

import (
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
)

var debugClass = os.Getenv("M28_DEBUG_CLASS") != ""

// classForm implements the class definition special form:
// (class ClassName)                    - define a simple class
// (class ClassName (ParentClass))      - define a class with inheritance
// (class ClassName () body...)         - define a class with methods/attributes
// (class ClassName (ParentClass) body...) - full class definition
// (class ClassName (ParentClass) (keywords) body...) - with keywords like metaclass
func classForm(args core.ListValue, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("class requires at least a name")
	}

	// Get class name
	className, ok := args[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	if debugClass {
		fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Defining class '%s' with %d args\n", className, len(args))
	}

	// Parse parent classes and keywords
	var parentClasses []*core.Class
	var metaclass *core.Class
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
				for i, parentElem := range parentList {
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Evaluating parent %d: %v\n", i, core.PrintValue(parentElem))
					}
					// Evaluate the parent expression (could be a symbol or dot expression)
					parentVal, err := Eval(parentElem, ctx)
					if err != nil {
						if debugClass {
							fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Error evaluating parent: %v\n", err)
						}
						return nil, fmt.Errorf("error evaluating parent class: %v", err)
					}

					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Parent evaluated to: %T\n", parentVal)
					}

					var parent *core.Class
					switch p := parentVal.(type) {
					case *core.Class:
						parent = p
					case interface{ GetClass() *core.Class }:
						// Handle wrapper types that embed Class (like staticmethod, classmethod)
						parent = p.GetClass()
					default:
						return nil, fmt.Errorf("parent must be a class, got %T", parentVal)
					}

					parentClasses = append(parentClasses, parent)
				}
			}
		}
	}

	// Parse keywords (e.g., metaclass=ABCMeta)
	if len(args) > bodyStart {
		if kwList, ok := args[bodyStart].(core.ListValue); ok {
			if debugClass {
				fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Checking potential keywords at index %d: %v\n", bodyStart, kwList)
			}

			// Handle bracket syntax: [[key val]] becomes [list-literal [list-literal key val]]
			// Skip the first 'list-literal' symbol if present
			startIdx := 0
			if len(kwList) > 0 {
				if sym, ok := kwList[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
					startIdx = 1
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Skipping list-literal marker\n")
					}
				}
			}

			// Check if this looks like a keywords list
			// Keywords are represented as [("metaclass" ABCMeta), ...]
			isKeywords := true
			for i := startIdx; i < len(kwList); i++ {
				kw := kwList[i]
				if debugClass {
					fmt.Fprintf(os.Stderr, "[DEBUG CLASS] kwList[%d] = %v (type %T)\n", i, kw, kw)
				}

				// Handle nested list-literal for each keyword pair
				var kwPair core.ListValue
				if pair, ok := kw.(core.ListValue); ok {
					// Skip list-literal if present
					pairStartIdx := 0
					if len(pair) > 0 {
						if sym, ok := pair[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
							pairStartIdx = 1
							if debugClass {
								fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Found inner list-literal, pairStartIdx=%d\n", pairStartIdx)
							}
						}
					}
					// Extract the actual key-value pair
					if len(pair)-pairStartIdx == 2 {
						kwPair = pair[pairStartIdx:]
					} else {
						kwPair = pair
					}
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Extracted kwPair: %v (len=%d)\n", kwPair, len(kwPair))
					}
				} else {
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] kw is not a ListValue: %T\n", kw)
					}
				}

				// Each keyword should be a 2-element list: (name, value)
				if len(kwPair) == 2 {
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Looks like keyword pair: %v\n", kwPair)
					}
					// Good, looks like a keyword pair
					continue
				} else {
					// Not a keyword pair, this is probably class body
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Not a keyword pair (len=%d), treating as body\n", len(kwPair))
					}
					isKeywords = false
					break
				}
			}

			if isKeywords && len(kwList) > startIdx {
				// This is a keywords list, process it
				bodyStart++ // Skip past keywords in body processing

				for i := startIdx; i < len(kwList); i++ {
					kw := kwList[i]

					// Extract keyword pair, handling list-literal markers
					var kwPair core.ListValue
					if pair, ok := kw.(core.ListValue); ok {
						pairStartIdx := 0
						if len(pair) > 0 {
							if sym, ok := pair[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
								pairStartIdx = 1
							}
						}
						kwPair = pair[pairStartIdx:]
					} else {
						continue
					}

					if len(kwPair) != 2 {
						continue
					}

					kwName, ok := kwPair[0].(core.StringValue)
					if !ok {
						continue
					}

					if string(kwName) == "metaclass" {
						// Evaluate the metaclass expression
						metaclassVal, err := Eval(kwPair[1], ctx)
						if err != nil {
							return nil, fmt.Errorf("error evaluating metaclass: %v", err)
						}

						// Check if it's a class
						if mc, ok := metaclassVal.(*core.Class); ok {
							metaclass = mc
						} else if wrapper, ok := metaclassVal.(interface{ GetClass() *core.Class }); ok {
							metaclass = wrapper.GetClass()
						}
					}
				}
			}
		}
	}

	// If no metaclass was explicitly specified, check if we're creating a metaclass
	// (i.e., if the parent is 'type' or a subclass of type)
	if metaclass == nil && len(parentClasses) > 0 {
		for _, parent := range parentClasses {
			if parent.Name == "type" {
				// This class inherits from type, so it's a metaclass
				// Use type as the metaclass to create it
				if typeClass, err := ctx.Lookup("type"); err == nil {
					if tc, ok := typeClass.(*core.Class); ok {
						metaclass = tc
					} else if wrapper, ok := typeClass.(interface{ GetClass() *core.Class }); ok {
						metaclass = wrapper.GetClass()
					}
				}
				break
			}
		}
	}

	// Create the class
	var class *core.Class

	// If metaclass was specified, we need to use it to create the class
	if metaclass != nil {
		if debugClass {
			fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Creating class '%s' using metaclass '%s'\n", className, metaclass.Name)
		}

		// First, create a basic class to collect body elements
		// We'll process the body first to build the namespace dict
		var tempClass *core.Class
		if len(parentClasses) > 1 {
			tempClass = core.NewClassWithParents(string(className), parentClasses)
		} else if len(parentClasses) == 1 {
			tempClass = core.NewClass(string(className), parentClasses[0])
		} else {
			tempClass = core.NewClass(string(className), nil)
		}

		// We'll process the body later and collect namespace, then call metaclass.__new__
		// For now, use the tempClass and we'll call __new__ after processing body
		class = tempClass
	} else {
		// No metaclass, create class normally
		if len(parentClasses) > 1 {
			class = core.NewClassWithParents(string(className), parentClasses)
		} else if len(parentClasses) == 1 {
			class = core.NewClass(string(className), parentClasses[0])
		} else {
			class = core.NewClass(string(className), nil)
		}
	}

	if debugClass {
		fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Created class '%s', processing body from index %d to %d\n", className, bodyStart, len(args)-1)
	}

	// Create a class-body context that allows accessing class members as they're defined
	// This enables patterns like: __await__ = __iter__
	classBodyCtx := core.NewContext(ctx)

	// Process class body
	for i := bodyStart; i < len(args); i++ {
		stmt := args[i]
		if debugClass {
			fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Processing body statement %d: %T\n", i, stmt)
		}

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
							// Also add to class body context so later statements can reference it
							classBodyCtx.Define(string(name), method)
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

					// Evaluate the value in class body context (can reference previously defined methods/attrs)
					value, err := Eval(s[2], classBodyCtx)
					if err != nil {
						return nil, err
					}
					class.SetClassAttr(string(name), value)
					// Also add to class body context so later statements can reference it
					classBodyCtx.Define(string(name), value)

				default:
					// Evaluate other forms in class context
					if debugClass {
						if sym, ok := s[0].(core.SymbolValue); ok {
							fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Evaluating form '%s' in class context\n", sym)
						}
					}
					_, err := Eval(stmt, ctx)
					if err != nil {
						return nil, err
					}
				}
			} else {
				// Evaluate other forms in class context
				if debugClass {
					fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Evaluating non-symbol list in class context\n")
				}
				_, err := Eval(stmt, ctx)
				if err != nil {
					return nil, err
				}
			}
		default:
			// Evaluate the statement
			if debugClass {
				fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Evaluating non-list statement: %T\n", stmt)
			}
			_, err := Eval(stmt, ctx)
			if err != nil {
				return nil, err
			}
		}
	}

	// If metaclass was specified, call its __new__ method to finalize the class
	if metaclass != nil {
		if debugClass {
			fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Calling metaclass.__new__ for class '%s'\n", className)
		}

		// Check if metaclass has __new__ method
		if newMethod, hasNew := metaclass.GetMethod("__new__"); hasNew {
			if callable, ok := newMethod.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				// Build namespace dict from class methods and attributes
				namespace := core.NewDict()
				for name, method := range class.Methods {
					namespace.Set(name, method)
				}
				for name, attr := range class.Attributes {
					namespace.Set(name, attr)
				}

				// Build bases tuple
				bases := make(core.TupleValue, len(parentClasses))
				for i, parent := range parentClasses {
					bases[i] = parent
				}

				// Call metaclass.__new__(metaclass, name, bases, namespace)
				args := []core.Value{
					metaclass,                   // mcls
					core.StringValue(className), // name
					bases,                       // bases
					namespace,                   // namespace
				}

				// Create a new context with __class__ set for super() support
				callCtx := core.NewContext(ctx)
				callCtx.Define("__class__", metaclass)

				result, err := callable.Call(args, callCtx)
				if err != nil {
					return nil, fmt.Errorf("error calling metaclass.__new__: %v", err)
				}

				// The result should be a class
				if newClass, ok := result.(*core.Class); ok {
					class = newClass
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] metaclass.__new__ returned new class\n")
					}
				} else {
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] metaclass.__new__ returned %T, keeping original class\n", result)
					}
				}
			}
		}

		// After __new__, copy metaclass methods to the class as class methods
		// First check metaclass.Methods
		for methodName, method := range metaclass.Methods {
			// Don't override methods created by __new__
			if _, exists := class.Methods[methodName]; exists {
				continue
			}

			// Skip __new__ and __init__
			if methodName != "__new__" && methodName != "__init__" {
				// Wrap the method so it receives the class as first argument
				if callable, ok := method.(interface {
					Call([]core.Value, *core.Context) (core.Value, error)
				}); ok {
					boundMethod := createBoundClassMethod(class, callable)
					class.SetMethod(methodName, boundMethod)
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Copied method '%s' from metaclass.Methods\n", methodName)
					}
				}
			}
		}

		// Also check metaclass.Attributes (for Python-defined metaclass methods in __dict__)
		for attrName, attr := range metaclass.Attributes {
			// Don't override existing methods
			if _, exists := class.Methods[attrName]; exists {
				continue
			}

			// Skip special attributes
			if strings.HasPrefix(attrName, "__") && strings.HasSuffix(attrName, "__") {
				continue
			}

			// Check if it's callable (a method)
			if callable, ok := attr.(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			}); ok {
				boundMethod := createBoundClassMethod(class, callable)
				class.SetMethod(attrName, boundMethod)
				if debugClass {
					fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Copied method '%s' from metaclass.Attributes\n", attrName)
				}
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
	// Special form super with no args - look up self/cls/mcls
	if len(args) == 0 {
		// First, check if __class__ is defined in the context
		// This tells us which class's method we're currently executing in
		classVal, classErr := ctx.Lookup("__class__")
		if classErr == nil {
			if class, ok := classVal.(*core.Class); ok {
				// We know which class we're in, so use its parent
				// Now determine if we're in an instance method or class method
				// by checking for self/cls/mcls
				if selfVal, err := ctx.Lookup("self"); err == nil {
					if instance, ok := selfVal.(*core.Instance); ok {
						// Instance method - return super for parent class with instance
						if len(class.Parents) > 0 {
							return core.NewSuper(class.Parents[0], instance), nil
						} else if class.Parent != nil {
							return core.NewSuper(class.Parent, instance), nil
						}
						return nil, fmt.Errorf("super: class has no parent")
					}
				}

				// Class method or metaclass method - no instance
				if len(class.Parents) > 0 {
					return core.NewSuper(class.Parents[0], nil), nil
				} else if class.Parent != nil {
					return core.NewSuper(class.Parent, nil), nil
				}
				return nil, fmt.Errorf("super: class has no parent")
			}
		}

		// Fallback: Try to find the first parameter (could be self, cls, mcls, etc.)
		// Try common names in order
		var firstArg core.Value
		var err error

		// Try self first (for instance methods)
		firstArg, err = ctx.Lookup("self")
		if err != nil {
			// Try cls (for class methods)
			firstArg, err = ctx.Lookup("cls")
			if err != nil {
				// Try mcls (for metaclass methods)
				firstArg, err = ctx.Lookup("mcls")
				if err != nil {
					return nil, fmt.Errorf("super: no arguments given and cannot determine self/cls/mcls")
				}
			}
		}

		// Check if it's an instance
		if instance, ok := firstArg.(*core.Instance); ok {
			// It's an instance, use its class
			return core.NewSuper(instance.Class, instance), nil
		}

		// Check if it's a class (for class methods or metaclass methods)
		if class, ok := firstArg.(*core.Class); ok {
			// It's a class, use its parent
			if class.Parent != nil {
				// Return a super object for the parent class
				// Pass nil as instance since we're in a class/metaclass method
				return core.NewSuper(class.Parent, nil), nil
			}
			// No parent, can't use super
			return nil, fmt.Errorf("super: class has no parent")
		}

		// Check for wrapper types
		if wrapper, ok := firstArg.(interface{ GetClass() *core.Class }); ok {
			class := wrapper.GetClass()
			if class.Parent != nil {
				// Pass nil as instance
				return core.NewSuper(class.Parent, nil), nil
			}
			return nil, fmt.Errorf("super: class has no parent")
		}

		return nil, fmt.Errorf("super: first argument must be an instance or class, got %T", firstArg)
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

	// Handle wrapper types that have GetClass() method (like TypeType)
	if !isClass {
		if wrapper, ok := classVal.(interface{ GetClass() *core.Class }); ok {
			class = wrapper.GetClass()
			isClass = true
		}
	}

	if !isClass {
		return nil, fmt.Errorf("isinstance second argument must be a class or string type name")
	}

	// Handle primitive types (StringValue, NumberValue, etc.) that are not Instances
	if !isInst {
		// Check if obj is a primitive and class matches its type
		switch obj.(type) {
		case core.StringValue:
			// Check if class is the str class
			if class.Name == "string" || class.Name == "str" {
				return core.True, nil
			}
		case core.NumberValue:
			// Check if class is the number/int/float class
			if class.Name == "number" || class.Name == "int" || class.Name == "float" {
				return core.True, nil
			}
		case core.BoolValue:
			// Check if class is the bool class
			if class.Name == "bool" {
				return core.True, nil
			}
		case core.NilValue:
			// Check if class is NoneType
			if class.Name == "nil" || class.Name == "NoneType" {
				return core.True, nil
			}
		case core.ListValue:
			// Check if class is list
			if class.Name == "list" {
				return core.True, nil
			}
		case *core.DictValue:
			// Check if class is dict
			if class.Name == "dict" {
				return core.True, nil
			}
		case core.TupleValue:
			// Check if class is tuple
			if class.Name == "tuple" {
				return core.True, nil
			}
		case *core.SetValue:
			// Check if class is set
			if class.Name == "set" {
				return core.True, nil
			}
		}
	}

	if !isInst {
		// Special case: isinstance(Foo, type) where Foo is a class
		// type is the metaclass, so classes are instances of type
		if class.Name == "type" {
			// Check if obj is a class
			if _, ok := obj.(*core.Class); ok {
				return core.True, nil
			}
			// Also check wrapper types that are classes
			if _, ok := obj.(interface{ GetClass() *core.Class }); ok {
				return core.True, nil
			}
			// Check for builtin type constructors (like frozenset, set, list, tuple)
			// These are BuiltinFunctions but should be considered types for isinstance(x, type)
			if bf, ok := obj.(*core.BuiltinFunction); ok {
				if nameAttr, hasName := bf.GetAttr("__name__"); hasName {
					if nameStr, ok := nameAttr.(core.StringValue); ok {
						typeName := string(nameStr)
						// List of builtin type constructors and iterator types
						typeConstructors := map[string]bool{
							// Basic types
							"frozenset":    true,
							"set":          true,
							"list":         true,
							"tuple":        true,
							"bytes":        true,
							"bytearray":    true,
							"str":          true,
							"int":          true,
							"float":        true,
							"bool":         true,
							"dict":         true,
							"range":        true,
							"memoryview":   true,
							"complex":      true,
							"object":       true,
							"property":     true,
							"staticmethod": true,
							"classmethod":  true,
							"function":     true,
							"generator":    true,
							"coroutine":    true,
							// Iterator types
							"bytes_iterator":       true,
							"bytearray_iterator":   true,
							"dict_keyiterator":     true,
							"dict_valueiterator":   true,
							"dict_itemiterator":    true,
							"list_iterator":        true,
							"list_reverseiterator": true,
							"range_iterator":       true,
							"longrange_iterator":   true,
							"set_iterator":         true,
							"str_iterator":         true,
							"tuple_iterator":       true,
							"zip_iterator":         true,
							// Dict view types
							"dict_keys":   true,
							"dict_values": true,
							"dict_items":  true,
							// Special types
							"ellipsis":     true,
							"mappingproxy": true,
						}
						if typeConstructors[typeName] {
							if debugClass {
								fmt.Fprintf(os.Stderr, "[DEBUG isinstance] Recognized %s as type constructor\n", typeName)
							}
							return core.True, nil
						}
					}
				}
			}
			// Log what we're rejecting (only in debug mode)
			if debugClass {
				if bf, ok := obj.(*core.BuiltinFunction); ok {
					name, _ := getBuiltinName(bf)
					if name == "<anonymous>" {
						fmt.Fprintf(os.Stderr, "[DEBUG isinstance] ANONYMOUS builtin rejected: String()=%s Type()=%s\n", bf.String(), bf.Type())
					} else {
						fmt.Fprintf(os.Stderr, "[DEBUG isinstance] builtin __name__='%s' rejected\n", name)
					}
				} else {
					fmt.Fprintf(os.Stderr, "[DEBUG isinstance] non-builtin %T rejected\n", obj)
				}
			}
		}
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

	// Extract classes, handling wrapper types and builtin type constructors
	subClass, ok1 := subClassVal.(*core.Class)
	if !ok1 {
		// Try wrapper types
		if wrapper, ok := subClassVal.(interface{ GetClass() *core.Class }); ok {
			subClass = wrapper.GetClass()
			ok1 = true
		}
	}

	baseClass, ok2 := baseClassVal.(*core.Class)
	if !ok2 {
		// Try wrapper types
		if wrapper, ok := baseClassVal.(interface{ GetClass() *core.Class }); ok {
			baseClass = wrapper.GetClass()
			ok2 = true
		}
	}

	// Special handling for builtin type constructors
	// If either argument is a builtin type constructor, we can't really check subclass relationships
	// but we can check for equality
	if bf, ok := subClassVal.(*core.BuiltinFunction); ok && !ok1 {
		// subclass is a builtin function, check if it's a type constructor
		if isTypeConstructor(bf) {
			// For builtin types, we can only check equality
			if bf2, ok := baseClassVal.(*core.BuiltinFunction); ok {
				// Both are builtin functions, check if they're the same
				subName, _ := getBuiltinName(bf)
				baseName, _ := getBuiltinName(bf2)
				if subName == baseName {
					return core.True, nil
				}
				return core.False, nil
			}
			// Can't determine subclass for builtin vs class
			return core.False, nil
		}
	}

	if bf, ok := baseClassVal.(*core.BuiltinFunction); ok && !ok2 {
		// base is a builtin function, check if it's a type constructor
		if isTypeConstructor(bf) {
			// Can't be a subclass of a builtin type
			return core.False, nil
		}
	}

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

// isTypeConstructor checks if a builtin function is a type constructor
func isTypeConstructor(bf *core.BuiltinFunction) bool {
	if nameAttr, hasName := bf.GetAttr("__name__"); hasName {
		if nameStr, ok := nameAttr.(core.StringValue); ok {
			typeName := string(nameStr)
			typeConstructors := map[string]bool{
				// Basic types
				"frozenset": true, "set": true, "list": true, "tuple": true,
				"bytes": true, "bytearray": true, "str": true,
				"int": true, "float": true, "bool": true, "dict": true,
				"range": true, "memoryview": true, "complex": true,
				"object": true, "property": true,
				"staticmethod": true, "classmethod": true,
				"function": true, "generator": true, "coroutine": true,
				// Iterator types
				"bytes_iterator": true, "bytearray_iterator": true,
				"dict_keyiterator": true, "dict_valueiterator": true, "dict_itemiterator": true,
				"list_iterator": true, "list_reverseiterator": true,
				"range_iterator": true, "longrange_iterator": true,
				"set_iterator": true, "str_iterator": true, "tuple_iterator": true,
				"zip_iterator": true,
				// Dict view types
				"dict_keys": true, "dict_values": true, "dict_items": true,
				// Special types
				"ellipsis": true, "mappingproxy": true,
			}
			return typeConstructors[typeName]
		}
	}
	return false
}

// getBuiltinName returns the __name__ of a builtin function
func getBuiltinName(bf *core.BuiltinFunction) (string, bool) {
	if nameAttr, hasName := bf.GetAttr("__name__"); hasName {
		if nameStr, ok := nameAttr.(core.StringValue); ok {
			return string(nameStr), true
		}
	}
	return "", false
}

// createBoundClassMethod creates a wrapper that binds a metaclass method to a class
// When called, it automatically prepends the class as the first argument
func createBoundClassMethod(class *core.Class, method interface {
	Call([]core.Value, *core.Context) (core.Value, error)
}) *core.BuiltinFunction {
	return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Prepend the class as the first argument (cls parameter)
		callArgs := append([]core.Value{class}, args...)
		return method.Call(callArgs, ctx)
	})
}

// RegisterClassForms registers class-related special forms
func RegisterClassForms() {
	RegisterSpecialForm("class", classForm)
	RegisterSpecialForm("super", superForm) // Special form for bare "super" syntax
	RegisterSpecialForm("isinstance", isinstanceForm)
	RegisterSpecialForm("issubclass", issubclassForm)
}

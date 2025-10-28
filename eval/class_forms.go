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
func classForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() < 1 {
		return nil, fmt.Errorf("class requires at least a name")
	}

	// Get class name
	className, ok := args.Items()[0].(core.SymbolValue)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	if debugClass {
		fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Defining class '%s' with %d args\n", className, args.Len())
	}

	// Parse parent classes and keywords
	var parentClasses []*core.Class
	var metaclass *core.Class
	bodyStart := 1

	if args.Len() > 1 {
		// Check if second argument is parent class specification
		// Parent class spec is a list that either:
		// - Is empty: ()
		// - Contains one or more symbols: (ParentClass) or (Parent1, Parent2)
		// - Does NOT start with a special form like "def"
		if parentList, ok := args.Items()[1].(*core.ListValue); ok {
			isParentSpec := false

			if parentList.Len() == 0 {
				// Empty list means no parent but still a parent spec
				isParentSpec = true
			} else {
				// Check if all elements look like class references
				// (symbols or dot expressions)
				allClassRefs := true
				for _, elem := range parentList.Items() {
					switch e := elem.(type) {
					case core.SymbolValue:
						// Simple class name
						continue
					case *core.ListValue:
						// Could be a dot expression like (. unittest TestCase)
						// But NOT a special form like (def ...)
						if e.Len() > 0 {
							if sym, ok := e.Items()[0].(core.SymbolValue); ok {
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
				parentItems := parentList.Items()
				for i, parentElem := range parentItems {
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
					case *core.BuiltinFunction:
						// TEMPORARY: Allow builtin types as base classes
						// TODO: Make list/int/float proper classes
						if nameVal, ok := p.GetAttr("__name__"); ok {
							if nameStr, ok := nameVal.(core.StringValue); ok {
								parent = core.NewClass(string(nameStr), nil)
							} else {
								return nil, fmt.Errorf("parent must be a class for class '%s', got %T from expression: %v", className, parentVal, parentItems[i])
							}
						} else {
							return nil, fmt.Errorf("parent must be a class for class '%s', got %T from expression: %v", className, parentVal, parentItems[i])
						}
					default:
						return nil, fmt.Errorf("parent must be a class for class '%s', got %T from expression: %v", className, parentVal, parentItems[i])
					}

					parentClasses = append(parentClasses, parent)
				}
			}
		}
	}

	// Parse keywords (e.g., metaclass=ABCMeta)
	if args.Len() > bodyStart {
		if kwList, ok := args.Items()[bodyStart].(*core.ListValue); ok {
			if debugClass {
				fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Checking potential keywords at index %d: %v\n", bodyStart, kwList)
			}

			// Handle bracket syntax: [[key val]] becomes [list-literal [list-literal key val]]
			// Skip the first 'list-literal' symbol if present
			kwItems := kwList.Items()
			startIdx := 0
			if kwList.Len() > 0 {
				if sym, ok := kwItems[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
					startIdx = 1
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Skipping list-literal marker\n")
					}
				}
			}

			// Check if this looks like a keywords list
			// Keywords are represented as [("metaclass" ABCMeta), ...]
			isKeywords := true
			for i := startIdx; i < kwList.Len(); i++ {
				kw := kwItems[i]
				if debugClass {
					fmt.Fprintf(os.Stderr, "[DEBUG CLASS] kwList[%d] = %v (type %T)\n", i, kw, kw)
				}

				// Handle nested list-literal for each keyword pair
				var kwPair *core.ListValue
				if pair, ok := kw.(*core.ListValue); ok {
					// Skip list-literal if present
					pairItems := pair.Items()
					pairStartIdx := 0
					if pair.Len() > 0 {
						if sym, ok := pairItems[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
							pairStartIdx = 1
							if debugClass {
								fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Found inner list-literal, pairStartIdx=%d\n", pairStartIdx)
							}
						}
					}
					// Extract the actual key-value pair
					if pair.Len()-pairStartIdx == 2 {
						kwPair = core.NewList(pairItems[pairStartIdx:]...)
					} else {
						kwPair = pair
					}
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Extracted kwPair: %v (len=%d)\n", kwPair, kwPair.Len())
					}
				} else {
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] kw is not a ListValue: %T\n", kw)
					}
				}

				// Each keyword should be a 2-element list: (name, value)
				if kwPair != nil && kwPair.Len() == 2 {
					if debugClass {
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Looks like keyword pair: %v\n", kwPair)
					}
					// Good, looks like a keyword pair
					continue
				} else {
					// Not a keyword pair, this is probably class body
					if debugClass {
						kwLen := 0
						if kwPair != nil {
							kwLen = kwPair.Len()
						}
						fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Not a keyword pair (len=%d), treating as body\n", kwLen)
					}
					isKeywords = false
					break
				}
			}

			if isKeywords && kwList.Len() > startIdx {
				// This is a keywords list, process it
				bodyStart++ // Skip past keywords in body processing

				for i := startIdx; i < kwList.Len(); i++ {
					kw := kwItems[i]

					// Extract keyword pair, handling list-literal markers
					var kwPair *core.ListValue
					if pair, ok := kw.(*core.ListValue); ok {
						pairItems := pair.Items()
						pairStartIdx := 0
						if pair.Len() > 0 {
							if sym, ok := pairItems[0].(core.SymbolValue); ok && string(sym) == "list-literal" {
								pairStartIdx = 1
							}
						}
						if pair.Len()-pairStartIdx == 2 {
							kwPair = core.NewList(pairItems[pairStartIdx:]...)
						} else {
							kwPair = pair
						}
					} else {
						continue
					}

					if kwPair.Len() != 2 {
						continue
					}

					kwPairItems := kwPair.Items()
					kwName, ok := kwPairItems[0].(core.StringValue)
					if !ok {
						continue
					}

					if string(kwName) == "metaclass" {
						// Evaluate the metaclass expression
						metaclassVal, err := Eval(kwPairItems[1], ctx)
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

	// If no parent classes specified, default to object (like Python)
	// But not for the object class itself to avoid circular reference
	if len(parentClasses) == 0 && string(className) != "object" {
		// Try to look up object class
		if objectVal, err := ctx.Lookup("object"); err == nil {
			if objectClass, ok := objectVal.(*core.Class); ok {
				parentClasses = []*core.Class{objectClass}
				if debugClass {
					fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Using object as default parent for class '%s'\n", className)
				}
			}
		}
	}

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

	// Debug for Path classes
	if string(className) == "Path" || string(className) == "PosixPath" || string(className) == "PurePath" {
		fmt.Printf("[DEBUG classForm] Created class '%s' with %d parents\n", className, len(parentClasses))
		for i, p := range parentClasses {
			fmt.Printf("[DEBUG classForm]   Parent[%d] = %s\n", i, p.Name)
		}
		fmt.Printf("[DEBUG classForm] %s.Parents = %v\n", className, func() []string {
			names := make([]string, len(class.Parents))
			for i, p := range class.Parents {
				names[i] = p.Name
			}
			return names
		}())
		if class.Parent != nil {
			fmt.Printf("[DEBUG classForm] %s.Parent = %s\n", className, class.Parent.Name)
		} else {
			fmt.Printf("[DEBUG classForm] %s.Parent = nil\n", className)
		}
	}

	if debugClass {
		fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Created class '%s', processing body from index %d to %d\n", className, bodyStart, args.Len()-1)
	}

	// Create a class-body context that allows accessing class members as they're defined
	// This enables patterns like: __await__ = __iter__
	classBodyCtx := core.NewContext(ctx)

	// Process class body
	argsItems := args.Items()
	for i := bodyStart; i < args.Len(); i++ {
		stmt := argsItems[i]
		if debugClass {
			fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Processing body statement %d: %T\n", i, stmt)
		}

		// Handle different statement types
		switch s := stmt.(type) {
		case *core.ListValue:
			if s.Len() == 0 {
				continue
			}

			sItems := s.Items()
			// Check for def form (methods), = form (class variables), or if form (conditional defs)
			if sym, ok := sItems[0].(core.SymbolValue); ok {
				switch string(sym) {
				case "if":
					// Special handling for if statements at class level
					// Evaluate the if statement, then check if any new methods/attrs were defined
					beforeMethods := make(map[string]core.Value)
					for k, v := range class.Methods {
						beforeMethods[k] = v
					}
					beforeAttrs := make(map[string]core.Value)
					for k, v := range class.Attributes {
						beforeAttrs[k] = v
					}

					// Create a temporary context that inherits from classBodyCtx
					// and tracks definitions
					ifCtx := core.NewContext(classBodyCtx)

					// Store a reference to the class so inner defs can register themselves
					ifCtx.Define("__defining_class__", class)

					// Evaluate the if statement in this context
					_, err := Eval(stmt, ifCtx)
					if err != nil {
						return nil, err
					}

					// Check for any newly defined values in ifCtx
					for name, value := range ifCtx.Vars {
						// Skip special names
						if strings.HasPrefix(name, "__") && strings.HasSuffix(name, "__") {
							continue
						}

						// Check if it's a function (method)
						if fn, ok := value.(*UserFunction); ok {
							class.SetMethod(name, fn)
							classBodyCtx.Define(name, value)
							if debugClass {
								fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Added conditional method '%s' to class\n", name)
							}
						} else {
							// It's a class attribute
							class.SetClassAttr(name, value)
							classBodyCtx.Define(name, value)
							if debugClass {
								fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Added conditional attribute '%s' to class\n", name)
							}
						}
					}
					continue

				case "def":
					// Method definition
					if s.Len() < 3 {
						return nil, fmt.Errorf("def requires at least name and value")
					}

					name, ok := sItems[1].(core.SymbolValue)
					if !ok {
						return nil, fmt.Errorf("def name must be a symbol")
					}

					// Check if it's a method definition
					if s.Len() >= 3 {
						if paramList, ok := sItems[2].(*core.ListValue); ok {
							// It's a method definition
							method, err := createMethod(string(name), paramList, sItems[3:], ctx)
							if err != nil {
								return nil, err
							}

							// Special methods that are implicitly classmethods
							// __init_subclass__ and __class_getitem__ automatically receive the class as first arg
							methodName := string(name)
							var finalMethod core.Value = method
							if methodName == "__init_subclass__" || methodName == "__class_getitem__" {
								// Wrap as a classmethod that receives the class as first argument
								finalMethod = createBoundClassMethod(class, method)
							}

							// Debug for PurePath.__init__
							if (string(className) == "PurePath" || string(className) == "Path") && methodName == "__init__" {
								fmt.Printf("[DEBUG classForm] Setting method %s.%s via SetMethod\n", className, methodName)
							}

							class.SetMethod(methodName, finalMethod)
							// Also add to class body context so later statements can reference it
							classBodyCtx.Define(methodName, finalMethod)
							continue
						}
					}

					// def should only be used for functions
					return nil, fmt.Errorf("def can only be used for method definitions in classes")

				case "=":
					// Class variable definition or subscript assignment
					if s.Len() != 3 {
						return nil, fmt.Errorf("= requires exactly 2 arguments in class definition")
					}

					// Check if this is a simple variable assignment or subscript assignment
					name, ok := sItems[1].(core.SymbolValue)
					if ok {
						// Simple variable assignment: x = value
						value, err := Eval(sItems[2], classBodyCtx)
						if err != nil {
							return nil, err
						}
						class.SetClassAttr(string(name), value)
						// Also add to class body context so later statements can reference it
						classBodyCtx.Define(string(name), value)
					} else {
						// Could be subscript assignment: obj[key] = value
						// Check if sItems[1] is a list starting with get-item
						if lhs, isList := sItems[1].(*core.ListValue); isList && lhs.Len() >= 3 {
							if sym, isSymbol := lhs.Items()[0].(core.SymbolValue); isSymbol && string(sym) == "get-item" {
								// This is obj[key] = value, evaluate it as a setitem operation
								// Evaluate the whole assignment as an expression
								_, err := Eval(stmt, classBodyCtx)
								if err != nil {
									return nil, err
								}
								// Continue to next statement
								continue
							}
						}
						// If we get here, it's an unsupported assignment pattern
						return nil, fmt.Errorf("class variable name must be a symbol or subscript expression, got %T", sItems[1])
					}

				default:
					// Evaluate other forms in class context
					if debugClass {
						if sym, ok := sItems[0].(core.SymbolValue); ok {
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

	// Check for __slots__ and set up slot descriptors
	if slotsAttr, hasSlots := class.GetClassAttr("__slots__"); hasSlots {
		if debugClass {
			fmt.Fprintf(os.Stderr, "[DEBUG CLASS] Setting up __slots__ for class '%s'\n", className)
		}
		err := core.SetupSlots(class, slotsAttr)
		if err != nil {
			return nil, fmt.Errorf("error setting up __slots__ for class '%s': %v", className, err)
		}
		if debugClass {
			fmt.Fprintf(os.Stderr, "[DEBUG CLASS] __slots__ setup complete with %d slots\n", len(class.SlotNames))
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
func createMethod(name string, params *core.ListValue, body []core.Value, ctx *core.Context) (*UserFunction, error) {
	// Debug for __init__
	if name == "__init__" {
		fmt.Printf("[DEBUG createMethod] Creating __init__ with %d body items\n", len(body))
		for i, item := range body {
			fmt.Printf("  Body[%d]: %T = %v\n", i, item, item)
		}
	}

	// Try to parse as new-style parameter list with defaults
	signature, err := ParseParameterList(params.Items())
	if err != nil {
		// Fall back to legacy simple parameter parsing
		paramSyms := make([]core.SymbolValue, 0, params.Len())
		for _, p := range params.Items() {
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
			methodBody = core.NewList(append([]core.Value{core.SymbolValue("do")}, body...)...)
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
		methodBody = core.NewList(append([]core.Value{core.SymbolValue("do")}, body...)...)
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
func superForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	// Special form super with no args - look up self/cls/mcls
	if args.Len() == 0 {
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
						// Debug for pathlib classes
						if class.Name == "PosixPath" || class.Name == "Path" || class.Name == "PurePath" || class.Name == "PurePosixPath" {
							fmt.Printf("[DEBUG super] __class__=%s, instance.Class=%s\n", class.Name, instance.Class.Name)
							if len(class.Parents) > 0 {
								fmt.Printf("[DEBUG super] %s -> Parents[0] = %s\n", class.Name, class.Parents[0].Name)
							} else if class.Parent != nil {
								fmt.Printf("[DEBUG super] %s -> Parent = %s\n", class.Name, class.Parent.Name)
							} else {
								fmt.Printf("[DEBUG super] %s has NO parent!\n", class.Name)
							}
						}
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
	if args.Len() != 2 {
		return nil, fmt.Errorf("super requires 0 or 2 arguments")
	}

	// Get class
	classVal, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	class, ok := classVal.(*core.Class)
	if !ok {
		return nil, fmt.Errorf("first argument to super must be a class")
	}

	// Get instance or class (second argument)
	// In __new__, the second argument is a class (cls parameter)
	// In regular methods, it's an instance (self parameter)
	secondArgVal, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, err
	}

	// Check if it's an instance
	if instance, ok := secondArgVal.(*core.Instance); ok {
		return core.NewSuper(class, instance), nil
	}

	// Check if it's a class (for use in __new__ or classmethods)
	if secondClass, ok := secondArgVal.(*core.Class); ok {
		// Create a super proxy for class-level lookups
		// This is used in __new__ where cls is passed instead of self
		return core.NewSuperForClass(class, secondClass), nil
	}

	return nil, fmt.Errorf("second argument to super must be an instance or class, got %T", secondArgVal)
}

// isinstanceForm checks if an object is an instance of a class
func isinstanceForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, fmt.Errorf("isinstance requires 2 arguments")
	}

	// Evaluate object
	obj, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	// Evaluate class
	classVal, err := Eval(args.Items()[1], ctx)
	if err != nil {
		return nil, err
	}

	// Handle tuple of types - check if obj is instance of any type in the tuple
	if tuple, ok := classVal.(core.TupleValue); ok {
		for _, typeVal := range tuple {
			// Check each type in the tuple without recursion
			// We need to check if obj matches typeVal

			// Handle string type names
			if typeName, ok := typeVal.(core.StringValue); ok {
				actualType := string(obj.Type())
				expectedType := string(typeName)

				// Handle Python type name aliases
				match := false
				switch expectedType {
				case "int", "float":
					match = actualType == "number" || (expectedType == "int" && actualType == "bigint")
				case "str":
					match = actualType == "string"
				case "bool":
					match = actualType == "bool"
				case "NoneType":
					match = actualType == "nil"
				case "list":
					match = actualType == "list"
				case "dict":
					match = actualType == "dict"
				case "tuple":
					match = actualType == "tuple"
				case "set":
					match = actualType == "set"
				case "bytes":
					match = actualType == "bytes"
				default:
					match = actualType == expectedType
				}
				if match {
					return core.True, nil
				}
				continue
			}

			// Handle BuiltinFunction type constructors
			if builtinFunc, ok := typeVal.(*core.BuiltinFunction); ok {
				if nameVal, hasName := builtinFunc.GetAttr("__name__"); hasName {
					if nameStr, ok := nameVal.(core.StringValue); ok {
						typeName := string(nameStr)
						actualType := string(obj.Type())

						match := false
						switch typeName {
						case "list":
							match = actualType == "list"
						case "int", "float":
							match = actualType == "number" || (typeName == "int" && actualType == "bigint")
						case "str":
							match = actualType == "string"
						case "bool":
							match = actualType == "bool"
						case "tuple":
							match = actualType == "tuple"
						case "dict":
							match = actualType == "dict"
						case "set":
							match = actualType == "set"
						case "bytes":
							match = actualType == "bytes"
						}
						if match {
							return core.True, nil
						}
					}
				}
				continue
			}

			// Handle class types
			if class, ok := typeVal.(*core.Class); ok {
				// Check if obj is an instance of this class
				if instance, ok := obj.(*core.Instance); ok {
					if instance.Class == class || (instance.Class.Parent != nil && instance.Class.Parent == class) {
						return core.True, nil
					}
				}
				// Also handle primitive types that aren't Instances
				switch obj.(type) {
				case core.StringValue:
					if class.Name == "string" || class.Name == "str" {
						return core.True, nil
					}
				case core.NumberValue:
					if class.Name == "number" || class.Name == "int" || class.Name == "float" {
						return core.True, nil
					}
				case core.BigIntValue:
					if class.Name == "bigint" || class.Name == "int" {
						return core.True, nil
					}
				case core.BoolValue:
					if class.Name == "bool" {
						return core.True, nil
					}
				case core.NilValue:
					if class.Name == "nil" || class.Name == "NoneType" {
						return core.True, nil
					}
				case *core.ListValue:
					if class.Name == "list" {
						return core.True, nil
					}
				case *core.DictValue:
					if class.Name == "dict" {
						return core.True, nil
					}
				case core.TupleValue:
					if class.Name == "tuple" {
						return core.True, nil
					}
				case *core.SetValue:
					if class.Name == "set" {
						return core.True, nil
					}
				case core.BytesValue:
					if class.Name == "bytes" {
						return core.True, nil
					}
				}
				continue
			}

			// Handle wrapper types that have GetClass() method
			if wrapper, ok := typeVal.(interface{ GetClass() *core.Class }); ok {
				class := wrapper.GetClass()
				// Check if obj is an instance of this class
				if instance, ok := obj.(*core.Instance); ok {
					if instance.Class == class || (instance.Class.Parent != nil && instance.Class.Parent == class) {
						return core.True, nil
					}
				}
				// Also handle primitive types
				switch obj.(type) {
				case core.StringValue:
					if class.Name == "string" || class.Name == "str" {
						return core.True, nil
					}
				case core.NumberValue:
					if class.Name == "number" || class.Name == "int" || class.Name == "float" {
						return core.True, nil
					}
				case core.BigIntValue:
					if class.Name == "bigint" || class.Name == "int" {
						return core.True, nil
					}
				case core.BoolValue:
					if class.Name == "bool" {
						return core.True, nil
					}
				case core.NilValue:
					if class.Name == "nil" || class.Name == "NoneType" {
						return core.True, nil
					}
				case *core.ListValue:
					if class.Name == "list" {
						return core.True, nil
					}
				case *core.DictValue:
					if class.Name == "dict" {
						return core.True, nil
					}
				case core.TupleValue:
					if class.Name == "tuple" {
						return core.True, nil
					}
				case *core.SetValue:
					if class.Name == "set" {
						return core.True, nil
					}
				case core.BytesValue:
					if class.Name == "bytes" {
						return core.True, nil
					}
				}
				continue
			}
		}
		// If none matched, return false
		return core.False, nil
	}

	// Handle string type names
	if typeName, ok := classVal.(core.StringValue); ok {
		actualType := string(obj.Type())
		expectedType := string(typeName)

		// Handle Python type name aliases
		switch expectedType {
		case "int", "float":
			return core.BoolValue(actualType == "number" || (expectedType == "int" && actualType == "bigint")), nil
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

	// Handle BuiltinFunction type constructors (list, int, float, etc.)
	if builtinFunc, ok := classVal.(*core.BuiltinFunction); ok {
		// Check the function's name to determine what type to check
		if nameVal, hasName := builtinFunc.GetAttr("__name__"); hasName {
			if nameStr, ok := nameVal.(core.StringValue); ok {
				typeName := string(nameStr)
				actualType := string(obj.Type())

				// Map builtin function names to type checks
				switch typeName {
				case "list":
					return core.BoolValue(actualType == "list"), nil
				case "int", "float":
					return core.BoolValue(actualType == "number" || (typeName == "int" && actualType == "bigint")), nil
				case "str":
					return core.BoolValue(actualType == "string"), nil
				case "bool":
					return core.BoolValue(actualType == "bool"), nil
				case "tuple":
					return core.BoolValue(actualType == "tuple"), nil
				case "dict":
					return core.BoolValue(actualType == "dict"), nil
				case "set":
					return core.BoolValue(actualType == "set"), nil
				case "bytes":
					return core.BoolValue(actualType == "bytes"), nil
				}
			}
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
		return nil, fmt.Errorf("isinstance second argument must be a class or string type name, got %T: %v", classVal, classVal)
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
		case core.BigIntValue:
			// Check if class is the bigint/int class
			if class.Name == "bigint" || class.Name == "int" {
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
		case *core.ListValue:
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
func issubclassForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
	if args.Len() != 2 {
		return nil, fmt.Errorf("issubclass requires 2 arguments")
	}

	// Evaluate both arguments
	subClassVal, err := Eval(args.Items()[0], ctx)
	if err != nil {
		return nil, err
	}

	baseClassVal, err := Eval(args.Items()[1], ctx)
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

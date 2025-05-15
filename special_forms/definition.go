package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least a name and a body")
	}

	// Check if first arg is a list (the function signature)
	firstArg := args[0]

	// If it's a LocatedValue, unwrap it
	if located, isLocated := firstArg.(core.LocatedValue); isLocated {
		firstArg = located.Value
	}

	// Ensure we're defining a function
	funcDef, isFuncDef := firstArg.(core.LispList)

	if !isFuncDef || len(funcDef) == 0 {
		// This is NOT a function definition. Raise an error.
		return nil, fmt.Errorf("def can only be used for function definitions, use '=' for variable assignment")
	}

	// This is a function definition: (def (name param1 param2) body...)
	// Extract function name and parameters
	var funcName core.LispSymbol
	var params []core.LispSymbol
	var defaultValues map[core.LispSymbol]core.LispValue = make(map[core.LispSymbol]core.LispValue)

	nameVal := funcDef[0]

	// Unwrap if it's a LocatedValue
	if located, isLocated := nameVal.(core.LocatedValue); isLocated {
		nameVal = located.Value
	}

	// Check if it's a symbol
	nameSymbol, ok := nameVal.(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("function name must be a symbol, got %T", nameVal)
	}
	funcName = nameSymbol

	// Extract parameters
	for _, param := range funcDef[1:] {
		// Unwrap parameter if it's a LocatedValue
		if located, isLocated := param.(core.LocatedValue); isLocated {
			param = located.Value
		}

		// Handle symbol parameter
		if paramSymbol, ok := param.(core.LispSymbol); ok {
			params = append(params, paramSymbol)
		} else if paramStr, ok := param.(string); ok {
			params = append(params, core.LispSymbol(paramStr))
		} else {
			return nil, fmt.Errorf("function parameter must be a symbol, got %T", param)
		}
	}

	// Create function body from remaining arguments
	body := core.LispList(args[1:])

	// Create the Lambda function with a unique instance ID
	function := &core.Lambda{
		Params:        params,
		Body:          body,
		Env:           env,
		Closure:       env,
		DefaultValues: defaultValues,
		// Set SharedEnv to nil - it will be created in ApplyLambda
		SharedEnv: nil,
		// Assign a unique ID to this function instance
		InstanceID: getNextInstanceID(),
	}

	// Define the function in the current environment
	env.Define(funcName, function)
	return function, nil
}

func EvalClass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("class definition requires at least a name")
	}

	// Extract class name
	className, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	// Extract parent classes if specified
	var parentClasses []*core.PythonicClass
	var classBody []core.LispValue

	// Check if parent classes are specified
	if len(args) > 1 {
		if parentsList, ok := args[1].(core.LispList); ok {
			// Process parent classes
			for _, parent := range parentsList {
				parentName, ok := parent.(core.LispSymbol)
				if !ok {
					return nil, fmt.Errorf("parent class name must be a symbol")
				}

				// Look up parent class in environment
				parentValue, exists := env.Get(parentName)
				if !exists {
					return nil, fmt.Errorf("parent class '%s' not found", parentName)
				}

				parentClass, ok := parentValue.(*core.PythonicClass)
				if !ok {
					return nil, fmt.Errorf("'%s' is not a class", parentName)
				}

				parentClasses = append(parentClasses, parentClass)
			}

			// Class body starts after parents list
			if len(args) > 2 {
				classBody = args[2:]
			}
		} else {
			// No parent classes specified, body starts at index 1
			classBody = args[1:]
		}
	}

	// Create the new class
	newClass := core.NewPythonicClass(string(className), parentClasses)

	// Analyze class body to identify class attributes and methods
	for _, expr := range classBody {
		// If expression is a list, analyze further
		if exprList, ok := expr.(core.LispList); ok && len(exprList) >= 1 {
			// Check for class attribute assignments (= attr-name value)
			if firstSymbol, ok := exprList[0].(core.LispSymbol); ok {
				if firstSymbol == "=" && len(exprList) >= 3 {
					// Handle attribute assignment
					attrExpr := exprList[1]

					// Check if it's a regular attribute name (symbol)
					if attrName, ok := attrExpr.(core.LispSymbol); ok {
						// Store the class attribute
						attrValue, err := e.Eval(exprList[2], env)
						if err != nil {
							return nil, fmt.Errorf("error evaluating class attribute value: %v", err)
						}
						newClass.AddAttribute(string(attrName), attrValue)
						continue
					}

					// Check if it's a dot expression for initialization
					if dotList, ok := attrExpr.(core.LispList); ok && len(dotList) >= 2 {
						if dotSymbol, ok := dotList[0].(core.LispSymbol); ok && (dotSymbol == "dot" || dotSymbol == ".") {
							return nil, fmt.Errorf("dot notation in class attribute initialization not supported yet")
						}
					}
				}

				// Check for method definitions (def (method-name self ...) ...)
				if firstSymbol == "def" && len(exprList) >= 2 {
					if methodSig, ok := exprList[1].(core.LispList); ok && len(methodSig) >= 2 {
						methodName, ok := methodSig[0].(core.LispSymbol)
						if !ok {
							return nil, fmt.Errorf("method name must be a symbol")
						}

						// Skip the method name in parameters
						methodParams := make([]core.LispSymbol, 0, len(methodSig)-1)
						for i := 1; i < len(methodSig); i++ {
							if paramName, ok := methodSig[i].(core.LispSymbol); ok {
								methodParams = append(methodParams, paramName)
							} else {
								return nil, fmt.Errorf("method parameter must be a symbol")
							}
						}

						// Create method body
						methodBody := core.LispList(exprList[2:])

						// Process method body to support self.attr assignments
						processedBody := methodBody // No special processing needed with dot notation

						// Create the method Lambda
						method := &core.Lambda{
							Params:        methodParams,
							Body:          processedBody,
							Env:           env,
							Closure:       env,
							DefaultValues: make(map[core.LispSymbol]core.LispValue),
							SharedEnv:     nil,
							InstanceID:    getNextInstanceID(),
						}

						// Add the method to the class
						newClass.AddMethod(string(methodName), method)
						continue
					}
				}
			}
		}
	}

	// Create a constructor function for the class
	// This allows us to create instances with: (ClassName args...)
	constructorFunc := core.BuiltinFunc(func(args []core.LispValue, callEnv core.Environment) (core.LispValue, error) {
		// Retrieve evaluator from environment if available
		var evalCtx core.Evaluator
		if evalVal, exists := callEnv.Get("EVALUATOR"); exists {
			if eval, ok := evalVal.(core.Evaluator); ok {
				evalCtx = eval
			}
		}

		// Create a new instance with evaluator context
		instance := core.NewPythonicObject(newClass, evalCtx)

		// Call init method if it exists
		if initMethod, exists := newClass.GetMethod("init"); exists {
			// Use normal function application if we have evaluator
			if evalCtx != nil {
				// Prepare arguments with instance as first arg
				initArgs := make([]core.LispValue, len(args)+1)
				initArgs[0] = instance
				copy(initArgs[1:], args)

				// Call the init method with evaluator
				_, err := evalCtx.Apply(initMethod, initArgs, callEnv)
				if err != nil {
					return nil, fmt.Errorf("error initializing instance: %v", err)
				}
			} else {
				// Create a bound method manually
				boundInit := core.NewBoundMethod(initMethod, instance, nil)

				// Call the method
				_, err := boundInit.Apply(nil, args, callEnv)
				if err != nil {
					return nil, fmt.Errorf("error initializing instance: %v", err)
				}
			}
		}

		return instance, nil
	})

	// Class initialization complete

	// Register the class and its constructor in the environment
	env.Define(className, constructorFunc)
	env.Define(core.LispSymbol(string(className)+"_class"), newClass)

	// Define builtins for class operations if they don't exist yet
	if _, exists := env.Get("NewPythonicObject"); !exists {
		env.Define("NewPythonicObject", core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("NewPythonicObject requires a class argument")
			}
			class, ok := args[0].(*core.PythonicClass)
			if !ok {
				return nil, fmt.Errorf("first argument to NewPythonicObject must be a class")
			}

			// Get evaluator if available
			var evalCtx core.Evaluator
			if evalVal, exists := env.Get("EVALUATOR"); exists {
				if eval, ok := evalVal.(core.Evaluator); ok {
					evalCtx = eval
				}
			}

			return core.NewPythonicObject(class, evalCtx), nil
		}))
	}

	if _, exists := env.Get("hasattr"); !exists {
		env.Define("hasattr", core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
			if len(args) != 2 {
				return nil, fmt.Errorf("hasattr requires object and attribute name arguments")
			}

			// Get the object
			obj, ok := args[0].(*core.PythonicObject)
			if !ok {
				return core.PythonicBool(false), nil
			}

			// Get the attribute name
			attrName, ok := args[1].(string)
			if !ok {
				if sym, ok := args[1].(core.LispSymbol); ok {
					attrName = string(sym)
				} else {
					return nil, fmt.Errorf("attribute name must be a string or symbol")
				}
			}

			// Check if the attribute exists
			_, exists := obj.GetAttribute(attrName)
			return core.PythonicBool(exists), nil
		}))
	}

	if _, exists := env.Get("getattr"); !exists {
		env.Define("getattr", core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
			if len(args) < 2 {
				return nil, fmt.Errorf("getattr requires object and attribute name arguments")
			}

			// Get the object
			obj, ok := args[0].(*core.PythonicObject)
			if !ok {
				return nil, fmt.Errorf("first argument to getattr must be an object")
			}

			// Get the attribute name
			attrName, ok := args[1].(string)
			if !ok {
				if sym, ok := args[1].(core.LispSymbol); ok {
					attrName = string(sym)
				} else {
					return nil, fmt.Errorf("attribute name must be a string or symbol")
				}
			}

			// Get the attribute
			attr, exists := obj.GetAttribute(attrName)
			if !exists {
				if len(args) > 2 {
					// Return default value if provided
					return args[2], nil
				}
				return nil, fmt.Errorf("object has no attribute '%s'", attrName)
			}

			return attr, nil
		}))
	}

	if _, exists := env.Get("super"); !exists {
		env.Define("super", core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
			if len(args) < 1 {
				return nil, fmt.Errorf("super requires at least a current object argument")
			}

			// Get the current object
			obj, ok := args[0].(*core.PythonicObject)
			if !ok {
				return nil, fmt.Errorf("first argument to super must be an object")
			}

			// Create a super wrapper object that searches parent classes
			superObj := &core.SuperObject{
				Object: obj,
			}

			return superObj, nil
		}))
	}

	if _, exists := env.Get("concat"); !exists {
		env.Define("concat", core.BuiltinFunc(func(args []core.LispValue, env core.Environment) (core.LispValue, error) {
			var result core.LispList

			for _, arg := range args {
				switch v := arg.(type) {
				case core.LispList:
					result = append(result, v...)
				case core.LispListLiteral:
					result = append(result, core.LispList(v)...)
				case core.LispTuple:
					result = append(result, core.LispList(v)...)
				default:
					return nil, fmt.Errorf("concat expects list arguments, got %T", arg)
				}
			}

			return result, nil
		}))
	}

	return newClass, nil
}

func EvalLambdaPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("lambda requires at least parameters and a body")
	}

	params, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}

	paramSymbols := make([]core.LispSymbol, len(params))
	for i, param := range params {
		symbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols[i] = symbol
	}

	// If there are multiple body expressions, combine them into a list
	var body core.LispValue
	if len(args) > 2 {
		// Multiple expressions in the body
		body = core.LispList(args[1:])
	} else {
		// Single expression in the body - but wrap it in a list to ensure
		// it's evaluated as a complete expression
		body = core.LispList{args[1]}
	}

	// Create a new Lambda with a unique instance ID for proper closure behavior
	// Each lambda instance gets a unique ID to maintain separate state
	return &core.Lambda{
		Params:        paramSymbols,
		Body:          body,
		Env:           env,
		Closure:       env,
		DefaultValues: make(map[core.LispSymbol]core.LispValue),
		SharedEnv:     nil,                 // Will be initialized in ApplyLambda
		InstanceID:    getNextInstanceID(), // Get a unique ID for this lambda
	}, nil
}

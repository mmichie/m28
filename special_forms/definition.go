package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least a name and a body")
	}

	// Extract function name and parameters
	var funcName core.LispSymbol
	var params []core.LispSymbol
	var defaultValues map[core.LispSymbol]core.LispValue = make(map[core.LispSymbol]core.LispValue)

	// Check if first arg is a list (the function signature)
	funcDef, ok := args[0].(core.LispList)
	if ok && len(funcDef) > 0 {
		// (def (name param1 param2) body...)
		nameVal, ok := funcDef[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("function name must be a symbol")
		}
		funcName = nameVal

		// Extract parameters
		for _, param := range funcDef[1:] {
			// Handle symbol parameter
			if paramSymbol, ok := param.(core.LispSymbol); ok {
				params = append(params, paramSymbol)
			} else if paramStr, ok := param.(string); ok {
				params = append(params, core.LispSymbol(paramStr))
			} else {
				return nil, fmt.Errorf("function parameter must be a symbol, got %T", param)
			}
		}
	} else {
		// Check if first arg is a symbol (simple named function)
		nameVal, ok := args[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("function name must be a symbol")
		}
		funcName = nameVal
		// No parameters
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
	if len(args) < 2 {
		return nil, fmt.Errorf("class definition requires at least a name and a body")
	}

	// Extract class name
	className, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	// Extract class body expressions
	classBody := args[1:]

	// Analyze class body to identify instance variables and methods
	instanceVars := make(map[core.LispSymbol]core.LispValue)
	methods := []core.LispList{}

	for _, expr := range classBody {
		// If expression is a list, analyze further
		if exprList, ok := expr.(core.LispList); ok && len(exprList) >= 1 {
			// Check for instance variable assignments (= var-name value)
			if firstSymbol, ok := exprList[0].(core.LispSymbol); ok {
				if firstSymbol == "=" && len(exprList) >= 3 {
					if varName, ok := exprList[1].(core.LispSymbol); ok {
						// Store the variable name and default value
						defaultValue, err := e.Eval(exprList[2], env)
						if err != nil {
							return nil, fmt.Errorf("error evaluating instance variable default value: %v", err)
						}
						instanceVars[varName] = defaultValue
						continue
					}
				}

				// Check for method definitions (def (method-name self ...) ...)
				if firstSymbol == "def" && len(exprList) >= 2 {
					if methodSig, ok := exprList[1].(core.LispList); ok && len(methodSig) >= 1 {
						// Capture the method definition for later processing
						methods = append(methods, exprList)
						continue
					}
				}
			}
		}
	}

	// Generate a factory function that creates new instances
	// The factory function follows the pattern of a closure-based object creator
	// (def (ClassName arg1 arg2...)
	//   (= state1 val1)
	//   (= state2 val2)
	//   (def (method1 self arg1...) ...)
	//   (def (method2 self arg1...) ...)
	//   (= obj (dict))
	//   (= obj (dict "method1" method1 "method2" method2))
	//   obj)

	// First, determine constructor parameters
	var constructorParams []core.LispSymbol
	constructorFound := false

	for _, method := range methods {
		if len(method) >= 2 {
			if methodSig, ok := method[1].(core.LispList); ok && len(methodSig) >= 1 {
				methodName, _ := methodSig[0].(core.LispSymbol)
				if methodName == "init" || methodName == "__init__" {
					constructorFound = true
					// Skip method name and 'self'
					if len(methodSig) > 2 {
						constructorParams = make([]core.LispSymbol, 0, len(methodSig)-2)
						for i := 2; i < len(methodSig); i++ {
							if paramName, ok := methodSig[i].(core.LispSymbol); ok {
								constructorParams = append(constructorParams, paramName)
							}
						}
					}
					break
				}
			}
		}
	}

	// Create the factory function expression
	factoryFuncExpr := core.LispList{
		core.LispSymbol("def"),
		core.LispList{className}, // Function name
	}

	// Add constructor parameters
	for _, param := range constructorParams {
		factoryFuncExpr[1] = append(factoryFuncExpr[1].(core.LispList), param)
	}

	// Add instance variable definitions with default values
	for varName, defaultValue := range instanceVars {
		varAssign := core.LispList{
			core.LispSymbol("="),
			varName,
			defaultValue,
		}
		factoryFuncExpr = append(factoryFuncExpr, varAssign)
	}

	// Process constructor
	if constructorFound {
		for _, method := range methods {
			if len(method) >= 2 {
				if methodSig, ok := method[1].(core.LispList); ok && len(methodSig) >= 1 {
					methodName, _ := methodSig[0].(core.LispSymbol)
					if methodName == "init" || methodName == "__init__" {
						// Inject constructor body directly (skip the def and signature)
						for i := 2; i < len(method); i++ {
							factoryFuncExpr = append(factoryFuncExpr, method[i])
						}
						break
					}
				}
			}
		}
	}

	// Add method definitions
	for _, method := range methods {
		if len(method) >= 2 {
			if methodSig, ok := method[1].(core.LispList); ok && len(methodSig) >= 1 {
				methodName, _ := methodSig[0].(core.LispSymbol)
				// Skip constructor (already processed)
				if methodName == "init" || methodName == "__init__" {
					continue
				}

				// Add the method definition directly
				factoryFuncExpr = append(factoryFuncExpr, method)
			}
		}
	}

	// Create the return object dictionary
	factoryFuncExpr = append(factoryFuncExpr,
		core.LispList{
			core.LispSymbol("="),
			core.LispSymbol("obj"),
			core.LispList{core.LispSymbol("dict")},
		},
	)

	// Add methods to the return dictionary
	methodAssignments := core.LispList{
		core.LispSymbol("="),
		core.LispSymbol("obj"),
		core.LispList{core.LispSymbol("dict")},
	}

	// Add each method to the dict literal
	for _, method := range methods {
		if len(method) >= 2 {
			if methodSig, ok := method[1].(core.LispList); ok && len(methodSig) >= 1 {
				methodName, _ := methodSig[0].(core.LispSymbol)
				// Skip constructor in return dictionary
				if methodName == "init" || methodName == "__init__" {
					continue
				}

				// Add method to the dict literal
				methodAssignments = append(methodAssignments, string(methodName))
				methodAssignments = append(methodAssignments, methodName)
			}
		}
	}

	factoryFuncExpr = append(factoryFuncExpr, methodAssignments)

	// Return the object
	factoryFuncExpr = append(factoryFuncExpr, core.LispSymbol("obj"))

	// Evaluate the factory function expression
	result, err := e.Eval(factoryFuncExpr, env)
	if err != nil {
		return nil, fmt.Errorf("error defining class: %v", err)
	}

	return result, nil
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

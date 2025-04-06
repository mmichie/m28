package special_forms

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

func EvalDef(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("def requires at least a name and a body")
	}

	funcDef, ok := args[0].(core.LispList)
	if !ok || len(funcDef) == 0 {
		return nil, fmt.Errorf("invalid function definition")
	}

	funcName, ok := funcDef[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("function name must be a symbol")
	}

	// Parse params and default values
	params := []core.LispSymbol{}
	defaultValues := map[core.LispSymbol]core.LispValue{}

	fmt.Printf("DEBUG: Function definition params: %v\n", funcDef[1:])

	for _, param := range funcDef[1:] {
		fmt.Printf("DEBUG: Processing parameter: %v (type %T)\n", param, param)
		
		// Check for string parameter (which could be a keyword argument)
		if paramStr, isString := param.(string); isString {
			fmt.Printf("DEBUG: Found string parameter: %s\n", paramStr)
			
			// Check if it has an equals sign for default value
			if equalPos := strings.Index(paramStr, "="); equalPos > 0 {
				name := core.LispSymbol(paramStr[:equalPos])
				defaultValueStr := paramStr[equalPos+1:]
				
				fmt.Printf("DEBUG: Found default param %s = %s\n", name, defaultValueStr)
				
				// Parse the default value
				var val core.LispValue
				if defaultValueStr == "True" {
					val = core.PythonicBool(true)
				} else if defaultValueStr == "False" {
					val = core.PythonicBool(false)
				} else if defaultValueStr == "None" {
					val = core.PythonicNone{}
				} else if num, err := strconv.ParseFloat(defaultValueStr, 64); err == nil {
					val = num
				} else {
					// Keep as string for other values
					val = defaultValueStr
				}
				
				params = append(params, name)
				defaultValues[name] = val
				continue
			}
			
			// Regular parameter as string
			params = append(params, core.LispSymbol(paramStr))
			continue
		}
		
		// Handle core.LispSymbol parameter
		if paramSymbol, isSymbol := param.(core.LispSymbol); isSymbol {
			fmt.Printf("DEBUG: Found symbol parameter: %s\n", paramSymbol)
			
			// Check if symbol contains an equals sign
			symbolStr := string(paramSymbol)
			if equalPos := strings.Index(symbolStr, "="); equalPos > 0 {
				name := core.LispSymbol(symbolStr[:equalPos])
				defaultValueStr := symbolStr[equalPos+1:]
				
				fmt.Printf("DEBUG: Found default param from symbol %s = %s\n", name, defaultValueStr)
				
				// Parse the default value
				var val core.LispValue
				if defaultValueStr == "True" {
					val = core.PythonicBool(true)
				} else if defaultValueStr == "False" {
					val = core.PythonicBool(false)
				} else if defaultValueStr == "None" {
					val = core.PythonicNone{}
				} else if num, err := strconv.ParseFloat(defaultValueStr, 64); err == nil {
					val = num
				} else {
					val = defaultValueStr
				}
				
				params = append(params, name)
				defaultValues[name] = val
				continue
			}
			
			// Regular parameter as symbol
			params = append(params, paramSymbol)
			continue
		}
		
		// If we got here, it's not a valid parameter type
		return nil, fmt.Errorf("function parameter must be a symbol or string, got %T", param)
	}

	fmt.Printf("DEBUG: Parsed params: %v\n", params)
	fmt.Printf("DEBUG: Default values: %v\n", defaultValues)

	body := core.LispList(args[1:])

	function := &core.Lambda{
		Params:        params, 
		Body:          body, 
		Env:           env, 
		Closure:       env,
		DefaultValues: defaultValues,
	}
	env.Define(funcName, function)
	return function, nil
}

func EvalClass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("class definition requires at least a name and a body")
	}
	className, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("class name must be a symbol")
	}

	classEnv := env.NewEnvironment(env)
	for _, expr := range args[1:] {
		_, err := e.Eval(expr, classEnv)
		if err != nil {
			return nil, err
		}
	}

	class := &core.Lambda{Params: []core.LispSymbol{}, Body: core.LispList(args[1:]), Env: classEnv}
	env.Define(className, class)
	return class, nil
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

	return &core.Lambda{Params: paramSymbols, Body: args[1], Env: env}, nil
}

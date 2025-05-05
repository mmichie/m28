package special_forms

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

func ApplyLambda(e core.Evaluator, lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Create a new environment for the lambda execution
	// Use the lambda's closure as the outer environment to support lexical closures
	lambdaEnv := env.NewEnvironment(lambda.Closure)
	
	// Manually register special forms in the environment
	// This ensures lambda bodies can recognize forms like "if"
	registerSpecialFormsIn(lambdaEnv)
	
	// Register built-in functions in the lambda environment
	// This is needed because operators like + and - are registered as built-ins
	registerBuiltinsIn(lambdaEnv)
	
	// Bind parameters to arguments
	if err := bindParams(lambda, args, lambdaEnv); err != nil {
		return nil, err
	}

	// Evaluate the lambda body in the newly created environment
	result, err := evalLambdaBody(e, lambda, lambdaEnv)
	if err != nil {
		return nil, err
	}

	// Return the result directly
	return result, nil
}

// registerSpecialFormsIn registers all special forms in the environment
func registerSpecialFormsIn(env core.Environment) {
	// Get all builtin special forms and register them
	specialForms := GetSpecialForms()
	for name := range specialForms {
		env.Define(name, core.SpecialFormMarker{Name: name})
	}
}

// registerBuiltinsIn registers all builtin functions in the environment
func registerBuiltinsIn(env core.Environment) {
	// Register all builtin functions (including arithmetic operators)
	for name, fn := range core.BuiltinFuncs {
		env.Define(name, fn)
	}
}

func evalLambdaBody(e core.Evaluator, lambda *core.Lambda, env core.Environment) (core.LispValue, error) {
	// If the lambda body is a list, evaluate each expression in the list
	// and return the result of the last expression
	if list, ok := lambda.Body.(core.LispList); ok {
		var result core.LispValue = core.PythonicNone{}
		var err error
		
		// For a single expression that's a list (like (+ a b)), evaluate it as one unit
		if len(list) == 1 {
			if exprList, isList := list[0].(core.LispList); isList {
				result, err = e.Eval(exprList, env)
				if err != nil {
					// Check if it's a return signal
					if returnSig, ok := err.(ReturnSignal); ok {
						return returnSig.Value, nil
					}
					return nil, err
				}
				return result, nil
			}
		}
		
		// Multiple expressions (e.g., from multiline lambda), evaluate each in sequence
		for _, expr := range list {
			result, err = e.Eval(expr, env)
			if err != nil {
				// Check if it's a return signal
				if returnSig, ok := err.(ReturnSignal); ok {
					return returnSig.Value, nil
				}
				
				fmt.Printf("Error evaluating expression in lambda body: %v\n", err)
				return nil, err
			}
		}
		return result, nil
	}
	
	// If body is a single expression, just evaluate it
	result, err := e.Eval(lambda.Body, env)
	if err != nil {
		// Check if it's a return signal
		if returnSig, ok := err.(ReturnSignal); ok {
			return returnSig.Value, nil
		}
		return nil, err
	}
	return result, nil
}

func bindParams(lambda *core.Lambda, args []core.LispValue, env core.Environment) error {
	// Extract any keyword arguments first
	positionalArgs := []core.LispValue{}
	keywordArgs := make(map[core.LispSymbol]core.LispValue)

	for i := 0; i < len(args); i++ {
		// Check if this is a keyword argument (e.g., "name=value")
		if str, ok := args[i].(string); ok && strings.Contains(str, "=") {
			parts := strings.SplitN(str, "=", 2)
			if len(parts) == 2 {
				paramName := core.LispSymbol(parts[0])

				// Parse the value
				var val core.LispValue
				if parts[1] == "True" {
					val = core.PythonicBool(true)
				} else if parts[1] == "False" {
					val = core.PythonicBool(false)
				} else if parts[1] == "None" {
					val = core.PythonicNone{}
				} else if num, err := strconv.ParseFloat(parts[1], 64); err == nil {
					val = num
				} else {
					val = parts[1]
				}

				keywordArgs[paramName] = val
				continue
			}
		}

		// If not a keyword arg, it's a positional arg
		positionalArgs = append(positionalArgs, args[i])
	}

	// Now handle positional arguments
	if len(positionalArgs) > len(lambda.Params) {
		return fmt.Errorf("lambda expected at most %d positional arguments, got %d", len(lambda.Params), len(positionalArgs))
	}

	// Set provided positional arguments
	for i, arg := range positionalArgs {
		env.Define(lambda.Params[i], arg)
	}

	// Set values from keyword arguments and defaults for remaining params
	for i := len(positionalArgs); i < len(lambda.Params); i++ {
		param := lambda.Params[i]

		// Check if we have a keyword argument for this param
		if val, hasKeyword := keywordArgs[param]; hasKeyword {
			env.Define(param, val)
			delete(keywordArgs, param) // Mark as used
		} else if defaultVal, hasDefault := lambda.DefaultValues[param]; hasDefault {
			// Use default value
			env.Define(param, defaultVal)
		} else {
			// No positional, keyword, or default value
			return fmt.Errorf("missing required argument: %s", param)
		}
	}

	// Check if there are unused keyword arguments
	if len(keywordArgs) > 0 {
		var unusedKeys []string
		for k := range keywordArgs {
			unusedKeys = append(unusedKeys, string(k))
		}
		return fmt.Errorf("unexpected keyword arguments: %v", unusedKeys)
	}

	return nil
}

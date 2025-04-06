package special_forms

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

func ApplyLambda(e core.Evaluator, lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	lambdaEnv := env.NewEnvironment(lambda.Closure)

	if err := bindParams(lambda, args, lambdaEnv); err != nil {
		return nil, err
	}

	result, err := evalLambdaBody(e, lambda, lambdaEnv)
	if err != nil {
		return nil, err
	}

	// If the result is a literal value (like a number), return it directly
	switch result.(type) {
	case float64, int, string, core.PythonicBool, core.PythonicNone:
		return result, nil
	}

	// Otherwise, evaluate the result
	return e.Eval(result, lambdaEnv)
}

func evalLambdaBody(e core.Evaluator, lambda *core.Lambda, env core.Environment) (core.LispValue, error) {
	if list, ok := lambda.Body.(core.LispList); ok {
		var result core.LispValue
		var err error
		for _, expr := range list {
			result, err = e.Eval(expr, env)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}
	return e.Eval(lambda.Body, env)
}

func bindParams(lambda *core.Lambda, args []core.LispValue, env core.Environment) error {
	fmt.Printf("DEBUG: Lambda params: %v\n", lambda.Params)
	fmt.Printf("DEBUG: Lambda default values: %v\n", lambda.DefaultValues)
	fmt.Printf("DEBUG: Args received: %v\n", args)
	
	// Extract any keyword arguments first
	positionalArgs := []core.LispValue{}
	keywordArgs := make(map[core.LispSymbol]core.LispValue)
	
	for i := 0; i < len(args); i++ {
		fmt.Printf("DEBUG: Examining arg[%d]: %v (%T)\n", i, args[i], args[i])
		
		// Check if this is a keyword argument (e.g., "name=value")
		if str, ok := args[i].(string); ok && strings.Contains(str, "=") {
			fmt.Printf("DEBUG: Found potential keyword arg: %s\n", str)
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
				fmt.Printf("DEBUG: Added keyword arg %s = %v\n", paramName, val)
				continue
			}
		}
		
		// If not a keyword arg, it's a positional arg
		positionalArgs = append(positionalArgs, args[i])
		fmt.Printf("DEBUG: Added positional arg: %v\n", args[i])
	}
	
	fmt.Printf("DEBUG: Positional args: %v\n", positionalArgs)
	fmt.Printf("DEBUG: Keyword args: %v\n", keywordArgs)
	
	// Now handle positional arguments
	if len(positionalArgs) > len(lambda.Params) {
		return fmt.Errorf("lambda expected at most %d positional arguments, got %d", len(lambda.Params), len(positionalArgs))
	}
	
	// Set provided positional arguments
	for i, arg := range positionalArgs {
		env.Define(lambda.Params[i], arg)
		fmt.Printf("DEBUG: Set positional param %s = %v\n", lambda.Params[i], arg)
	}
	
	// Set values from keyword arguments and defaults for remaining params
	for i := len(positionalArgs); i < len(lambda.Params); i++ {
		param := lambda.Params[i]
		fmt.Printf("DEBUG: Processing param %s\n", param)
		
		// Check if we have a keyword argument for this param
		if val, hasKeyword := keywordArgs[param]; hasKeyword {
			env.Define(param, val)
			delete(keywordArgs, param)  // Mark as used
			fmt.Printf("DEBUG: Set keyword param %s = %v\n", param, val)
		} else if defaultVal, hasDefault := lambda.DefaultValues[param]; hasDefault {
			// Use default value
			env.Define(param, defaultVal)
			fmt.Printf("DEBUG: Set default value param %s = %v\n", param, defaultVal)
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

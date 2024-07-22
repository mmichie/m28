package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func ApplyLambda(e core.Evaluator, lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	lambdaEnv := env.NewEnvironment(lambda.Closure)

	// Bind required parameters
	for i, param := range lambda.Params {
		if i < len(args) {
			lambdaEnv.Define(param, args[i])
		} else {
			return nil, fmt.Errorf("not enough arguments: expected at least %d, got %d", len(lambda.Params), len(args))
		}
	}

	// Bind optional parameters
	optionalIndex := len(lambda.Params)
	for i, opt := range lambda.Optional {
		if optionalIndex+i < len(args) {
			lambdaEnv.Define(opt.Name, args[optionalIndex+i])
		} else if opt.DefaultValue != nil {
			defaultValue, err := e.Eval(opt.DefaultValue, lambdaEnv)
			if err != nil {
				return nil, err
			}
			lambdaEnv.Define(opt.Name, defaultValue)
		} else {
			lambdaEnv.Define(opt.Name, nil)
		}
	}

	// Bind key parameters
	for key, defaultValue := range lambda.KeyParams {
		found := false
		for i := len(lambda.Params) + len(lambda.Optional); i < len(args); i += 2 {
			if i+1 < len(args) && args[i] == core.LispSymbol(":"+string(key)) {
				lambdaEnv.Define(key, args[i+1])
				found = true
				break
			}
		}
		if !found {
			if defaultValue != nil {
				value, err := e.Eval(defaultValue, lambdaEnv)
				if err != nil {
					return nil, err
				}
				lambdaEnv.Define(key, value)
			} else {
				lambdaEnv.Define(key, nil)
			}
		}
	}

	// Process lambda body
	bodyList, ok := lambda.Body.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("lambda body must be a list")
	}

	result, err := EvalProgn(e, bodyList, lambdaEnv)

	return result, err
}

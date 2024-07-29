package special_forms

import (
	"fmt"

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
	if len(args) != len(lambda.Params) {
		return fmt.Errorf("lambda expected %d arguments, got %d", len(lambda.Params), len(args))
	}

	for i, param := range lambda.Params {
		env.Define(param, args[i])
	}

	return nil
}

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

	return evalLambdaBody(e, lambda, lambdaEnv)
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

func evalLambdaBody(e core.Evaluator, lambda *core.Lambda, env core.Environment) (core.LispValue, error) {
	return e.Eval(lambda.Body, env)
}

// EvalLambda is now defined in special_forms/special_forms.go as EvalLambdaPython
// You can remove it from here if it's not needed anymore

// Helper function for parsing parameters is no longer needed for Python-like lambdas
// You can remove the parseParameters function if it's not used elsewhere

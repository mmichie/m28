package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func ApplyLambda(e core.Evaluator, lambda *core.Lambda, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	lambdaEnv := env.NewEnvironment(lambda.Closure)

	if err := bindRequiredParams(lambda, args, lambdaEnv); err != nil {
		return nil, err
	}

	if err := bindOptionalParams(e, lambda, args, lambdaEnv); err != nil {
		return nil, err
	}

	bindRestParam(lambda, args, lambdaEnv)

	if err := bindKeyParams(e, lambda, args, lambdaEnv); err != nil {
		return nil, err
	}

	return evalLambdaBody(e, lambda, lambdaEnv)
}

func bindRequiredParams(lambda *core.Lambda, args []core.LispValue, env core.Environment) error {
	if len(args) < len(lambda.Params) {
		return fmt.Errorf("not enough arguments: expected at least %d, got %d", len(lambda.Params), len(args))
	}
	for i, param := range lambda.Params {
		env.Define(param, args[i])
	}
	return nil
}

func bindOptionalParams(e core.Evaluator, lambda *core.Lambda, args []core.LispValue, env core.Environment) error {
	optionalIndex := len(lambda.Params)
	for i, opt := range lambda.Optional {
		if optionalIndex+i < len(args) {
			env.Define(opt.Name, args[optionalIndex+i])
		} else if opt.DefaultValue != nil {
			defaultValue, err := e.Eval(opt.DefaultValue, env)
			if err != nil {
				return err
			}
			env.Define(opt.Name, defaultValue)
		} else {
			env.Define(opt.Name, nil)
		}
	}
	return nil
}

func bindRestParam(lambda *core.Lambda, args []core.LispValue, env core.Environment) {
	if lambda.Rest != "" {
		restArgs := args[len(lambda.Params)+len(lambda.Optional):]
		env.Define(lambda.Rest, core.LispList(restArgs))
	}
}

func bindKeyParams(e core.Evaluator, lambda *core.Lambda, args []core.LispValue, env core.Environment) error {
	for key, defaultValue := range lambda.KeyParams {
		if value, found := findKeyParam(key, args[len(lambda.Params)+len(lambda.Optional):]); found {
			env.Define(key, value)
		} else if defaultValue != nil {
			value, err := e.Eval(defaultValue, env)
			if err != nil {
				return err
			}
			env.Define(key, value)
		} else {
			env.Define(key, nil)
		}
	}
	return nil
}

func findKeyParam(key core.LispSymbol, args []core.LispValue) (core.LispValue, bool) {
	for i := 0; i < len(args); i += 2 {
		if i+1 < len(args) && args[i] == core.LispSymbol(":"+string(key)) {
			return args[i+1], true
		}
	}
	return nil, false
}

func evalLambdaBody(e core.Evaluator, lambda *core.Lambda, env core.Environment) (core.LispValue, error) {
	bodyList, ok := lambda.Body.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("lambda body must be a list")
	}
	return EvalProgn(e, bodyList, env)
}

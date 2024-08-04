package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalRaise(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("raise requires exactly one argument")
	}

	exception, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	return nil, fmt.Errorf("%v", exception)
}

func EvalAssert(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("assert takes 1 or 2 arguments")
	}

	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if !core.IsTruthy(condition) {
		message := "Assertion failed"
		if len(args) == 2 {
			messageVal, err := e.Eval(args[1], env)
			if err != nil {
				return nil, err
			}
			message = fmt.Sprintf("%v", messageVal)
		}
		return nil, fmt.Errorf(message)
	}

	return nil, nil
}

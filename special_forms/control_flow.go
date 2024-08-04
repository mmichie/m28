package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalIfPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("if requires 2 or 3 arguments")
	}

	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if core.IsTruthy(condition) {
		return e.Eval(args[1], env)
	} else if len(args) == 3 {
		return e.Eval(args[2], env)
	}

	return core.PythonicNone{}, nil
}

func EvalFor(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("for loop requires at least 3 arguments")
	}

	iterVar, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("iteration variable must be a symbol")
	}

	iterable, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	iter, ok := iterable.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("for loop requires a list")
	}

	loopEnv := env.NewEnvironment(env)
	var result core.LispValue
	for _, item := range iter {
		loopEnv.Set(iterVar, item)
		for _, expr := range args[2:] {
			result, err = e.Eval(expr, loopEnv)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalWhilePython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("while loop requires at least 2 arguments")
	}

	var result core.LispValue

	for {
		condition, err := e.Eval(args[0], env)
		if err != nil {
			return nil, err
		}

		if !core.IsTruthy(condition) {
			break
		}

		for _, expr := range args[1:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				return nil, err
			}
		}
	}

	return result, nil
}

func EvalTry(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("try requires at least a try block and an except block")
	}

	tryBlock := args[0]
	exceptBlocks := args[1:]

	result, err := e.Eval(tryBlock, env)
	if err == nil {
		return result, nil
	}

	for _, exceptBlock := range exceptBlocks {
		exceptClause, ok := exceptBlock.(core.LispList)
		if !ok || len(exceptClause) < 2 {
			continue
		}

		exceptionType, ok := exceptClause[0].(core.LispSymbol)
		if !ok {
			continue
		}

		if string(exceptionType) == "except" {
			return e.Eval(exceptClause[1], env)
		}
	}

	return nil, err
}

func EvalBreak(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, fmt.Errorf("break encountered")
}

func EvalContinue(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, fmt.Errorf("continue encountered")
}

func EvalPass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, nil
}

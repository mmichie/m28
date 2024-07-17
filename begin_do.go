package m28

import (
	"fmt"
)

func evalBegin(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("'begin' expects at least one form")
	}
	var result LispValue
	var err error
	for _, form := range args {
		result, err = EvalExpression(form, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalDo(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'do' expects at least two arguments")
	}
	return evalDoLoop(args[0], args[1], args[2:], env)
}

func evalDoLoop(bindings, test LispValue, body []LispValue, env *Environment) (LispValue, error) {
	localEnv, err := setupDoBindings(bindings, env)
	if err != nil {
		return nil, err
	}

	for {
		endResult, shouldEnd, err := evalDoTest(test, localEnv)
		if err != nil {
			return nil, err
		}
		if shouldEnd {
			return endResult, nil
		}

		err = evalDoBody(body, localEnv)
		if err != nil {
			return nil, err
		}

		err = updateDoBindings(bindings, localEnv)
		if err != nil {
			return nil, err
		}
	}
}

func setupDoBindings(bindings LispValue, env *Environment) (*Environment, error) {
	bindingList, ok := bindings.(LispList)
	if !ok {
		return nil, fmt.Errorf("invalid bindings in 'do'")
	}

	localEnv := NewEnvironment(env)
	for _, binding := range bindingList {
		bindingVars, ok := binding.(LispList)
		if !ok || len(bindingVars) < 2 {
			return nil, fmt.Errorf("invalid binding in 'do'")
		}
		symbol, ok := bindingVars[0].(LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding variable must be a symbol")
		}
		initValue, err := EvalExpression(bindingVars[1], localEnv)
		if err != nil {
			return nil, err
		}
		localEnv.Define(symbol, initValue)
	}
	return localEnv, nil
}

func evalDoTest(test LispValue, env *Environment) (LispValue, bool, error) {
	testList, ok := test.(LispList)
	if !ok || len(testList) < 1 {
		return nil, false, fmt.Errorf("invalid end test in 'do'")
	}

	endResult, err := EvalExpression(testList[0], env)
	if err != nil {
		return nil, false, err
	}

	if IsTruthy(endResult) {
		if len(testList) > 1 {
			var result LispValue
			for _, resultForm := range testList[1:] {
				result, err = EvalExpression(resultForm, env)
				if err != nil {
					return nil, false, err
				}
			}
			return result, true, nil
		}
		return nil, true, nil
	}

	return nil, false, nil
}

func evalDoBody(body []LispValue, env *Environment) error {
	for _, expr := range body {
		_, err := EvalExpression(expr, env)
		if err != nil {
			return err
		}
	}
	return nil
}

func updateDoBindings(bindings LispValue, env *Environment) error {
	bindingList, ok := bindings.(LispList)
	if !ok {
		return fmt.Errorf("invalid bindings in 'do'")
	}

	for _, binding := range bindingList {
		bindingVars, ok := binding.(LispList)
		if !ok || len(bindingVars) < 2 {
			return fmt.Errorf("invalid binding in 'do'")
		}
		symbol, ok := bindingVars[0].(LispSymbol)
		if !ok {
			return fmt.Errorf("binding variable must be a symbol")
		}
		if len(bindingVars) > 2 {
			newValue, err := EvalExpression(bindingVars[2], env)
			if err != nil {
				return err
			}
			env.Set(symbol, newValue)
		}
	}
	return nil
}

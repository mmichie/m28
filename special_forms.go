package m28

import (
	"fmt"
)

type SpecialFormFunc func([]LispValue, *Environment) (LispValue, error)

var specialForms map[LispSymbol]SpecialFormFunc

func init() {
	specialForms = map[LispSymbol]SpecialFormFunc{
		"quote":  evalQuote,
		"if":     evalIf,
		"define": evalDefine,
		"lambda": evalLambda,
		"begin":  evalBegin,
		"do":     evalDo,
		"let":    evalLet,
		"set!":   evalSet,
		"cond":   evalCond,
		"case":   evalCase,
		"and":    evalAnd,
	}
}

func evalQuote(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("'quote' expects exactly one argument")
	}
	return args[0], nil
}

func evalIf(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("'if' expects exactly three arguments")
	}
	condition, err := EvalExpression(args[0], env)
	if err != nil {
		return nil, err
	}
	if IsTruthy(condition) {
		return EvalExpression(args[1], env)
	}
	return EvalExpression(args[2], env)
}

func evalDefine(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("'define' expects exactly two arguments")
	}
	symbol, ok := args[0].(LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to 'define' must be a symbol")
	}
	value, err := EvalExpression(args[1], env)
	if err != nil {
		return nil, err
	}
	env.Define(symbol, value)
	return symbol, nil
}

func evalLambda(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'lambda' expects at least two arguments")
	}
	params, ok := args[0].(LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}
	var paramSymbols []LispSymbol
	var restParam LispSymbol
	for i, param := range params {
		if i == len(params)-2 && param == LispSymbol(".") {
			restParam, ok = params[i+1].(LispSymbol)
			if !ok {
				return nil, fmt.Errorf("rest parameter must be a symbol")
			}
			break
		}
		symbol, ok := param.(LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols = append(paramSymbols, symbol)
	}
	return &Lambda{
		Params:    paramSymbols,
		RestParam: restParam,
		Body:      LispList(append([]LispValue{LispSymbol("begin")}, args[1:]...)),
		Env:       env,
		Closure:   NewEnvironment(env),
	}, nil
}

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

func evalLet(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'let' expects at least two arguments")
	}
	bindings, ok := args[0].(LispList)
	if !ok {
		return nil, fmt.Errorf("'let' bindings must be a list")
	}
	letEnv := NewEnvironment(env)
	for _, binding := range bindings {
		bindingList, ok := binding.(LispList)
		if !ok || len(bindingList) != 2 {
			return nil, fmt.Errorf("invalid binding in 'let'")
		}
		symbol, ok := bindingList[0].(LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding name must be a symbol")
		}
		value, err := EvalExpression(bindingList[1], env)
		if err != nil {
			return nil, err
		}
		letEnv.Define(symbol, value)
	}
	return evalBegin(args[1:], letEnv)
}

func evalSet(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("'set!' expects exactly two arguments")
	}
	symbol, ok := args[0].(LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to 'set!' must be a symbol")
	}
	value, err := EvalExpression(args[1], env)
	if err != nil {
		return nil, err
	}
	if env.SetMutable(symbol, value) {
		return value, nil
	}
	return nil, fmt.Errorf("cannot set! undefined variable: %s", symbol)
}

func evalCond(args []LispValue, env *Environment) (LispValue, error) {
	for _, clause := range args {
		clauseList, ok := clause.(LispList)
		if !ok || len(clauseList) < 2 {
			return nil, fmt.Errorf("invalid cond clause")
		}
		if clauseList[0] == LispSymbol("else") {
			return evalBegin(clauseList[1:], env)
		}
		condition, err := EvalExpression(clauseList[0], env)
		if err != nil {
			return nil, err
		}
		if IsTruthy(condition) {
			return evalBegin(clauseList[1:], env)
		}
	}
	return nil, nil // No clause was true
}

func evalCase(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'case' expects at least two arguments")
	}
	key, err := EvalExpression(args[0], env)
	if err != nil {
		return nil, err
	}
	for _, clause := range args[1:] {
		clauseList, ok := clause.(LispList)
		if !ok || len(clauseList) < 2 {
			return nil, fmt.Errorf("invalid case clause")
		}
		datums := clauseList[0]
		if datumList, ok := datums.(LispList); ok {
			for _, datum := range datumList {
				if EqualValues(key, datum) {
					return evalBegin(clauseList[1:], env)
				}
			}
		} else if EqualValues(key, datums) || datums == LispSymbol("else") {
			return evalBegin(clauseList[1:], env)
		}
	}
	return nil, nil
}

func evalAnd(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) == 0 {
		return true, nil
	}
	var result LispValue = true
	for _, arg := range args {
		result, err := EvalExpression(arg, env)
		if err != nil {
			return nil, err
		}
		if !IsTruthy(result) {
			return false, nil
		}
	}
	return result, nil
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

package m28

import "fmt"

// EvalExpression evaluates a LispValue in the given environment
func EvalExpression(expr LispValue, env *Environment) (LispValue, error) {
	switch e := expr.(type) {
	case LispSymbol:
		value, ok := env.Get(e)
		if !ok {
			return nil, fmt.Errorf("undefined symbol: %s", e)
		}
		return value, nil
	case float64, string:
		return e, nil
	case LispList:
		if len(e) == 0 {
			return nil, fmt.Errorf("empty list")
		}
		first := e[0]

		// Handle special forms
		if symbol, ok := first.(LispSymbol); ok {
			switch symbol {
			case "quote":
				return evalQuote(e)
			case "if":
				return evalIf(e, env)
			case "define":
				return evalDefine(e, env)
			case "lambda":
				return evalLambda(e, env)
			case "begin":
				return evalBegin(e, env)
			case "do":
				return evalDo(e, env)
			case "let":
				return evalLet(e, env)
			case "set!":
				return evalSet(e, env)
			case "cond":
				return evalCond(e, env)
			}
		}

		// Function application
		fn, err := EvalExpression(first, env)
		if err != nil {
			return nil, err
		}

		args, err := evalArgs(e[1:], env)
		if err != nil {
			return nil, err
		}

		return apply(fn, args, env)
	default:
		return nil, fmt.Errorf("unknown expression type: %T", e)
	}
}

func evalCond(list LispList, env *Environment) (LispValue, error) {
	for _, clause := range list[1:] {
		clauseList, ok := clause.(LispList)
		if !ok || len(clauseList) < 2 {
			return nil, fmt.Errorf("invalid cond clause")
		}

		condition, err := EvalExpression(clauseList[0], env)
		if err != nil {
			return nil, err
		}

		if IsTruthy(condition) {
			return evalBegin(LispList(append([]LispValue{LispSymbol("begin")}, clauseList[1:]...)), env)
		}
	}

	return nil, nil // No clause was true
}

func evalDo(list LispList, env *Environment) (LispValue, error) {
	if len(list) < 3 {
		return nil, fmt.Errorf("'do' expects at least three arguments")
	}

	// Parse variable bindings
	bindings, ok := list[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("first argument to 'do' must be a list of bindings")
	}

	// Create a new environment for the do loop
	localEnv := NewEnvironment(env)

	// Initialize variables
	for _, binding := range bindings {
		bindingList, ok := binding.(LispList)
		if !ok || len(bindingList) < 2 {
			return nil, fmt.Errorf("invalid binding in 'do'")
		}
		symbol, ok := bindingList[0].(LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding variable must be a symbol")
		}
		initValue, err := EvalExpression(bindingList[1], localEnv)
		if err != nil {
			return nil, err
		}
		localEnv.Set(symbol, initValue)
	}

	// Parse end test and result forms
	endTest, ok := list[2].(LispList)
	if !ok || len(endTest) < 1 {
		return nil, fmt.Errorf("invalid end test in 'do'")
	}

	// Main loop
	for {
		// Check end condition
		endResult, err := EvalExpression(endTest[0], localEnv)
		if err != nil {
			return nil, err
		}
		if IsTruthy(endResult) {
			// Execute result forms and return
			if len(endTest) > 1 {
				var result LispValue
				for _, resultForm := range endTest[1:] {
					result, err = EvalExpression(resultForm, localEnv)
					if err != nil {
						return nil, err
					}
				}
				return result, nil
			}
			return nil, nil // If no result forms, return nil
		}

		// Execute body
		for _, bodyForm := range list[3:] {
			_, err := EvalExpression(bodyForm, localEnv)
			if err != nil {
				return nil, err
			}
		}

		// Update bindings
		for _, binding := range bindings {
			bindingList := binding.(LispList)
			symbol := bindingList[0].(LispSymbol)
			if len(bindingList) > 2 {
				newValue, err := EvalExpression(bindingList[2], localEnv)
				if err != nil {
					return nil, err
				}
				localEnv.Set(symbol, newValue)
			}
		}
	}
}

func evalQuote(list LispList) (LispValue, error) {
	if len(list) != 2 {
		return nil, fmt.Errorf("'quote' expects exactly one argument")
	}
	return list[1], nil
}

func evalIf(list LispList, env *Environment) (LispValue, error) {
	if len(list) != 4 {
		return nil, fmt.Errorf("'if' expects exactly three arguments")
	}
	condition, err := EvalExpression(list[1], env)
	if err != nil {
		return nil, err
	}
	if IsTruthy(condition) {
		return EvalExpression(list[2], env)
	}
	return EvalExpression(list[3], env)
}

func evalDefine(list LispList, env *Environment) (LispValue, error) {
	if len(list) != 3 {
		return nil, fmt.Errorf("'define' expects exactly two arguments")
	}
	symbol, ok := list[1].(LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to 'define' must be a symbol")
	}
	value, err := EvalExpression(list[2], env)
	if err != nil {
		return nil, err
	}
	env.Define(symbol, value)
	return symbol, nil
}

func evalLambda(list LispList, env *Environment) (LispValue, error) {
	if len(list) < 3 {
		return nil, fmt.Errorf("'lambda' expects at least two arguments")
	}
	params, ok := list[1].(LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}
	var paramSymbols []LispSymbol
	for _, param := range params {
		symbol, ok := param.(LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols = append(paramSymbols, symbol)
	}
	return &Lambda{
		Params:  paramSymbols,
		Body:    LispList(append([]LispValue{LispSymbol("begin")}, list[2:]...)),
		Env:     env,
		Closure: NewEnvironment(env),
	}, nil
}

func evalBegin(list LispList, env *Environment) (LispValue, error) {
	if len(list) < 2 {
		return nil, fmt.Errorf("'begin' expects at least one form")
	}
	var result LispValue
	var err error
	for _, form := range list[1:] {
		result, err = EvalExpression(form, env)
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func evalArgs(args LispList, env *Environment) ([]LispValue, error) {
	evaluated := make([]LispValue, len(args))
	for i, arg := range args {
		value, err := EvalExpression(arg, env)
		if err != nil {
			return nil, err
		}
		evaluated[i] = value
	}
	return evaluated, nil
}

func apply(fn LispValue, args []LispValue, env *Environment) (LispValue, error) {
	switch f := fn.(type) {
	case LispFunc:
		return f(args, env)
	case *Lambda:
		if len(args) != len(f.Params) {
			return nil, fmt.Errorf("lambda called with wrong number of arguments")
		}
		callEnv := NewEnvironment(f.Closure)
		for i, param := range f.Params {
			callEnv.Define(param, args[i])
		}
		return EvalExpression(f.Body, callEnv)
	default:
		return nil, fmt.Errorf("not a function: %v", fn)
	}
}

func IsTruthy(v LispValue) bool {
	switch v := v.(type) {
	case nil:
		return false
	case bool:
		return v
	case float64:
		return v != 0
	case string:
		return v != ""
	case LispList:
		return len(v) > 0
	default:
		return true
	}
}

func evalLet(list LispList, env *Environment) (LispValue, error) {
	if len(list) < 3 {
		return nil, fmt.Errorf("'let' expects at least two arguments")
	}

	bindings, ok := list[1].(LispList)
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

		letEnv.Set(symbol, value)
	}

	var result LispValue
	var err error
	for _, expr := range list[2:] {
		result, err = EvalExpression(expr, letEnv)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func evalSet(list LispList, env *Environment) (LispValue, error) {
	if len(list) != 3 {
		return nil, fmt.Errorf("'set!' expects exactly two arguments")
	}

	symbol, ok := list[1].(LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to 'set!' must be a symbol")
	}

	value, err := EvalExpression(list[2], env)
	if err != nil {
		return nil, err
	}

	if env.SetMutable(symbol, value) {
		return value, nil
	}

	return nil, fmt.Errorf("cannot set! undefined variable: %s", symbol)
}

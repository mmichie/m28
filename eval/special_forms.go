package eval

import (
	"fmt"
	"log"

	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(*Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

func GetSpecialForms() map[core.LispSymbol]SpecialFormFunc {
	return map[core.LispSymbol]SpecialFormFunc{
		"quote":     evalQuote,
		"if":        evalIf,
		"defun":     evalDefun,
		"defvar":    evalDefvar,
		"lambda":    evalLambda,
		"progn":     evalProgn,
		"do":        evalDo,
		"setq":      evalSetq,
		"let":       evalLet,
		"and":       evalAnd,
		"or":        evalOr,
		"cond":      evalCond,
		"defmacro":  evalDefmacro,
		"backquote": evalBackquote,
		"case":      evalCase,
		"when":      evalWhen,
		"unless":    evalUnless,
		"all":       evalAll,
	}
}

func evalQuote(_ *Evaluator, args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quote requires exactly one argument")
	}
	return args[0], nil
}

func evalIf(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

	return nil, nil
}

func evalDefun(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("defun requires at least 3 arguments")
	}

	symbol, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to defun must be a symbol")
	}

	params, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to defun must be a parameter list")
	}

	paramSymbols := make([]core.LispSymbol, len(params))
	for i, param := range params {
		symbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols[i] = symbol
	}

	body := core.LispList{core.LispSymbol("progn")}
	body = append(body, args[2:]...)

	lambda := &core.Lambda{
		Params:  paramSymbols,
		Body:    body,
		Env:     env,
		Closure: env,
	}

	env.Define(symbol, lambda)
	return symbol, nil
}

func evalDefvar(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("defvar requires exactly 2 arguments")
	}

	symbol, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to defvar must be a symbol")
	}

	value, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	env.Define(symbol, value)
	return symbol, nil
}

func evalLambda(_ *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("lambda requires at least 2 arguments")
	}

	params, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("lambda parameters must be a list")
	}

	paramSymbols := make([]core.LispSymbol, len(params))
	for i, param := range params {
		symbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("lambda parameter must be a symbol")
		}
		paramSymbols[i] = symbol
	}

	body := core.LispList{core.LispSymbol("progn")}
	body = append(body, args[1:]...)

	return &core.Lambda{
		Params:  paramSymbols,
		Body:    body,
		Env:     env,
		Closure: env,
	}, nil
}

func evalProgn(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	var result core.LispValue
	var err error

	for _, arg := range args {
		result, err = e.Eval(arg, env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

func evalDo(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("'do' expects at least two arguments")
	}

	bindings, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("first argument to 'do' must be a list of bindings")
	}

	test, ok := args[1].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("second argument to 'do' must be a test expression")
	}

	body := args[2:]

	localEnv := env.NewEnvironment(env)

	// Initialize bindings
	for _, binding := range bindings {
		bindingList, ok := binding.(core.LispList)
		if !ok || len(bindingList) < 2 {
			return nil, fmt.Errorf("invalid binding in 'do'")
		}
		symbol, ok := bindingList[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding variable must be a symbol")
		}
		initValue, err := e.Eval(bindingList[1], localEnv)
		if err != nil {
			return nil, err
		}
		localEnv.Define(symbol, initValue)
	}

	for {
		// Evaluate test
		testResult, err := e.Eval(test[0], localEnv)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(testResult) {
			// Return the result of evaluating the rest of the test expression
			if len(test) > 1 {
				return evalProgn(e, test[1:], localEnv)
			}
			return core.Nil{}, nil // Return nil if there are no expressions after the test
		}

		// Evaluate body
		_, err = evalProgn(e, body, localEnv)
		if err != nil {
			return nil, err
		}

		// Update bindings
		for _, binding := range bindings {
			bindingList, ok := binding.(core.LispList)
			if !ok || len(bindingList) < 2 {
				return nil, fmt.Errorf("invalid binding in 'do'")
			}
			if len(bindingList) > 2 {
				symbol, ok := bindingList[0].(core.LispSymbol)
				if !ok {
					return nil, fmt.Errorf("binding variable must be a symbol")
				}
				newValue, err := e.Eval(bindingList[2], localEnv)
				if err != nil {
					return nil, err
				}
				localEnv.Set(symbol, newValue)
			}
		}
	}
}

func evalSetq(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("setq requires exactly 2 arguments")
	}

	symbol, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to setq must be a symbol")
	}

	value, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	if !env.SetMutable(symbol, value) {
		return nil, fmt.Errorf("cannot setq undefined variable: %s", symbol)
	}

	return value, nil
}

func evalLet(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("let requires at least 2 arguments")
	}

	bindings, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("let bindings must be a list")
	}

	letEnv := env.NewEnvironment(env)
	for _, binding := range bindings {
		bindingList, ok := binding.(core.LispList)
		if !ok || len(bindingList) != 2 {
			return nil, fmt.Errorf("invalid binding in let")
		}

		symbol, ok := bindingList[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding name must be a symbol")
		}

		value, err := e.Eval(bindingList[1], env)
		if err != nil {
			return nil, err
		}

		letEnv.Define(symbol, value)
	}

	return evalProgn(e, args[1:], letEnv)
}

func evalAnd(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		result, err := e.Eval(arg, env)
		if err != nil {
			return nil, err
		}
		if !core.IsTruthy(result) {
			return false, nil
		}
	}
	if len(args) == 0 {
		return true, nil
	}
	return args[len(args)-1], nil
}

func evalOr(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		result, err := e.Eval(arg, env)
		if err != nil {
			return nil, err
		}
		if core.IsTruthy(result) {
			return result, nil
		}
	}
	return false, nil
}

func evalCond(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, clause := range args {
		clauseList, ok := clause.(core.LispList)
		if !ok || len(clauseList) < 2 {
			return nil, fmt.Errorf("invalid cond clause")
		}

		if clauseList[0] == core.LispSymbol("else") {
			return evalProgn(e, clauseList[1:], env)
		}

		condition, err := e.Eval(clauseList[0], env)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(condition) {
			return evalProgn(e, clauseList[1:], env)
		}
	}
	return nil, nil
}

func evalDefmacro(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("defmacro requires at least 3 arguments")
	}

	macroName, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("macro name must be a symbol")
	}

	lambda, err := evalLambda(e, args[1:], env)
	if err != nil {
		return nil, err
	}

	lambdaVal, ok := lambda.(*core.Lambda)
	if !ok {
		return nil, fmt.Errorf("macro body must be a lambda")
	}

	macro := &core.Macro{
		Params: lambdaVal.Params,
		Body:   lambdaVal.Body,
		Env:    lambdaVal.Env,
	}

	env.Define(macroName, macro)
	return macroName, nil
}

func evalBackquote(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("backquote requires exactly one argument")
	}
	return e.evalQuasiquote(args[0], env, 0)
}

func evalCase(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("case requires at least 2 arguments")
	}

	key, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	for _, clause := range args[1:] {
		clauseList, ok := clause.(core.LispList)
		if !ok || len(clauseList) < 2 {
			return nil, fmt.Errorf("invalid case clause")
		}

		if clauseList[0] == core.LispSymbol("else") {
			return evalProgn(e, clauseList[1:], env)
		}

		for _, test := range clauseList[0].(core.LispList) {
			if core.EqualValues(key, test) {
				return evalProgn(e, clauseList[1:], env)
			}
		}
	}

	return nil, nil
}

func evalWhen(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("when requires at least two arguments")
	}
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}
	if core.IsTruthy(condition) {
		return evalProgn(e, args[1:], env)
	}
	return nil, nil
}

func evalUnless(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("unless requires at least two arguments")
	}
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}
	if !core.IsTruthy(condition) {
		return evalProgn(e, args[1:], env)
	}
	return nil, nil
}

func evalAll(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	log.Printf("evalAll called with args: %v", core.PrintValue(args))

	if len(args) < 2 {
		return nil, fmt.Errorf("'all' requires at least two arguments")
	}

	var fn core.LispValue
	var list core.LispValue
	var err error
	opType := MapOperation

	// Determine the operation type
	if symbol, ok := args[0].(core.LispSymbol); ok {
		switch symbol {
		case "filter":
			opType = FilterOperation
		case "remove":
			opType = RemoveOperation
		case "find":
			opType = FindOperation
		case "position":
			opType = PositionOperation
		case "any":
			opType = AnyOperation
		case "every":
			opType = EveryOperation
		}
	}

	if opType != MapOperation {
		if len(args) != 3 {
			return nil, fmt.Errorf("'all %s' requires exactly three arguments", args[0])
		}
		fn = args[1]
		list, err = e.Eval(args[2], env)
	} else {
		fn = args[0]
		list, err = e.Eval(args[1], env)
	}

	if err != nil {
		return nil, err
	}

	log.Printf("'all' function argument: %v", core.PrintValue(fn))
	log.Printf("'all' list argument: %v", core.PrintValue(list))

	listVal, ok := list.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("'all' requires a list as its second argument")
	}

	return processListOperation(e, opType, fn, listVal, env)
}

// ListOperationType defines the type of list operation
type ListOperationType int

const (
	MapOperation ListOperationType = iota
	FilterOperation
	RemoveOperation
	FindOperation
	PositionOperation
	AnyOperation
	EveryOperation
)

func processListOperation(e *Evaluator, opType ListOperationType, fn core.LispValue, list core.LispList, env core.Environment) (core.LispValue, error) {
	result := make(core.LispList, 0) // Initialize result as an empty LispList
	for i, item := range list {
		predicateResult, err := e.Apply(fn, []core.LispValue{item}, env)
		if err != nil {
			return nil, err
		}

		switch opType {
		case MapOperation:
			result = append(result, predicateResult)
		case FilterOperation:
			if core.IsTruthy(predicateResult) {
				result = append(result, item)
			}
		case RemoveOperation:
			if !core.IsTruthy(predicateResult) {
				result = append(result, item)
			}
		case FindOperation:
			if core.IsTruthy(predicateResult) {
				return item, nil
			}
		case PositionOperation:
			if core.IsTruthy(predicateResult) {
				return float64(i), nil
			}
		case AnyOperation:
			if core.IsTruthy(predicateResult) {
				return true, nil
			}
		case EveryOperation:
			if !core.IsTruthy(predicateResult) {
				return false, nil
			}
		}
	}

	// Handle cases where no element was found or all elements were processed
	switch opType {
	case MapOperation, FilterOperation, RemoveOperation:
		return result, nil
	case FindOperation:
		return nil, nil
	case PositionOperation:
		return float64(-1), nil
	case AnyOperation:
		return false, nil
	case EveryOperation:
		return true, nil
	}

	log.Printf("'all' result: %v", core.PrintValue(result))
	return result, nil
}

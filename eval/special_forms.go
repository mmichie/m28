package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

type SpecialFormFunc func(*Evaluator, []core.LispValue, core.Environment) (core.LispValue, error)

func GetSpecialForms() map[core.LispSymbol]SpecialFormFunc {
	return map[core.LispSymbol]SpecialFormFunc{
		"quote":        evalQuote,
		"if":           evalIf,
		"define":       evalDefine,
		"lambda":       evalLambda,
		"begin":        evalBegin,
		"set!":         evalSet,
		"let":          evalLet,
		"and":          evalAnd,
		"or":           evalOr,
		"cond":         evalCond,
		"define-macro": evalDefineMacro,
		"quasiquote":   evalQuasiquoteForm,
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

func evalDefine(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("define requires exactly 2 arguments")
	}

	symbol, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to define must be a symbol")
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

	body := core.LispList{core.LispSymbol("begin")}
	body = append(body, args[1:]...)

	return &core.Lambda{
		Params:  paramSymbols,
		Body:    body,
		Env:     env,
		Closure: env,
	}, nil
}

func evalBegin(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

func evalSet(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("set! requires exactly 2 arguments")
	}

	symbol, ok := args[0].(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("first argument to set! must be a symbol")
	}

	value, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	if !env.SetMutable(symbol, value) {
		return nil, fmt.Errorf("cannot set! undefined variable: %s", symbol)
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

	return evalBegin(e, args[1:], letEnv)
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
			return evalBegin(e, clauseList[1:], env)
		}

		condition, err := e.Eval(clauseList[0], env)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(condition) {
			return evalBegin(e, clauseList[1:], env)
		}
	}
	return nil, nil
}

func evalDefineMacro(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("define-macro requires at least 2 arguments")
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

func evalQuasiquoteForm(e *Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quasiquote requires exactly one argument")
	}
	return e.evalQuasiquote(args[0], env, 0)
}

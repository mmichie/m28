package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalDefun(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

	paramInfo, err := parseParameters(params)
	if err != nil {
		return nil, err
	}

	body := core.LispList{core.LispSymbol("progn")}
	body = append(body, args[2:]...)

	lambda := &core.Lambda{
		Params:    paramInfo.required,
		Optional:  paramInfo.optional,
		Rest:      paramInfo.rest,
		KeyParams: paramInfo.keyParams,
		Body:      body,
		Env:       env,
		Closure:   env,
	}

	env.Define(symbol, lambda)
	return symbol, nil
}

func EvalDefvar(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

func EvalLet(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

	return EvalProgn(e, args[1:], letEnv)
}

func EvalSetq(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

// Helper function for parsing parameters
func parseParameters(params core.LispList) (parameterInfo, error) {
	info := parameterInfo{
		keyParams: make(map[core.LispSymbol]core.LispValue),
	}
	state := "required"

	for _, param := range params {
		switch param {
		case core.LispSymbol("&optional"):
			state = "optional"
		case core.LispSymbol("&rest"):
			state = "rest"
		case core.LispSymbol("&key"):
			state = "key"
		default:
			switch state {
			case "required":
				symbol, ok := param.(core.LispSymbol)
				if !ok {
					return info, fmt.Errorf("invalid required parameter: %v", param)
				}
				info.required = append(info.required, symbol)
			case "optional":
				switch p := param.(type) {
				case core.LispSymbol:
					info.optional = append(info.optional, core.OptionalParam{Name: p, DefaultValue: nil})
				case core.LispList:
					if len(p) != 2 {
						return info, fmt.Errorf("invalid optional parameter: %v", p)
					}
					name, ok := p[0].(core.LispSymbol)
					if !ok {
						return info, fmt.Errorf("invalid optional parameter name: %v", p[0])
					}
					info.optional = append(info.optional, core.OptionalParam{Name: name, DefaultValue: p[1]})
				default:
					return info, fmt.Errorf("invalid optional parameter: %v", p)
				}
			case "rest":
				symbol, ok := param.(core.LispSymbol)
				if !ok {
					return info, fmt.Errorf("invalid rest parameter: %v", param)
				}
				info.rest = symbol
				state = "after_rest"
			case "key":
				switch p := param.(type) {
				case core.LispSymbol:
					info.keyParams[p] = nil
				case core.LispList:
					if len(p) != 2 {
						return info, fmt.Errorf("invalid key parameter: %v", p)
					}
					name, ok := p[0].(core.LispSymbol)
					if !ok {
						return info, fmt.Errorf("invalid key parameter name: %v", p[0])
					}
					info.keyParams[name] = p[1]
				default:
					return info, fmt.Errorf("invalid key parameter: %v", p)
				}
			case "after_rest":
				return info, fmt.Errorf("parameters not allowed after &rest")
			}
		}
	}

	return info, nil
}

type parameterInfo struct {
	required  []core.LispSymbol
	optional  []core.OptionalParam
	rest      core.LispSymbol
	keyParams map[core.LispSymbol]core.LispValue
}

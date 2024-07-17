package m28

import (
	"fmt"
)

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

package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func parseFunctionDefinition(args []core.LispValue) ([]core.LispSymbol, core.LispValue, error) {
	if len(args) < 1 {
		return nil, nil, fmt.Errorf("function definition requires parameters and a body")
	}

	paramList, ok := args[0].(core.LispList)
	if !ok {
		return nil, nil, fmt.Errorf("function parameters must be a list")
	}

	params := make([]core.LispSymbol, len(paramList))
	for i, param := range paramList {
		symbol, ok := param.(core.LispSymbol)
		if !ok {
			return nil, nil, fmt.Errorf("function parameter must be a symbol")
		}
		params[i] = symbol
	}

	body := args[1]

	return params, body, nil
}

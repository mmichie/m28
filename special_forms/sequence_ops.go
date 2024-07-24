package special_forms

import (
	"fmt"
	"github.com/mmichie/m28/core"
)

func EvalAll(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

	listVal, ok := list.(core.LispList)
	if !ok {
		return nil, fmt.Errorf("'all' requires a list as its second argument")
	}

	return processListOperation(e, opType, fn, listVal, env)
}

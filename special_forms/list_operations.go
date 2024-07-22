package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

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

func processListOperation(e core.Evaluator, opType ListOperationType, fn core.LispValue, list core.LispList, env core.Environment) (core.LispValue, error) {
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

	return result, nil
}

package special_forms

import (
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

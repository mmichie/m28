package m28

import (
	"fmt"
)

func evalIf(args []LispValue, env *Environment) (LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("'if' expects two or three arguments")
	}

	condition, err := EvalExpression(args[0], env)
	if err != nil {
		return nil, err
	}

	if IsTruthy(condition) {
		return EvalExpression(args[1], env)
	} else if len(args) == 3 {
		return EvalExpression(args[2], env)
	}

	return nil, nil // If no else clause and condition is false, return nil
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

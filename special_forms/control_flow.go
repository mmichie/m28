package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalIf(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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

func EvalCond(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, clause := range args {
		clauseList, ok := clause.(core.LispList)
		if !ok || len(clauseList) < 2 {
			return nil, fmt.Errorf("invalid cond clause")
		}

		if clauseList[0] == core.LispSymbol("else") {
			return EvalProgn(e, clauseList[1:], env)
		}

		condition, err := e.Eval(clauseList[0], env)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(condition) {
			return EvalProgn(e, clauseList[1:], env)
		}
	}
	return nil, nil
}

func EvalCase(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
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
			return EvalProgn(e, clauseList[1:], env)
		}

		for _, test := range clauseList[0].(core.LispList) {
			if core.EqualValues(key, test) {
				return EvalProgn(e, clauseList[1:], env)
			}
		}
	}

	return nil, nil
}

func EvalWhen(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("when requires at least two arguments")
	}
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}
	if core.IsTruthy(condition) {
		return EvalProgn(e, args[1:], env)
	}
	return nil, nil
}

func EvalUnless(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("unless requires at least two arguments")
	}
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}
	if !core.IsTruthy(condition) {
		return EvalProgn(e, args[1:], env)
	}
	return nil, nil
}

func EvalProgn(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {

	if len(args) == 0 {
		return nil, nil
	}

	var result core.LispValue
	var err error

	// Skip the first element if it's the 'progn' symbol
	startIndex := 0
	if symbol, ok := args[0].(core.LispSymbol); ok && symbol == "progn" {
		startIndex = 1
	}

	for i := startIndex; i < len(args); i++ {
		result, err = e.Eval(args[i], env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

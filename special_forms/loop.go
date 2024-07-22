package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalLoop(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("loop requires at least one argument")
	}

	// Check if it's a simple loop (just body)
	if _, ok := args[0].(core.LispSymbol); !ok {
		return evalSimpleLoop(e, args, env)
	}

	// It's a complex loop with clauses
	return evalComplexLoop(e, args, env)
}

func evalSimpleLoop(e core.Evaluator, body []core.LispValue, env core.Environment) (core.LispValue, error) {
	loopEnv := env.NewEnvironment(env)
	var err error

	for {
		for _, expr := range body {
			_, err = e.Eval(expr, loopEnv)
			if err != nil {
				// Check if the error is a special return signal
				if returnErr, ok := err.(returnError); ok {
					return returnErr.value, nil
				}
				return nil, err
			}
		}
	}
}

func evalComplexLoop(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	loopEnv := env.NewEnvironment(env)
	var result core.LispValue
	var err error

	// Parse loop clauses
	i := 0
	for i < len(args) {
		clause, ok := args[i].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("expected loop clause, got: %v", args[i])
		}
		switch clause {
		case "for":
			i, err = handleForClause(e, args, i, loopEnv)
			if err != nil {
				return nil, err
			}
		case "do":
			i++
			for i < len(args) && args[i] != core.LispSymbol("for") {
				result, err = e.Eval(args[i], loopEnv)
				if err != nil {
					// Check if the error is a special return signal
					if returnErr, ok := err.(returnError); ok {
						return returnErr.value, nil
					}
					return nil, err
				}
				i++
			}
		default:
			return nil, fmt.Errorf("unsupported loop clause: %v", clause)
		}
	}

	return result, nil
}

func handleForClause(e core.Evaluator, args []core.LispValue, i int, env core.Environment) (int, error) {
	if i+3 >= len(args) {
		return 0, fmt.Errorf("incomplete for clause")
	}

	varSymbol, ok := args[i+1].(core.LispSymbol)
	if !ok {
		return 0, fmt.Errorf("for clause requires a symbol")
	}

	if args[i+2] != core.LispSymbol("across") {
		return 0, fmt.Errorf("only 'across' is supported in for clause")
	}

	sequence, err := e.Eval(args[i+3], env)
	if err != nil {
		return 0, err
	}

	var seqList core.LispList
	switch seq := sequence.(type) {
	case core.LispList:
		seqList = seq
	case string:
		seqList = make(core.LispList, len(seq))
		for i, ch := range seq {
			seqList[i] = string(ch)
		}
	default:
		return 0, fmt.Errorf("for across requires a list or string")
	}

	for _, item := range seqList {
		env.Set(varSymbol, item)
		// Evaluate the rest of the loop body
		for j := i + 4; j < len(args); j++ {
			if args[j] == core.LispSymbol("for") {
				break
			}
			_, err = e.Eval(args[j], env)
			if err != nil {
				// Check if the error is a special return signal
				if returnErr, ok := err.(returnError); ok {
					return 0, returnErr
				}
				return 0, err
			}
		}
	}

	return len(args), nil // Return to the end of args to finish the loop
}

// Define a custom error type for signaling a return from the loop
type returnError struct {
	value core.LispValue
}

func (e returnError) Error() string {
	return "return from loop"
}

// Add a new built-in function to allow breaking out of the loop
func init() {
	core.RegisterBuiltin("return-from-loop", returnFromLoopFunc)
}

func returnFromLoopFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("return-from-loop requires exactly one argument")
	}
	return nil, returnError{value: args[0]}
}

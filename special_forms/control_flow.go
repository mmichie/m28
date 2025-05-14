package special_forms

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func EvalIfPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("if requires 2 or 3 arguments")
	}

	// Evaluate the condition in the parent environment
	// This ensures that variables from outer scopes are accessible
	condition, err := e.Eval(args[0], env)
	if err != nil {
		// Special handling for control flow signals
		if IsBreakSignal(err) || IsContinueSignal(err) {
			return nil, err
		}

		// Handle early returns
		if returnSig, ok := err.(ReturnSignal); ok {
			return returnSig.Value, err
		}

		return nil, err
	}

	// We'll evaluate directly in the parent environment
	// This allows variable assignments to propagate back to the parent
	// while still properly handling nested contexts
	if core.IsTruthy(condition) {
		// Evaluate the true branch
		result, err := e.Eval(args[1], env)
		if err != nil {
			// Control flow signals should propagate through if/else
			if IsBreakSignal(err) || IsContinueSignal(err) {
				return nil, err
			}

			// Handle return signals
			if returnSig, ok := err.(ReturnSignal); ok {
				return returnSig.Value, err
			}

			return nil, err
		}
		return result, nil
	} else if len(args) == 3 {
		// Evaluate the else branch
		result, err := e.Eval(args[2], env)
		if err != nil {
			// Control flow signals should propagate through if/else
			if IsBreakSignal(err) || IsContinueSignal(err) {
				return nil, err
			}

			// Handle return signals
			if returnSig, ok := err.(ReturnSignal); ok {
				return returnSig.Value, err
			}

			return nil, err
		}
		return result, nil
	}

	return core.PythonicNone{}, nil
}

func EvalElif(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("elif requires 2 or 3 arguments")
	}

	// Evaluate the condition in the parent environment
	condition, err := e.Eval(args[0], env)
	if err != nil {
		// Special handling for control flow signals
		if IsBreakSignal(err) || IsContinueSignal(err) {
			return nil, err
		}

		// Handle early returns
		if returnSig, ok := err.(ReturnSignal); ok {
			return returnSig.Value, err
		}

		return nil, err
	}

	if core.IsTruthy(condition) {
		// Evaluate the true branch
		result, err := e.Eval(args[1], env)
		if err != nil {
			// Control flow signals should propagate through elif
			if IsBreakSignal(err) || IsContinueSignal(err) {
				return nil, err
			}

			// Handle return signals
			if returnSig, ok := err.(ReturnSignal); ok {
				return returnSig.Value, err
			}

			return nil, err
		}
		return result, nil
	} else if len(args) == 3 {
		// Evaluate the else branch
		result, err := e.Eval(args[2], env)
		if err != nil {
			// Control flow signals should propagate
			if IsBreakSignal(err) || IsContinueSignal(err) {
				return nil, err
			}

			// Handle return signals
			if returnSig, ok := err.(ReturnSignal); ok {
				return returnSig.Value, err
			}

			return nil, err
		}
		return result, nil
	}

	return core.PythonicNone{}, nil
}

func EvalElse(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("else requires exactly 1 argument")
	}

	// Evaluate the body directly in the parent environment
	result, err := e.Eval(args[0], env)
	if err != nil {
		// Control flow signals should propagate through else
		if IsBreakSignal(err) || IsContinueSignal(err) {
			return nil, err
		}

		// Handle return signals
		if returnSig, ok := err.(ReturnSignal); ok {
			return returnSig.Value, err
		}

		return nil, err
	}
	return result, nil
}

// unwrapLocatedValue extracts the value from a LocatedValue
func unwrapLocatedValue(expr core.LispValue) core.LispValue {
	if located, ok := expr.(core.LocatedValue); ok {
		return located.Value
	}
	return expr
}

func EvalFor(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 3 {
		return nil, fmt.Errorf("for loop requires at least 3 arguments")
	}

	// Unwrap the first argument in case it's wrapped in a LocatedValue
	unwrappedArg := unwrapLocatedValue(args[0])

	iterVar, ok := unwrappedArg.(core.LispSymbol)
	if !ok {
		return nil, fmt.Errorf("iteration variable must be a symbol, got %T", unwrappedArg)
	}

	iterable, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	// Handle both regular lists and list literals
	var iter core.LispList

	if list, ok := iterable.(core.LispList); ok {
		iter = list
	} else if list, ok := iterable.(core.LispListLiteral); ok {
		iter = core.LispList(list)
	} else {
		// Try to handle strings by treating them as character lists
		if str, ok := iterable.(string); ok {
			strList := make(core.LispList, len(str))
			for i, ch := range str {
				strList[i] = string(ch)
			}
			iter = strList
		} else {
			return nil, fmt.Errorf("for loop requires a list, list literal, or string; got %T", iterable)
		}
	}

	// We'll evaluate in the parent environment to allow access to variables
	// This maintains expected semantics for variable updates
	var result core.LispValue = core.PythonicNone{}

	for _, item := range iter {
		// Define the iterator variable in the parent environment
		env.Define(iterVar, item)

		// Execute each body expression
		for _, expr := range args[2:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				// Handle special control flow signals
				if IsBreakSignal(err) {
					// Break out of the loop
					return result, nil
				} else if IsContinueSignal(err) {
					// Skip to the next iteration
					break
				} else if returnSig, ok := err.(ReturnSignal); ok {
					// Return signal should propagate out of the loop
					return returnSig.Value, nil
				} else {
					// Propagate other errors
					return nil, err
				}
			}
		}
	}

	return result, nil
}

func EvalWhilePython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("while loop requires at least 2 arguments")
	}

	// Use the parent environment for loop execution
	// This allows modifying variables in the parent scope
	var result core.LispValue = core.PythonicNone{}

	for {
		// Evaluate the loop condition
		condition, err := e.Eval(args[0], env)
		if err != nil {
			// Check if this is a control flow signal
			if IsBreakSignal(err) {
				// Break out of the loop immediately
				return result, nil
			} else if IsContinueSignal(err) {
				// Skip to the next iteration (re-evaluate condition)
				continue
			} else if returnSig, ok := err.(ReturnSignal); ok {
				// Return signal should propagate out of the loop
				return returnSig.Value, nil
			}
			// Propagate other errors
			return nil, err
		}

		// Exit loop if condition is false
		if !core.IsTruthy(condition) {
			break
		}

		// Execute the loop body
		for _, expr := range args[1:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				// Handle special control flow signals
				if IsBreakSignal(err) {
					// Break out of the loop
					return result, nil
				} else if IsContinueSignal(err) {
					// Skip to the next iteration
					break
				} else if returnSig, ok := err.(ReturnSignal); ok {
					// Return signal should propagate out of the loop
					return returnSig.Value, nil
				} else {
					// Propagate other errors
					return nil, err
				}
			}
		}
	}

	return result, nil
}

// EvalTry is now defined in exception.go for better organization
// It has been rewritten with comprehensive LocatedValue handling

// BreakSignal is a special error type that signals a break from a loop
type BreakSignal struct{}

func (b BreakSignal) Error() string {
	return "break"
}

// ContinueSignal is a special error type that signals a continue in a loop
type ContinueSignal struct{}

func (c ContinueSignal) Error() string {
	return "continue"
}

// IsBreakSignal checks if an error is a BreakSignal
func IsBreakSignal(err error) bool {
	_, ok := err.(BreakSignal)
	return ok
}

// IsContinueSignal checks if an error is a ContinueSignal
func IsContinueSignal(err error) bool {
	_, ok := err.(ContinueSignal)
	return ok
}

func EvalBreak(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("break does not take any arguments")
	}
	return nil, BreakSignal{}
}

func EvalContinue(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("continue does not take any arguments")
	}
	return nil, ContinueSignal{}
}

func EvalPass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	return nil, nil
}

package eval

import (
	"github.com/mmichie/m28/core"
)

// TailCall represents a tail call that needs to be processed
type TailCall struct {
	Function core.LispValue
	Args     []core.LispValue
	Env      core.Environment
}

// isTailCall checks if an expression is in tail position
func (e *Evaluator) isTailPosition(expr core.LispValue, parent core.LispValue) bool {
	if _, ok := parent.(core.LispList); !ok {
		return false
	}

	list := parent.(core.LispList)
	if len(list) == 0 {
		return false
	}

	// Get the first element to check if it's a special form
	first := e.unwrapLocatedValue(list[0])
	if sym, ok := first.(core.LispSymbol); ok {
		// A call is in tail position if it's the last expression in one of these forms
		switch sym {
		case "begin", "do":
			// In (begin expr1 expr2 ... exprN), only exprN is in tail position
			return list[len(list)-1] == expr
		case "if":
			// In (if condition true-branch false-branch), both branches are in tail position
			return (len(list) >= 3 && list[2] == expr) || (len(list) >= 4 && list[3] == expr)
		case "cond":
			// For cond, the last expression in each clause is in tail position
			for i := 1; i < len(list); i++ {
				if clause, ok := list[i].(core.LispList); ok && len(clause) >= 2 {
					if clause[len(clause)-1] == expr {
						return true
					}
				}
			}
		case "lambda", "def", "define":
			// For lambda/def/define, the last expression in the body is in tail position
			if len(list) >= 3 {
				body := list[2:]
				return len(body) > 0 && body[len(body)-1] == expr
			}
		}
	}

	return false
}

// detectTailCall checks if a function call is a tail call that can be optimized
func (e *Evaluator) detectTailCall(fn core.LispValue, args []core.LispValue, env core.Environment, expr core.LispValue, parent core.LispValue) (bool, *TailCall) {
	// Check if this call is in a tail position
	if !e.isTailPosition(expr, parent) {
		return false, nil
	}

	// We can optimize calls to lambda functions
	if lambda, ok := fn.(*core.Lambda); ok {
		return true, &TailCall{
			Function: lambda,
			Args:     args,
			Env:      env,
		}
	}

	return false, nil
}

// trampoline repeatedly evaluates tail calls until a final result is obtained
func (e *Evaluator) trampoline(initialTailCall *TailCall) (core.LispValue, error) {
	var tailCall *TailCall = initialTailCall
	var result core.LispValue
	var err error

	for tailCall != nil {
		// Process the tail call
		lambda, ok := tailCall.Function.(*core.Lambda)
		if !ok {
			// If it's not a lambda, just evaluate it normally
			return e.Apply(tailCall.Function, tailCall.Args, tailCall.Env)
		}

		// Create a new environment for the lambda call
		callEnv := tailCall.Env.NewEnvironment(lambda.Closure)

		// Bind parameters to arguments
		if err := bindParamsForTailCall(lambda, tailCall.Args, callEnv); err != nil {
			return nil, err
		}

		// Evaluate the lambda body
		result, err, tailCall = e.evalTailCallBody(lambda, callEnv)
		if err != nil || tailCall == nil {
			return result, err
		}
	}

	return result, nil
}

// evalTailCallBody evaluates a lambda body with tail call detection
func (e *Evaluator) evalTailCallBody(lambda *core.Lambda, env core.Environment) (core.LispValue, error, *TailCall) {
	// Handle lambda body as a list of expressions
	if list, ok := lambda.Body.(core.LispList); ok {
		var result core.LispValue = core.PythonicNone{}
		var err error

		// For a single expression, evaluate it directly
		if len(list) == 1 {
			if exprList, isList := list[0].(core.LispList); isList {
				if len(exprList) > 0 {
					// Check if this is a tail call
					firstElem := e.unwrapLocatedValue(exprList[0])
					// We'll use the first element regardless of whether it's a symbol
					// Evaluate the function and arguments
					fn, fnErr := e.Eval(firstElem, env)
					if fnErr != nil {
						return nil, fnErr, nil
					}

					args := exprList[1:]
					evalArgs, argsErr := e.evalArgs(args, env)
					if argsErr != nil {
						return nil, argsErr, nil
					}

					// Check for tail call
					isTail, tailCall := e.detectTailCall(fn, evalArgs, env, exprList, list)
					if isTail && tailCall != nil {
						return nil, nil, tailCall
					}
				}

				// If not a tail call, evaluate normally
				result, err = e.Eval(exprList, env)
				if err != nil {
					return nil, err, nil
				}
				return result, nil, nil
			}
		}

		// Multiple expressions - evaluate all but the last one normally
		for i := 0; i < len(list)-1; i++ {
			result, err = e.Eval(list[i], env)
			if err != nil {
				return nil, err, nil
			}
		}

		// For the last expression, check if it's a tail call
		lastExpr := list[len(list)-1]
		if exprList, isList := lastExpr.(core.LispList); isList && len(exprList) > 0 {
			// Check if this is a tail call
			firstElem := e.unwrapLocatedValue(exprList[0])
			// We'll use the first element regardless of whether it's a symbol
			// Evaluate the function and arguments
			fn, fnErr := e.Eval(firstElem, env)
			if fnErr != nil {
				return nil, fnErr, nil
			}

			args := exprList[1:]
			evalArgs, argsErr := e.evalArgs(args, env)
			if argsErr != nil {
				return nil, argsErr, nil
			}

			// Check for tail call
			isTail, tailCall := e.detectTailCall(fn, evalArgs, env, exprList, list)
			if isTail && tailCall != nil {
				return nil, nil, tailCall
			}

			// If not a tail call, apply normally
			result, err = e.Apply(fn, evalArgs, env)
			return result, err, nil
		}

		// If not a tail call, evaluate the last expression normally
		result, err = e.Eval(lastExpr, env)
		return result, err, nil
	}

	// For a single expression that's not a list, just evaluate it
	result, err := e.Eval(lambda.Body, env)
	return result, err, nil
}

// bindParamsForTailCall binds parameters to arguments for tail call optimization
func bindParamsForTailCall(lambda *core.Lambda, args []core.LispValue, env core.Environment) error {
	// We can reuse the existing bindParams implementation
	return bindParams(lambda, args, env)
}

// Helper function to use the one from special_forms package
func bindParams(lambda *core.Lambda, args []core.LispValue, env core.Environment) error {
	// This is a simple implementation to match the one in special_forms
	// In a real implementation, you'd import and use the original function

	// Set positional arguments
	minArgs := len(lambda.Params)
	if len(args) > minArgs {
		return nil // Too many arguments error
	}

	for i, arg := range args {
		env.Define(lambda.Params[i], arg)
	}

	// Set default values for remaining params
	for i := len(args); i < minArgs; i++ {
		param := lambda.Params[i]
		if defaultVal, hasDefault := lambda.DefaultValues[param]; hasDefault {
			env.Define(param, defaultVal)
		} else {
			return nil // Missing required argument error
		}
	}

	return nil
}

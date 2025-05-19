package special_forms

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

// Signal types for control flow within loops
type BreakSignal struct{}
type ContinueSignal struct{}
type ReturnSignal struct {
	Value core.LispValue
}

// Error interfaces for control flow signals
func (b BreakSignal) Error() string {
	return "break signal"
}

func (c ContinueSignal) Error() string {
	return "continue signal"
}

func (r ReturnSignal) Error() string {
	return "return signal"
}

// Helper check functions
func IsBreakSignal(err error) bool {
	_, ok := err.(BreakSignal)
	return ok
}

func IsContinueSignal(err error) bool {
	_, ok := err.(ContinueSignal)
	return ok
}

// EvalIf evaluates the if special form
func EvalIf(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("if requires at least 2 arguments")
	}

	// Evaluate the condition
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if core.IsTruthy(condition) {
		// Evaluate the consequent branch
		return e.Eval(args[1], env)
	} else if len(args) > 2 {
		// If there's an alternate branch, evaluate it
		return e.Eval(args[2], env)
	}

	// No alternate branch, return nil equivalent
	return core.PythonicNone{}, nil
}

// EvalIfPython implements a more Python-like if-elif-else chain
func EvalIfPython(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// We need at least a condition and a body
	if len(args) < 2 {
		return nil, fmt.Errorf("if requires at least a condition and a body")
	}

	// Evaluate the condition
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if core.IsTruthy(condition) {
		// Execute the body statements
		var lastResult core.LispValue = core.PythonicNone{}
		for _, stmt := range args[1:] {
			// Skip special forms like elif and else
			if symbol, ok := stmt.(core.LispSymbol); ok {
				if symbol == "elif" || symbol == "else" {
					break
				}
			}

			// Evaluate the statement
			lastResult, err = e.Eval(stmt, env)
			if err != nil {
				return nil, err
			}
		}
		return lastResult, nil
	}

	// If condition is false, look for elif or else
	for i := 1; i < len(args); i++ {
		if symbol, ok := args[i].(core.LispSymbol); ok {
			if symbol == "elif" && i+2 < len(args) {
				// Found an elif, evaluate it recursively
				return EvalIfPython(e, args[i+1:], env)
			} else if symbol == "else" && i+1 < len(args) {
				// Found an else, execute its body
				var lastResult core.LispValue = core.PythonicNone{}
				for _, stmt := range args[i+1:] {
					lastResult, err = e.Eval(stmt, env)
					if err != nil {
						return nil, err
					}
				}
				return lastResult, nil
			}
		}
	}

	// No matching branch found, return None
	return core.PythonicNone{}, nil
}

// EvalCond evaluates the cond special form
func EvalCond(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	for _, clause := range args {
		list, ok := clause.(core.LispList)
		if !ok {
			return nil, fmt.Errorf("cond requires list clauses")
		}

		if len(list) < 1 {
			return nil, fmt.Errorf("cond clauses must have at least a condition")
		}

		// Evaluate the condition
		condition, err := e.Eval(list[0], env)
		if err != nil {
			return nil, err
		}

		if core.IsTruthy(condition) {
			// This clause matches, evaluate its body
			if len(list) == 1 {
				// If there's only a condition, return its value
				return condition, nil
			}

			// Evaluate each expression in the body, returning the last one
			var result core.LispValue
			for _, expr := range list[1:] {
				result, err = e.Eval(expr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}

	// No matching clause found
	return core.PythonicNone{}, nil
}

// EvalFor evaluates the for special form
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

	// Handle regular lists, list literals, and tuples
	var iter core.LispList

	switch v := iterable.(type) {
	case core.LispList:
		iter = v
	case core.LispListLiteral:
		iter = core.LispList(v)
	case core.LispTuple:
		// Convert tuple to list for iteration
		iter = make(core.LispList, len(v))
		for i, item := range v {
			iter[i] = item
		}
	case string:
		// Handle strings as lists of characters
		strList := make(core.LispList, len(v))
		for i, ch := range v {
			strList[i] = string(ch)
		}
		iter = strList
	default:
		return nil, fmt.Errorf("for loop requires a list, list literal, tuple, or string; got %T", iterable)
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

	// Store the body expressions
	var bodyExprs []core.LispValue
	var elseExprs []core.LispValue
	var foundElse bool

	// Find else clause if it exists
	for i, expr := range args[1:] {
		if symbol, ok := expr.(core.LispSymbol); ok && symbol == "else" {
			bodyExprs = args[1 : i+1]
			elseExprs = args[i+2:]
			foundElse = true
			break
		}
	}

	if !foundElse {
		bodyExprs = args[1:]
	}

	// Main loop
	for {
		// Evaluate the condition
		condition, err := e.Eval(args[0], env)
		if err != nil {
			return nil, err
		}

		if !core.IsTruthy(condition) {
			break
		}

		// Execute the body expressions
		for _, expr := range bodyExprs {
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

	// If there's an else clause and we didn't break out of the loop, execute it
	if foundElse {
		for _, expr := range elseExprs {
			result, err := e.Eval(expr, env)
			if err != nil {
				return nil, err
			}
			return result, nil
		}
	}

	return result, nil
}

// EvalBreak evaluates the break special form
func EvalBreak(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Break doesn't accept any arguments
	if len(args) > 0 {
		return nil, fmt.Errorf("break doesn't accept any arguments")
	}

	// Return a special break signal
	return nil, BreakSignal{}
}

// EvalContinue evaluates the continue special form
func EvalContinue(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// Continue doesn't accept any arguments
	if len(args) > 0 {
		return nil, fmt.Errorf("continue doesn't accept any arguments")
	}

	// Return a special continue signal
	return nil, ContinueSignal{}
}

// EvalReturn evaluates the return special form
func EvalReturn(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) == 0 {
		// Return None if no value is provided
		return nil, ReturnSignal{Value: core.PythonicNone{}}
	}

	// Evaluate the return value
	value, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Return a special return signal with the value
	return nil, ReturnSignal{Value: value}
}

// EvalLet evaluates the let special form
func EvalLet(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("let requires at least 1 argument")
	}

	// Create a new environment for the let form
	letEnv := env.NewEnvironment(env)

	// Parse the bindings
	bindings, ok := args[0].(core.LispList)
	if !ok {
		return nil, fmt.Errorf("let bindings must be a list")
	}

	// Process each binding
	for _, binding := range bindings {
		bindingList, ok := binding.(core.LispList)
		if !ok || len(bindingList) != 2 {
			return nil, fmt.Errorf("each binding must be a list of form (symbol value)")
		}

		symbol, ok := bindingList[0].(core.LispSymbol)
		if !ok {
			return nil, fmt.Errorf("binding name must be a symbol")
		}

		// Evaluate the binding value in the parent environment
		value, err := e.Eval(bindingList[1], env)
		if err != nil {
			return nil, err
		}

		// Define the binding in the let environment
		letEnv.Define(symbol, value)
	}

	// Evaluate each body expression in the let environment
	var result core.LispValue = core.PythonicNone{}
	for _, expr := range args[1:] {
		result, err := e.Eval(expr, letEnv)
		if err != nil {
			return nil, err
		}
		return result, nil
	}

	return result, nil
}

// unwrapLocatedValue unwraps a LocatedValue if needed
func unwrapLocatedValue(value core.LispValue) core.LispValue {
	if located, ok := value.(core.LocatedValue); ok {
		return located.Value
	}
	return value
}

// parseIntegerOrFail tries to parse a string as an integer
func parseIntegerOrFail(str string) (int, error) {
	intVal, err := strconv.ParseInt(strings.TrimSpace(str), 10, 64)
	if err != nil {
		return 0, fmt.Errorf("failed to parse integer: %s", err)
	}
	return int(intVal), nil
}

// EvalCase evaluates the case special form
func EvalCase(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("case requires at least 2 arguments")
	}

	// Evaluate the key expression
	key, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Check each clause
	for i := 1; i < len(args); i++ {
		clause, ok := args[i].(core.LispList)
		if !ok {
			return nil, fmt.Errorf("case clause must be a list")
		}

		if len(clause) < 2 {
			return nil, fmt.Errorf("case clause must have at least a key and a body")
		}

		// Check if this is the else/default clause
		if symbol, ok := clause[0].(core.LispSymbol); ok && (symbol == "else" || symbol == "default") {
			// Execute the else clause body
			var result core.LispValue = core.PythonicNone{}
			for _, expr := range clause[1:] {
				result, err = e.Eval(expr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}

		// Evaluate the clause key
		clauseKey, err := e.Eval(clause[0], env)
		if err != nil {
			return nil, err
		}

		// If the keys match, execute this clause
		if core.EqualValues(key, clauseKey) {
			var result core.LispValue = core.PythonicNone{}
			for _, expr := range clause[1:] {
				result, err = e.Eval(expr, env)
				if err != nil {
					return nil, err
				}
			}
			return result, nil
		}
	}

	// No matching clause found
	return core.PythonicNone{}, nil
}

// EvalWhen evaluates the when special form
func EvalWhen(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("when requires at least 2 arguments")
	}

	// Evaluate the condition
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if core.IsTruthy(condition) {
		// Execute the body
		var result core.LispValue = core.PythonicNone{}
		for _, expr := range args[1:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	// Condition is false, return nil
	return core.PythonicNone{}, nil
}

// EvalUnless evaluates the unless special form
func EvalUnless(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("unless requires at least 2 arguments")
	}

	// Evaluate the condition
	condition, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	if !core.IsTruthy(condition) {
		// Execute the body
		var result core.LispValue = core.PythonicNone{}
		for _, expr := range args[1:] {
			result, err = e.Eval(expr, env)
			if err != nil {
				return nil, err
			}
		}
		return result, nil
	}

	// Condition is true, return nil
	return core.PythonicNone{}, nil
}

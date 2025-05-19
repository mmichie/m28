package special_forms

import (
	"fmt"
	"math"
	"math/rand"
	"time"

	"github.com/mmichie/m28/core"
)

func init() {
	// Seed the random number generator
	rand.Seed(time.Now().UnixNano())
}

// EvalBegin evaluates a sequence of expressions and returns the value of the last one
func EvalBegin(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	var result core.LispValue = core.PythonicNone{}
	var err error

	for _, expr := range args {
		result, err = e.Eval(expr, env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

// EvalQuote returns its argument unevaluated
func EvalQuote(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quote requires exactly one argument")
	}
	return args[0], nil
}

// EvalQuasiquote handles quasiquote expressions
func EvalQuasiquote(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("quasiquote requires exactly one argument")
	}

	return evalQuasiquote(e, args[0], env)
}

// Helper function for quasiquote evaluation
func evalQuasiquote(e core.Evaluator, expr core.LispValue, env core.Environment) (core.LispValue, error) {
	// Handle unquote
	if isUnquote(expr) {
		unquote := expr.(core.Unquote)
		return e.Eval(unquote.Expr, env)
	}

	// Handle unquote-splicing
	if isUnquoteSplicing(expr) {
		return nil, fmt.Errorf("unquote-splicing can only be used within a list")
	}

	// Handle lists specially
	if list, ok := expr.(core.LispList); ok {
		return evalQuasiquoteList(e, list, env)
	}

	// Any other value is returned as-is
	return expr, nil
}

// Helper for handling lists in quasiquote
func evalQuasiquoteList(e core.Evaluator, list core.LispList, env core.Environment) (core.LispValue, error) {
	result := core.LispList{}

	for i := 0; i < len(list); i++ {
		item := list[i]

		// Handle unquote-splicing
		if isUnquoteSplicing(item) {
			unquoteSplicing := item.(core.UnquoteSplicing)
			splicedValues, err := e.Eval(unquoteSplicing.Expr, env)
			if err != nil {
				return nil, err
			}

			// Make sure it's a list
			splicedList, ok := splicedValues.(core.LispList)
			if !ok {
				return nil, fmt.Errorf("unquote-splicing expects a list, got %T", splicedValues)
			}

			// Append all values from the spliced list
			result = append(result, splicedList...)
		} else {
			// For regular items, recurse and append
			evaluated, err := evalQuasiquote(e, item, env)
			if err != nil {
				return nil, err
			}
			result = append(result, evaluated)
		}
	}

	return result, nil
}

// Helper functions for checking quasiquote expressions
func isUnquote(expr core.LispValue) bool {
	_, ok := expr.(core.Unquote)
	return ok
}

func isUnquoteSplicing(expr core.LispValue) bool {
	_, ok := expr.(core.UnquoteSplicing)
	return ok
}

// EvalRandom evaluates the random function
func EvalRandom(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("random takes at most one argument")
	}

	if len(args) == 0 {
		// Return a random value between 0.0 and 1.0
		return rand.Float64(), nil
	}

	// Handle the case with an argument
	max, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Make sure the argument is a number
	maxValue, ok := max.(float64)
	if !ok {
		return nil, fmt.Errorf("random expects a number, got %T", max)
	}

	if maxValue <= 0 {
		return nil, fmt.Errorf("random argument must be positive")
	}

	// Check if it's an integer
	isInt := math.Floor(maxValue) == maxValue

	if isInt {
		// For integers, return a random integer in [0, max)
		return float64(rand.Intn(int(maxValue))), nil
	} else {
		// For floats, return a random float in [0, max)
		return rand.Float64() * maxValue, nil
	}
}

// YieldSignal is a custom error type that signals a generator yield
type YieldSignal struct {
	Value core.LispValue
}

func (y YieldSignal) Error() string {
	return "yield with value"
}

// EvalYield implements the yield special form for generator functions
func EvalYield(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("yield requires exactly one argument")
	}

	// Evaluate the yield value
	val, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Return a YieldSignal with the value
	return nil, YieldSignal{Value: val}
}

// EvalPass is a no-op special form that does nothing
func EvalPass(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 0 {
		return nil, fmt.Errorf("pass doesn't accept any arguments")
	}

	// Just return None
	return core.PythonicNone{}, nil
}

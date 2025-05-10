package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// HandleDotNotation processes dot notation in symbols
// This is called from the Eval method when a symbol contains dots
func (e *Evaluator) HandleDotNotation(symbol core.LispSymbol, env core.Environment) (core.LispValue, error) {
	fmt.Printf("DEBUG: HandleDotNotation called with symbol: %s\n", symbol)
	parts := strings.Split(string(symbol), ".")

	// Special case for dict methods and set methods
	if parts[0] == "dict" || parts[0] == "set" {
		// Functions like dict.keys, dict.update, set.intersection, etc.
		// should be looked up directly in the environment
		val, ok := env.Get(symbol)
		if ok {
			return val, nil
		}
	}

	// Get the base object
	baseSymbol := core.LispSymbol(parts[0])
	baseObj, err := e.Eval(baseSymbol, env)
	if err != nil {
		return nil, e.enrichErrorWithTraceback(err)
	}

	// Process the member access chain
	result := baseObj
	for i := 1; i < len(parts); i++ {
		memberName := parts[i]

		// Get the member using the AccessObjectMember helper
		memberValue, err := core.AccessObjectMember(result, memberName, e, env)
		if err != nil {
			return nil, e.enrichErrorWithTraceback(err)
		}

		// Update the result for next iteration
		result = memberValue
	}

	return result, nil
}

// HandleDotNotationCall handles function calls with dot notation
// For example: module.function(args) or object.method(args)
func (e *Evaluator) HandleDotNotationCall(symbol core.LispSymbol, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	// First get the function/method using normal dot notation
	fn, err := e.HandleDotNotation(symbol, env)
	if err != nil {
		return nil, err
	}

	// Then apply it with the arguments
	return e.Apply(fn, args, env)
}

// Helper method to prepare a dot notation function call
func (e *Evaluator) prepareDotNotationCall(symbol core.LispSymbol, args []core.LispValue, env core.Environment) (core.LispSymbol, []core.LispValue) {
	// Just return the symbol and args as-is for now
	// In the future, this could do preprocessing
	return symbol, args
}

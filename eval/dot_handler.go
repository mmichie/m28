package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// HandleDotNotation processes dot notation in symbols
// This is called from the Eval method when a symbol contains dots
func (e *Evaluator) HandleDotNotation(symbol core.LispSymbol, env core.Environment) (core.LispValue, error) {
	// Add debug output
	if core.Debug {
		fmt.Printf("DEBUG HandleDotNotation: Processing '%s'\n", symbol)
	}

	parts := strings.Split(string(symbol), ".")

	// Special case for dict methods and set methods
	if parts[0] == "dict" || parts[0] == "set" {
		// Functions like dict.keys, dict.update, set.intersection, etc.
		// should be looked up directly in the environment
		val, ok := env.Get(symbol)
		if ok {
			if core.Debug {
				fmt.Printf("DEBUG HandleDotNotation: Found direct method '%s' in environment\n", symbol)
			}
			return val, nil
		}
	}

	// Get the base object
	baseSymbol := core.LispSymbol(parts[0])
	if core.Debug {
		fmt.Printf("DEBUG HandleDotNotation: Looking up base symbol '%s'\n", baseSymbol)
	}
	baseObj, err := e.Eval(baseSymbol, env)
	if err != nil {
		if core.Debug {
			fmt.Printf("DEBUG HandleDotNotation: Error evaluating base symbol '%s': %v\n", baseSymbol, err)
		}
		return nil, e.enrichErrorWithTraceback(err)
	}

	if core.Debug {
		fmt.Printf("DEBUG HandleDotNotation: Found base object of type %T\n", baseObj)
	}

	// Special handling for dictionaries
	if dict, isDictionary := baseObj.(*core.PythonicDict); isDictionary {
		// Make sure the dictionary has an evaluator reference
		if core.Debug {
			fmt.Printf("DEBUG HandleDotNotation: Setting evaluator on dictionary\n")
		}
		dict.SetEvaluator(e)

		// If we're accessing a method, handle it specially
		if len(parts) > 1 {
			methodName := parts[1]
			if core.Debug {
				fmt.Printf("DEBUG HandleDotNotation: Checking for dictionary method '%s'\n", methodName)
			}

			// Check if we have this as a dictionary method
			if dict.HasMethod(methodName) {
				if core.Debug {
					fmt.Printf("DEBUG HandleDotNotation: Found dictionary method '%s'\n", methodName)
				}
				// Return a function that, when called, will call the method on the dictionary
				return core.BuiltinFunc(func(args []core.LispValue, callEnv core.Environment) (core.LispValue, error) {
					return dict.CallMethod(methodName, args)
				}), nil
			}
		}
	}

	// Process the member access chain
	result := baseObj
	for i := 1; i < len(parts); i++ {
		memberName := parts[i]
		if core.Debug {
			fmt.Printf("DEBUG HandleDotNotation: Accessing member '%s' in chain\n", memberName)
		}

		// Get the member using FastGetPropFrom directly for optimized property access
		if memberValue, exists := core.FastGetPropFrom(result, memberName); exists {
			// Update the result for next iteration
			result = memberValue
			if core.Debug {
				fmt.Printf("DEBUG HandleDotNotation: Found member '%s', result type: %T\n", memberName, result)
			}
		} else {
			// Property not found
			if core.Debug {
				fmt.Printf("DEBUG HandleDotNotation: Property '%s' not found\n", memberName)
			}
			return nil, e.enrichErrorWithTraceback(fmt.Errorf("object has no property '%s'", memberName))
		}
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

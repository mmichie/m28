package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// DictCreateFunc implements the built-in dict() function
// It creates a new dictionary with the given key-value pairs
func DictCreateFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	dict := core.NewPythonicDict()

	// Store evaluator reference if available in the environment
	if evalAware, ok := env.(core.MappableEnvironment); ok {
		if eval, exists := evalAware.GetSymbolMap()[core.LispSymbol("EVALUATOR")]; exists {
			dict.SetEvaluator(eval.(core.Evaluator))
		}
	}

	// With no arguments, return an empty dict
	if len(args) == 0 {
		return dict, nil
	}

	// If the first argument is already a dict, create a copy
	if inputDict, ok := args[0].(*core.PythonicDict); ok && len(args) == 1 {
		// Create a copy
		var copyErr error
		inputDict.Iterate(func(k, v core.LispValue) error {
			if err := dict.Set(k, v); err != nil {
				copyErr = err
				return err
			}
			return nil
		})
		if copyErr != nil {
			return nil, copyErr
		}
		return dict, nil
	}

	// Parse key-value pairs from arguments
	if len(args)%2 != 0 {
		return nil, fmt.Errorf("dict() requires an even number of arguments to create key-value pairs")
	}

	for i := 0; i < len(args); i += 2 {
		key := args[i]
		value := args[i+1]
		if err := dict.Set(key, value); err != nil {
			return nil, err
		}
	}

	return dict, nil
}

// Register the dict function in init
func init() {
	core.RegisterBuiltin("dict", DictCreateFunc)
}

package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func RegisterIOBuiltins() {
	core.RegisterBuiltin("input", inputFunc)
	core.RegisterBuiltin("print", printFunc)
}

func inputFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("input() takes at most one argument")
	}
	if len(args) == 1 {
		prompt, ok := args[0].(string)
		if !ok {
			return nil, fmt.Errorf("input() argument must be a string")
		}
		fmt.Print(prompt)
	}
	var input string
	_, err := fmt.Scanln(&input)
	if err != nil {
		return nil, err
	}
	return input, nil
}

func printFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	// Default parameters (Python-like)
	end := "\n"
	sep := " "

	// Extract keyword arguments if present - look for a dictionary as the last argument
	values := args
	if len(args) > 0 {
		if dict, ok := args[len(args)-1].(*core.PythonicDict); ok {
			// Check for 'end' parameter
			if endVal, found := dict.Get("end"); found {
				if endStr, ok := endVal.(string); ok {
					end = endStr
				}
			}
			// Check for 'sep' parameter
			if sepVal, found := dict.Get("sep"); found {
				if sepStr, ok := sepVal.(string); ok {
					sep = sepStr
				}
			}
			// Only remove the last argument if it was used as kwargs
			values = args[:len(args)-1]
		}
	}

	// Print values with separator
	for i, arg := range values {
		if i > 0 {
			fmt.Print(sep)
		}
		fmt.Print(core.PrintValue(arg))
	}

	// Print end string
	fmt.Print(end)

	return core.PythonicNone{}, nil
}

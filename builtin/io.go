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
	for i, arg := range args {
		if i > 0 {
			fmt.Print(" ")
		}
		fmt.Print(core.PrintValue(arg))
	}
	fmt.Println()
	return core.PythonicNone{}, nil
}

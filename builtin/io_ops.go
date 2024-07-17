package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func RegisterIOOps() {
	core.RegisterBuiltin("print", printFunc)
}

func printFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	for _, arg := range args {
		fmt.Print(core.PrintValue(arg), " ")
	}
	fmt.Println()
	return nil, nil
}

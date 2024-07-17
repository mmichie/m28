package m28

import (
	"fmt"
)

func registerIOOps() {
	builtinFuncs["print"] = printFunc
}

func printFunc(args []LispValue, _ *Environment) (LispValue, error) {
	for _, arg := range args {
		fmt.Print(PrintValue(arg), " ")
	}
	fmt.Println()
	return nil, nil
}

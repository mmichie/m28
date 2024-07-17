package m28

import (
	"fmt"
	"strings"
)

func registerErrorHandling() {
	builtinFuncs["error"] = errorFunc
}

func errorFunc(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) == 0 {
		return nil, fmt.Errorf("Error")
	}
	errorMsg := make([]string, len(args))
	for i, arg := range args {
		errorMsg[i] = PrintValue(arg)
	}
	return nil, fmt.Errorf(strings.Join(errorMsg, " "))
}

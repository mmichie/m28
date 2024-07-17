package m28

import (
	"fmt"
)

func evalQuote(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("'quote' expects exactly one argument")
	}
	return args[0], nil
}

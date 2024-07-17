package m28

import (
	"fmt"
	"strconv"
	"strings"
)

func registerStringOps() {
	builtinFuncs["string-append"] = stringAppend
	builtinFuncs["number->string"] = numberToString
}

func stringAppend(args []LispValue, _ *Environment) (LispValue, error) {
	var parts []string
	for _, arg := range args {
		switch v := arg.(type) {
		case string:
			parts = append(parts, v)
		case LispSymbol:
			parts = append(parts, string(v))
		default:
			parts = append(parts, fmt.Sprint(v))
		}
	}
	return strings.Join(parts, ""), nil
}

func numberToString(args []LispValue, _ *Environment) (LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number->string expects exactly one argument")
	}
	num, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("number->string expects a number, got %T", args[0])
	}
	return strconv.FormatFloat(num, 'f', -1, 64), nil
}

package builtin

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

func RegisterStringOps() {
	core.RegisterBuiltin("string-append", stringAppend)
	core.RegisterBuiltin("number->string", numberToString)
	core.RegisterBuiltin("concatenate", concatenate)
}

func stringAppend(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	var parts []string
	for _, arg := range args {
		switch v := arg.(type) {
		case string:
			parts = append(parts, v)
		case core.LispSymbol:
			parts = append(parts, string(v))
		default:
			parts = append(parts, fmt.Sprint(v))
		}
	}
	return strings.Join(parts, ""), nil
}

func numberToString(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("number->string requires exactly one argument")
	}
	switch num := args[0].(type) {
	case float64:
		return strconv.FormatFloat(num, 'f', -1, 64), nil
	case int:
		return strconv.Itoa(num), nil
	default:
		return nil, fmt.Errorf("number->string requires a numeric argument, got %T", args[0])
	}
}

func concatenate(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("concatenate requires at least two arguments")
	}

	resultType, ok := args[0].(core.LispSymbol)
	if !ok || resultType != "string" {
		return nil, fmt.Errorf("concatenate first argument must be 'string")
	}

	var result strings.Builder
	for _, arg := range args[1:] {
		str, ok := arg.(string)
		if !ok {
			return nil, fmt.Errorf("concatenate arguments must be strings")
		}
		result.WriteString(str)
	}

	return result.String(), nil
}

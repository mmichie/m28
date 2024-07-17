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

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
	core.RegisterBuiltin("string->number", stringToNumber)
	core.RegisterBuiltin("concatenate", concatenate)
	core.RegisterBuiltin("string-upcase", stringUpcase)
	core.RegisterBuiltin("print-value", printValue)
	core.RegisterBuiltin("to-number", toNumber)
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

func stringUpcase(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string-upcase expects 1 argument, got %d", len(args))
	}

	str, ok := args[0].(string)
	if !ok {
		return nil, fmt.Errorf("string-upcase expects a string argument, got %T", args[0])
	}

	return strings.ToUpper(str), nil
}

func stringToNumber(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("string->number requires exactly one argument")
	}

	switch arg := args[0].(type) {
	case string:
		num, err := strconv.ParseFloat(arg, 64)
		if err != nil {
			return nil, fmt.Errorf("invalid number format: %s", arg)
		}
		return num, nil
	case float64:
		return arg, nil
	case int:
		return float64(arg), nil
	default:
		return nil, fmt.Errorf("string->number requires a string or number argument, got %T", arg)
	}
}

func printValue(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("print-value requires exactly one argument")
	}
	return core.PrintValue(args[0]), nil
}

func toNumber(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("to-number requires exactly one argument")
	}

	switch v := args[0].(type) {
	case float64:
		return v, nil
	case int:
		return float64(v), nil
	case string:
		num, err := strconv.ParseFloat(v, 64)
		if err != nil {
			return nil, fmt.Errorf("invalid number format: %s", v)
		}
		return num, nil
	default:
		str := core.PrintValue(v)
		num, err := strconv.ParseFloat(str, 64)
		if err != nil {
			return nil, fmt.Errorf("cannot convert to number: %s", str)
		}
		return num, nil
	}
}

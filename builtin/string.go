package builtin

import (
	"fmt"
	"strconv"

	"github.com/mmichie/m28/core"
)

func RegisterStringBuiltins() {
	core.RegisterBuiltin("ascii", asciiFunc)
	core.RegisterBuiltin("chr", chrFunc)
	core.RegisterBuiltin("format", formatFunc)
	core.RegisterBuiltin("ord", ordFunc)
	core.RegisterBuiltin("repr", reprFunc)
	core.RegisterBuiltin("str", strFunc)
}

func asciiFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("ascii() takes exactly one argument")
	}
	return strconv.Quote(fmt.Sprint(args[0])), nil
}

func chrFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("chr() takes exactly one argument")
	}
	i, ok := args[0].(float64)
	if !ok {
		return nil, fmt.Errorf("chr() argument must be an integer")
	}
	return string(rune(i)), nil
}

func formatFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("format() takes at least one argument")
	}
	formatStr, ok := args[0].(string)
	if !ok {
		return nil, fmt.Errorf("format() first argument must be a string")
	}
	// Convert []core.LispValue to []interface{}
	interfaceArgs := make([]interface{}, len(args)-1)
	for i, arg := range args[1:] {
		interfaceArgs[i] = arg
	}
	return fmt.Sprintf(formatStr, interfaceArgs...), nil
}

func ordFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("ord() takes exactly one argument")
	}
	s, ok := args[0].(string)
	if !ok || len(s) != 1 {
		return nil, fmt.Errorf("ord() expected a character, but string of length %d found", len(s))
	}
	return float64(s[0]), nil
}

func reprFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("repr() takes exactly one argument")
	}
	return fmt.Sprintf("%#v", args[0]), nil
}

func strFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("str() takes exactly one argument")
	}
	return PrintValueOverride(args[0]), nil
}

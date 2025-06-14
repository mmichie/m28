// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// RegisterStringLegacyFunctions registers legacy string functions for backward compatibility
func RegisterStringLegacyFunctions(ctx *core.Context) {
	// Note: "length" function is now registered in list.go
	ctx.Define("str-len", core.NewBuiltinFunction(StrLenFunc)) // Legacy name for compatibility
	ctx.Define("str-concat", core.NewBuiltinFunction(StrConcatFunc))
}

// StrLenFunc returns the length of a string (legacy function)
func StrLenFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("str-len requires exactly 1 argument, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("str-len requires a string argument, got %s", args[0].Type())
	}

	return core.NumberValue(len([]rune(string(str)))), nil
}

// StrConcatFunc concatenates multiple strings (legacy function)
func StrConcatFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.StringValue(""), nil
	}

	var result string
	for i, arg := range args {
		str, ok := arg.(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("str-concat requires all arguments to be strings, argument %d is %s", i+1, arg.Type())
		}
		result += string(str)
	}

	return core.StringValue(result), nil
}

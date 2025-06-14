// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// RegisterStringSearchFunctions registers string search and test operation functions
func RegisterStringSearchFunctions(ctx *core.Context) {
	ctx.Define("contains", core.NewBuiltinFunction(ContainsFunc))
	ctx.Define("starts-with", core.NewBuiltinFunction(StartsWithFunc))
	ctx.Define("ends-with", core.NewBuiltinFunction(EndsWithFunc))
	ctx.Define("find", core.NewBuiltinFunction(FindFunc))
	ctx.Define("count", core.NewBuiltinFunction(CountFunc))
}

// ContainsFunc checks if a string contains a substring
func ContainsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("contains requires exactly 2 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("contains requires a string as first argument, got %s", args[0].Type())
	}

	substr, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("contains requires a string as second argument, got %s", args[1].Type())
	}

	return core.BoolValue(strings.Contains(string(str), string(substr))), nil
}

// StartsWithFunc checks if a string starts with a prefix
func StartsWithFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("starts-with requires exactly 2 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("starts-with requires a string as first argument, got %s", args[0].Type())
	}

	prefix, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("starts-with requires a string as second argument, got %s", args[1].Type())
	}

	return core.BoolValue(strings.HasPrefix(string(str), string(prefix))), nil
}

// EndsWithFunc checks if a string ends with a suffix
func EndsWithFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("ends-with requires exactly 2 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("ends-with requires a string as first argument, got %s", args[0].Type())
	}

	suffix, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("ends-with requires a string as second argument, got %s", args[1].Type())
	}

	return core.BoolValue(strings.HasSuffix(string(str), string(suffix))), nil
}

// FindFunc finds the index of a substring in a string
func FindFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 4 {
		return nil, fmt.Errorf("find requires 2 to 4 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("find requires a string as first argument, got %s", args[0].Type())
	}

	substr, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("find requires a string as second argument, got %s", args[1].Type())
	}

	s := string(str)
	sub := string(substr)

	// Optional start parameter
	start := 0
	if len(args) >= 3 {
		n, ok := args[2].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("find start must be a number, got %s", args[2].Type())
		}
		start = int(n)
		if start < 0 {
			start = 0
		}
	}

	// Optional end parameter
	end := len(s)
	if len(args) == 4 {
		n, ok := args[3].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("find end must be a number, got %s", args[3].Type())
		}
		end = int(n)
		if end > len(s) {
			end = len(s)
		}
	}

	// Search within the specified range
	if start >= end || start >= len(s) {
		return core.NumberValue(-1), nil
	}

	searchStr := s[start:end]
	index := strings.Index(searchStr, sub)

	if index == -1 {
		return core.NumberValue(-1), nil
	}

	// Adjust index to account for start offset
	return core.NumberValue(index + start), nil
}

// CountFunc counts occurrences of a substring in a string
func CountFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 4 {
		return nil, fmt.Errorf("count requires 2 to 4 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("count requires a string as first argument, got %s", args[0].Type())
	}

	substr, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("count requires a string as second argument, got %s", args[1].Type())
	}

	s := string(str)
	sub := string(substr)

	// Optional start parameter
	start := 0
	if len(args) >= 3 {
		n, ok := args[2].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("count start must be a number, got %s", args[2].Type())
		}
		start = int(n)
		if start < 0 {
			start = 0
		}
	}

	// Optional end parameter
	end := len(s)
	if len(args) == 4 {
		n, ok := args[3].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("count end must be a number, got %s", args[3].Type())
		}
		end = int(n)
		if end > len(s) {
			end = len(s)
		}
	}

	// Count within the specified range
	if start >= end || start >= len(s) {
		return core.NumberValue(0), nil
	}

	searchStr := s[start:end]
	count := strings.Count(searchStr, sub)

	return core.NumberValue(count), nil
}

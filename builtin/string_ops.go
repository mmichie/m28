// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

// RegisterStringOpsFunctions registers basic string operation functions
func RegisterStringOpsFunctions(ctx *core.Context) {
	// Case operations
	ctx.Define("upper", core.NewBuiltinFunction(UpperFunc))
	ctx.Define("lower", core.NewBuiltinFunction(LowerFunc))

	// Trimming operations
	ctx.Define("trim", core.NewBuiltinFunction(TrimFunc))
	ctx.Define("strip", core.NewBuiltinFunction(StripFunc))
	ctx.Define("lstrip", core.NewBuiltinFunction(LStripFunc))
	ctx.Define("rstrip", core.NewBuiltinFunction(RStripFunc))

	// Manipulation operations
	ctx.Define("replace", core.NewBuiltinFunction(ReplaceFunc))
	ctx.Define("split", core.NewBuiltinFunction(SplitFunc))
	ctx.Define("join", core.NewBuiltinFunction(JoinFunc))
	ctx.Define("substring", core.NewBuiltinFunction(SubstringFunc))
}

// UpperFunc converts a string to uppercase
func UpperFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("upper requires exactly 1 argument, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("upper requires a string argument, got %s", args[0].Type())
	}

	return core.StringValue(strings.ToUpper(string(str))), nil
}

// LowerFunc converts a string to lowercase
func LowerFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("lower requires exactly 1 argument, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("lower requires a string argument, got %s", args[0].Type())
	}

	return core.StringValue(strings.ToLower(string(str))), nil
}

// TrimFunc removes whitespace from both ends of a string
func TrimFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("trim requires exactly 1 argument, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("trim requires a string argument, got %s", args[0].Type())
	}

	return core.StringValue(strings.TrimSpace(string(str))), nil
}

// StripFunc is an alias for trim (Python compatibility)
func StripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	return TrimFunc(args, ctx)
}

// LStripFunc removes whitespace from the left side of a string
func LStripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("lstrip requires 1 or 2 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("lstrip requires a string as first argument, got %s", args[0].Type())
	}

	// Default: strip whitespace
	if len(args) == 1 {
		return core.StringValue(strings.TrimLeft(string(str), " \t\n\r\f\v")), nil
	}

	// Custom characters to strip
	chars, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("lstrip chars argument must be a string, got %s", args[1].Type())
	}

	return core.StringValue(strings.TrimLeft(string(str), string(chars))), nil
}

// RStripFunc removes whitespace from the right side of a string
func RStripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("rstrip requires 1 or 2 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("rstrip requires a string as first argument, got %s", args[0].Type())
	}

	// Default: strip whitespace
	if len(args) == 1 {
		return core.StringValue(strings.TrimRight(string(str), " \t\n\r\f\v")), nil
	}

	// Custom characters to strip
	chars, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("rstrip chars argument must be a string, got %s", args[1].Type())
	}

	return core.StringValue(strings.TrimRight(string(str), string(chars))), nil
}

// ReplaceFunc replaces occurrences of a substring in a string
func ReplaceFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 3 || len(args) > 4 {
		return nil, fmt.Errorf("replace requires 3 or 4 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("replace requires a string as first argument, got %s", args[0].Type())
	}

	oldStr, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("replace requires a string as second argument, got %s", args[1].Type())
	}

	newStr, ok := args[2].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("replace requires a string as third argument, got %s", args[2].Type())
	}

	// Optional count parameter
	count := -1
	if len(args) == 4 {
		n, ok := args[3].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("replace count must be a number, got %s", args[3].Type())
		}
		count = int(n)
	}

	result := strings.Replace(string(str), string(oldStr), string(newStr), count)
	return core.StringValue(result), nil
}

// SplitFunc splits a string by a delimiter
func SplitFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 3 {
		return nil, fmt.Errorf("split requires 1 to 3 arguments, got %d", len(args))
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("split requires a string as first argument, got %s", args[0].Type())
	}

	// Default: split on whitespace
	if len(args) == 1 {
		fields := strings.Fields(string(str))
		result := make(core.ListValue, len(fields))
		for i, field := range fields {
			result[i] = core.StringValue(field)
		}
		return result, nil
	}

	// Get delimiter
	delim, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("split delimiter must be a string, got %s", args[1].Type())
	}

	// Optional maxsplit parameter
	maxSplit := -1
	if len(args) == 3 {
		n, ok := args[2].(core.NumberValue)
		if !ok {
			return nil, fmt.Errorf("split maxsplit must be a number, got %s", args[2].Type())
		}
		maxSplit = int(n)
	}

	// Split the string
	var parts []string
	if maxSplit < 0 {
		parts = strings.Split(string(str), string(delim))
	} else {
		parts = strings.SplitN(string(str), string(delim), maxSplit+1)
	}

	// Convert to ListValue
	result := make(core.ListValue, len(parts))
	for i, part := range parts {
		result[i] = core.StringValue(part)
	}

	return result, nil
}

// JoinFunc joins a list of strings with a separator
func JoinFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("join requires exactly 2 arguments, got %d", len(args))
	}

	sep, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("join requires a string separator as first argument, got %s", args[0].Type())
	}

	list, ok := args[1].(core.ListValue)
	if !ok {
		return nil, fmt.Errorf("join requires a list as second argument, got %s", args[1].Type())
	}

	// Convert list elements to strings
	strs := make([]string, len(list))
	for i, elem := range list {
		switch v := elem.(type) {
		case core.StringValue:
			strs[i] = string(v)
		default:
			strs[i] = elem.String()
		}
	}

	return core.StringValue(strings.Join(strs, string(sep))), nil
}

// SubstringFunc extracts a substring from a string
func SubstringFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("substring requires 2 or 3 arguments (string, start[, end]), got %d", len(args))
	}

	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("substring: first argument must be a string, got %s", args[0].Type())
	}

	// Get start index
	var start int
	switch v := args[1].(type) {
	case core.NumberValue:
		start = int(v)
	case core.StringValue:
		// Try to parse as number
		n, err := strconv.Atoi(string(v))
		if err != nil {
			return nil, fmt.Errorf("substring: start index must be a number, got %s", string(v))
		}
		start = n
	default:
		return nil, fmt.Errorf("substring: start index must be a number, got %s", args[1].Type())
	}

	s := string(str)
	// Handle negative indices
	if start < 0 {
		start = len(s) + start
	}
	if start < 0 {
		start = 0
	}
	if start > len(s) {
		return core.StringValue(""), nil
	}

	// Get end index (optional)
	end := len(s)
	if len(args) == 3 {
		switch v := args[2].(type) {
		case core.NumberValue:
			end = int(v)
		case core.StringValue:
			// Try to parse as number
			n, err := strconv.Atoi(string(v))
			if err != nil {
				return nil, fmt.Errorf("substring: end index must be a number, got %s", string(v))
			}
			end = n
		default:
			return nil, fmt.Errorf("substring: end index must be a number, got %s", args[2].Type())
		}

		// Handle negative indices
		if end < 0 {
			end = len(s) + end
		}
		if end < 0 {
			end = 0
		}
		if end > len(s) {
			end = len(s)
		}
	}

	// Extract substring
	if start >= end {
		return core.StringValue(""), nil
	}

	return core.StringValue(s[start:end]), nil
}

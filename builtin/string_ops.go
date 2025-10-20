// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/common/validation"
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
	v := validation.NewArgs("upper", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	return core.StringValue(strings.ToUpper(str)), nil
}

// LowerFunc converts a string to lowercase
func LowerFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("lower", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	return core.StringValue(strings.ToLower(str)), nil
}

// TrimFunc removes whitespace from both ends of a string
func TrimFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("trim", args)
	if err := v.Exact(1); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	return core.StringValue(strings.TrimSpace(str)), nil
}

// StripFunc is an alias for trim (Python compatibility)
func StripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	return TrimFunc(args, ctx)
}

// LStripFunc removes whitespace from the left side of a string
func LStripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("lstrip", args)
	if err := v.Range(1, 2); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	// Get optional chars argument, default to whitespace
	chars, _ := v.GetStringOrDefault(1, " \t\n\r\f\v")

	return core.StringValue(strings.TrimLeft(str, chars)), nil
}

// RStripFunc removes whitespace from the right side of a string
func RStripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("rstrip", args)
	if err := v.Range(1, 2); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	// Get optional chars argument, default to whitespace
	chars, _ := v.GetStringOrDefault(1, " \t\n\r\f\v")

	return core.StringValue(strings.TrimRight(str, chars)), nil
}

// ReplaceFunc replaces occurrences of a substring in a string
func ReplaceFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("replace", args)
	if err := v.Range(3, 4); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	oldStr, err := v.GetString(1)
	if err != nil {
		return nil, err
	}

	newStr, err := v.GetString(2)
	if err != nil {
		return nil, err
	}

	// Optional count parameter, default -1 (replace all)
	count := -1
	if v.Count() == 4 {
		n, err := v.GetNumber(3)
		if err != nil {
			return nil, err
		}
		count = int(n)
	}

	result := strings.Replace(str, oldStr, newStr, count)
	return core.StringValue(result), nil
}

// SplitFunc splits a string by a delimiter
func SplitFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("split", args)
	if err := v.Range(1, 3); err != nil {
		return nil, err
	}

	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	// Default: split on whitespace
	if v.Count() == 1 {
		fields := strings.Fields(str)
		result := make([]core.Value, len(fields))
		for i, field := range fields {
			result[i] = core.StringValue(field)
		}
		return core.NewList(result...), nil
	}

	// Get delimiter
	delim, err := v.GetString(1)
	if err != nil {
		return nil, err
	}

	// Optional maxsplit parameter
	maxSplit := -1
	if v.Count() == 3 {
		n, err := v.GetNumber(2)
		if err != nil {
			return nil, err
		}
		maxSplit = int(n)
	}

	// Split the string
	var parts []string
	if maxSplit < 0 {
		parts = strings.Split(str, delim)
	} else {
		parts = strings.SplitN(str, delim, maxSplit+1)
	}

	// Convert to ListValue
	result := make([]core.Value, len(parts))
	for i, part := range parts {
		result[i] = core.StringValue(part)
	}

	return core.NewList(result...), nil
}

// JoinFunc joins a list of strings with a separator
func JoinFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("join", args)
	if err := v.Exact(2); err != nil {
		return nil, err
	}

	sep, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	list, err := v.GetList(1)
	if err != nil {
		return nil, err
	}

	// Convert list elements to strings
	strs := make([]string, list.Len())
	for i, elem := range list.Items() {
		switch v := elem.(type) {
		case core.StringValue:
			strs[i] = string(v)
		default:
			strs[i] = elem.String()
		}
	}

	return core.StringValue(strings.Join(strs, sep)), nil
}

// SubstringFunc extracts a substring from a string
func SubstringFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	v := validation.NewArgs("substring", args)
	if err := v.Range(2, 3); err != nil {
		return nil, err
	}

	// Get the string
	str, err := v.GetString(0)
	if err != nil {
		return nil, err
	}

	// Get start index - support both number and string
	var start int
	if num, err := v.GetNumber(1); err == nil {
		start = int(num)
	} else if s, err := v.GetString(1); err == nil {
		// Try to parse as number
		n, err := strconv.Atoi(s)
		if err != nil {
			return nil, fmt.Errorf("substring: start index must be a number, got %s", s)
		}
		start = n
	} else {
		return nil, fmt.Errorf("substring: start index must be a number, got %s", args[1].Type())
	}

	// Handle negative indices
	if start < 0 {
		start = len(str) + start
	}
	if start < 0 {
		start = 0
	}
	if start > len(str) {
		return core.StringValue(""), nil
	}

	// Get end index (optional)
	end := len(str)
	if v.Count() == 3 {
		if num, err := v.GetNumber(2); err == nil {
			end = int(num)
		} else if s, err := v.GetString(2); err == nil {
			// Try to parse as number
			n, err := strconv.Atoi(s)
			if err != nil {
				return nil, fmt.Errorf("substring: end index must be a number, got %s", s)
			}
			end = n
		} else {
			return nil, fmt.Errorf("substring: end index must be a number, got %s", args[2].Type())
		}

		// Handle negative indices
		if end < 0 {
			end = len(str) + end
		}
		if end < 0 {
			end = 0
		}
		if end > len(str) {
			end = len(str)
		}
	}

	// Extract substring
	if start >= end {
		return core.StringValue(""), nil
	}

	return core.StringValue(str[start:end]), nil
}

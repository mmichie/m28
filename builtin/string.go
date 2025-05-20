// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"fmt"
	"strings"
	"unicode/utf8"
	
	"github.com/mmichie/m28/core"
)

// RegisterStringFunctions registers string-related functions in the global context
func RegisterStringFunctions(ctx *core.Context) {
	// String creation and conversion
	ctx.Define("str", core.NewBuiltinFunction(StrFunc))
	ctx.Define("format", core.NewBuiltinFunction(FormatFunc))
	
	// String properties
	ctx.Define("length", core.NewBuiltinFunction(LengthFunc)) // This function is already in list.go
	ctx.Define("str-len", core.NewBuiltinFunction(StrLenFunc)) // Legacy name for compatibility
	
	// String operations
	ctx.Define("upper", core.NewBuiltinFunction(UpperFunc))
	ctx.Define("lower", core.NewBuiltinFunction(LowerFunc))
	ctx.Define("trim", core.NewBuiltinFunction(TrimFunc))
	ctx.Define("replace", core.NewBuiltinFunction(ReplaceFunc))
	ctx.Define("split", core.NewBuiltinFunction(SplitFunc))
	ctx.Define("join", core.NewBuiltinFunction(JoinFunc))
	ctx.Define("str-concat", core.NewBuiltinFunction(StrConcatFunc)) // Legacy name for compatibility
	ctx.Define("contains", core.NewBuiltinFunction(ContainsFunc))
	ctx.Define("starts-with", core.NewBuiltinFunction(StartsWithFunc))
	ctx.Define("ends-with", core.NewBuiltinFunction(EndsWithFunc))
	ctx.Define("substring", core.NewBuiltinFunction(SubstringFunc))
}

// StrFunc converts a value to a string
func StrFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("str requires 1 argument")
	}
	
	// Use the PrintValue function to convert to string
	return core.StringValue(core.PrintValueWithoutQuotes(args[0])), nil
}

// FormatFunc formats a string with placeholders
func FormatFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 {
		return nil, fmt.Errorf("format requires at least 1 argument: format string")
	}
	
	// Get the format string
	formatStr, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Prepare values for formatting
	values := make([]interface{}, len(args)-1)
	for i, arg := range args[1:] {
		// Convert to Go native types for fmt.Sprintf
		switch v := arg.(type) {
		case core.NumberValue:
			values[i] = float64(v)
		case core.StringValue:
			values[i] = string(v)
		case core.BoolValue:
			values[i] = bool(v)
		default:
			values[i] = core.PrintValueWithoutQuotes(arg)
		}
	}
	
	// Format the string
	result := fmt.Sprintf(string(formatStr), values...)
	return core.StringValue(result), nil
}

// UpperFunc converts a string to uppercase
func UpperFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("upper requires 1 argument: string")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("argument must be a string, got %s", args[0].Type())
	}
	
	// Convert to uppercase
	return core.StringValue(strings.ToUpper(string(str))), nil
}

// LowerFunc converts a string to lowercase
func LowerFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("lower requires 1 argument: string")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("argument must be a string, got %s", args[0].Type())
	}
	
	// Convert to lowercase
	return core.StringValue(strings.ToLower(string(str))), nil
}

// TrimFunc removes whitespace from the beginning and end of a string
func TrimFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("trim requires 1 or 2 arguments: string[, chars]")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Default is to trim whitespace
	if len(args) == 1 {
		return core.StringValue(strings.TrimSpace(string(str))), nil
	}
	
	// Get the characters to trim
	cutset, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}
	
	// Trim the specified characters
	return core.StringValue(strings.Trim(string(str), string(cutset))), nil
}

// ReplaceFunc replaces all occurrences of a substring
func ReplaceFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 3 {
		return nil, fmt.Errorf("replace requires 3 arguments: string, old, new")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Get the old substring
	old, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}
	
	// Get the new substring
	new, ok := args[2].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("third argument must be a string, got %s", args[2].Type())
	}
	
	// Replace all occurrences
	return core.StringValue(strings.ReplaceAll(string(str), string(old), string(new))), nil
}

// SplitFunc splits a string by a delimiter
func SplitFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("split requires 2 arguments: string, delimiter")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Get the delimiter
	delimiter, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}
	
	// Split the string
	parts := strings.Split(string(str), string(delimiter))
	
	// Convert to a list of StringValues
	result := make(core.ListValue, len(parts))
	for i, part := range parts {
		result[i] = core.StringValue(part)
	}
	
	return result, nil
}

// JoinFunc joins a list of strings with a delimiter
func JoinFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("join requires 2 arguments: list, delimiter")
	}
	
	// Get the list
	list, ok := args[0].(core.ListValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a list, got %s", args[0].Type())
	}
	
	// Get the delimiter
	delimiter, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}
	
	// Convert list elements to strings
	strs := make([]string, len(list))
	for i, item := range list {
		strs[i] = core.PrintValueWithoutQuotes(item)
	}
	
	// Join the strings
	return core.StringValue(strings.Join(strs, string(delimiter))), nil
}

// ContainsFunc checks if a string contains a substring
func ContainsFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("contains requires 2 arguments: string, substring")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Get the substring
	substr, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}
	
	// Check if the string contains the substring
	return core.BoolValue(strings.Contains(string(str), string(substr))), nil
}

// StartsWithFunc checks if a string starts with a prefix
func StartsWithFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("starts-with requires 2 arguments: string, prefix")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Get the prefix
	prefix, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}
	
	// Check if the string starts with the prefix
	return core.BoolValue(strings.HasPrefix(string(str), string(prefix))), nil
}

// EndsWithFunc checks if a string ends with a suffix
func EndsWithFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("ends-with requires 2 arguments: string, suffix")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Get the suffix
	suffix, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}
	
	// Check if the string ends with the suffix
	return core.BoolValue(strings.HasSuffix(string(str), string(suffix))), nil
}

// SubstringFunc extracts a substring
func SubstringFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 2 || len(args) > 3 {
		return nil, fmt.Errorf("substring requires 2 or 3 arguments: string, start[, end]")
	}
	
	// Get the string
	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}
	
	// Get the start index
	var start int
	if num, ok := args[1].(core.NumberValue); ok {
		start = int(num)
		if start < 0 {
			// Negative index counts from the end
			start = utf8.RuneCountInString(string(str)) + start
			if start < 0 {
				start = 0
			}
		}
	} else {
		return nil, fmt.Errorf("second argument must be a number, got %s", args[1].Type())
	}
	
	// Get the end index (optional)
	var end int
	if len(args) == 3 {
		if num, ok := args[2].(core.NumberValue); ok {
			end = int(num)
			if end < 0 {
				// Negative index counts from the end
				end = utf8.RuneCountInString(string(str)) + end
				if end < 0 {
					end = 0
				}
			}
		} else {
			return nil, fmt.Errorf("third argument must be a number, got %s", args[2].Type())
		}
	} else {
		// If no end is specified, go to the end of the string
		end = utf8.RuneCountInString(string(str))
	}
	
	// Bounds checking
	if start > end {
		return core.StringValue(""), nil
	}
	
	// Handle UTF-8 properly by converting to runes
	runes := []rune(string(str))
	if start >= len(runes) {
		return core.StringValue(""), nil
	}
	if end > len(runes) {
		end = len(runes)
	}
	
	// Extract the substring
	return core.StringValue(string(runes[start:end])), nil
}

// Note: LengthFunc is imported from list.go

// StrLenFunc returns the length of a string (legacy function for backward compatibility)
func StrLenFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("str-len requires 1 argument")
	}
	
	if str, ok := args[0].(core.StringValue); ok {
		return core.NumberValue(len(str)), nil
	}
	
	return nil, fmt.Errorf("str-len expects a string, got %v", args[0].Type())
}

// StrConcatFunc concatenates strings (legacy function for backward compatibility)
func StrConcatFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	var sb strings.Builder
	
	for _, arg := range args {
		sb.WriteString(core.PrintValueWithoutQuotes(arg))
	}
	
	return core.StringValue(sb.String()), nil
}
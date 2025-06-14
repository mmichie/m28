// Package builtin provides standard library functions for the M28 language.
package builtin

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/mmichie/m28/core"
)

// RegisterStringFunctions registers string-related functions in the global context
func RegisterStringFunctions(ctx *core.Context) {
	// String creation and conversion
	// str is defined in type_checking.go with full Python-style functionality
	ctx.Define("format", core.NewBuiltinFunction(FormatFunc))
	ctx.Define("str-format", core.NewBuiltinFunction(StrFormatFunc))
	ctx.Define("format-expr", core.NewBuiltinFunction(FormatExprFunc))

	// String properties
	// Note: "length" function is now registered in list.go
	ctx.Define("str-len", core.NewBuiltinFunction(StrLenFunc)) // Legacy name for compatibility

	// String operations
	ctx.Define("upper", core.NewBuiltinFunction(UpperFunc))
	ctx.Define("lower", core.NewBuiltinFunction(LowerFunc))
	ctx.Define("trim", core.NewBuiltinFunction(TrimFunc))
	ctx.Define("strip", core.NewBuiltinFunction(StripFunc))
	ctx.Define("lstrip", core.NewBuiltinFunction(LStripFunc))
	ctx.Define("rstrip", core.NewBuiltinFunction(RStripFunc))
	ctx.Define("replace", core.NewBuiltinFunction(ReplaceFunc))
	ctx.Define("split", core.NewBuiltinFunction(SplitFunc))
	ctx.Define("join", core.NewBuiltinFunction(JoinFunc))
	ctx.Define("str-concat", core.NewBuiltinFunction(StrConcatFunc)) // Legacy name for compatibility
	ctx.Define("contains", core.NewBuiltinFunction(ContainsFunc))
	ctx.Define("starts-with", core.NewBuiltinFunction(StartsWithFunc))
	ctx.Define("ends-with", core.NewBuiltinFunction(EndsWithFunc))
	ctx.Define("substring", core.NewBuiltinFunction(SubstringFunc))
	ctx.Define("find", core.NewBuiltinFunction(FindFunc))
	ctx.Define("count", core.NewBuiltinFunction(CountFunc))
}

// FormatExprFunc handles format expressions from enhanced f-strings
func FormatExprFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	formatted, err := formatExpression(args, ctx)
	if err != nil {
		return nil, err
	}
	return core.StringValue(formatted), nil
}

// StrFormatFunc concatenates strings and values for f-string formatting
func StrFormatFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) == 0 {
		return core.StringValue(""), nil
	}

	var result strings.Builder
	for _, arg := range args {
		switch v := arg.(type) {
		case core.StringValue:
			result.WriteString(string(v))
		case core.ListValue:
			// Check if this is a format expression
			if len(v) >= 2 {
				if sym, ok := v[0].(core.SymbolValue); ok && string(sym) == "format-expr" {
					// Handle format expression
					formatted, err := formatExpression(v[1:], ctx)
					if err != nil {
						return nil, err
					}
					result.WriteString(formatted)
					continue
				}
			}
			// Regular list - convert to string
			result.WriteString(core.PrintValueWithoutQuotes(arg))
		default:
			result.WriteString(core.PrintValueWithoutQuotes(arg))
		}
	}

	return core.StringValue(result.String()), nil
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

// StripFunc removes whitespace from both ends (Python-style)
func StripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	return TrimFunc(args, ctx) // strip is alias for trim
}

// LStripFunc removes whitespace from the left side
func LStripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("lstrip requires 1 or 2 arguments: string[, chars]")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}

	if len(args) == 1 {
		return core.StringValue(strings.TrimLeft(string(str), " \t\n\r")), nil
	}

	cutset, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}

	return core.StringValue(strings.TrimLeft(string(str), string(cutset))), nil
}

// RStripFunc removes whitespace from the right side
func RStripFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) < 1 || len(args) > 2 {
		return nil, fmt.Errorf("rstrip requires 1 or 2 arguments: string[, chars]")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}

	if len(args) == 1 {
		return core.StringValue(strings.TrimRight(string(str), " \t\n\r")), nil
	}

	cutset, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}

	return core.StringValue(strings.TrimRight(string(str), string(cutset))), nil
}

// FindFunc finds the index of a substring
func FindFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("find requires 2 arguments: string, substring")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}

	substr, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}

	index := strings.Index(string(str), string(substr))
	return core.NumberValue(index), nil
}

// CountFunc counts occurrences of a substring
func CountFunc(args []core.Value, ctx *core.Context) (core.Value, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("count requires 2 arguments: string, substring")
	}

	str, ok := args[0].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("first argument must be a string, got %s", args[0].Type())
	}

	substr, ok := args[1].(core.StringValue)
	if !ok {
		return nil, fmt.Errorf("second argument must be a string, got %s", args[1].Type())
	}

	count := strings.Count(string(str), string(substr))
	return core.NumberValue(count), nil
}

// formatExpression handles enhanced format expressions from f-strings
func formatExpression(args []core.Value, ctx *core.Context) (string, error) {
	if len(args) == 0 {
		return "", fmt.Errorf("format expression requires at least one argument")
	}

	// Get the value to format
	value := args[0]

	// Default formatting
	result := core.PrintValueWithoutQuotes(value)

	// Parse the enhanced format spec string if present
	if len(args) > 1 {
		if specStr, ok := args[1].(core.StringValue); ok {
			spec := string(specStr)

			// The spec may contain format spec, conversion, and self-doc separated by |
			parts := strings.Split(spec, "|")
			formatSpec := ""
			conversion := ""
			selfDocExpr := ""

			for _, part := range parts {
				if strings.HasPrefix(part, "!") {
					conversion = part[1:]
				} else if strings.HasPrefix(part, "=") {
					selfDocExpr = part[1:]
				} else if part != "" {
					formatSpec = part
				}
			}

			// Apply conversion first if present
			if conversion != "" {
				result = applyConversion(value, conversion)
				value = core.StringValue(result) // Convert for format spec
			}

			// Apply format spec if present
			if formatSpec != "" {
				result = applyFormatSpecString(value, formatSpec)
			}

			// Apply self-documenting if present
			if selfDocExpr != "" {
				result = fmt.Sprintf("%s=%s", selfDocExpr, result)
			}
		}
	}

	return result, nil
}

// applyFormatSpecString parses and applies a Python-style format specification string
func applyFormatSpecString(value core.Value, specStr string) string {
	if specStr == "" {
		return core.PrintValueWithoutQuotes(value)
	}

	// Parse the format spec string
	spec := parseFormatSpecString(specStr)

	// Apply formatting based on value type
	switch v := value.(type) {
	case core.NumberValue:
		return formatNumber(float64(v), spec.Fill, spec.Align, spec.Sign, spec.Alt, spec.Zero, spec.Width, spec.Precision, spec.Type, spec.Thousands)
	case core.StringValue:
		return formatString(string(v), spec.Fill, spec.Align, spec.Width, spec.Precision)
	default:
		str := core.PrintValueWithoutQuotes(value)
		return formatString(str, spec.Fill, spec.Align, spec.Width, spec.Precision)
	}
}

// FormatSpec represents a parsed format specification
type FormatSpec struct {
	Fill      rune
	Align     rune // '<', '>', '^', '='
	Sign      rune // '+', '-', ' '
	Alt       bool // '#' flag
	Zero      bool // '0' flag
	Width     int
	Precision int  // -1 if not specified
	Type      rune // 'f', 'd', 's', etc.
	Thousands bool // ',' flag for thousands separator
}

// parseFormatSpecString parses a Python format specification string
func parseFormatSpecString(spec string) FormatSpec {
	fs := FormatSpec{
		Fill:      ' ',
		Precision: -1,
	}

	if spec == "" {
		return fs
	}

	i := 0

	// Parse fill and align
	if len(spec) >= 2 {
		possibleAlign := rune(spec[1])
		if possibleAlign == '<' || possibleAlign == '>' || possibleAlign == '^' || possibleAlign == '=' {
			fs.Fill = rune(spec[0])
			fs.Align = possibleAlign
			i = 2
		} else if len(spec) >= 1 {
			possibleAlign = rune(spec[0])
			if possibleAlign == '<' || possibleAlign == '>' || possibleAlign == '^' || possibleAlign == '=' {
				fs.Align = possibleAlign
				i = 1
			}
		}
	}

	// Parse sign
	if i < len(spec) {
		sign := rune(spec[i])
		if sign == '+' || sign == '-' || sign == ' ' {
			fs.Sign = sign
			i++
		}
	}

	// Parse # flag
	if i < len(spec) && spec[i] == '#' {
		fs.Alt = true
		i++
	}

	// Parse 0 flag
	if i < len(spec) && spec[i] == '0' {
		fs.Zero = true
		if fs.Align == 0 {
			fs.Align = '='
		}
		i++
	}

	// Parse width
	widthStart := i
	for i < len(spec) && spec[i] >= '0' && spec[i] <= '9' {
		i++
	}
	if i > widthStart {
		fmt.Sscanf(spec[widthStart:i], "%d", &fs.Width)
	}

	// Parse thousands separator
	if i < len(spec) && spec[i] == ',' {
		fs.Thousands = true
		i++
	}

	// Parse precision
	if i < len(spec) && spec[i] == '.' {
		i++
		precStart := i
		for i < len(spec) && spec[i] >= '0' && spec[i] <= '9' {
			i++
		}
		if i > precStart {
			fmt.Sscanf(spec[precStart:i], "%d", &fs.Precision)
		} else {
			fs.Precision = 0
		}
	}

	// Parse type
	if i < len(spec) {
		fs.Type = rune(spec[i])
	}

	return fs
}

// applyFormatSpec applies Python-style format specifications
func applyFormatSpec(value core.Value, spec *core.DictValue) string {
	// Extract format spec components
	var fill rune = ' '
	var align rune
	var sign rune
	var alt bool
	var zero bool
	var width int
	var precision int = -1
	var fmtType rune

	// Parse spec dict
	if v, ok := spec.Get("fill"); ok {
		if s, ok := v.(core.StringValue); ok && len(s) > 0 {
			fill = rune(s[0])
		}
	}

	if v, ok := spec.Get("align"); ok {
		if s, ok := v.(core.StringValue); ok && len(s) > 0 {
			align = rune(s[0])
		}
	}

	if v, ok := spec.Get("sign"); ok {
		if s, ok := v.(core.StringValue); ok && len(s) > 0 {
			sign = rune(s[0])
		}
	}

	if v, ok := spec.Get("alt"); ok {
		if b, ok := v.(core.BoolValue); ok {
			alt = bool(b)
		}
	}

	if v, ok := spec.Get("zero"); ok {
		if b, ok := v.(core.BoolValue); ok {
			zero = bool(b)
		}
	}

	if v, ok := spec.Get("width"); ok {
		if n, ok := v.(core.NumberValue); ok {
			width = int(n)
		}
	}

	if v, ok := spec.Get("precision"); ok {
		if n, ok := v.(core.NumberValue); ok {
			precision = int(n)
		}
	}

	if v, ok := spec.Get("type"); ok {
		if s, ok := v.(core.StringValue); ok && len(s) > 0 {
			fmtType = rune(s[0])
		}
	}

	// Apply formatting based on value type
	switch v := value.(type) {
	case core.NumberValue:
		return formatNumber(float64(v), fill, align, sign, alt, zero, width, precision, fmtType, false)
	case core.StringValue:
		return formatString(string(v), fill, align, width, precision)
	default:
		str := core.PrintValueWithoutQuotes(value)
		return formatString(str, fill, align, width, precision)
	}
}

// formatNumber formats a number according to Python format spec
func formatNumber(num float64, fill rune, align rune, sign rune, alt bool, zero bool, width int, precision int, fmtType rune, thousands bool) string {
	var result string

	// Build format string for Go's fmt package
	var fmtStr strings.Builder
	fmtStr.WriteRune('%')

	// Sign
	if sign == '+' {
		fmtStr.WriteRune('+')
	} else if sign == ' ' {
		fmtStr.WriteRune(' ')
	}

	// Alt mode (for real alt mode, not thousands separator)
	if alt && (fmtType == 'b' || fmtType == 'o' || fmtType == 'x' || fmtType == 'X') {
		fmtStr.WriteRune('#')
	}

	// Zero padding
	if zero && align != '<' && align != '>' && align != '^' {
		fmtStr.WriteRune('0')
	}

	// Width
	if width > 0 {
		fmtStr.WriteString(strconv.Itoa(width))
	}

	// Precision
	if precision >= 0 {
		fmtStr.WriteRune('.')
		fmtStr.WriteString(strconv.Itoa(precision))
	}

	// Type
	switch fmtType {
	case 'b': // Binary
		intVal := int64(num)
		result = strconv.FormatInt(intVal, 2)
		if alt {
			result = "0b" + result
		}
	case 'o': // Octal
		intVal := int64(num)
		result = strconv.FormatInt(intVal, 8)
		if alt {
			result = "0o" + result
		}
	case 'x', 'X': // Hex
		intVal := int64(num)
		result = strconv.FormatInt(intVal, 16)
		if fmtType == 'X' {
			result = strings.ToUpper(result)
		}
		if alt {
			result = "0x" + result
		}
	case 'd': // Decimal integer
		if thousands {
			result = addThousandsSeparator(num, -1)
		} else {
			fmtStr.WriteRune('d')
			result = fmt.Sprintf(fmtStr.String(), int64(num))
		}
	case 'e', 'E': // Scientific
		fmtStr.WriteByte(byte(fmtType))
		result = fmt.Sprintf(fmtStr.String(), num)
	case 'f', 'F': // Fixed point
		if precision < 0 {
			precision = 6
		}
		if thousands {
			result = addThousandsSeparator(num, precision)
		} else {
			fmtStr.WriteRune('f')
			result = fmt.Sprintf(fmtStr.String(), num)
		}
	case 'g', 'G': // General format
		if thousands {
			// For 'g' format with commas, format first then add commas
			tempResult := fmt.Sprintf("%g", num)
			result = addThousandsSeparatorToString(tempResult)
		} else {
			fmtStr.WriteByte(byte(fmtType))
			result = fmt.Sprintf(fmtStr.String(), num)
		}
	case '%': // Percentage
		if precision < 0 {
			precision = 6
		}
		result = fmt.Sprintf("%.*f%%", precision, num*100)
	case 0: // No type specified
		if thousands {
			// Default to 'f' format for thousands separator without type
			if precision < 0 {
				// Use a reasonable default precision
				if num == float64(int64(num)) {
					// Integer - no decimal places
					result = addThousandsSeparator(num, 0)
				} else {
					// Float - use default precision
					result = addThousandsSeparator(num, -1)
				}
			} else {
				result = addThousandsSeparator(num, precision)
			}
		} else {
			if precision >= 0 {
				result = fmt.Sprintf("%.*f", precision, num)
			} else {
				result = fmt.Sprintf("%g", num)
			}
		}
	default:
		// Default float formatting
		if thousands {
			// Format first, then add thousands separator
			if precision >= 0 {
				result = fmt.Sprintf("%.*f", precision, num)
			} else {
				result = fmt.Sprintf("%g", num)
			}
			result = addThousandsSeparatorToString(result)
		} else {
			if precision >= 0 {
				result = fmt.Sprintf("%.*f", precision, num)
			} else {
				result = fmt.Sprintf("%g", num)
			}
		}
	}

	// Apply sign formatting for positive numbers
	if num >= 0 && sign == '+' && !strings.HasPrefix(result, "+") {
		result = "+" + result
	}

	// Apply alignment if needed
	if width > 0 && len(result) < width {
		result = applyAlignment(result, fill, align, width)
	}

	return result
}

// formatString formats a string according to Python format spec
func formatString(str string, fill rune, align rune, width int, precision int) string {
	// Truncate if precision is specified
	if precision >= 0 && len(str) > precision {
		str = str[:precision]
	}

	// Apply alignment if width is specified
	if width > 0 && len(str) < width {
		str = applyAlignment(str, fill, align, width)
	}

	return str
}

// applyAlignment aligns a string within a given width
func applyAlignment(str string, fill rune, align rune, width int) string {
	if len(str) >= width {
		return str
	}

	padding := width - len(str)
	fillStr := strings.Repeat(string(fill), padding)

	switch align {
	case '<': // Left align
		return str + fillStr
	case '>': // Right align
		return fillStr + str
	case '^': // Center align
		leftPad := padding / 2
		rightPad := padding - leftPad
		return strings.Repeat(string(fill), leftPad) + str + strings.Repeat(string(fill), rightPad)
	case '=': // Numeric alignment (sign/prefix, then padding, then number)
		// For numeric alignment, we need to split sign/prefix from the number
		if len(str) > 0 && (str[0] == '+' || str[0] == '-' || strings.HasPrefix(str, "0x") || strings.HasPrefix(str, "0o") || strings.HasPrefix(str, "0b")) {
			if strings.HasPrefix(str, "0x") || strings.HasPrefix(str, "0o") || strings.HasPrefix(str, "0b") {
				return str[:2] + fillStr + str[2:]
			}
			return string(str[0]) + fillStr + str[1:]
		}
		return fillStr + str
	default:
		// Default to right align for numbers, left for strings
		if _, err := strconv.ParseFloat(str, 64); err == nil {
			return fillStr + str // Right align numbers
		}
		return str + fillStr // Left align strings
	}
}

// applyConversion applies Python-style conversions (!r, !s, !a)
func applyConversion(value core.Value, conversion string) string {
	switch conversion {
	case "r": // repr()
		return core.PrintValue(value)
	case "s": // str()
		return core.PrintValueWithoutQuotes(value)
	case "a": // ascii()
		// Convert non-ASCII characters to escape sequences
		str := core.PrintValue(value)
		var result strings.Builder
		for _, r := range str {
			if r < 128 {
				result.WriteRune(r)
			} else {
				result.WriteString(fmt.Sprintf("\\u%04x", r))
			}
		}
		return result.String()
	default:
		return core.PrintValueWithoutQuotes(value)
	}
}

// addThousandsSeparator adds commas to a number
func addThousandsSeparator(num float64, precision int) string {
	// Format the number first
	var str string
	if precision >= 0 {
		str = fmt.Sprintf("%.*f", precision, num)
	} else {
		// For -1, use default float representation
		str = fmt.Sprintf("%f", num)
		// Trim trailing zeros for cleaner output
		if strings.Contains(str, ".") {
			str = strings.TrimRight(str, "0")
			str = strings.TrimRight(str, ".")
		}
	}

	return addThousandsSeparatorToString(str)
}

// addThousandsSeparatorToString adds commas to a formatted number string
func addThousandsSeparatorToString(str string) string {
	// Split into integer and decimal parts
	parts := strings.Split(str, ".")
	intPart := parts[0]

	// Handle negative sign
	negative := false
	if strings.HasPrefix(intPart, "-") {
		negative = true
		intPart = intPart[1:]
	}

	// Handle positive sign
	positive := false
	if strings.HasPrefix(intPart, "+") {
		positive = true
		intPart = intPart[1:]
	}

	// Add commas to integer part
	var result strings.Builder
	for i, digit := range intPart {
		if i > 0 && (len(intPart)-i)%3 == 0 {
			result.WriteRune(',')
		}
		result.WriteRune(digit)
	}

	// Build final result
	finalResult := result.String()

	// Add back sign
	if negative {
		finalResult = "-" + finalResult
	} else if positive {
		finalResult = "+" + finalResult
	}

	// Add back decimal part
	if len(parts) > 1 {
		finalResult = finalResult + "." + parts[1]
	}

	return finalResult
}

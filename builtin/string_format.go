// Package builtin provides built-in functions for the M28 language.
package builtin

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/eval"
)

// RegisterStringFormatFunctions registers all string formatting functions
func RegisterStringFormatFunctions(ctx *core.Context) {
	ctx.Define("format", core.NewBuiltinFunction(FormatFunc))
	ctx.Define("str-format", core.NewBuiltinFunction(StrFormatFunc))
	ctx.Define("format-expr", core.NewBuiltinFunction(FormatExprFunc))
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

// formatExpression formats a Python-style format expression
func formatExpression(args []core.Value, ctx *core.Context) (string, error) {
	if len(args) == 0 {
		return "", fmt.Errorf("format-expr requires at least 1 argument")
	}

	// Evaluate the expression
	expr := args[0]
	value, err := eval.Eval(expr, ctx)
	if err != nil {
		// If it's a raw string, use it as-is
		if str, ok := expr.(core.StringValue); ok {
			value = str
		} else {
			return "", err
		}
	}

	// Default format
	if len(args) == 1 {
		return applyFormatSpecString(value, "", ctx)
	}

	// Get format spec (second argument)
	var formatSpec string
	if len(args) >= 2 {
		if spec, ok := args[1].(core.StringValue); ok {
			formatSpec = string(spec)
		}
	}

	// Check for conversion (!r, !s, !a)
	var conversion string
	if len(args) >= 3 {
		if conv, ok := args[2].(core.StringValue); ok {
			conversion = string(conv)
		}
	}

	// Apply conversion first
	if conversion != "" {
		switch conversion {
		case "r":
			value = core.StringValue(core.PrintValue(value))
		case "s":
			value = core.StringValue(value.String())
		case "a":
			// ASCII representation - for now just escape non-ASCII
			s := value.String()
			var result strings.Builder
			for _, r := range s {
				if r > 127 {
					result.WriteString(fmt.Sprintf("\\u%04x", r))
				} else {
					result.WriteRune(r)
				}
			}
			value = core.StringValue(result.String())
		}
	}

	return applyFormatSpecString(value, formatSpec, ctx)
}

// applyFormatSpecString applies Python-style format specification to a value
func applyFormatSpecString(value core.Value, spec string, ctx *core.Context) (string, error) {
	if spec == "" {
		// Default formatting
		switch v := value.(type) {
		case core.StringValue:
			return string(v), nil
		case core.NumberValue:
			// Check if it's an integer
			f := float64(v)
			if f == math.Floor(f) {
				return fmt.Sprintf("%.0f", f), nil
			}
			return fmt.Sprintf("%g", f), nil
		default:
			return value.String(), nil
		}
	}

	// Parse format spec
	fs, err := parseFormatSpecString(spec)
	if err != nil {
		return "", err
	}

	return applyFormatSpec(value, fs, ctx)
}

// FormatSpec represents a Python-style format specification
type FormatSpec struct {
	Fill      rune
	Align     rune // '<', '>', '^', '='
	Sign      rune // '+', '-', ' '
	Alternate bool // '#'
	ZeroPad   bool // '0'
	Width     int
	Grouping  rune // ',' or '_'
	Precision int  // -1 if not specified
	Type      rune // 's', 'd', 'f', 'e', 'g', 'x', 'o', 'b', '%', etc.
}

// parseFormatSpecString parses a Python-style format specification
func parseFormatSpecString(spec string) (*FormatSpec, error) {
	fs := &FormatSpec{
		Fill:      ' ',
		Align:     0,
		Sign:      0,
		Alternate: false,
		ZeroPad:   false,
		Width:     0,
		Grouping:  0,
		Precision: -1,
		Type:      0,
	}

	if spec == "" {
		return fs, nil
	}

	// Convert to runes for easier parsing
	runes := []rune(spec)
	i := 0

	// Check for fill and align
	if len(runes) >= 2 && (runes[1] == '<' || runes[1] == '>' || runes[1] == '^' || runes[1] == '=') {
		fs.Fill = runes[0]
		fs.Align = runes[1]
		i = 2
	} else if len(runes) >= 1 && (runes[0] == '<' || runes[0] == '>' || runes[0] == '^' || runes[0] == '=') {
		fs.Align = runes[0]
		i = 1
	}

	// Sign
	if i < len(runes) && (runes[i] == '+' || runes[i] == '-' || runes[i] == ' ') {
		fs.Sign = runes[i]
		i++
	}

	// Alternate form
	if i < len(runes) && runes[i] == '#' {
		fs.Alternate = true
		i++
	}

	// Zero padding
	if i < len(runes) && runes[i] == '0' {
		fs.ZeroPad = true
		if fs.Align == 0 {
			fs.Align = '='
		}
		i++
	}

	// Width
	widthStart := i
	for i < len(runes) && unicode.IsDigit(runes[i]) {
		i++
	}
	if i > widthStart {
		width, _ := strconv.Atoi(string(runes[widthStart:i]))
		fs.Width = width
	}

	// Grouping
	if i < len(runes) && (runes[i] == ',' || runes[i] == '_') {
		fs.Grouping = runes[i]
		i++
	}

	// Precision
	if i < len(runes) && runes[i] == '.' {
		i++
		precStart := i
		for i < len(runes) && unicode.IsDigit(runes[i]) {
			i++
		}
		if i > precStart {
			prec, _ := strconv.Atoi(string(runes[precStart:i]))
			fs.Precision = prec
		} else {
			fs.Precision = 0
		}
	}

	// Type
	if i < len(runes) {
		fs.Type = runes[i]
	}

	return fs, nil
}

// applyFormatSpec applies a format specification to a value
func applyFormatSpec(value core.Value, fs *FormatSpec, ctx *core.Context) (string, error) {
	var result string

	// Handle different types
	switch v := value.(type) {
	case core.NumberValue:
		result = formatNumber(float64(v), fs)
	case core.StringValue:
		result = formatString(string(v), fs)
	default:
		result = formatString(value.String(), fs)
	}

	// Apply width and alignment
	if fs.Width > 0 {
		result = applyAlignment(result, fs)
	}

	return result, nil
}

// formatNumber formats a number according to the format spec
func formatNumber(num float64, fs *FormatSpec) string {
	var result string
	isInt := num == math.Floor(num) && !math.IsInf(num, 0)

	// Handle percentage first
	if fs.Type == '%' {
		num *= 100
		if fs.Precision == -1 {
			fs.Precision = 6
		}
	}

	// Apply format type
	switch fs.Type {
	case 'd', 'n':
		// Integer decimal
		if !isInt {
			num = math.Trunc(num)
		}
		result = fmt.Sprintf("%.0f", num)
	case 'f', 'F':
		// Fixed-point
		prec := fs.Precision
		if prec == -1 {
			prec = 6
		}
		result = fmt.Sprintf("%.*f", prec, num)
	case 'e':
		// Scientific notation (lowercase)
		prec := fs.Precision
		if prec == -1 {
			prec = 6
		}
		result = fmt.Sprintf("%.*e", prec, num)
	case 'E':
		// Scientific notation (uppercase)
		prec := fs.Precision
		if prec == -1 {
			prec = 6
		}
		result = fmt.Sprintf("%.*E", prec, num)
	case 'g':
		// General format (lowercase)
		prec := fs.Precision
		if prec == -1 {
			prec = 6
		}
		result = fmt.Sprintf("%.*g", prec, num)
	case 'G':
		// General format (uppercase)
		prec := fs.Precision
		if prec == -1 {
			prec = 6
		}
		result = fmt.Sprintf("%.*G", prec, num)
	case 'x':
		// Hexadecimal (lowercase)
		result = fmt.Sprintf("%x", int64(num))
	case 'X':
		// Hexadecimal (uppercase)
		result = fmt.Sprintf("%X", int64(num))
	case 'o':
		// Octal
		result = fmt.Sprintf("%o", int64(num))
	case 'b':
		// Binary
		result = strconv.FormatInt(int64(num), 2)
	case '%':
		// Percentage
		prec := fs.Precision
		if prec == -1 {
			prec = 6
		}
		result = fmt.Sprintf("%.*f%%", prec, num)
	default:
		// Default numeric formatting
		if isInt {
			result = fmt.Sprintf("%.0f", num)
		} else if fs.Precision >= 0 {
			result = fmt.Sprintf("%.*f", fs.Precision, num)
		} else {
			result = fmt.Sprintf("%g", num)
		}
	}

	// Apply alternate form
	if fs.Alternate {
		switch fs.Type {
		case 'x':
			result = "0x" + result
		case 'X':
			result = "0X" + result
		case 'o':
			if !strings.HasPrefix(result, "0") {
				result = "0" + result
			}
		case 'b':
			result = "0b" + result
		}
	}

	// Apply grouping
	if fs.Grouping != 0 && (fs.Type == 'd' || fs.Type == 'n' || fs.Type == 0 || fs.Type == 'f' || fs.Type == 'F') {
		result = addThousandsSeparator(result, fs.Grouping)
	}

	// Apply sign
	if num >= 0 && !strings.HasPrefix(result, "-") {
		switch fs.Sign {
		case '+':
			result = "+" + result
		case ' ':
			result = " " + result
		}
	}

	return result
}

// formatString formats a string according to the format spec
func formatString(str string, fs *FormatSpec) string {
	result := str

	// Apply precision (truncation for strings)
	if fs.Precision >= 0 && len(result) > fs.Precision {
		result = result[:fs.Precision]
	}

	// Apply conversion for string type
	if fs.Type == 'r' {
		// repr() style - add quotes
		result = fmt.Sprintf("%q", result)
	}

	return result
}

// applyAlignment applies width and alignment to a string
func applyAlignment(str string, fs *FormatSpec) string {
	if len(str) >= fs.Width {
		return str
	}

	fillChar := string(fs.Fill)
	padding := fs.Width - len(str)

	switch fs.Align {
	case '<':
		// Left align
		return str + strings.Repeat(fillChar, padding)
	case '>':
		// Right align
		return strings.Repeat(fillChar, padding) + str
	case '^':
		// Center align
		leftPad := padding / 2
		rightPad := padding - leftPad
		return strings.Repeat(fillChar, leftPad) + str + strings.Repeat(fillChar, rightPad)
	case '=':
		// Pad after sign but before digits
		if len(str) > 0 && (str[0] == '+' || str[0] == '-' || str[0] == ' ') {
			return string(str[0]) + strings.Repeat(fillChar, padding) + str[1:]
		}
		// Fall back to right align
		return strings.Repeat(fillChar, padding) + str
	default:
		// Default: right align for numbers, left for strings
		if fs.Type == 's' || fs.Type == 0 {
			return str + strings.Repeat(fillChar, padding)
		}
		return strings.Repeat(fillChar, padding) + str
	}
}

// addThousandsSeparator adds thousand separators to a number string
func addThousandsSeparator(s string, sep rune) string {
	// Find the decimal point (if any)
	parts := strings.Split(s, ".")
	intPart := parts[0]

	// Handle sign
	sign := ""
	if len(intPart) > 0 && (intPart[0] == '+' || intPart[0] == '-') {
		sign = string(intPart[0])
		intPart = intPart[1:]
	}

	// Add separators
	result := addThousandsSeparatorToString(intPart, sep)

	// Reconstruct
	if len(parts) > 1 {
		return sign + result + "." + parts[1]
	}
	return sign + result
}

// addThousandsSeparatorToString adds separators to the integer part
func addThousandsSeparatorToString(s string, sep rune) string {
	if len(s) <= 3 {
		return s
	}

	// Work from right to left
	var result []rune
	for i, r := range []rune(s) {
		if i > 0 && (len(s)-i)%3 == 0 {
			result = append(result, sep)
		}
		result = append(result, r)
	}

	return string(result)
}

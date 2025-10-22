package core

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"
)

// formatValueWithSpec formats a value according to a Python-style format specification
func formatValueWithSpec(value Value, spec string) (string, error) {
	if spec == "" {
		// No format spec - use default formatting
		if str, ok := value.(StringValue); ok {
			return string(str), nil
		} else if num, ok := value.(NumberValue); ok {
			f := float64(num)
			if f == math.Floor(f) {
				return fmt.Sprintf("%.0f", f), nil
			}
			return fmt.Sprintf("%g", f), nil
		}
		return PrintValueWithoutQuotes(value), nil
	}

	// Parse the format spec
	// Format: [[fill]align][sign][#][0][width][,][.precision][type]
	i := 0
	var fill rune = ' '
	var align rune
	var sign rune
	var alt bool
	var zero bool
	var width int
	var precision int = -1
	var fmtType rune

	// Parse fill and align (must be first)
	if len(spec) >= 2 {
		possibleAlign := rune(spec[1])
		if possibleAlign == '<' || possibleAlign == '>' || possibleAlign == '^' || possibleAlign == '=' {
			fill = rune(spec[0])
			align = possibleAlign
			i = 2
		} else if len(spec) >= 1 {
			possibleAlign = rune(spec[0])
			if possibleAlign == '<' || possibleAlign == '>' || possibleAlign == '^' || possibleAlign == '=' {
				align = possibleAlign
				i = 1
			}
		}
	}

	// Parse sign
	if i < len(spec) {
		ch := rune(spec[i])
		if ch == '+' || ch == '-' || ch == ' ' {
			sign = ch
			i++
		}
	}

	// Parse # (alternate form)
	if i < len(spec) && spec[i] == '#' {
		alt = true
		i++
	}

	// Parse 0 (zero padding)
	if i < len(spec) && spec[i] == '0' {
		zero = true
		if align == 0 {
			align = '='
		}
		i++
	}

	// Parse width
	widthStart := i
	for i < len(spec) && unicode.IsDigit(rune(spec[i])) {
		i++
	}
	if i > widthStart {
		fmt.Sscanf(spec[widthStart:i], "%d", &width)
	}

	// Skip grouping option (,)
	if i < len(spec) && spec[i] == ',' {
		i++
	}

	// Parse precision
	if i < len(spec) && spec[i] == '.' {
		i++
		precStart := i
		for i < len(spec) && unicode.IsDigit(rune(spec[i])) {
			i++
		}
		if i > precStart {
			fmt.Sscanf(spec[precStart:i], "%d", &precision)
		} else {
			precision = 0
		}
	}

	// Parse type
	if i < len(spec) {
		fmtType = rune(spec[i])
	}

	// Format the value
	var result string

	if num, ok := value.(NumberValue); ok {
		f := float64(num)
		// Number formatting
		switch fmtType {
		case 'f', 'F':
			// Fixed-point
			prec := precision
			if prec == -1 {
				prec = 6
			}
			result = fmt.Sprintf("%.*f", prec, f)
		case 'e':
			// Scientific notation (lowercase)
			prec := precision
			if prec == -1 {
				prec = 6
			}
			result = fmt.Sprintf("%.*e", prec, f)
		case 'E':
			// Scientific notation (uppercase)
			prec := precision
			if prec == -1 {
				prec = 6
			}
			result = fmt.Sprintf("%.*E", prec, f)
		case 'g', 'G':
			// General format
			prec := precision
			if prec == -1 {
				prec = 6
			}
			if fmtType == 'g' {
				result = fmt.Sprintf("%.*g", prec, f)
			} else {
				result = fmt.Sprintf("%.*G", prec, f)
			}
		case 'd':
			// Integer decimal
			result = fmt.Sprintf("%.0f", math.Trunc(f))
		case 'x':
			// Hexadecimal (lowercase)
			result = fmt.Sprintf("%x", int64(f))
			if alt {
				result = "0x" + result
			}
		case 'X':
			// Hexadecimal (uppercase)
			result = fmt.Sprintf("%X", int64(f))
			if alt {
				result = "0X" + result
			}
		case 'o':
			// Octal
			result = fmt.Sprintf("%o", int64(f))
			if alt && !strings.HasPrefix(result, "0") {
				result = "0" + result
			}
		case 'b':
			// Binary
			result = strconv.FormatInt(int64(f), 2)
			if alt {
				result = "0b" + result
			}
		case '%':
			// Percentage
			prec := precision
			if prec == -1 {
				prec = 6
			}
			result = fmt.Sprintf("%.*f%%", prec, f*100)
		default:
			// Default number formatting
			if f == math.Floor(f) {
				result = fmt.Sprintf("%.0f", f)
			} else if precision >= 0 {
				result = fmt.Sprintf("%.*f", precision, f)
			} else {
				result = fmt.Sprintf("%g", f)
			}
		}

		// Apply sign
		if f >= 0 && !strings.HasPrefix(result, "-") {
			switch sign {
			case '+':
				result = "+" + result
			case ' ':
				result = " " + result
			}
		}
	} else if str, ok := value.(StringValue); ok {
		// String formatting
		result = string(str)
		if precision >= 0 && len(result) > precision {
			result = result[:precision]
		}
	} else {
		result = PrintValueWithoutQuotes(value)
	}

	// Apply width and alignment
	if width > 0 && len(result) < width {
		padding := width - len(result)
		fillStr := string(fill)
		if zero && fillStr == " " {
			fillStr = "0"
		}

		switch align {
		case '<':
			// Left align
			result = result + strings.Repeat(fillStr, padding)
		case '>':
			// Right align
			result = strings.Repeat(fillStr, padding) + result
		case '^':
			// Center align
			leftPad := padding / 2
			rightPad := padding - leftPad
			result = strings.Repeat(fillStr, leftPad) + result + strings.Repeat(fillStr, rightPad)
		case '=':
			// Pad after sign
			if len(result) > 0 && (result[0] == '+' || result[0] == '-' || result[0] == ' ') {
				result = string(result[0]) + strings.Repeat(fillStr, padding) + result[1:]
			} else {
				result = strings.Repeat(fillStr, padding) + result
			}
		default:
			// Default: right align for numbers, left for strings
			if _, ok := value.(NumberValue); ok {
				result = strings.Repeat(fillStr, padding) + result
			} else {
				result = result + strings.Repeat(fillStr, padding)
			}
		}
	}

	return result, nil
}

// formatStringWithPercent implements Python's % string formatting
func formatStringWithPercent(formatStr string, values Value) (Value, error) {
	// Check if using dictionary-based formatting
	var valueDict *DictValue
	var valueTuple []Value

	if dict, ok := values.(*DictValue); ok {
		valueDict = dict
	} else if tuple, ok := values.(TupleValue); ok {
		valueTuple = []Value(tuple)
	} else {
		valueTuple = []Value{values}
	}

	var result strings.Builder
	valueIdx := 0
	i := 0

	for i < len(formatStr) {
		if formatStr[i] != '%' {
			result.WriteByte(formatStr[i])
			i++
			continue
		}

		// Found % - check next character
		i++
		if i >= len(formatStr) {
			return nil, fmt.Errorf("incomplete format")
		}

		// %% means literal %
		if formatStr[i] == '%' {
			result.WriteByte('%')
			i++
			continue
		}

		var value Value
		var fmtType byte

		// Check for %(name)s dictionary-style formatting
		if formatStr[i] == '(' {
			if valueDict == nil {
				return nil, fmt.Errorf("format requires a mapping")
			}

			// Find the closing )
			i++ // skip (
			keyStart := i
			for i < len(formatStr) && formatStr[i] != ')' {
				i++
			}
			if i >= len(formatStr) {
				return nil, fmt.Errorf("incomplete format key")
			}

			keyName := formatStr[keyStart:i]
			i++ // skip )

			// Parse format specifier after )
			// Skip optional flags (-, +, 0, space, #)
			for i < len(formatStr) && (formatStr[i] == '-' || formatStr[i] == '+' ||
				formatStr[i] == '0' || formatStr[i] == ' ' || formatStr[i] == '#') {
				i++
			}

			// Skip optional width
			for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
				i++
			}

			// Skip optional .precision
			if i < len(formatStr) && formatStr[i] == '.' {
				i++
				for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
					i++
				}
			}

			if i >= len(formatStr) {
				return nil, fmt.Errorf("incomplete format")
			}

			fmtType = formatStr[i]
			i++

			// Look up the value in the dictionary
			var found bool
			keyStr := ValueToKey(StringValue(keyName))
			value, found = valueDict.Get(keyStr)
			if !found {
				return nil, fmt.Errorf("KeyError: '%s'", keyName)
			}
		} else {
			// Positional formatting
			if valueDict != nil {
				return nil, fmt.Errorf("format requires a sequence, not a dict")
			}

			// Parse format specifier (simplified version)
			// Full format: %[flags][width][.precision]type
			// For now, just handle the type and ignore flags/width/precision

			// Skip optional flags (-, +, 0, space, #)
			for i < len(formatStr) && (formatStr[i] == '-' || formatStr[i] == '+' ||
				formatStr[i] == '0' || formatStr[i] == ' ' || formatStr[i] == '#') {
				i++
			}

			// Skip optional width
			for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
				i++
			}

			// Skip optional .precision
			if i < len(formatStr) && formatStr[i] == '.' {
				i++
				for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
					i++
				}
			}

			if i >= len(formatStr) {
				return nil, fmt.Errorf("incomplete format")
			}

			// Get format type
			fmtType = formatStr[i]
			i++

			// Get the value to format
			if valueIdx >= len(valueTuple) {
				return nil, fmt.Errorf("not enough arguments for format string")
			}
			value = valueTuple[valueIdx]
			valueIdx++
		}

		// Format the value based on type
		var formatted string
		switch fmtType {
		case 's': // String
			if str, ok := value.(StringValue); ok {
				formatted = string(str)
			} else {
				formatted = PrintValueWithoutQuotes(value)
			}
		case 'd', 'i': // Integer
			if num, ok := value.(NumberValue); ok {
				formatted = fmt.Sprintf("%d", int64(num))
			} else {
				return nil, fmt.Errorf("%%d format: a number is required, not %s", value.Type())
			}
		case 'f', 'F': // Float
			if num, ok := value.(NumberValue); ok {
				formatted = fmt.Sprintf("%f", float64(num))
			} else {
				return nil, fmt.Errorf("%%f format: a number is required, not %s", value.Type())
			}
		case 'r': // Repr
			formatted = value.String()
		case 'c': // Character
			if num, ok := value.(NumberValue); ok {
				formatted = string(rune(int(num)))
			} else if str, ok := value.(StringValue); ok {
				if len(str) == 1 {
					formatted = string(str)
				} else {
					return nil, fmt.Errorf("%%c requires int or char")
				}
			} else {
				return nil, fmt.Errorf("%%c requires int or char")
			}
		case 'x', 'X': // Hex
			if num, ok := value.(NumberValue); ok {
				if fmtType == 'x' {
					formatted = fmt.Sprintf("%x", int64(num))
				} else {
					formatted = fmt.Sprintf("%X", int64(num))
				}
			} else {
				return nil, fmt.Errorf("%%%c format: a number is required", fmtType)
			}
		case 'o': // Octal
			if num, ok := value.(NumberValue); ok {
				formatted = fmt.Sprintf("%o", int64(num))
			} else {
				return nil, fmt.Errorf("%%o format: a number is required")
			}
		default:
			return nil, fmt.Errorf("unsupported format character '%c'", fmtType)
		}

		result.WriteString(formatted)
	}

	if valueDict == nil && valueIdx < len(valueTuple) {
		return nil, fmt.Errorf("not all arguments converted during string formatting")
	}

	return StringValue(result.String()), nil
}

// InitStringMethods adds additional string methods to the string type descriptor
func InitStringMethods() {
	// Get the string type descriptor
	td := GetTypeDescriptor("string")
	if td == nil {
		return
	}

	// Add __len__ method that properly counts Unicode characters
	td.Methods["__len__"] = &MethodDescriptor{
		Name:    "__len__",
		Arity:   0,
		Doc:     "Return the number of characters in the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			// Count runes (Unicode characters) instead of bytes
			return NumberValue(len([]rune(s))), nil
		},
	}

	// Add missing methods
	td.Methods["replace"] = &MethodDescriptor{
		Name:    "replace",
		Arity:   -1,
		Doc:     "Return a copy with all occurrences of substring old replaced by new",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 2 {
				return nil, fmt.Errorf("replace() takes at least 2 arguments (%d given)", len(args))
			}

			s := string(receiver.(StringValue))

			old, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("replace() argument 1 must be a string")
			}

			new, ok := args[1].(StringValue)
			if !ok {
				return nil, fmt.Errorf("replace() argument 2 must be a string")
			}

			if len(args) >= 3 {
				if count, ok := args[2].(NumberValue); ok {
					return StringValue(strings.Replace(s, string(old), string(new), int(count))), nil
				}
				return nil, fmt.Errorf("replace() argument 3 must be a number")
			}

			return StringValue(strings.ReplaceAll(s, string(old), string(new))), nil
		},
	}

	td.Methods["startswith"] = &MethodDescriptor{
		Name:    "startswith",
		Arity:   1,
		Doc:     "Return True if string starts with the prefix, otherwise return False",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("startswith() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			prefix, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("startswith() argument must be a string")
			}

			return BoolValue(strings.HasPrefix(s, string(prefix))), nil
		},
	}

	td.Methods["endswith"] = &MethodDescriptor{
		Name:    "endswith",
		Arity:   1,
		Doc:     "Return True if string ends with the suffix, otherwise return False",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("endswith() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			suffix, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("endswith() argument must be a string")
			}

			return BoolValue(strings.HasSuffix(s, string(suffix))), nil
		},
	}

	td.Methods["find"] = &MethodDescriptor{
		Name:    "find",
		Arity:   1,
		Doc:     "Return the lowest index where substring is found, or -1 if not found",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("find() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("find() argument must be a string")
			}

			return NumberValue(strings.Index(s, string(sub))), nil
		},
	}

	td.Methods["join"] = &MethodDescriptor{
		Name:    "join",
		Arity:   1,
		Doc:     "Return a string which is the concatenation of the strings in the iterable",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("join() takes exactly one argument (%d given)", len(args))
			}

			sep := string(receiver.(StringValue))

			// Handle different iterable types
			var parts []string
			switch v := args[0].(type) {
			case *ListValue:
				parts = make([]string, v.Len())
				for i, item := range v.Items() {
					if s, ok := item.(StringValue); ok {
						parts[i] = string(s)
					} else {
						parts[i] = PrintValueWithoutQuotes(item)
					}
				}
			case TupleValue:
				parts = make([]string, len(v))
				for i, item := range v {
					if s, ok := item.(StringValue); ok {
						parts[i] = string(s)
					} else {
						parts[i] = PrintValueWithoutQuotes(item)
					}
				}
			default:
				// Check if it implements Iterable interface
				if iter, ok := args[0].(Iterable); ok {
					parts = make([]string, 0)
					iterator := iter.Iterator()
					for {
						item, hasNext := iterator.Next()
						if !hasNext {
							break
						}
						if s, ok := item.(StringValue); ok {
							parts = append(parts, string(s))
						} else {
							parts = append(parts, PrintValueWithoutQuotes(item))
						}
					}
				} else {
					return nil, fmt.Errorf("join() argument must be an iterable")
				}
			}

			return StringValue(strings.Join(parts, sep)), nil
		},
	}

	td.Methods["capitalize"] = &MethodDescriptor{
		Name:    "capitalize",
		Arity:   0,
		Doc:     "Return a capitalized version of the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return receiver, nil
			}
			// Capitalize first letter, lowercase the rest
			return StringValue(strings.ToUpper(s[:1]) + strings.ToLower(s[1:])), nil
		},
	}

	td.Methods["title"] = &MethodDescriptor{
		Name:    "title",
		Arity:   0,
		Doc:     "Return a titlecased version of the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			return StringValue(strings.Title(s)), nil
		},
	}

	td.Methods["count"] = &MethodDescriptor{
		Name:    "count",
		Arity:   1,
		Doc:     "Return the number of non-overlapping occurrences of substring",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("count() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("count() argument must be a string")
			}

			return NumberValue(strings.Count(s, string(sub))), nil
		},
	}

	td.Methods["format"] = &MethodDescriptor{
		Name:    "format",
		Arity:   -1,
		Doc:     "Return a formatted version of the string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			result := &strings.Builder{}

			autoArgIndex := 0
			i := 0

			for i < len(s) {
				// Look for '{'
				if s[i] == '{' {
					// Check for escaped brace
					if i+1 < len(s) && s[i+1] == '{' {
						result.WriteByte('{')
						i += 2
						continue
					}

					// Find the matching '}'
					j := i + 1
					depth := 1
					for j < len(s) && depth > 0 {
						if s[j] == '{' {
							depth++
						} else if s[j] == '}' {
							depth--
						}
						j++
					}

					if depth != 0 {
						return nil, fmt.Errorf("unmatched '{' in format string")
					}

					// Extract the format spec
					spec := s[i+1 : j-1]

					// Parse the spec: [field][!conversion][:format_spec]
					var fieldSpec, formatSpec string
					var argIndex int

					// Split on ':' to separate field from format spec
					colonIdx := strings.IndexByte(spec, ':')
					if colonIdx >= 0 {
						fieldSpec = spec[:colonIdx]
						formatSpec = spec[colonIdx+1:]
					} else {
						fieldSpec = spec
					}

					// Determine which argument to use
					if fieldSpec == "" {
						// Auto-indexing: {}
						argIndex = autoArgIndex
						autoArgIndex++
					} else {
						// Try to parse as integer
						var err error
						_, err = fmt.Sscanf(fieldSpec, "%d", &argIndex)
						if err != nil {
							return nil, fmt.Errorf("invalid field name '%s' in format string", fieldSpec)
						}
					}

					if argIndex >= len(args) {
						return nil, fmt.Errorf("replacement index %d out of range for positional args tuple", argIndex)
					}

					// Format the argument
					formatted, err := formatValueWithSpec(args[argIndex], formatSpec)
					if err != nil {
						return nil, err
					}
					result.WriteString(formatted)

					i = j
				} else if s[i] == '}' {
					// Check for escaped brace
					if i+1 < len(s) && s[i+1] == '}' {
						result.WriteByte('}')
						i += 2
						continue
					}
					return nil, fmt.Errorf("single '}' encountered in format string")
				} else {
					result.WriteByte(s[i])
					i++
				}
			}

			return StringValue(result.String()), nil
		},
	}

	td.Methods["lstrip"] = &MethodDescriptor{
		Name:    "lstrip",
		Arity:   -1,
		Doc:     "Return a copy of the string with leading characters removed",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(args) == 0 {
				return StringValue(strings.TrimLeft(s, " \t\n\r")), nil
			}
			if chars, ok := args[0].(StringValue); ok {
				return StringValue(strings.TrimLeft(s, string(chars))), nil
			}
			return nil, fmt.Errorf("lstrip() argument must be a string")
		},
	}

	td.Methods["rstrip"] = &MethodDescriptor{
		Name:    "rstrip",
		Arity:   -1,
		Doc:     "Return a copy of the string with trailing characters removed",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(args) == 0 {
				return StringValue(strings.TrimRight(s, " \t\n\r")), nil
			}
			if chars, ok := args[0].(StringValue); ok {
				return StringValue(strings.TrimRight(s, string(chars))), nil
			}
			return nil, fmt.Errorf("rstrip() argument must be a string")
		},
	}

	td.Methods["isdigit"] = &MethodDescriptor{
		Name:    "isdigit",
		Arity:   0,
		Doc:     "Return True if all characters in the string are digits",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if r < '0' || r > '9' {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["isalpha"] = &MethodDescriptor{
		Name:    "isalpha",
		Arity:   0,
		Doc:     "Return True if all characters in the string are alphabetic",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if !((r >= 'a' && r <= 'z') || (r >= 'A' && r <= 'Z')) {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["isspace"] = &MethodDescriptor{
		Name:    "isspace",
		Arity:   0,
		Doc:     "Return True if all characters in the string are whitespace",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if r != ' ' && r != '\t' && r != '\n' && r != '\r' {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["index"] = &MethodDescriptor{
		Name:    "index",
		Arity:   1,
		Doc:     "Like find(), but raises ValueError when the substring is not found",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("index() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("index() argument must be a string")
			}

			idx := strings.Index(s, string(sub))
			if idx == -1 {
				return nil, fmt.Errorf("substring not found")
			}
			return NumberValue(idx), nil
		},
	}

	// Update strip to accept optional argument
	td.Methods["strip"] = &MethodDescriptor{
		Name:    "strip",
		Arity:   -1,
		Doc:     "Return a copy of the string with leading and trailing characters removed",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(args) == 0 {
				return StringValue(strings.TrimSpace(s)), nil
			}
			if chars, ok := args[0].(StringValue); ok {
				return StringValue(strings.Trim(s, string(chars))), nil
			}
			return nil, fmt.Errorf("strip() argument must be a string")
		},
	}

	// Update split to support maxsplit
	td.Methods["split"] = &MethodDescriptor{
		Name:    "split",
		Arity:   -1,
		Doc:     "Return a list of the words in the string, using sep as the delimiter",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			sep := " "
			limit := -1

			if len(args) > 0 {
				if sepStr, ok := args[0].(StringValue); ok {
					sep = string(sepStr)
				} else {
					return nil, fmt.Errorf("sep must be a string")
				}
			}
			if len(args) > 1 {
				if maxSplit, ok := args[1].(NumberValue); ok {
					limit = int(maxSplit) + 1
				} else {
					return nil, fmt.Errorf("maxsplit must be a number")
				}
			}

			var parts []string
			if limit == -1 {
				parts = strings.Split(s, sep)
			} else {
				parts = strings.SplitN(s, sep, limit)
			}

			result := make([]Value, len(parts))
			for i, part := range parts {
				result[i] = StringValue(part)
			}
			return NewList(result...), nil
		},
	}

	// removeprefix - Python 3.9+
	td.Methods["removeprefix"] = &MethodDescriptor{
		Name:    "removeprefix",
		Arity:   1,
		Doc:     "Return a string with the given prefix removed if present (Python 3.9+)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("removeprefix() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			prefix, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("removeprefix() argument must be a string")
			}

			return StringValue(strings.TrimPrefix(s, string(prefix))), nil
		},
	}

	// removesuffix - Python 3.9+
	td.Methods["removesuffix"] = &MethodDescriptor{
		Name:    "removesuffix",
		Arity:   1,
		Doc:     "Return a string with the given suffix removed if present (Python 3.9+)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("removesuffix() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			suffix, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("removesuffix() argument must be a string")
			}

			return StringValue(strings.TrimSuffix(s, string(suffix))), nil
		},
	}

	// partition
	td.Methods["partition"] = &MethodDescriptor{
		Name:    "partition",
		Arity:   1,
		Doc:     "Split the string at the first occurrence of sep, return 3-tuple (before, sep, after)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("partition() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			sep, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("partition() argument must be a string")
			}

			sepStr := string(sep)
			idx := strings.Index(s, sepStr)
			if idx == -1 {
				// Separator not found - return (original, "", "")
				return TupleValue{StringValue(s), StringValue(""), StringValue("")}, nil
			}

			before := s[:idx]
			after := s[idx+len(sepStr):]
			return TupleValue{StringValue(before), sep, StringValue(after)}, nil
		},
	}

	// rpartition
	td.Methods["rpartition"] = &MethodDescriptor{
		Name:    "rpartition",
		Arity:   1,
		Doc:     "Split the string at the last occurrence of sep, return 3-tuple (before, sep, after)",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("rpartition() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			sep, ok := args[0].(StringValue)
			if !ok {
				return nil, fmt.Errorf("rpartition() argument must be a string")
			}

			sepStr := string(sep)
			idx := strings.LastIndex(s, sepStr)
			if idx == -1 {
				// Separator not found - return ("", "", original)
				return TupleValue{StringValue(""), StringValue(""), StringValue(s)}, nil
			}

			before := s[:idx]
			after := s[idx+len(sepStr):]
			return TupleValue{StringValue(before), sep, StringValue(after)}, nil
		},
	}

	// expandtabs
	td.Methods["expandtabs"] = &MethodDescriptor{
		Name:    "expandtabs",
		Arity:   -1,
		Doc:     "Return a copy where all tab characters are replaced by spaces",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			tabsize := 8 // Default tabsize

			if len(args) > 0 {
				if ts, ok := args[0].(NumberValue); ok {
					tabsize = int(ts)
					if tabsize < 0 {
						tabsize = 0
					}
				} else {
					return nil, fmt.Errorf("expandtabs() argument must be a number")
				}
			}

			var result strings.Builder
			col := 0

			for _, r := range s {
				if r == '\t' {
					// Add spaces to next tab stop
					spaces := tabsize - (col % tabsize)
					result.WriteString(strings.Repeat(" ", spaces))
					col += spaces
				} else if r == '\n' || r == '\r' {
					result.WriteRune(r)
					col = 0
				} else {
					result.WriteRune(r)
					col++
				}
			}

			return StringValue(result.String()), nil
		},
	}

	// translate - basic implementation
	td.Methods["translate"] = &MethodDescriptor{
		Name:    "translate",
		Arity:   1,
		Doc:     "Return a string where each character has been mapped through the given translation table",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("translate() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))

			// Translation table should be a dict mapping rune codes to replacement strings
			table, ok := args[0].(*DictValue)
			if !ok {
				return nil, fmt.Errorf("translate() argument must be a dict")
			}

			var result strings.Builder
			for _, r := range s {
				// Look up the character code in the translation table
				key := NumberValue(r)
				if replacement, ok := table.Get(ValueToKey(key)); ok {
					// Check if replacement is None (delete the character)
					if _, isNil := replacement.(NilValue); isNil {
						continue // Skip this character
					}
					// Convert replacement to string
					if repStr, ok := replacement.(StringValue); ok {
						result.WriteString(string(repStr))
					} else if repNum, ok := replacement.(NumberValue); ok {
						// If it's a number, convert to character
						result.WriteRune(rune(repNum))
					} else {
						result.WriteString(PrintValueWithoutQuotes(replacement))
					}
				} else {
					// No translation, keep original character
					result.WriteRune(r)
				}
			}

			return StringValue(result.String()), nil
		},
	}

	// zfill - zero-fill to width
	td.Methods["zfill"] = &MethodDescriptor{
		Name:    "zfill",
		Arity:   1,
		Doc:     "Pad a numeric string with zeros on the left, to fill a field of the given width",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("zfill() takes exactly one argument (%d given)", len(args))
			}

			s := string(receiver.(StringValue))
			width, ok := args[0].(NumberValue)
			if !ok {
				return nil, fmt.Errorf("zfill() argument must be a number")
			}

			w := int(width)
			if w <= len(s) {
				return receiver, nil
			}

			// Check for sign
			sign := ""
			if len(s) > 0 && (s[0] == '+' || s[0] == '-') {
				sign = s[:1]
				s = s[1:]
			}

			padding := w - len(s) - len(sign)
			return StringValue(sign + strings.Repeat("0", padding) + s), nil
		},
	}

	// Add __mod__ for % string formatting
	td.Methods["__mod__"] = &MethodDescriptor{
		Name:    "__mod__",
		Arity:   1,
		Doc:     "String formatting using % operator",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, fmt.Errorf("__mod__() takes exactly 1 argument (%d given)", len(args))
			}

			formatStr := string(receiver.(StringValue))
			return formatStringWithPercent(formatStr, args[0])
		},
	}
}

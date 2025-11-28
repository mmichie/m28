package core

import (
	"fmt"
	"math"
	"strconv"
	"strings"
	"unicode"
)

// applyConversion applies a Python conversion specifier to a value
// !s - str() - default string representation
// !r - repr() - quoted string representation
// !a - ascii() - like repr() but escape non-ASCII characters
func applyConversion(value Value, conversion string) (Value, error) {
	switch conversion {
	case "s":
		// str() conversion - default string representation
		return StringValue(PrintValueWithoutQuotes(value)), nil
	case "r":
		// repr() conversion - quoted representation
		return StringValue(PrintValue(value)), nil
	case "a":
		// ascii() conversion - repr() with non-ASCII escaped
		repr := PrintValue(value)
		// Escape non-ASCII characters
		var result strings.Builder
		for _, r := range repr {
			if r < 128 {
				result.WriteRune(r)
			} else {
				// Escape as \uXXXX or \UXXXXXXXX
				if r <= 0xFFFF {
					result.WriteString(fmt.Sprintf("\\u%04x", r))
				} else {
					result.WriteString(fmt.Sprintf("\\U%08x", r))
				}
			}
		}
		return StringValue(result.String()), nil
	default:
		return nil, &ValueError{Message: fmt.Sprintf("unknown conversion specifier '!%s'", conversion)}
	}
}

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
			return nil, &ValueError{Message: "incomplete format"}
		}

		// %% means literal %
		if formatStr[i] == '%' {
			result.WriteByte('%')
			i++
			continue
		}

		var value Value
		var fmtType byte
		var leftAlign bool
		var width int
		var hasWidth bool

		// Check for %(name)s dictionary-style formatting
		if formatStr[i] == '(' {
			if valueDict == nil {
				return nil, &TypeError{Message: "format requires a mapping"}
			}

			// Find the closing )
			i++ // skip (
			keyStart := i
			for i < len(formatStr) && formatStr[i] != ')' {
				i++
			}
			if i >= len(formatStr) {
				return nil, &ValueError{Message: "incomplete format key"}
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
				return nil, &ValueError{Message: "incomplete format"}
			}

			fmtType = formatStr[i]
			i++

			// Look up the value in the dictionary
			var found bool
			keyStr := ValueToKey(StringValue(keyName))
			value, found = valueDict.Get(keyStr)
			if !found {
				return nil, &KeyError{Key: StringValue(keyName)}
			}
		} else {
			// Positional formatting
			if valueDict != nil {
				return nil, &TypeError{Message: "format requires a sequence, not a dict"}
			}

			// Parse format specifier (simplified version)
			// Full format: %[flags][width][.precision]type

			// Skip optional flags (-, +, 0, space, #)
			leftAlign = false
			for i < len(formatStr) && (formatStr[i] == '-' || formatStr[i] == '+' ||
				formatStr[i] == '0' || formatStr[i] == ' ' || formatStr[i] == '#') {
				if formatStr[i] == '-' {
					leftAlign = true
				}
				i++
			}

			// Parse optional width (can be * for dynamic width)
			width = 0
			hasWidth = false
			if i < len(formatStr) && formatStr[i] == '*' {
				// Dynamic width from next argument
				if valueIdx >= len(valueTuple) {
					return nil, &TypeError{Message: "not enough arguments for format string"}
				}
				if widthNum, ok := valueTuple[valueIdx].(NumberValue); ok {
					width = int(widthNum)
					hasWidth = true
					valueIdx++
				} else {
					return nil, &TypeError{Message: "* wants int"}
				}
				i++
			} else {
				// Static width
				widthStart := i
				for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
					i++
				}
				if i > widthStart {
					widthStr := formatStr[widthStart:i]
					fmt.Sscanf(widthStr, "%d", &width)
					hasWidth = true
				}
			}

			// Skip optional .precision
			if i < len(formatStr) && formatStr[i] == '.' {
				i++
				for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
					i++
				}
			}

			if i >= len(formatStr) {
				return nil, &ValueError{Message: "incomplete format"}
			}

			// Get format type
			fmtType = formatStr[i]
			i++

			// Get the value to format
			if valueIdx >= len(valueTuple) {
				return nil, &TypeError{Message: "not enough arguments for format string"}
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
			} else if b, ok := value.(BoolValue); ok {
				// In Python, bool is a subclass of int, so True is 1 and False is 0
				if b {
					formatted = "1"
				} else {
					formatted = "0"
				}
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("%%d format: a number is required, not %s", value.Type())}
			}
		case 'f', 'F': // Float
			if num, ok := value.(NumberValue); ok {
				formatted = fmt.Sprintf("%f", float64(num))
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("%%f format: a number is required, not %s", value.Type())}
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
					return nil, &TypeError{Message: "%c requires int or char"}
				}
			} else {
				return nil, &TypeError{Message: "%c requires int or char"}
			}
		case 'x', 'X': // Hex
			if num, ok := value.(NumberValue); ok {
				if fmtType == 'x' {
					formatted = fmt.Sprintf("%x", int64(num))
				} else {
					formatted = fmt.Sprintf("%X", int64(num))
				}
			} else if b, ok := value.(BoolValue); ok {
				// In Python, bool is a subclass of int, so True is 1 and False is 0
				if b {
					if fmtType == 'x' {
						formatted = "1"
					} else {
						formatted = "1"
					}
				} else {
					formatted = "0"
				}
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("%%%c format: a number is required", fmtType)}
			}
		case 'o': // Octal
			if num, ok := value.(NumberValue); ok {
				formatted = fmt.Sprintf("%o", int64(num))
			} else if b, ok := value.(BoolValue); ok {
				// In Python, bool is a subclass of int, so True is 1 and False is 0
				if b {
					formatted = "1"
				} else {
					formatted = "0"
				}
			} else {
				return nil, &TypeError{Message: "%o format: a number is required"}
			}
		default:
			return nil, &ValueError{Message: fmt.Sprintf("unsupported format character '%c'", fmtType)}
		}

		// Apply width padding if specified
		if hasWidth {
			currentLen := len(formatted)
			if width > currentLen {
				padding := strings.Repeat(" ", width-currentLen)
				if leftAlign {
					formatted = formatted + padding
				} else {
					formatted = padding + formatted
				}
			}
		}

		result.WriteString(formatted)
	}

	if valueDict == nil && valueIdx < len(valueTuple) {
		return nil, &TypeError{Message: "not all arguments converted during string formatting"}
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
				return nil, &TypeError{Message: fmt.Sprintf("replace() takes at least 2 arguments (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))

			old, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "replace() argument 1 must be a string"}
			}

			new, ok := args[1].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "replace() argument 2 must be a string"}
			}

			if len(args) >= 3 {
				if count, ok := args[2].(NumberValue); ok {
					return StringValue(strings.Replace(s, string(old), string(new), int(count))), nil
				}
				return nil, &TypeError{Message: "replace() argument 3 must be a number"}
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
				return nil, &TypeError{Message: fmt.Sprintf("startswith() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))

			// Handle single string prefix
			if prefix, ok := args[0].(StringValue); ok {
				return BoolValue(strings.HasPrefix(s, string(prefix))), nil
			}

			// Handle tuple of string prefixes
			if prefixTuple, ok := args[0].(TupleValue); ok {
				for _, prefixVal := range prefixTuple {
					prefix, ok := prefixVal.(StringValue)
					if !ok {
						return nil, &TypeError{Message: "tuple for startswith must contain only strings"}
					}
					if strings.HasPrefix(s, string(prefix)) {
						return BoolValue(true), nil
					}
				}
				return BoolValue(false), nil
			}

			return nil, &TypeError{Message: "startswith() argument must be a string or tuple of strings"}
		},
	}

	td.Methods["endswith"] = &MethodDescriptor{
		Name:    "endswith",
		Arity:   1,
		Doc:     "Return True if string ends with the suffix, otherwise return False",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: fmt.Sprintf("endswith() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))

			// Handle single string suffix
			if suffix, ok := args[0].(StringValue); ok {
				return BoolValue(strings.HasSuffix(s, string(suffix))), nil
			}

			// Handle tuple of string suffixes
			if suffixTuple, ok := args[0].(TupleValue); ok {
				for _, suffixVal := range suffixTuple {
					suffix, ok := suffixVal.(StringValue)
					if !ok {
						return nil, &TypeError{Message: "tuple for endswith must contain only strings"}
					}
					if strings.HasSuffix(s, string(suffix)) {
						return BoolValue(true), nil
					}
				}
				return BoolValue(false), nil
			}

			return nil, &TypeError{Message: "endswith() argument must be a string or tuple of strings"}
		},
	}

	td.Methods["find"] = &MethodDescriptor{
		Name:    "find",
		Arity:   -1, // Variable arity: 1-3 args (sub, start=0, end=len)
		Doc:     "Return the lowest index where substring is found, or -1 if not found. Optional start and end positions",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 3 {
				return nil, &TypeError{Message: fmt.Sprintf("find() takes 1 to 3 arguments (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "find() argument must be a string"}
			}

			// Optional start parameter (default 0)
			start := 0
			if len(args) >= 2 {
				startNum, ok := args[1].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "find() start position must be an integer"}
				}
				start = int(startNum)
				if start < 0 {
					start = 0
				}
				if start > len(s) {
					start = len(s)
				}
			}

			// Optional end parameter (default len(s))
			end := len(s)
			if len(args) >= 3 {
				endNum, ok := args[2].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "find() end position must be an integer"}
				}
				end = int(endNum)
				if end < 0 {
					end = 0
				}
				if end > len(s) {
					end = len(s)
				}
			}

			// Search within the slice
			substr := s[start:end]
			idx := strings.Index(substr, string(sub))
			if idx == -1 {
				return NumberValue(-1), nil
			}
			return NumberValue(start + idx), nil
		},
	}

	td.Methods["join"] = &MethodDescriptor{
		Name:    "join",
		Arity:   1,
		Doc:     "Return a string which is the concatenation of the strings in the iterable",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: fmt.Sprintf("join() takes exactly one argument (%d given)", len(args))}
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
						return nil, &TypeError{Message: fmt.Sprintf("sequence item %d: expected str instance, %s found", i, item.Type())}
					}
				}
			case TupleValue:
				parts = make([]string, len(v))
				for i, item := range v {
					if s, ok := item.(StringValue); ok {
						parts[i] = string(s)
					} else {
						return nil, &TypeError{Message: fmt.Sprintf("sequence item %d: expected str instance, %s found", i, item.Type())}
					}
				}
			default:
				// Check if it implements Iterable interface
				if iter, ok := args[0].(Iterable); ok {
					parts = make([]string, 0)
					iterator := iter.Iterator()
					idx := 0
					for {
						item, hasNext := iterator.Next()
						if !hasNext {
							break
						}
						if s, ok := item.(StringValue); ok {
							parts = append(parts, string(s))
						} else {
							return nil, &TypeError{Message: fmt.Sprintf("sequence item %d: expected str instance, %s found", idx, item.Type())}
						}
						idx++
					}
				} else {
					return nil, &TypeError{Message: "join() argument must be an iterable"}
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
				return nil, &TypeError{Message: fmt.Sprintf("count() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "count() argument must be a string"}
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
						return nil, &ValueError{Message: "unmatched '{' in format string"}
					}

					// Extract the format spec
					spec := s[i+1 : j-1]

					// Parse the spec: [field][!conversion][:format_spec]
					var fieldSpec, conversion, formatSpec string
					var argIndex int

					// Split on ':' to separate field from format spec
					colonIdx := strings.IndexByte(spec, ':')
					if colonIdx >= 0 {
						fieldSpec = spec[:colonIdx]
						formatSpec = spec[colonIdx+1:]
					} else {
						fieldSpec = spec
					}

					// Check for conversion specifier (!r, !s, !a)
					bangIdx := strings.IndexByte(fieldSpec, '!')
					if bangIdx >= 0 {
						conversion = fieldSpec[bangIdx+1:]
						fieldSpec = fieldSpec[:bangIdx]
						// Validate conversion specifier
						if conversion != "r" && conversion != "s" && conversion != "a" {
							return nil, &ValueError{Message: fmt.Sprintf("invalid conversion specifier '!%s' in format string", conversion)}
						}
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
							return nil, &ValueError{Message: fmt.Sprintf("invalid field name '%s' in format string", fieldSpec)}
						}
					}

					if argIndex >= len(args) {
						return nil, &ValueError{Message: fmt.Sprintf("replacement index %d out of range for positional args tuple", argIndex)}
					}

					// Apply conversion if specified
					value := args[argIndex]
					if conversion != "" {
						var err error
						value, err = applyConversion(value, conversion)
						if err != nil {
							return nil, err
						}
					}

					// Format the argument
					formatted, err := formatValueWithSpec(value, formatSpec)
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
					return nil, &ValueError{Message: "single '}' encountered in format string"}
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
			return nil, &TypeError{Message: "lstrip() argument must be a string"}
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
			return nil, &TypeError{Message: "rstrip() argument must be a string"}
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
				if !unicode.IsSpace(r) {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["isalnum"] = &MethodDescriptor{
		Name:    "isalnum",
		Arity:   0,
		Doc:     "Return True if all characters in the string are alphanumeric",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if !unicode.IsLetter(r) && !unicode.IsDigit(r) {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["isdecimal"] = &MethodDescriptor{
		Name:    "isdecimal",
		Arity:   0,
		Doc:     "Return True if all characters in the string are decimal characters",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if !unicode.IsDigit(r) {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["isnumeric"] = &MethodDescriptor{
		Name:    "isnumeric",
		Arity:   0,
		Doc:     "Return True if all characters in the string are numeric characters",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			for _, r := range s {
				if !unicode.IsNumber(r) {
					return False, nil
				}
			}
			return True, nil
		},
	}

	td.Methods["islower"] = &MethodDescriptor{
		Name:    "islower",
		Arity:   0,
		Doc:     "Return True if all cased characters in the string are lowercase",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			hasCased := false
			for _, r := range s {
				if unicode.IsUpper(r) {
					return False, nil
				}
				if unicode.IsLower(r) {
					hasCased = true
				}
			}
			return BoolValue(hasCased), nil
		},
	}

	td.Methods["isupper"] = &MethodDescriptor{
		Name:    "isupper",
		Arity:   0,
		Doc:     "Return True if all cased characters in the string are uppercase",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			hasCased := false
			for _, r := range s {
				if unicode.IsLower(r) {
					return False, nil
				}
				if unicode.IsUpper(r) {
					hasCased = true
				}
			}
			return BoolValue(hasCased), nil
		},
	}

	td.Methods["istitle"] = &MethodDescriptor{
		Name:    "istitle",
		Arity:   0,
		Doc:     "Return True if the string is a titlecased string",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(s) == 0 {
				return False, nil
			}
			hasCased := false
			prevCased := false
			for _, r := range s {
				if unicode.IsUpper(r) {
					if prevCased {
						return False, nil
					}
					hasCased = true
					prevCased = true
				} else if unicode.IsLower(r) {
					if !prevCased {
						return False, nil
					}
					hasCased = true
					prevCased = true
				} else {
					prevCased = false
				}
			}
			return BoolValue(hasCased), nil
		},
	}

	td.Methods["index"] = &MethodDescriptor{
		Name:    "index",
		Arity:   1,
		Doc:     "Like find(), but raises ValueError when the substring is not found",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: fmt.Sprintf("index() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "index() argument must be a string"}
			}

			idx := strings.Index(s, string(sub))
			if idx == -1 {
				return nil, &ValueError{Message: "substring not found"}
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
			return nil, &TypeError{Message: "strip() argument must be a string"}
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
			var parts []string
			limit := -1

			// Check if separator is provided
			hasSep := len(args) > 0 && args[0] != Nil

			if len(args) > 1 {
				if maxSplit, ok := args[1].(NumberValue); ok {
					limit = int(maxSplit)
					if limit < 0 {
						limit = -1
					}
				} else {
					return nil, &TypeError{Message: "maxsplit must be a number"}
				}
			}

			if !hasSep {
				// Python behavior: split on any whitespace and remove empty strings
				// This is different from splitting on a single space!
				if limit == -1 {
					parts = strings.Fields(s)
				} else {
					// Manual implementation of FieldsN (doesn't exist in Go)
					parts = strings.Fields(s)
					if len(parts) > limit+1 {
						// Rejoin the parts beyond the limit
						remainder := strings.Join(parts[limit:], " ")
						parts = append(parts[:limit], remainder)
					}
				}
			} else {
				// Separator provided - use normal split
				sep := ""
				if sepStr, ok := args[0].(StringValue); ok {
					sep = string(sepStr)
				} else if args[0] != Nil {
					return nil, &TypeError{Message: "sep must be a string or None"}
				}

				if limit == -1 {
					parts = strings.Split(s, sep)
				} else {
					parts = strings.SplitN(s, sep, limit+1)
				}
			}

			result := make([]Value, len(parts))
			for i, part := range parts {
				result[i] = StringValue(part)
			}
			return NewList(result...), nil
		},
	}

	// splitlines - split on line boundaries
	td.Methods["splitlines"] = &MethodDescriptor{
		Name:    "splitlines",
		Arity:   -1,
		Doc:     "Return a list of the lines in the string, breaking at line boundaries",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			return stringSplitlinesImpl(receiver, args, nil)
		},
		KwargHandler: func(receiver Value, args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
			return stringSplitlinesImpl(receiver, args, kwargs)
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
				return nil, &TypeError{Message: fmt.Sprintf("removeprefix() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			prefix, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "removeprefix() argument must be a string"}
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
				return nil, &TypeError{Message: fmt.Sprintf("removesuffix() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			suffix, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "removesuffix() argument must be a string"}
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
				return nil, &TypeError{Message: fmt.Sprintf("partition() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			sep, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "partition() argument must be a string"}
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
				return nil, &TypeError{Message: fmt.Sprintf("rpartition() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			sep, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "rpartition() argument must be a string"}
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
					return nil, &TypeError{Message: "expandtabs() argument must be a number"}
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
				return nil, &TypeError{Message: fmt.Sprintf("translate() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))

			// Translation table should be a dict mapping rune codes to replacement strings
			table, ok := args[0].(*DictValue)
			if !ok {
				return nil, &TypeError{Message: "translate() argument must be a dict"}
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
				return nil, &TypeError{Message: fmt.Sprintf("zfill() takes exactly one argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			width, ok := args[0].(NumberValue)
			if !ok {
				return nil, &TypeError{Message: "zfill() argument must be a number"}
			}

			// Use rune length for proper Unicode handling
			runes := []rune(s)
			runeLen := len(runes)
			w := int(width)
			if w <= runeLen {
				return receiver, nil
			}

			// Check for sign (first character)
			sign := ""
			if runeLen > 0 && (runes[0] == '+' || runes[0] == '-') {
				sign = string(runes[0])
				runes = runes[1:]
				runeLen--
			}

			padding := w - runeLen - len([]rune(sign))
			return StringValue(sign + strings.Repeat("0", padding) + string(runes)), nil
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
				return nil, &TypeError{Message: fmt.Sprintf("__mod__() takes exactly 1 argument (%d given)", len(args))}
			}

			formatStr := string(receiver.(StringValue))
			return formatStringWithPercent(formatStr, args[0])
		},
	}
}

// stringSplitlinesImpl implements str.splitlines() with support for keyword arguments
func stringSplitlinesImpl(receiver Value, args []Value, kwargs map[string]Value) (Value, error) {
	s := string(receiver.(StringValue))
	keepends := false

	// Check for keyword argument first
	if kwargs != nil {
		if keependsVal, hasKeepends := kwargs["keepends"]; hasKeepends {
			if keependsVal == True {
				keepends = true
			} else if keependsVal == False {
				keepends = false
			} else if num, ok := keependsVal.(NumberValue); ok {
				keepends = num != 0
			} else {
				return nil, &TypeError{Message: "splitlines() argument must be a bool or int"}
			}
		}
		// Check for unknown keyword arguments
		for k := range kwargs {
			if k != "keepends" {
				return nil, &TypeError{Message: fmt.Sprintf("splitlines() got an unexpected keyword argument '%s'", k)}
			}
		}
	}

	// Positional argument overrides keyword argument
	if len(args) > 0 {
		if args[0] == True {
			keepends = true
		} else if args[0] == False {
			keepends = false
		} else if args[0] != Nil {
			if num, ok := args[0].(NumberValue); ok {
				keepends = num != 0
			} else {
				return nil, &TypeError{Message: "splitlines() argument must be a bool or int"}
			}
		}
	}

	var lines []string
	var currentLine strings.Builder
	runes := []rune(s)

	for i := 0; i < len(runes); i++ {
		r := runes[i]

		// Check for line breaks
		if r == '\n' {
			if keepends {
				currentLine.WriteRune('\n')
			}
			lines = append(lines, currentLine.String())
			currentLine.Reset()
		} else if r == '\r' {
			// Check for \r\n
			if i+1 < len(runes) && runes[i+1] == '\n' {
				if keepends {
					currentLine.WriteString("\r\n")
				}
				lines = append(lines, currentLine.String())
				currentLine.Reset()
				i++ // Skip the \n
			} else {
				if keepends {
					currentLine.WriteRune('\r')
				}
				lines = append(lines, currentLine.String())
				currentLine.Reset()
			}
		} else if r == '\v' || r == '\f' || r == '\x1c' || r == '\x1d' || r == '\x1e' || r == '\x85' || r == '\u2028' || r == '\u2029' {
			// Other line breaks
			if keepends {
				currentLine.WriteRune(r)
			}
			lines = append(lines, currentLine.String())
			currentLine.Reset()
		} else {
			currentLine.WriteRune(r)
		}
	}

	// Add the last line if there's anything left
	if currentLine.Len() > 0 || len(lines) == 0 {
		lines = append(lines, currentLine.String())
	}

	// Convert to list of StringValues
	result := make([]Value, len(lines))
	for i, line := range lines {
		result[i] = StringValue(line)
	}
	return NewList(result...), nil
}

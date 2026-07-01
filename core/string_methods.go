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
// strFormat implements str.format(args, **kwargs) with full Python semantics:
// positional {0}/{} and keyword {name} field access, conversion (!r/!s/!a),
// format specs (:>10.2f), and attribute/index sub-fields (e.g. {obj.attr}).
func strFormat(s string, args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	result := &strings.Builder{}
	autoIdx := 0
	i := 0
	for i < len(s) {
		ch := s[i]
		if ch == '{' {
			if i+1 < len(s) && s[i+1] == '{' {
				result.WriteByte('{')
				i += 2
				continue
			}
			// Find the closing '}'
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
			spec := s[i+1 : j-1]

			// Split field from format_spec at ':'
			var fieldExpr, formatSpec string
			colonIdx := strings.IndexByte(spec, ':')
			if colonIdx >= 0 {
				fieldExpr = spec[:colonIdx]
				formatSpec = spec[colonIdx+1:]
			} else {
				fieldExpr = spec
			}

			// Split conversion from field at '!'
			var conversion string
			bangIdx := strings.IndexByte(fieldExpr, '!')
			if bangIdx >= 0 {
				conversion = fieldExpr[bangIdx+1:]
				fieldExpr = fieldExpr[:bangIdx]
				if conversion != "r" && conversion != "s" && conversion != "a" {
					return nil, &ValueError{Message: fmt.Sprintf("unknown conversion specifier '!%s'", conversion)}
				}
			}

			// Resolve the value for fieldExpr
			val, err := resolveFormatField(fieldExpr, args, kwargs, &autoIdx)
			if err != nil {
				return nil, err
			}

			// Apply conversion
			if conversion != "" {
				val, err = applyConversion(val, conversion)
				if err != nil {
					return nil, err
				}
			}

			// Resolve any nested replacement fields in the format spec
			// (dynamic width/precision, e.g. "{:^{}}" or "{:.{}f}"). These
			// draw from the same autoIdx, so they consume positional args in
			// the order they appear after the outer field's own value.
			if strings.IndexByte(formatSpec, '{') >= 0 {
				formatSpec, err = resolveNestedSpec(formatSpec, args, kwargs, &autoIdx)
				if err != nil {
					return nil, err
				}
			}

			// Apply format spec
			formatted, err := formatValueWithSpec(val, formatSpec)
			if err != nil {
				return nil, err
			}
			result.WriteString(formatted)
			i = j
		} else if ch == '}' {
			if i+1 < len(s) && s[i+1] == '}' {
				result.WriteByte('}')
				i += 2
				continue
			}
			return nil, &ValueError{Message: "single '}' encountered in format string"}
		} else {
			result.WriteByte(ch)
			i++
		}
	}
	return StringValue(result.String()), nil
}

// resolveFormatField resolves a format field expression like "0", "name",
// "obj.attr", "obj[key]" against positional args and kwargs.
func resolveFormatField(field string, args []Value, kwargs map[string]Value, autoIdx *int) (Value, error) {
	if field == "" {
		// Auto-numbering: {}
		idx := *autoIdx
		*autoIdx++
		if idx >= len(args) {
			return nil, &IndexError{Message: fmt.Sprintf("replacement index %d out of range for positional args tuple", idx)}
		}
		return args[idx], nil
	}

	// Split off attribute/index lookups: "obj.attr", "obj[key]"
	// The root name is up to the first '.' or '['
	root := field
	rest := ""
	for k, c := range field {
		if c == '.' || c == '[' {
			root = field[:k]
			rest = field[k:]
			break
		}
	}

	// Resolve root: integer index or keyword name
	var val Value
	if n, err := fmt.Sscanf(root, "%d", new(int)); n == 1 && err == nil {
		var idx int
		fmt.Sscanf(root, "%d", &idx)
		if idx >= len(args) {
			return nil, &IndexError{Message: fmt.Sprintf("replacement index %d out of range for positional args tuple", idx)}
		}
		val = args[idx]
	} else if root != "" {
		var ok bool
		if kwargs != nil {
			val, ok = kwargs[root]
		}
		if !ok {
			return nil, &KeyError{Key: StringValue(root)}
		}
	} else {
		idx := *autoIdx
		*autoIdx++
		if idx >= len(args) {
			return nil, &IndexError{Message: fmt.Sprintf("replacement index %d out of range for positional args tuple", idx)}
		}
		val = args[idx]
	}

	// Walk attribute/index lookups
	for rest != "" {
		if rest[0] == '.' {
			// Attribute: .attr
			end := 1
			for end < len(rest) && rest[end] != '.' && rest[end] != '[' {
				end++
			}
			attr := rest[1:end]
			rest = rest[end:]
			if obj, ok := val.(interface{ GetAttr(string) (Value, bool) }); ok {
				if v, found := obj.GetAttr(attr); found {
					val = v
					continue
				}
			}
			return nil, &AttributeError{ObjType: string(val.Type()), AttrName: attr}
		} else if rest[0] == '[' {
			// Index: [key] or [n]
			end := strings.IndexByte(rest, ']')
			if end < 0 {
				return nil, &ValueError{Message: "invalid format field: missing ']'"}
			}
			key := rest[1:end]
			rest = rest[end+1:]
			if n, err := fmt.Sscanf(key, "%d", new(int)); n == 1 && err == nil {
				var idx int
				fmt.Sscanf(key, "%d", &idx)
				if lst, ok := val.(*ListValue); ok {
					if idx < 0 || idx >= lst.Len() {
						return nil, &IndexError{Index: idx}
					}
					val = lst.items[idx]
				} else {
					return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not subscriptable", val.Type())}
				}
			} else {
				if d, ok := val.(*DictValue); ok {
					if v, exists := d.entries[key]; exists {
						val = v
					} else {
						return nil, &KeyError{Key: StringValue(key)}
					}
				} else {
					return nil, &TypeError{Message: fmt.Sprintf("'%s' object is not subscriptable with key '%s'", val.Type(), key)}
				}
			}
		} else {
			break
		}
	}
	return val, nil
}

// resolveNestedSpec substitutes one level of nested replacement fields inside a
// format spec — Python's dynamic width/precision, e.g. "^{}" -> "^6" or
// ".{prec}f" -> ".2f". Nested auto-fields ({}) draw from the shared autoIdx, so
// they consume positional arguments in order alongside the outer fields. A
// nested field may carry a conversion (!r/!s/!a) but, per Python, not a further
// nested format spec.
func resolveNestedSpec(spec string, args []Value, kwargs map[string]Value, autoIdx *int) (string, error) {
	var b strings.Builder
	i := 0
	for i < len(spec) {
		ch := spec[i]
		switch ch {
		case '{':
			if i+1 < len(spec) && spec[i+1] == '{' {
				b.WriteByte('{')
				i += 2
				continue
			}
			j := i + 1
			for j < len(spec) && spec[j] != '}' {
				j++
			}
			if j >= len(spec) {
				return "", &ValueError{Message: "unmatched '{' in format spec"}
			}
			field := spec[i+1 : j]
			var conversion string
			if bang := strings.IndexByte(field, '!'); bang >= 0 {
				conversion = field[bang+1:]
				field = field[:bang]
			}
			val, err := resolveFormatField(field, args, kwargs, autoIdx)
			if err != nil {
				return "", err
			}
			if conversion != "" {
				if val, err = applyConversion(val, conversion); err != nil {
					return "", err
				}
			}
			s, err := formatValueWithSpec(val, "")
			if err != nil {
				return "", err
			}
			b.WriteString(s)
			i = j + 1
		case '}':
			if i+1 < len(spec) && spec[i+1] == '}' {
				b.WriteByte('}')
				i += 2
				continue
			}
			b.WriteByte('}')
			i++
		default:
			b.WriteByte(ch)
			i++
		}
	}
	return b.String(), nil
}

func applyConversion(value Value, conversion string) (Value, error) {
	switch conversion {
	case "s":
		// str() conversion - default string representation
		return StringValue(PrintValueWithoutQuotes(value)), nil
	case "r":
		// repr() conversion - must match the repr() builtin (core.Repr),
		// not String(): e.g. 'hi' not "hi", <function f at 0x..> not <function f(x)>.
		return StringValue(Repr(value)), nil
	case "a":
		// ascii() conversion - repr() with non-ASCII escaped
		return StringValue(asciiOf(value)), nil
	default:
		return nil, &ValueError{Message: fmt.Sprintf("unknown conversion specifier '!%s'", conversion)}
	}
}

// asciiOf returns the ascii() representation of value: repr() with every
// non-ASCII character escaped, matching CPython (\xXX for U+0080..U+00FF,
// \uXXXX up to U+FFFF, \UXXXXXXXX beyond). Used by both the !a string
// conversion and the %a printf-style format code.
func asciiOf(value Value) string {
	repr := Repr(value)
	var result strings.Builder
	for _, r := range repr {
		switch {
		case r < 128:
			result.WriteRune(r)
		case r <= 0xFF:
			result.WriteString(fmt.Sprintf("\\x%02x", r))
		case r <= 0xFFFF:
			result.WriteString(fmt.Sprintf("\\u%04x", r))
		default:
			result.WriteString(fmt.Sprintf("\\U%08x", r))
		}
	}
	return result.String()
}

// formatValueWithSpec formats a value according to a Python-style format specification
// addThousandsSeparator inserts a grouping separator (',' or '_') every three
// digits of the integer part of a formatted number string, preserving any sign
// and fractional/exponent tail. Mirrors builtin/string_format.go's helper (core
// cannot import builtin).
func addThousandsSeparator(s string, sep rune) string {
	// Split off a fractional/exponent tail so only the integer part is grouped.
	intPart := s
	tail := ""
	if dot := strings.IndexByte(s, '.'); dot >= 0 {
		intPart = s[:dot]
		tail = s[dot:]
	}

	sign := ""
	if len(intPart) > 0 && (intPart[0] == '+' || intPart[0] == '-' || intPart[0] == ' ') {
		sign = string(intPart[0])
		intPart = intPart[1:]
	}

	// Only group runs of plain digits (leave things like 'inf'/'nan' alone).
	for _, r := range intPart {
		if r < '0' || r > '9' {
			return s
		}
	}
	if len(intPart) <= 3 {
		return sign + intPart + tail
	}

	var grouped []rune
	n := len(intPart)
	for i, r := range intPart {
		if i > 0 && (n-i)%3 == 0 {
			grouped = append(grouped, sep)
		}
		grouped = append(grouped, r)
	}
	return sign + string(grouped) + tail
}

func formatValueWithSpec(value Value, spec string) (string, error) {
	if spec == "" {
		// No format spec - use default formatting
		if str, ok := value.(StringValue); ok {
			return string(str), nil
		} else if _, ok := value.(NumberValue); ok {
			f, _ := AsFloat(value)
			if f == math.Floor(f) {
				return fmt.Sprintf("%.0f", f), nil
			}
			return fmt.Sprintf("%g", f), nil
		}
		// A float with no spec uses its Python repr (str(float)), e.g. "3.0".
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
	var grouping rune

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

	// Grouping option (',' or '_'). NOTE: application of the separator is not
	// yet implemented on this (.format() method) path -- see formatValueWithSpec
	// vs builtin parseFormatSpecString. But a second grouping char is invalid
	// and must raise, matching CPython's exact messages.
	if i < len(spec) && (spec[i] == ',' || spec[i] == '_') {
		grouping = rune(spec[i])
		i++
		if i < len(spec) && (spec[i] == ',' || spec[i] == '_') {
			if rune(spec[i]) == grouping {
				return "", &ValueError{Message: fmt.Sprintf("Cannot specify '%c' with '%c'.", grouping, grouping)}
			}
			return "", &ValueError{Message: "Cannot specify both ',' and '_'."}
		}
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

	if f, ok := AsFloat(value); ok {
		// Number formatting (int or float). A BigIntValue must use its exact
		// value for integer formats, not the lossy float64 from AsFloat; only
		// the float formats ('f','e','g','%') convert to float (as CPython does).
		bigInt, isBig := value.(BigIntValue)
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
			if isBig {
				result = bigInt.String()
			} else {
				result = fmt.Sprintf("%.0f", math.Trunc(f))
			}
		case 'x':
			// Hexadecimal (lowercase)
			if isBig {
				result = bigInt.GetBigInt().Text(16)
			} else {
				result = fmt.Sprintf("%x", int64(f))
			}
			if alt {
				result = "0x" + result
			}
		case 'X':
			// Hexadecimal (uppercase)
			if isBig {
				result = strings.ToUpper(bigInt.GetBigInt().Text(16))
			} else {
				result = fmt.Sprintf("%X", int64(f))
			}
			if alt {
				result = "0X" + result
			}
		case 'o':
			// Octal
			if isBig {
				result = bigInt.GetBigInt().Text(8)
			} else {
				result = fmt.Sprintf("%o", int64(f))
			}
			if alt && !strings.HasPrefix(result, "0") {
				result = "0" + result
			}
		case 'b':
			// Binary
			if isBig {
				result = bigInt.GetBigInt().Text(2)
			} else {
				result = strconv.FormatInt(int64(f), 2)
			}
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
			if isBig {
				result = bigInt.String()
			} else if f == math.Floor(f) {
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

		// Apply digit grouping (thousands separator) for decimal/float types.
		// (Edge case: combined with zero-pad-to-width the separators are not
		// woven into the leading zeros -- rare, and previously ungrouped anyway.)
		if grouping != 0 {
			switch fmtType {
			case 0, 'd', 'f', 'F', 'g', 'G', '%', 'n':
				result = addThousandsSeparator(result, grouping)
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
			// Default: right align for numbers, left for everything else
			// (CPython). Must cover every real numeric type, not just the
			// fixed-size int, else floats/bigints wrongly left-align.
			switch value.(type) {
			case NumberValue, FloatValue, BigIntValue, BoolValue, ComplexValue:
				result = strings.Repeat(fillStr, padding) + result
			default:
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
		var zeroPad bool
		var plusFlag bool
		var spaceFlag bool
		var altFlag bool
		var width int
		var hasWidth bool
		var precision int
		var hasPrecision bool

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

			// Parse optional .precision (a bare "." means precision 0).
			precision = 0
			hasPrecision = false
			if i < len(formatStr) && formatStr[i] == '.' {
				i++
				hasPrecision = true
				precStart := i
				for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
					i++
				}
				if i > precStart {
					fmt.Sscanf(formatStr[precStart:i], "%d", &precision)
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

			// Parse optional flags (-, +, 0, space, #)
			leftAlign = false
			zeroPad = false
			plusFlag = false
			spaceFlag = false
			altFlag = false
			for i < len(formatStr) && (formatStr[i] == '-' || formatStr[i] == '+' ||
				formatStr[i] == '0' || formatStr[i] == ' ' || formatStr[i] == '#') {
				switch formatStr[i] {
				case '-':
					leftAlign = true
				case '0':
					zeroPad = true
				case '+':
					plusFlag = true
				case ' ':
					spaceFlag = true
				case '#':
					altFlag = true
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

			// Parse optional .precision (a bare "." means precision 0).
			precision = 0
			hasPrecision = false
			if i < len(formatStr) && formatStr[i] == '.' {
				i++
				hasPrecision = true
				precStart := i
				for i < len(formatStr) && formatStr[i] >= '0' && formatStr[i] <= '9' {
					i++
				}
				if i > precStart {
					fmt.Sscanf(formatStr[precStart:i], "%d", &precision)
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
			// A precision on %s truncates the string to that many characters.
			if hasPrecision && precision < len([]rune(formatted)) {
				formatted = string([]rune(formatted)[:precision])
			}
		case 'd', 'i': // Integer
			if num, ok := value.(NumberValue); ok {
				formatted = fmt.Sprintf("%d", int64(num))
			} else if fv, ok := value.(FloatValue); ok {
				// Python's %d truncates a float toward zero ("%d" % 2.7 == "2").
				formatted = fmt.Sprintf("%d", int64(float64(fv)))
			} else if bi, ok := value.(BigIntValue); ok {
				formatted = bi.GetBigInt().String()
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
		case 'f', 'F', 'e', 'E', 'g', 'G': // Float formats (fixed, exponential, general)
			var f float64
			if num, ok := value.(NumberValue); ok {
				f = float64(num)
			} else if fv, ok := value.(FloatValue); ok {
				f = float64(fv)
			} else if bi, ok := value.(BigIntValue); ok {
				f = bi.ToFloat64()
			} else if b, ok := value.(BoolValue); ok {
				if b {
					f = 1
				}
			} else {
				return nil, &TypeError{Message: fmt.Sprintf("%%%c format: a number is required, not %s", fmtType, value.Type())}
			}
			// Python (like C printf) defaults float precision to 6 when the
			// spec omits it; an explicit ".N" overrides. Go's verbs match
			// Python's once the precision is supplied, so thread it through.
			prec := 6
			if hasPrecision {
				prec = precision
			}
			formatted = fmt.Sprintf("%.*"+string(fmtType), prec, f)
		case 'r': // Repr - must match the repr() builtin, not String()
			formatted = Repr(value)
		case 'a': // ascii() - repr() with non-ASCII escaped
			formatted = asciiOf(value)
		case 'c': // Character
			if num, ok := value.(NumberValue); ok {
				formatted = string(rune(int(num)))
			} else if bi, ok := value.(BigIntValue); ok {
				if i64, fits := bi.ToInt64(); fits && i64 >= 0 && i64 <= 0x10FFFF {
					formatted = string(rune(i64))
				} else {
					return nil, &OverflowError{Message: "%c arg not in range(0x110000)"}
				}
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
			} else if bi, ok := value.(BigIntValue); ok {
				if fmtType == 'x' {
					formatted = fmt.Sprintf("%x", bi.GetBigInt())
				} else {
					formatted = fmt.Sprintf("%X", bi.GetBigInt())
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
			} else if bi, ok := value.(BigIntValue); ok {
				formatted = fmt.Sprintf("%o", bi.GetBigInt())
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

		// Apply the # (alternate form) prefix for integer bases, then the
		// + / space sign flag. The default Go verbs used above omit both.
		if altFlag {
			var prefix string
			switch fmtType {
			case 'x':
				prefix = "0x"
			case 'X':
				prefix = "0X"
			case 'o':
				prefix = "0o"
			}
			if prefix != "" {
				if len(formatted) > 0 && (formatted[0] == '-' || formatted[0] == '+') {
					formatted = string(formatted[0]) + prefix + formatted[1:]
				} else {
					formatted = prefix + formatted
				}
			}
		}
		if (plusFlag || spaceFlag) && strings.IndexByte("diouxXeEfFgG", fmtType) >= 0 {
			if len(formatted) == 0 || (formatted[0] != '-' && formatted[0] != '+') {
				if plusFlag {
					formatted = "+" + formatted
				} else {
					formatted = " " + formatted
				}
			}
		}

		// Apply width padding if specified
		if hasWidth {
			currentLen := len(formatted)
			if width > currentLen {
				n := width - currentLen
				// The '0' flag zero-pads numeric conversions (ignored when
				// left-aligned, matching Python). Zeros go after any sign.
				isNumeric := strings.IndexByte("diouxXeEfFgG", fmtType) >= 0
				if zeroPad && !leftAlign && isNumeric {
					zeros := strings.Repeat("0", n)
					if len(formatted) > 0 && (formatted[0] == '-' || formatted[0] == '+') {
						formatted = string(formatted[0]) + zeros + formatted[1:]
					} else {
						formatted = zeros + formatted
					}
				} else if leftAlign {
					formatted = formatted + strings.Repeat(" ", n)
				} else {
					formatted = strings.Repeat(" ", n) + formatted
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
		Arity:   -1, // 1-3 args: prefix[, start[, end]]
		Doc:     "Return True if string starts with the prefix, otherwise return False. With optional start, test begins there; with optional end, stop comparing there.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 3 {
				return nil, &TypeError{Message: fmt.Sprintf("startswith() takes from 1 to 3 arguments (%d given)", len(args))}
			}

			// Apply optional start/end positions (CPython slice semantics).
			s := affixSubstring(string(receiver.(StringValue)), args, 1)

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

			return nil, &TypeError{Message: "startswith first arg must be str or a tuple of str, not " + string(args[0].Type())}
		},
	}

	td.Methods["endswith"] = &MethodDescriptor{
		Name:    "endswith",
		Arity:   -1, // 1-3 args: suffix[, start[, end]]
		Doc:     "Return True if string ends with the suffix, otherwise return False. With optional start/end, test within that slice.",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 3 {
				return nil, &TypeError{Message: fmt.Sprintf("endswith() takes from 1 to 3 arguments (%d given)", len(args))}
			}

			// Apply optional start/end positions (CPython slice semantics).
			s := affixSubstring(string(receiver.(StringValue)), args, 1)

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

			return nil, &TypeError{Message: "endswith first arg must be str or a tuple of str, not " + string(args[0].Type())}
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

			// Optional start parameter (default 0). Negative values count from
			// the end, matching Python slice semantics.
			start := 0
			if len(args) >= 2 {
				startNum, ok := args[1].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "find() start position must be an integer"}
				}
				start = int(startNum)
				if start < 0 {
					start = len(s) + start
					if start < 0 {
						start = 0
					}
				}
				if start > len(s) {
					start = len(s)
				}
			}

			// Optional end parameter (default len(s)). Negative values count from
			// the end, matching Python slice semantics.
			end := len(s)
			if len(args) >= 3 {
				endNum, ok := args[2].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "find() end position must be an integer"}
				}
				end = int(endNum)
				if end < 0 {
					end = len(s) + end
					if end < 0 {
						end = 0
					}
				}
				if end > len(s) {
					end = len(s)
				}
			}

			// If start is past end the search window is empty, so report
			// "not found" rather than slicing s[start:end] (which would panic).
			if start > end {
				return NumberValue(-1), nil
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
		Arity:   -1, // Variable arity: 1-3 arguments
		Doc:     "Return the number of non-overlapping occurrences of substring sub in string s[start:end]",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 3 {
				return nil, &TypeError{Message: fmt.Sprintf("count() takes 1 to 3 arguments (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "count() argument must be a string"}
			}

			// Handle optional start and end arguments
			start := 0
			end := len(s)

			if len(args) >= 2 {
				if startNum, ok := args[1].(NumberValue); ok {
					start = int(startNum)
					if start < 0 {
						start = len(s) + start
						if start < 0 {
							start = 0
						}
					}
					if start > len(s) {
						start = len(s)
					}
				}
			}

			if len(args) >= 3 {
				if endNum, ok := args[2].(NumberValue); ok {
					end = int(endNum)
					if end < 0 {
						end = len(s) + end
						if end < 0 {
							end = 0
						}
					}
					if end > len(s) {
						end = len(s)
					}
				}
			}

			// Ensure start <= end
			if start > end {
				return NumberValue(0), nil
			}

			// Count in the substring
			searchStr := s[start:end]
			return NumberValue(strings.Count(searchStr, string(sub))), nil
		},
	}

	td.Methods["format"] = &MethodDescriptor{
		Name:    "format",
		Arity:   -1,
		Doc:     "Return a formatted version of the string",
		Builtin: true,
		KwargHandler: func(receiver Value, args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
			return strFormat(string(receiver.(StringValue)), args, kwargs, ctx)
		},
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			return strFormat(string(receiver.(StringValue)), args, nil, ctx)
		},
	}

	// format_map - like format but takes a single mapping argument
	td.Methods["format_map"] = &MethodDescriptor{
		Name:    "format_map",
		Arity:   1,
		Doc:     "Return a formatted version of the string using a mapping",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			if len(args) != 1 {
				return nil, &TypeError{Message: "format_map() takes exactly one argument"}
			}
			// Build kwargs from the mapping
			kwargs := make(map[string]Value)
			if d, ok := args[0].(*DictValue); ok {
				for k, v := range d.entries {
					kwargs[k] = v
				}
			}
			return strFormat(s, nil, kwargs, ctx)
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
			// Unicode-aware, matching Python: e.g. Arabic-Indic and other Nd
			// digits count, not just ASCII 0-9.
			for _, r := range s {
				if !unicode.IsDigit(r) {
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
			// Unicode-aware, matching Python: accented and non-Latin letters
			// (café, naïve, Москва, 日本語) count, not just ASCII a-zA-Z.
			for _, r := range s {
				if !unicode.IsLetter(r) {
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

	// swapcase - swap case of each character
	td.Methods["swapcase"] = &MethodDescriptor{
		Name:    "swapcase",
		Arity:   0,
		Doc:     "Return a copy of the string with uppercase characters converted to lowercase and vice versa",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			var result strings.Builder
			for _, r := range s {
				if unicode.IsUpper(r) {
					result.WriteRune(unicode.ToLower(r))
				} else if unicode.IsLower(r) {
					result.WriteRune(unicode.ToUpper(r))
				} else {
					result.WriteRune(r)
				}
			}
			return StringValue(result.String()), nil
		},
	}

	// casefold - aggressive lowercase for caseless matching
	td.Methods["casefold"] = &MethodDescriptor{
		Name:    "casefold",
		Arity:   0,
		Doc:     "Return a casefolded copy of the string for caseless matching",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			// Go's strings.ToLower is similar to casefold for most cases
			// For full Unicode casefold, we'd need more complex logic
			return StringValue(strings.ToLower(s)), nil
		},
	}

	// rindex - like rfind but raises ValueError if not found
	td.Methods["rindex"] = &MethodDescriptor{
		Name:    "rindex",
		Arity:   -1,
		Doc:     "Like rfind() but raises ValueError when substring is not found",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 3 {
				return nil, &TypeError{Message: fmt.Sprintf("rindex() takes 1 to 3 arguments (%d given)", len(args))}
			}
			s := string(receiver.(StringValue))
			sub, ok := args[0].(StringValue)
			if !ok {
				return nil, &TypeError{Message: "rindex() argument 1 must be str"}
			}

			// Get start and end indices
			start := 0
			end := len(s)

			if len(args) >= 2 {
				startNum, ok := args[1].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "rindex() argument 2 must be int"}
				}
				start = int(startNum)
				if start < 0 {
					start = len(s) + start
					if start < 0 {
						start = 0
					}
				}
			}

			if len(args) >= 3 {
				endNum, ok := args[2].(NumberValue)
				if !ok {
					return nil, &TypeError{Message: "rindex() argument 3 must be int"}
				}
				end = int(endNum)
				if end < 0 {
					end = len(s) + end
					if end < 0 {
						end = 0
					}
				}
			}

			// Clamp indices
			if start > len(s) {
				start = len(s)
			}
			if end > len(s) {
				end = len(s)
			}
			if start > end {
				return nil, &ValueError{Message: "substring not found"}
			}

			slice := s[start:end]
			idx := strings.LastIndex(slice, string(sub))
			if idx == -1 {
				return nil, &ValueError{Message: "substring not found"}
			}
			return NumberValue(start + idx), nil
		},
	}

	// isprintable - check if all characters are printable
	td.Methods["isprintable"] = &MethodDescriptor{
		Name:    "isprintable",
		Arity:   0,
		Doc:     "Return True if all characters in the string are printable",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			// Empty string is printable
			if len(s) == 0 {
				return True, nil
			}
			for _, r := range s {
				// In Python, printable means IsPrint or is space (but not other whitespace like \n, \t)
				if !unicode.IsPrint(r) && r != ' ' {
					return False, nil
				}
			}
			return True, nil
		},
	}

	// center - center string in width
	td.Methods["center"] = &MethodDescriptor{
		Name:    "center",
		Arity:   -1,
		Doc:     "Return a centered string of length width",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 2 {
				return nil, &TypeError{Message: fmt.Sprintf("center() takes 1 to 2 arguments (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			width, ok := args[0].(NumberValue)
			if !ok {
				return nil, &TypeError{Message: "center() argument 1 must be int"}
			}

			fillchar := " "
			if len(args) == 2 {
				fc, ok := args[1].(StringValue)
				if !ok {
					return nil, &TypeError{Message: "center() argument 2 must be str"}
				}
				if len([]rune(string(fc))) != 1 {
					return nil, &TypeError{Message: "The fill character must be exactly one character long"}
				}
				fillchar = string(fc)
			}

			runeLen := len([]rune(s))
			w := int(width)
			if w <= runeLen {
				return receiver, nil
			}

			padding := w - runeLen
			leftPad := padding / 2
			rightPad := padding - leftPad
			return StringValue(strings.Repeat(fillchar, leftPad) + s + strings.Repeat(fillchar, rightPad)), nil
		},
	}

	// ljust - left justify string in width
	td.Methods["ljust"] = &MethodDescriptor{
		Name:    "ljust",
		Arity:   -1,
		Doc:     "Return a left-justified string of length width",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 2 {
				return nil, &TypeError{Message: fmt.Sprintf("ljust() takes 1 to 2 arguments (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			width, ok := args[0].(NumberValue)
			if !ok {
				return nil, &TypeError{Message: "ljust() argument 1 must be int"}
			}

			fillchar := " "
			if len(args) == 2 {
				fc, ok := args[1].(StringValue)
				if !ok {
					return nil, &TypeError{Message: "ljust() argument 2 must be str"}
				}
				if len([]rune(string(fc))) != 1 {
					return nil, &TypeError{Message: "The fill character must be exactly one character long"}
				}
				fillchar = string(fc)
			}

			runeLen := len([]rune(s))
			w := int(width)
			if w <= runeLen {
				return receiver, nil
			}

			padding := w - runeLen
			return StringValue(s + strings.Repeat(fillchar, padding)), nil
		},
	}

	// rjust - right justify string in width
	td.Methods["rjust"] = &MethodDescriptor{
		Name:    "rjust",
		Arity:   -1,
		Doc:     "Return a right-justified string of length width",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) < 1 || len(args) > 2 {
				return nil, &TypeError{Message: fmt.Sprintf("rjust() takes 1 to 2 arguments (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			width, ok := args[0].(NumberValue)
			if !ok {
				return nil, &TypeError{Message: "rjust() argument 1 must be int"}
			}

			fillchar := " "
			if len(args) == 2 {
				fc, ok := args[1].(StringValue)
				if !ok {
					return nil, &TypeError{Message: "rjust() argument 2 must be str"}
				}
				if len([]rune(string(fc))) != 1 {
					return nil, &TypeError{Message: "The fill character must be exactly one character long"}
				}
				fillchar = string(fc)
			}

			runeLen := len([]rune(s))
			w := int(width)
			if w <= runeLen {
				return receiver, nil
			}

			padding := w - runeLen
			return StringValue(strings.Repeat(fillchar, padding) + s), nil
		},
	}

	// rsplit - split from right
	td.Methods["rsplit"] = &MethodDescriptor{
		Name:    "rsplit",
		Arity:   -1,
		Doc:     "Return a list of the words in the string, splitting from the right",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			s := string(receiver.(StringValue))
			var parts []string
			limit := -1
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
				// Split on whitespace from right
				// For simplicity, use Fields and reverse if needed
				parts = strings.Fields(s)
				if limit >= 0 && len(parts) > limit+1 {
					// Join the parts from the left beyond the limit
					remainder := strings.Join(parts[:len(parts)-limit], " ")
					parts = append([]string{remainder}, parts[len(parts)-limit:]...)
				}
			} else {
				sep := ""
				if sepStr, ok := args[0].(StringValue); ok {
					sep = string(sepStr)
				} else {
					return nil, &TypeError{Message: "sep must be a string or None"}
				}

				if limit == -1 {
					parts = strings.Split(s, sep)
				} else {
					// Split from right: reverse, split, reverse results
					// Go doesn't have rsplit, so we implement it manually
					runes := []rune(s)
					sepRunes := []rune(sep)

					// Find all separator positions from right
					var splits []int
					for i := len(runes) - len(sepRunes); i >= 0 && (limit == -1 || len(splits) < limit); i-- {
						match := true
						for j := 0; j < len(sepRunes); j++ {
							if runes[i+j] != sepRunes[j] {
								match = false
								break
							}
						}
						if match {
							splits = append([]int{i}, splits...)
							i -= len(sepRunes) - 1 // Skip past this match
						}
					}

					// Build parts from splits
					prev := 0
					for _, pos := range splits {
						parts = append(parts, string(runes[prev:pos]))
						prev = pos + len(sepRunes)
					}
					parts = append(parts, string(runes[prev:]))
				}
			}

			result := make([]Value, len(parts))
			for i, part := range parts {
				result[i] = StringValue(part)
			}
			return NewList(result...), nil
		},
	}

	// format_map - format using mapping
	td.Methods["format_map"] = &MethodDescriptor{
		Name:    "format_map",
		Arity:   1,
		Doc:     "Return a formatted version of the string using a mapping",
		Builtin: true,
		Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
			if len(args) != 1 {
				return nil, &TypeError{Message: fmt.Sprintf("format_map() takes exactly 1 argument (%d given)", len(args))}
			}

			s := string(receiver.(StringValue))
			mapping, ok := args[0].(*DictValue)
			if !ok {
				return nil, &TypeError{Message: "format_map() argument must be a mapping"}
			}

			result := &strings.Builder{}
			i := 0

			for i < len(s) {
				if s[i] == '{' {
					// Check for escaped brace
					if i+1 < len(s) && s[i+1] == '{' {
						result.WriteByte('{')
						i += 2
						continue
					}

					// Find matching }
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

					// Extract field name (everything between { and })
					spec := s[i+1 : j-1]

					// Split on : for format spec
					colonIdx := strings.IndexByte(spec, ':')
					fieldName := spec
					formatSpec := ""
					if colonIdx >= 0 {
						fieldName = spec[:colonIdx]
						formatSpec = spec[colonIdx+1:]
					}

					// Look up in mapping
					key := ValueToKey(StringValue(fieldName))
					value, found := mapping.Get(key)
					if !found {
						return nil, &KeyError{Key: StringValue(fieldName)}
					}

					// Format the value
					formatted, err := formatValueWithSpec(value, formatSpec)
					if err != nil {
						return nil, err
					}
					result.WriteString(formatted)

					i = j
				} else if s[i] == '}' {
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

	// Add the trailing line only when it has content. An empty string yields no
	// lines at all ("".splitlines() == []), and a string ending in a line break
	// does not produce a spurious empty final line.
	if currentLine.Len() > 0 {
		lines = append(lines, currentLine.String())
	}

	// Convert to list of StringValues
	result := make([]Value, len(lines))
	for i, line := range lines {
		result[i] = StringValue(line)
	}
	return NewList(result...), nil
}

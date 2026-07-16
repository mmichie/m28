package modules

import (
	"fmt"
	"strconv"

	"github.com/mmichie/m28/core"
)

// Init_StringModule creates the _string C extension module stub
// This provides C-accelerated string functions for Python's string.py
func Init_StringModule() *core.DictValue {
	stringModule := core.NewDict()

	// formatter_parser parses a format string for string.Formatter.
	// Returns a list of (literal_text, field_name, format_spec, conversion) tuples,
	// one per replacement field. Trailing literal text (no field after it) has
	// field_name=None.
	//
	// Grammar (simplified): "literal {field[!conv][:spec]}literal..." with
	// {{ and }} for literal braces. Supports nested braces in format_spec for
	// format strings like "{x:{width}}".
	stringModule.SetStr("formatter_parser", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "formatter_parser() argument")
		}
		s, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "formatter_parser() argument")
		}
		parts, err := parseFormatString(string(s))
		if err != nil {
			return nil, err
		}
		result := core.NewList()
		for _, p := range parts {
			tup := make(core.TupleValue, 4)
			tup[0] = core.StringValue(p.literal)
			if p.hasField {
				tup[1] = core.StringValue(p.fieldName)
				tup[2] = core.StringValue(p.formatSpec)
				if p.conversion == "" {
					tup[3] = core.None
				} else {
					tup[3] = core.StringValue(p.conversion)
				}
			} else {
				tup[1] = core.None
				tup[2] = core.None
				tup[3] = core.None
			}
			result.Append(tup)
		}
		return result, nil
	}))

	// formatter_field_name_split splits a field name into (first, rest_iter)
	// where first is either a string (attribute name) or int (positional index),
	// and rest is an iterator of (is_attr, key) pairs for chained access.
	// e.g. "foo.bar[0]" -> ("foo", [(True, "bar"), (False, 0)])
	//      "0.name"     -> (0,     [(True, "name")])
	stringModule.SetStr("formatter_field_name_split", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, core.NewTypeError("str", nil, "formatter_field_name_split() argument")
		}
		fieldName, ok := args[0].(core.StringValue)
		if !ok {
			return nil, core.NewTypeError("str", args[0], "formatter_field_name_split() argument")
		}
		first, rest, err := splitFieldName(string(fieldName))
		if err != nil {
			return nil, err
		}
		return core.TupleValue{first, rest}, nil
	}))

	return stringModule
}

// formatPart is a single parsed piece of a format string.
type formatPart struct {
	literal    string
	hasField   bool
	fieldName  string
	formatSpec string
	conversion string
}

// parseFormatString walks `s` producing a slice of formatPart entries that
// matches CPython's _string.formatter_parser output.
func parseFormatString(s string) ([]formatPart, error) {
	var parts []formatPart
	var lit []byte
	i := 0
	flushLit := func(hasField bool, name, spec, conv string) {
		parts = append(parts, formatPart{
			literal:    string(lit),
			hasField:   hasField,
			fieldName:  name,
			formatSpec: spec,
			conversion: conv,
		})
		lit = lit[:0]
	}
	for i < len(s) {
		ch := s[i]
		switch ch {
		case '{':
			// Handle escaped {{
			if i+1 < len(s) && s[i+1] == '{' {
				lit = append(lit, '{')
				i += 2
				continue
			}
			// Start of a replacement field. Find matching '}' allowing nested braces.
			depth := 1
			j := i + 1
			for j < len(s) && depth > 0 {
				if s[j] == '{' {
					depth++
				} else if s[j] == '}' {
					depth--
					if depth == 0 {
						break
					}
				}
				j++
			}
			if depth != 0 {
				return nil, fmt.Errorf("Single '{' encountered in format string")
			}
			body := s[i+1 : j]
			i = j + 1

			// body is "name[!conv][:spec]"
			name := body
			conv := ""
			spec := ""
			if colon := indexOfTopLevel(body, ':'); colon >= 0 {
				spec = body[colon+1:]
				name = body[:colon]
			}
			if bang := indexOfTopLevel(name, '!'); bang >= 0 {
				conv = name[bang+1:]
				name = name[:bang]
			}
			flushLit(true, name, spec, conv)
		case '}':
			if i+1 < len(s) && s[i+1] == '}' {
				lit = append(lit, '}')
				i += 2
				continue
			}
			return nil, fmt.Errorf("Single '}' encountered in format string")
		default:
			lit = append(lit, ch)
			i++
		}
	}
	if len(lit) > 0 || len(parts) == 0 {
		// Always emit a final entry for trailing literal (or for empty format strings)
		flushLit(false, "", "", "")
	}
	return parts, nil
}

// indexOfTopLevel returns the index of `target` in s, skipping characters
// inside brackets. Returns -1 if not found.
func indexOfTopLevel(s string, target byte) int {
	depth := 0
	for i := 0; i < len(s); i++ {
		switch s[i] {
		case '[':
			depth++
		case ']':
			if depth > 0 {
				depth--
			}
		default:
			if depth == 0 && s[i] == target {
				return i
			}
		}
	}
	return -1
}

// splitFieldName implements _string.formatter_field_name_split. Returns the
// first segment (str or int) and an iterator of (is_attr, key) tuples.
func splitFieldName(name string) (core.Value, core.Value, error) {
	// Find the first '.' or '[' to delimit the head
	head := name
	rest := ""
	for i := 0; i < len(name); i++ {
		if name[i] == '.' || name[i] == '[' {
			head = name[:i]
			rest = name[i:]
			break
		}
	}
	var first core.Value
	if n, err := strconv.Atoi(head); err == nil {
		first = core.NumberValue(float64(n))
	} else {
		first = core.StringValue(head)
	}

	parts := core.NewList()
	i := 0
	for i < len(rest) {
		switch rest[i] {
		case '.':
			// .attr — attr runs until next . or [ or end
			j := i + 1
			for j < len(rest) && rest[j] != '.' && rest[j] != '[' {
				j++
			}
			parts.Append(core.TupleValue{core.BoolValue(true), core.StringValue(rest[i+1 : j])})
			i = j
		case '[':
			j := i + 1
			for j < len(rest) && rest[j] != ']' {
				j++
			}
			if j >= len(rest) {
				return nil, nil, fmt.Errorf("Missing ']' in field name")
			}
			key := rest[i+1 : j]
			var keyVal core.Value
			if n, err := strconv.Atoi(key); err == nil {
				keyVal = core.NumberValue(float64(n))
			} else {
				keyVal = core.StringValue(key)
			}
			parts.Append(core.TupleValue{core.BoolValue(false), keyVal})
			i = j + 1
		default:
			i++
		}
	}
	return first, parts, nil
}

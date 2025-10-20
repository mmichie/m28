package parser

import (
	"fmt"
	"strings"
	"unicode"

	"github.com/mmichie/m28/core"
)

// FormatSpec represents a format specification like :03.2f
type FormatSpec struct {
	HasSpec    bool
	Conversion string // 'r', 's', or 'a'
	Fill       rune
	Align      rune // '<', '>', '^', '='
	Sign       rune // '+', '-', ' '
	Alt        bool // '#' flag
	Zero       bool // '0' flag
	Width      int
	Precision  int  // -1 if not specified
	Type       rune // 'b', 'd', 'f', 's', etc.
	Debug      bool // '=' suffix for self-documenting
}

// parseFStringExpression parses a single f-string expression between { and }
// It handles nested quotes properly and extracts format specifications
func (p *Parser) parseFStringExpression(outerQuote rune) (expr core.Value, spec *FormatSpec, debugExpr string, err error) {
	startPos := p.pos
	startLine := p.line
	startCol := p.col

	// Track the expression for debug mode (expr=)
	var exprText strings.Builder

	// Parse the expression, handling nested strings with different quotes
	depth := 0
	inString := false
	var stringQuote rune
	escaped := false

	spec = &FormatSpec{Precision: -1}

	for p.pos < len(p.input) {
		ch := rune(p.input[p.pos])

		if escaped {
			exprText.WriteRune(ch)
			escaped = false
			p.advance()
			continue
		}

		if ch == '\\' {
			exprText.WriteRune(ch)
			escaped = true
			p.advance()
			continue
		}

		// Handle string quotes
		if !inString && (ch == '"' || ch == '\'') && ch != outerQuote {
			inString = true
			stringQuote = ch
			exprText.WriteRune(ch)
			p.advance()
			continue
		}

		if inString && ch == stringQuote {
			inString = false
			exprText.WriteRune(ch)
			p.advance()
			continue
		}

		if !inString {
			switch ch {
			case '{':
				depth++
				exprText.WriteRune(ch)
			case '}':
				if depth == 0 {
					// End of expression - check for format spec or debug mode
					exprStr := exprText.String()

					// Check for debug mode (=)
					if strings.HasSuffix(exprStr, "=") {
						spec.Debug = true
						debugExpr = strings.TrimSuffix(exprStr, "=")
						exprStr = debugExpr
					}

					// Check for conversion (!r, !s, !a)
					if idx := strings.LastIndex(exprStr, "!"); idx >= 0 {
						conversion := exprStr[idx+1:]
						if conversion == "r" || conversion == "s" || conversion == "a" {
							spec.Conversion = conversion
							exprStr = exprStr[:idx]
						}
					}

					// Check for format spec (:...)
					if idx := strings.LastIndex(exprStr, ":"); idx >= 0 {
						formatStr := exprStr[idx+1:]
						if formatStr != "" {
							spec.HasSpec = true
							if err := parseFormatSpec(formatStr, spec); err != nil {
								return nil, nil, "", fmt.Errorf("invalid format spec '%s': %v", formatStr, err)
							}
						}
						exprStr = exprStr[:idx]
					}

					// Parse the expression
					savedPos := p.pos
					savedLine := p.line
					savedCol := p.col

					p.pos = startPos
					p.line = startLine
					p.col = startCol
					p.input = p.input[:startPos] + exprStr + p.input[savedPos:]

					expr, err = p.parseExpr()
					if err != nil {
						return nil, nil, "", err
					}

					// Restore position
					p.pos = savedPos - (len(exprText.String()) - len(exprStr))
					p.line = savedLine
					p.col = savedCol
					p.input = p.input[:startPos] + exprText.String() + p.input[savedPos:]

					return expr, spec, debugExpr, nil
				}
				depth--
				exprText.WriteRune(ch)
			case '!', ':':
				// These are special in format specs but only at depth 0
				if depth == 0 && p.pos+1 < len(p.input) {
					// Look ahead to see if this starts a format spec
					next := p.input[p.pos+1]
					if ch == '!' && (next == 'r' || next == 's' || next == 'a') {
						// This is a conversion spec
						exprText.WriteRune(ch)
					} else if ch == ':' {
						// This is a format spec
						exprText.WriteRune(ch)
					} else {
						exprText.WriteRune(ch)
					}
				} else {
					exprText.WriteRune(ch)
				}
			case '=':
				// Check if this is the debug suffix
				if depth == 0 && p.pos+1 < len(p.input) && p.input[p.pos+1] == '}' {
					exprText.WriteRune(ch)
				} else {
					exprText.WriteRune(ch)
				}
			default:
				exprText.WriteRune(ch)
			}
		} else {
			// Inside a string
			exprText.WriteRune(ch)
		}

		p.advance()
	}

	return nil, nil, "", fmt.Errorf("unclosed f-string expression")
}

// parseFormatSpec parses a format specification string
func parseFormatSpec(spec string, fs *FormatSpec) error {
	if spec == "" {
		return nil
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
	for i < len(spec) && unicode.IsDigit(rune(spec[i])) {
		i++
	}
	if i > widthStart {
		fmt.Sscanf(spec[widthStart:i], "%d", &fs.Width)
	}

	// Parse precision
	if i < len(spec) && spec[i] == '.' {
		i++
		precStart := i
		for i < len(spec) && unicode.IsDigit(rune(spec[i])) {
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
		i++
	}

	// Check for extra characters
	if i < len(spec) {
		return fmt.Errorf("invalid format spec")
	}

	return nil
}

// parseFStringEnhanced is the new enhanced f-string parser
func (p *Parser) parseFStringEnhanced(quoteChar rune) (core.Value, error) {
	// Skip the 'f' and quote
	p.advance() // skip 'f'
	p.advance() // skip quote

	var parts []core.Value
	var currentString strings.Builder

	for p.pos < len(p.input) {
		ch := rune(p.input[p.pos])

		if ch == '\\' && p.pos+1 < len(p.input) {
			// Handle escape sequences
			p.advance()
			next := p.input[p.pos]
			switch next {
			case 'n':
				currentString.WriteByte('\n')
			case 't':
				currentString.WriteByte('\t')
			case 'r':
				currentString.WriteByte('\r')
			case '\\':
				currentString.WriteByte('\\')
			case '{':
				currentString.WriteByte('{')
			case '}':
				currentString.WriteByte('}')
			default:
				if next == byte(quoteChar) {
					currentString.WriteRune(quoteChar)
				} else {
					currentString.WriteByte(next)
				}
			}
			p.advance()
		} else if ch == quoteChar {
			// End of f-string
			if currentString.Len() > 0 {
				parts = append(parts, core.StringValue(currentString.String()))
			}
			p.advance()

			// Return the formatted result
			if len(parts) == 0 {
				return core.StringValue(""), nil
			} else if len(parts) == 1 {
				return parts[0], nil
			}

			// Create format expression with specs
			return createFormatExpression(parts), nil
		} else if ch == '{' {
			// Check for escaped brace
			if p.pos+1 < len(p.input) && p.input[p.pos+1] == '{' {
				currentString.WriteByte('{')
				p.advance()
				p.advance()
				continue
			}

			// Save current string part
			if currentString.Len() > 0 {
				parts = append(parts, core.StringValue(currentString.String()))
				currentString.Reset()
			}

			// Skip '{'
			p.advance()

			// Parse the expression
			expr, spec, debugExpr, err := p.parseFStringExpression(quoteChar)
			if err != nil {
				return nil, err
			}

			// Skip '}'
			if p.pos >= len(p.input) || p.input[p.pos] != '}' {
				return nil, fmt.Errorf("expected '}' after f-string expression")
			}
			p.advance()

			// Add the expression with format info
			if spec.HasSpec || spec.Conversion != "" || spec.Debug {
				// Create a format expression node
				formatExpr := []core.Value{
					core.SymbolValue("format-expr"),
					expr,
				}

				// Add format spec if present
				if spec.HasSpec {
					formatExpr = append(formatExpr, createFormatSpecValue(spec))
				}

				// Add conversion if present
				if spec.Conversion != "" {
					formatExpr = append(formatExpr, core.StringValue("!"+spec.Conversion))
				}

				// Add debug expression if present
				if spec.Debug {
					formatExpr = append(formatExpr, core.StringValue("="+debugExpr))
				}

				parts = append(parts, core.NewList(formatExpr...))
			} else {
				parts = append(parts, expr)
			}
		} else if ch == '}' {
			// Check for escaped brace
			if p.pos+1 < len(p.input) && p.input[p.pos+1] == '}' {
				currentString.WriteByte('}')
				p.advance()
				p.advance()
				continue
			}
			// Unmatched }
			return nil, fmt.Errorf("unmatched '}' in f-string")
		} else {
			currentString.WriteRune(ch)
			p.advance()
		}
	}

	return nil, fmt.Errorf("unclosed f-string")
}

// Helper functions

func createFormatExpression(parts []core.Value) core.Value {
	result := make([]core.Value, 0, len(parts)+1)
	result = append(result, core.SymbolValue("str-format"))
	result = append(result, parts...)
	return core.NewList(result...)
}

func createFormatSpecValue(spec *FormatSpec) core.Value {
	// Create a dict with format spec details
	specDict := core.NewDict()

	if spec.Fill != 0 {
		specDict.Set("fill", core.StringValue(string(spec.Fill)))
	}
	if spec.Align != 0 {
		specDict.Set("align", core.StringValue(string(spec.Align)))
	}
	if spec.Sign != 0 {
		specDict.Set("sign", core.StringValue(string(spec.Sign)))
	}
	if spec.Alt {
		specDict.Set("alt", core.BoolValue(true))
	}
	if spec.Zero {
		specDict.Set("zero", core.BoolValue(true))
	}
	if spec.Width > 0 {
		specDict.Set("width", core.NumberValue(spec.Width))
	}
	if spec.Precision >= 0 {
		specDict.Set("precision", core.NumberValue(spec.Precision))
	}
	if spec.Type != 0 {
		specDict.Set("type", core.StringValue(string(spec.Type)))
	}

	return specDict
}

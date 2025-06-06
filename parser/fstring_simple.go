package parser

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// parseFStringEnhancedSimple is a simpler version that focuses on key features
func (p *Parser) parseFStringEnhancedSimple(quoteChar rune) (core.Value, error) {
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

			// Create format expression
			formatExpr := make(core.ListValue, 0, len(parts)+1)
			formatExpr = append(formatExpr, core.SymbolValue("str-format"))
			formatExpr = append(formatExpr, parts...)
			return formatExpr, nil
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

			// Parse the expression with nested quote support
			expr, formatSpec, err := p.parseFStringExpressionSimple(quoteChar)
			if err != nil {
				return nil, err
			}

			// Skip '}'
			if p.pos >= len(p.input) || p.input[p.pos] != '}' {
				return nil, fmt.Errorf("expected '}' after f-string expression")
			}
			p.advance()

			// Add the expression with format spec if present
			if formatSpec != "" {
				// Create a format expression with the spec
				formatExpr := core.ListValue{
					core.SymbolValue("format-expr"),
					expr,
					core.StringValue(formatSpec),
				}
				parts = append(parts, formatExpr)
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

// parseFStringExpressionSimple parses expressions with nested quote support
func (p *Parser) parseFStringExpressionSimple(outerQuote rune) (expr core.Value, formatSpec string, err error) {
	// Collect the expression text
	var exprText strings.Builder
	depth := 0
	inString := false
	var stringQuote rune
	escaped := false

	// For tracking the original expression for self-documenting
	var originalExpr strings.Builder

	for p.pos < len(p.input) {
		ch := rune(p.input[p.pos])

		if escaped {
			exprText.WriteRune(ch)
			originalExpr.WriteRune(ch)
			escaped = false
			p.advance()
			continue
		}

		if ch == '\\' {
			exprText.WriteRune(ch)
			originalExpr.WriteRune(ch)
			escaped = true
			p.advance()
			continue
		}

		// Handle string quotes - allow different quotes than outer f-string
		if !inString && (ch == '"' || ch == '\'') && ch != outerQuote {
			inString = true
			stringQuote = ch
			exprText.WriteRune(ch)
			originalExpr.WriteRune(ch)
			p.advance()
			continue
		}

		if inString && ch == stringQuote {
			inString = false
			exprText.WriteRune(ch)
			originalExpr.WriteRune(ch)
			p.advance()
			continue
		}

		if !inString {
			switch ch {
			case '{':
				depth++
				exprText.WriteRune(ch)
				originalExpr.WriteRune(ch)
			case '}':
				if depth == 0 {
					// End of expression - parse format spec, conversion, and self-doc
					exprStr := exprText.String()
					origStr := originalExpr.String()

					// Check for self-documenting expression (=)
					// Must check for = that's not followed by : or !
					selfDoc := false
					equalIdx := strings.LastIndex(exprStr, "=")
					if equalIdx >= 0 && equalIdx == len(exprStr)-1 {
						// The = is at the end
						selfDoc = true
						exprStr = exprStr[:equalIdx]
						origStr = origStr[:equalIdx]
					}

					// Parse format spec and conversion from the = stripped version

					// First, check for format spec (:...)
					if idx := strings.LastIndex(exprStr, ":"); idx >= 0 {
						// Make sure this colon isn't inside a string or dict
						if isValidFormatSpecColon(exprStr, idx) {
							formatSpec = exprStr[idx+1:]
							exprStr = exprStr[:idx]
							// Don't update origStr - we want the original expression for self-doc
						}
					}

					// Then check for conversion (!r, !s, !a)
					conversion := ""
					if idx := strings.LastIndex(exprStr, "!"); idx >= 0 {
						possibleConv := exprStr[idx+1:]
						if possibleConv == "r" || possibleConv == "s" || possibleConv == "a" {
							conversion = possibleConv
							exprStr = exprStr[:idx]
							// Don't update origStr - we want the original expression for self-doc
						}
					}

					// Parse the expression by creating a temporary parser
					tempParser := NewParser()
					tempParser.SetFilename(p.filename)
					expr, err = tempParser.Parse(exprStr)
					if err != nil {
						return nil, "", fmt.Errorf("error parsing f-string expression: %v", err)
					}

					// Build enhanced format spec string that includes all modifiers
					var enhancedSpec strings.Builder
					if formatSpec != "" {
						enhancedSpec.WriteString(formatSpec)
					}
					if conversion != "" {
						enhancedSpec.WriteString("|!")
						enhancedSpec.WriteString(conversion)
					}
					if selfDoc {
						enhancedSpec.WriteString("|=")
						enhancedSpec.WriteString(origStr)
					}

					return expr, enhancedSpec.String(), nil
				}
				depth--
				exprText.WriteRune(ch)
				originalExpr.WriteRune(ch)
			default:
				exprText.WriteRune(ch)
				originalExpr.WriteRune(ch)
			}
		} else {
			// Inside a string
			exprText.WriteRune(ch)
			originalExpr.WriteRune(ch)
		}

		p.advance()
	}

	return nil, "", fmt.Errorf("unclosed f-string expression")
}

// isValidFormatSpecColon checks if a colon is a valid format spec separator
func isValidFormatSpecColon(expr string, colonIdx int) bool {
	// Simple heuristic: count quotes and braces before the colon
	// If they're balanced, it's likely a format spec
	quotes := 0
	braces := 0
	brackets := 0

	for i := 0; i < colonIdx; i++ {
		switch expr[i] {
		case '"', '\'':
			quotes++
		case '{':
			braces++
		case '}':
			braces--
		case '[':
			brackets++
		case ']':
			brackets--
		}
	}

	// If quotes are odd or braces/brackets unbalanced, colon is inside a construct
	return quotes%2 == 0 && braces == 0 && brackets == 0
}

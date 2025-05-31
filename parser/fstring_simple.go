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
				// For now, just add the expression without format spec processing
				// This can be enhanced later
				parts = append(parts, expr)
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

		// Handle string quotes - allow different quotes than outer f-string
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
					// End of expression - check for format spec
					exprStr := exprText.String()
					
					// Check for format spec (simplified - just look for :)
					colonIdx := strings.LastIndex(exprStr, ":")
					if colonIdx >= 0 {
						formatSpec = exprStr[colonIdx+1:]
						exprStr = exprStr[:colonIdx]
					}

					// Parse the expression by creating a temporary parser
					tempParser := NewParser()
					tempParser.SetFilename(p.filename)
					expr, err = tempParser.Parse(exprStr)
					if err != nil {
						return nil, "", fmt.Errorf("error parsing f-string expression: %v", err)
					}

					return expr, formatSpec, nil
				}
				depth--
				exprText.WriteRune(ch)
			default:
				exprText.WriteRune(ch)
			}
		} else {
			// Inside a string
			exprText.WriteRune(ch)
		}

		p.advance()
	}

	return nil, "", fmt.Errorf("unclosed f-string expression")
}
package parser

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// InterpolationType represents the type of interpolation in s-strings
type InterpolationType string

const (
	InterpValue      InterpolationType = "value"       // {x} - insert value
	InterpCode       InterpolationType = "code"        // {=x} - insert code/symbol
	InterpSplice     InterpolationType = "splice"      // {*x} - splice sequence
	InterpDictSplice InterpolationType = "dict-splice" // {**x} - splice dict
	InterpGensym     InterpolationType = "gensym"      // {x#} - auto-gensym
)

// Interpolation represents a single interpolation in an s-string
type Interpolation struct {
	Type     InterpolationType
	Expr     core.Value
	Position int // Position in the template for error reporting
}

// parseSString parses an s-string or raw s-string
// Examples:
//
//	s"(+ {x} 1)"        -> value interpolation
//	s"(+ {=x} 1)"       -> code interpolation
//	s"(sum {*args})"    -> splice interpolation
//	s"(func {**kwargs})" -> dict splice
//	s"(let (tmp# 10))"  -> auto-gensym
//	rs"no {interp}"     -> raw (no interpolation)
func (p *Parser) parseSString(quoteChar rune, raw bool) (core.Value, error) {
	// Skip prefix
	if raw {
		p.advance() // skip 'r'
		p.advance() // skip 's'
	} else {
		p.advance() // skip 's'
	}
	p.advance() // skip quote

	// For raw s-strings, just collect the literal text
	if raw {
		return p.parseRawSString(quoteChar)
	}

	// Parse with interpolation
	var template strings.Builder
	var interpolations []Interpolation
	position := 0

	for p.pos < len(p.input) {
		ch := rune(p.input[p.pos])

		if ch == '\\' && p.pos+1 < len(p.input) {
			// Handle escape sequences
			p.advance()
			next := p.input[p.pos]
			switch next {
			case 'n':
				template.WriteByte('\n')
			case 't':
				template.WriteByte('\t')
			case 'r':
				template.WriteByte('\r')
			case '\\':
				template.WriteByte('\\')
			case '{':
				template.WriteByte('{')
			case '}':
				template.WriteByte('}')
			default:
				if next == byte(quoteChar) {
					template.WriteRune(quoteChar)
				} else {
					// Unknown escape, keep backslash
					template.WriteByte('\\')
					template.WriteByte(next)
				}
			}
			p.advance()
			position++
		} else if ch == quoteChar {
			// End of s-string
			p.advance()
			return p.buildSStringAST(template.String(), interpolations)
		} else if ch == '{' {
			// Start of interpolation
			interp, err := p.parseSStringInterpolation(quoteChar)
			if err != nil {
				return nil, err
			}
			interp.Position = position
			interpolations = append(interpolations, interp)

			// Add placeholder in template
			template.WriteString(fmt.Sprintf("{%d}", len(interpolations)-1))
			position++
		} else {
			template.WriteRune(ch)
			p.advance()
			position++
		}
	}

	return nil, fmt.Errorf("unclosed s-string at line %d, column %d", p.line, p.col)
}

// parseRawSString parses a raw s-string (no interpolation)
func (p *Parser) parseRawSString(quoteChar rune) (core.Value, error) {
	var builder strings.Builder
	escaped := false

	for p.pos < len(p.input) {
		ch := rune(p.input[p.pos])

		if escaped {
			// Only handle escaped quote and backslash in raw strings
			if ch == quoteChar || ch == '\\' {
				builder.WriteRune(ch)
			} else {
				builder.WriteByte('\\')
				builder.WriteRune(ch)
			}
			escaped = false
			p.advance()
		} else if ch == '\\' {
			escaped = true
			p.advance()
		} else if ch == quoteChar {
			p.advance()
			// Return as raw s-string form
			return core.NewList(
				core.SymbolValue("s-string-raw"),
				core.StringValue(builder.String()),
			), nil
		} else {
			builder.WriteRune(ch)
			p.advance()
		}
	}

	return nil, fmt.Errorf("unclosed raw s-string at line %d, column %d", p.line, p.col)
}

// parseSStringInterpolation parses a single interpolation expression
// Handles: {x}, {=x}, {*x}, {**x}, {x#}
func (p *Parser) parseSStringInterpolation(outerQuote rune) (Interpolation, error) {
	if p.pos >= len(p.input) || p.input[p.pos] != '{' {
		return Interpolation{}, fmt.Errorf("expected '{' at line %d, column %d", p.line, p.col)
	}

	// Skip '{'
	p.advance()
	p.skipWhitespaceAndComments()

	if p.pos >= len(p.input) {
		return Interpolation{}, fmt.Errorf("unclosed interpolation at line %d, column %d", p.line, p.col)
	}

	// Determine interpolation type
	interpType := InterpValue
	var exprText strings.Builder

	// Check for special prefixes
	if p.input[p.pos] == '=' {
		// Code interpolation: {=x}
		interpType = InterpCode
		p.advance()
		p.skipWhitespaceAndComments()
	} else if p.input[p.pos] == '*' {
		// Check if it's ** (dict splice) or * (splice)
		if p.pos+1 < len(p.input) && p.input[p.pos+1] == '*' {
			// Dict splice: {**kwargs}
			interpType = InterpDictSplice
			p.advance()
			p.advance()
		} else {
			// Splice: {*args}
			interpType = InterpSplice
			p.advance()
		}
		p.skipWhitespaceAndComments()
	}

	// Parse expression until '}'
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
					// End of interpolation
					exprStr := strings.TrimSpace(exprText.String())

					// Check for gensym marker (#)
					if strings.HasSuffix(exprStr, "#") {
						interpType = InterpGensym
						exprStr = exprStr[:len(exprStr)-1]
					}

					if exprStr == "" {
						return Interpolation{}, fmt.Errorf("empty interpolation at line %d, column %d", p.line, p.col)
					}

					// Parse the expression
					tempParser := NewParser()
					tempParser.SetFilename(p.filename)
					expr, err := tempParser.Parse(exprStr)
					if err != nil {
						return Interpolation{}, fmt.Errorf("error parsing s-string interpolation: %v", err)
					}

					// Skip '}'
					p.advance()

					return Interpolation{
						Type: interpType,
						Expr: expr,
					}, nil
				}
				depth--
				exprText.WriteRune(ch)
			case '#':
				// Could be gensym marker - add to expression, will check at end
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

	return Interpolation{}, fmt.Errorf("unclosed s-string interpolation at line %d, column %d", p.line, p.col)
}

// buildSStringAST builds the AST for an s-string with interpolations
// Returns: (s-string template-string interp1 interp2 ...)
// Where each interpN is: (interp-type expr)
func (p *Parser) buildSStringAST(template string, interpolations []Interpolation) (core.Value, error) {
	result := make([]core.Value, 0, len(interpolations)*2+2)
	result = append(result, core.SymbolValue("s-string"))
	result = append(result, core.StringValue(template))

	// Add interpolations as pairs: (type expr)
	for _, interp := range interpolations {
		interpForm := core.NewList(
			core.StringValue(string(interp.Type)),
			interp.Expr,
		)
		result = append(result, interpForm)
	}

	return core.NewList(result...), nil
}

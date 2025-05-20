// Package parser provides a parser for the M28 programming language.
package parser

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	
	"github.com/mmichie/m28/core"
)

// Parser parses M28 code into expressions
type Parser struct {
	input    string // The input string
	pos      int    // Current position in the input
	line     int    // Current line number
	col      int    // Current column number
	filename string // Current filename for error reporting
}

// NewParser creates a new parser
func NewParser() *Parser {
	return &Parser{
		line: 1,
		col:  1,
	}
}

// SetFilename sets the filename for error reporting
func (p *Parser) SetFilename(filename string) {
	p.filename = filename
}

// Parse parses a string into a value
func (p *Parser) Parse(input string) (core.Value, error) {
	p.input = input
	p.pos = 0
	p.line = 1
	p.col = 1
	
	// Program is a list of expressions
	expressions := make(core.ListValue, 0)
	
	// Skip whitespace and comments at the beginning
	p.skipWhitespaceAndComments()
	
	// Parse expressions until the end of input
	for p.pos < len(p.input) {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		
		expressions = append(expressions, expr)
		p.skipWhitespaceAndComments()
	}
	
	// If there's only one expression, return it directly
	if len(expressions) == 1 {
		return expressions[0], nil
	}
	
	// Otherwise, wrap expressions in an implicit do form
	return core.ListValue(append(
		[]core.Value{core.SymbolValue("do")},
		expressions...,
	)), nil
}

// parseExpr parses a single expression
func (p *Parser) parseExpr() (core.Value, error) {
	p.skipWhitespaceAndComments()
	
	// Check for end of input
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unexpected end of input")
	}
	
	// Check what kind of expression we have
	switch p.input[p.pos] {
	case '(':
		return p.parseList()
	case '"':
		return p.parseString()
	case '[':
		return p.parseVectorLiteral()
	case '{':
		return p.parseDictLiteral()
	case '+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		if p.isNumberStart(p.input[p.pos]) {
			return p.parseNumber()
		}
		fallthrough
	default:
		// Try to parse a symbol or keyword
		return p.parseSymbolOrKeyword()
	}
}

// parseList parses a list expression (...)
func (p *Parser) parseList() (core.Value, error) {
	// Skip opening parenthesis
	p.advance()
	
	elements := make(core.ListValue, 0)
	
	// Skip whitespace after the opening paren
	p.skipWhitespaceAndComments()
	
	// Parse elements until we hit the closing parenthesis
	for p.pos < len(p.input) && p.input[p.pos] != ')' {
		element, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		
		elements = append(elements, element)
		p.skipWhitespaceAndComments()
	}
	
	// Check for unclosed list
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unclosed list")
	}
	
	// Skip closing parenthesis
	p.advance()
	
	return elements, nil
}

// parseVectorLiteral parses a vector literal [...]
func (p *Parser) parseVectorLiteral() (core.Value, error) {
	// Skip opening bracket
	p.advance()
	
	elements := make(core.ListValue, 0)
	
	// Skip whitespace after the opening bracket
	p.skipWhitespaceAndComments()
	
	// Parse elements until we hit the closing bracket
	for p.pos < len(p.input) && p.input[p.pos] != ']' {
		element, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		
		elements = append(elements, element)
		p.skipWhitespaceAndComments()
	}
	
	// Check for unclosed vector
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unclosed vector")
	}
	
	// Skip closing bracket
	p.advance()
	
	// A vector literal is represented as a list with special marker
	return core.ListValue(elements), nil
}

// parseDictLiteral parses a dictionary literal {...}
func (p *Parser) parseDictLiteral() (core.Value, error) {
	// Skip opening brace
	p.advance()
	
	// Create a new dictionary
	dict := core.NewDict()
	
	// Skip whitespace after the opening brace
	p.skipWhitespaceAndComments()
	
	// Parse key-value pairs until we hit the closing brace
	for p.pos < len(p.input) && p.input[p.pos] != '}' {
		// Parse the key
		key, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		
		// Convert key to string
		var keyStr string
		switch k := key.(type) {
		case core.StringValue:
			keyStr = string(k)
		case core.SymbolValue:
			keyStr = string(k)
		default:
			return nil, fmt.Errorf("dict keys must be strings or symbols")
		}
		
		// Skip whitespace after the key
		p.skipWhitespaceAndComments()
		
		// Check for the colon
		if p.pos >= len(p.input) || p.input[p.pos] != ':' {
			return nil, fmt.Errorf("expected ':' after dict key")
		}
		p.advance()
		
		// Skip whitespace after the colon
		p.skipWhitespaceAndComments()
		
		// Parse the value
		value, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		
		// Add the key-value pair to the dictionary
		dict.Set(keyStr, value)
		
		// Skip whitespace after the value
		p.skipWhitespaceAndComments()
		
		// Skip optional comma
		if p.pos < len(p.input) && p.input[p.pos] == ',' {
			p.advance()
			p.skipWhitespaceAndComments()
		}
	}
	
	// Check for unclosed dict
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unclosed dict")
	}
	
	// Skip closing brace
	p.advance()
	
	return dict, nil
}

// parseString parses a string literal "..."
func (p *Parser) parseString() (core.Value, error) {
	// Skip opening quote
	p.advance()
	
	var builder strings.Builder
	escaped := false
	
	// Parse characters until we hit the closing quote
	for p.pos < len(p.input) {
		ch := p.input[p.pos]
		
		if escaped {
			// Handle escaped characters
			switch ch {
			case 'n':
				builder.WriteByte('\n')
			case 't':
				builder.WriteByte('\t')
			case 'r':
				builder.WriteByte('\r')
			case '"':
				builder.WriteByte('"')
			case '\\':
				builder.WriteByte('\\')
			default:
				return nil, fmt.Errorf("invalid escape sequence: \\%c", ch)
			}
			escaped = false
		} else {
			// Handle regular characters
			if ch == '\\' {
				escaped = true
			} else if ch == '"' {
				// End of string
				p.advance()
				return core.StringValue(builder.String()), nil
			} else {
				builder.WriteByte(ch)
			}
		}
		
		p.advance()
	}
	
	return nil, fmt.Errorf("unclosed string")
}

// parseNumber parses a numeric literal
func (p *Parser) parseNumber() (core.Value, error) {
	start := p.pos
	
	// Skip sign if present
	if p.input[p.pos] == '+' || p.input[p.pos] == '-' {
		p.advance()
	}
	
	// Parse digits
	for p.pos < len(p.input) && isDigit(p.input[p.pos]) {
		p.advance()
	}
	
	// Check for decimal point
	if p.pos < len(p.input) && p.input[p.pos] == '.' {
		p.advance()
		
		// Parse fractional part
		for p.pos < len(p.input) && isDigit(p.input[p.pos]) {
			p.advance()
		}
	}
	
	// Parse the number
	numStr := p.input[start:p.pos]
	num, err := strconv.ParseFloat(numStr, 64)
	if err != nil {
		return nil, fmt.Errorf("invalid number: %s", numStr)
	}
	
	return core.NumberValue(num), nil
}

// parseSymbolOrKeyword parses a symbol or keyword
func (p *Parser) parseSymbolOrKeyword() (core.Value, error) {
	start := p.pos
	
	// Parse symbol characters
	for p.pos < len(p.input) && isSymbolChar(p.input[p.pos]) {
		p.advance()
	}
	
	// Get the symbol name
	name := p.input[start:p.pos]
	
	// Check for keywords
	switch name {
	case "true":
		return core.True, nil
	case "false":
		return core.False, nil
	case "nil":
		return core.Nil, nil
	default:
		return core.SymbolValue(name), nil
	}
}

// skipWhitespaceAndComments skips whitespace and comments
func (p *Parser) skipWhitespaceAndComments() {
	for p.pos < len(p.input) {
		if unicode.IsSpace(rune(p.input[p.pos])) {
			// Update line and column for newlines
			if p.input[p.pos] == '\n' {
				p.line++
				p.col = 1
			} else {
				p.col++
			}
			p.pos++
		} else if p.pos+1 < len(p.input) && p.input[p.pos] == ';' {
			// Skip comment to end of line
			for p.pos < len(p.input) && p.input[p.pos] != '\n' {
				p.pos++
			}
		} else {
			break
		}
	}
}

// advance moves the parser position forward
func (p *Parser) advance() {
	if p.pos < len(p.input) {
		if p.input[p.pos] == '\n' {
			p.line++
			p.col = 1
		} else {
			p.col++
		}
		p.pos++
	}
}

// isDigit checks if a character is a digit
func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

// isNumberStart checks if a character can start a number
func (p *Parser) isNumberStart(ch byte) bool {
	// Check if it's a digit
	if isDigit(ch) {
		return true
	}
	
	// + and - can only start a number if followed by a digit
	if (ch == '+' || ch == '-') && p.pos+1 < len(p.input) && isDigit(p.input[p.pos+1]) {
		return true
	}
	
	return false
}

// isSymbolChar checks if a character is valid in a symbol
func isSymbolChar(ch byte) bool {
	return !unicode.IsSpace(rune(ch)) &&
		ch != '(' && ch != ')' &&
		ch != '[' && ch != ']' &&
		ch != '{' && ch != '}' &&
		ch != '"' && ch != ';' &&
		ch != ':'
}
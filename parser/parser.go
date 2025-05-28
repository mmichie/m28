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
	// Parse the primary expression (atom, list, etc.)
	base, err := p.parseAtom()
	if err != nil {
		return nil, err
	}
	
	// Now handle postfix operations (dots, calls, indexing)
	return p.parsePostfix(base)
}

// parsePostfix handles postfix operations like dot notation, function calls, etc.
func (p *Parser) parsePostfix(base core.Value) (core.Value, error) {
	for {
		p.skipWhitespaceAndComments()
		if p.pos >= len(p.input) {
			return base, nil
		}
		
		switch p.input[p.pos] {
		case '.':
			// Check if it's a float continuation
			if num, ok := base.(core.NumberValue); ok {
				// This could be a decimal number continuation
				if p.pos+1 < len(p.input) && isDigit(p.input[p.pos+1]) {
					// This is actually a decimal number, not dot notation
					return base, nil
				}
				// Numbers can't have properties
				return nil, p.error(fmt.Sprintf("number %v has no properties", num))
			}
			
			// Property access or method call
			p.pos++ // consume '.'
			var err error
			base, err = p.parseDotAccess(base)
			if err != nil {
				return nil, err
			}
			
		// Note: Direct function calls on expressions not yet supported
		// case '(':
		// 	// Direct function call on the result
		// 	base, err = p.parseFunctionCall(base)
		// 	if err != nil {
		// 		return nil, err
		// 	}
			
		// Note: Index access to be implemented later
		// case '[':
		// 	// Index access
		// 	base, err = p.parseIndexAccess(base)
		// 	if err != nil {
		// 		return nil, err
		// 	}
			
		default:
			return base, nil
		}
	}
}

// parseAtom parses a single atomic expression (without dot notation)
func (p *Parser) parseAtom() (core.Value, error) {
	p.skipWhitespaceAndComments()

	// Check for end of input
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unexpected end of input")
	}

	// Check for f-string
	if p.pos+1 < len(p.input) && p.input[p.pos] == 'f' && p.input[p.pos+1] == '"' {
		return p.parseFString()
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
	case '.':
		// Special handling for dot symbol
		p.advance()
		return core.SymbolValue("."), nil
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

// parseVectorLiteral parses a vector literal [...] or list comprehension
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
		
		// Check for comma separator
		if p.pos < len(p.input) && p.input[p.pos] == ',' {
			p.advance() // consume comma
			p.skipWhitespaceAndComments()
		}
	}

	// Check for unclosed vector
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unclosed vector")
	}

	// Skip closing bracket
	p.advance()

	// Check if this is a list comprehension
	// Pattern: [expr for var in iterable] or [expr for var in iterable if condition]
	if p.isListComprehension(elements) {
		// List comprehensions return an evaluable form, not a quoted literal
		return p.parseListComprehension(elements)
	}

	// Return a quoted list so it's not evaluated as a function call
	return core.ListValue{
		core.SymbolValue("quote"),
		elements,
	}, nil
}

// parseDictLiteral parses a dictionary literal {...}
func (p *Parser) parseDictLiteral() (core.Value, error) {
	// Skip opening brace
	p.advance()

	// Build a dict construction form: (dict key1 val1 key2 val2 ...)
	elements := []core.Value{core.SymbolValue("dict-literal")}

	// Skip whitespace after the opening brace
	p.skipWhitespaceAndComments()

	// Parse key-value pairs until we hit the closing brace
	for p.pos < len(p.input) && p.input[p.pos] != '}' {
		// Parse the key
		key, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		// Add key to elements
		elements = append(elements, key)

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

		// Add value to elements
		elements = append(elements, value)

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

	return core.ListValue(elements), nil
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

// parseFString parses an f-string literal
func (p *Parser) parseFString() (core.Value, error) {
	// Skip 'f' and opening quote
	p.advance() // skip 'f'
	p.advance() // skip '"'

	var parts []core.Value
	var currentString strings.Builder
	escaped := false
	
	// Parse characters until closing quote
	for p.pos < len(p.input) {
		ch := p.input[p.pos]
		
		if escaped {
			// Handle escaped characters
			switch ch {
			case 'n':
				currentString.WriteByte('\n')
			case 't':
				currentString.WriteByte('\t')
			case 'r':
				currentString.WriteByte('\r')
			case '"':
				currentString.WriteByte('"')
			case '\\':
				currentString.WriteByte('\\')
			case '{':
				currentString.WriteByte('{')
			case '}':
				currentString.WriteByte('}')
			default:
				return nil, fmt.Errorf("invalid escape sequence: \\%c", ch)
			}
			escaped = false
			p.advance()
		} else if ch == '\\' {
			escaped = true
			p.advance()
		} else if ch == '"' {
			// End of f-string
			if currentString.Len() > 0 {
				parts = append(parts, core.StringValue(currentString.String()))
			}
			p.advance()
			
			// Return a format expression
			if len(parts) == 0 {
				return core.StringValue(""), nil
			} else if len(parts) == 1 {
				return parts[0], nil
			}
			
			// Create (str-format part1 part2 ...)
			formatExpr := make(core.ListValue, 0, len(parts)+1)
			formatExpr = append(formatExpr, core.SymbolValue("str-format"))
			formatExpr = append(formatExpr, parts...)
			return formatExpr, nil
		} else if ch == '{' {
			// Start of expression
			if currentString.Len() > 0 {
				parts = append(parts, core.StringValue(currentString.String()))
				currentString.Reset()
			}
			
			// Skip '{'
			p.advance()
			
			// Parse expression until '}'
			expr, err := p.parseExpr()
			if err != nil {
				return nil, fmt.Errorf("error parsing f-string expression: %v", err)
			}
			
			// Skip whitespace before '}'
			p.skipWhitespaceAndComments()
			
			if p.pos >= len(p.input) || p.input[p.pos] != '}' {
				return nil, fmt.Errorf("unclosed f-string expression")
			}
			
			// Skip '}'
			p.advance()
			
			// Add expression to parts
			parts = append(parts, expr)
		} else {
			currentString.WriteByte(ch)
			p.advance()
		}
	}
	
	return nil, fmt.Errorf("unclosed f-string")
}

// isListComprehension checks if the elements form a list comprehension pattern
func (p *Parser) isListComprehension(elements core.ListValue) bool {
	// Need at least 5 elements: expr for var in iterable
	if len(elements) < 5 {
		return false
	}
	
	// Look for "for" and "in" keywords
	hasFor := false
	hasIn := false
	forIndex := -1
	
	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && !hasFor {
				hasFor = true
				forIndex = i
			} else if string(sym) == "in" && hasFor && i > forIndex {
				hasIn = true
			}
		}
	}
	
	// Must have both "for" and "in", and "for" must not be the first element
	return hasFor && hasIn && forIndex > 0
}

// parseListComprehension converts comprehension elements into a comprehension form
func (p *Parser) parseListComprehension(elements core.ListValue) (core.Value, error) {
	// Find the positions of "for" and "in"
	forIndex := -1
	inIndex := -1
	
	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && forIndex == -1 {
				forIndex = i
			} else if string(sym) == "in" && forIndex != -1 && inIndex == -1 {
				inIndex = i
			}
		}
	}
	
	// Extract parts
	// expr: elements[0:forIndex]
	// var: elements[forIndex+1:inIndex]
	// iterable: elements[inIndex+1:]
	
	if forIndex <= 0 || inIndex <= forIndex+1 {
		return nil, fmt.Errorf("invalid list comprehension syntax")
	}
	
	// Get the expression (everything before "for")
	var expr core.Value
	if forIndex == 1 {
		// Single element - check if it's a list that should be unquoted
		if list, ok := elements[0].(core.ListValue); ok && len(list) > 0 {
			// This was a parenthesized expression like (* x 2)
			// It's already a list, use it directly as an expression
			expr = list
		} else {
			expr = elements[0]
		}
	} else {
		// Multiple elements - wrap in a do block
		exprElements := make(core.ListValue, forIndex+1)
		exprElements[0] = core.SymbolValue("do")
		copy(exprElements[1:], elements[:forIndex])
		expr = exprElements
	}
	
	// Get the variable (between "for" and "in")
	if inIndex != forIndex+2 {
		return nil, fmt.Errorf("list comprehension variable must be a single symbol")
	}
	variable := elements[forIndex+1]
	
	// Get the iterable (everything after "in")
	var iterable core.Value
	remainingElements := elements[inIndex+1:]
	
	// Check for "if" condition
	ifIndex := -1
	for i, elem := range remainingElements {
		if sym, ok := elem.(core.SymbolValue); ok && string(sym) == "if" {
			ifIndex = i
			break
		}
	}
	
	var condition core.Value
	if ifIndex >= 0 {
		// Has condition
		if ifIndex == 0 {
			return nil, fmt.Errorf("missing iterable in list comprehension")
		}
		
		// Iterable is everything before "if"
		if ifIndex == 1 {
			// Check if it's a list that should be unquoted
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, ifIndex+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements[:ifIndex])
			iterable = iterElements
		}
		
		// Condition is everything after "if"
		condElements := remainingElements[ifIndex+1:]
		if len(condElements) == 0 {
			return nil, fmt.Errorf("missing condition after 'if' in list comprehension")
		} else if len(condElements) == 1 {
			// Check if it's a list that should be unquoted
			if list, ok := condElements[0].(core.ListValue); ok && len(list) > 0 {
				condition = list
			} else {
				condition = condElements[0]
			}
		} else {
			condExpr := make(core.ListValue, len(condElements)+1)
			condExpr[0] = core.SymbolValue("do")
			copy(condExpr[1:], condElements)
			condition = condExpr
		}
	} else {
		// No condition
		if len(remainingElements) == 1 {
			// Check if it's a list that should be unquoted
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, len(remainingElements)+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements)
			iterable = iterElements
		}
	}
	
	// Build the list-comp form
	// (list-comp expr var iterable) or (list-comp expr var iterable condition)
	result := make(core.ListValue, 0, 5)
	result = append(result, core.SymbolValue("list-comp"))
	result = append(result, expr)
	result = append(result, variable)
	result = append(result, iterable)
	if condition != nil {
		result = append(result, condition)
	}
	
	return result, nil
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
	name := p.parseSymbolName()

	// Check for keywords (Python-style)
	switch name {
	case "True", "true":
		return core.True, nil
	case "False", "false":
		return core.False, nil
	case "None", "nil":
		return core.None, nil
	default:
		return core.SymbolValue(name), nil
	}
}

// parseSymbolName parses just the name part of a symbol
func (p *Parser) parseSymbolName() string {
	start := p.pos

	// Parse symbol characters
	for p.pos < len(p.input) && isSymbolChar(p.input[p.pos]) {
		p.advance()
	}

	// Get the symbol name
	return p.input[start:p.pos]
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
		} else if p.pos < len(p.input) && p.input[p.pos] == '#' {
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

// isLetter checks if a character is a letter
func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
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
		ch != ':' && ch != '.'  // dot is not valid in symbols (used for dot notation)
}

// error creates a parser error with current position info
func (p *Parser) error(msg string) error {
	return fmt.Errorf("%s at line %d, column %d", msg, p.line, p.col)
}

package parser

import (
	"fmt"
	"strconv"
	"strings"
	
	"github.com/mmichie/m28/core"
)

// parseDotNotation handles parsing of dot notation expressions
// It converts:
//   obj.prop -> (. obj prop)
//   obj.method(args) -> (. obj method args...)
//   obj.prop1.prop2 -> (. (. obj prop1) prop2)
func (p *Parser) parseDotNotation(obj core.Value) (core.Value, error) {
	// We've already parsed the object and consumed the dot
	// Now we need to parse the property/method name
	
	// Skip whitespace after dot
	p.skipWhitespaceAndComments()
	
	// Parse the property name
	if p.pos >= len(p.input) {
		return nil, p.error("unexpected end of input after '.'")
	}
	
	// Check what follows the dot
	ch := p.input[p.pos]
	
	var prop core.Value
	var propName string
	
	if isDigit(ch) {
		// Numeric index like list.0
		start := p.pos
		for p.pos < len(p.input) && isDigit(p.input[p.pos]) {
			p.pos++
		}
		numStr := p.input[start:p.pos]
		propName = numStr
		prop = core.StringValue(numStr) // Store as string, will convert in evaluator
	} else if isLetter(ch) || ch == '_' {
		// Regular property/method name
		propName = p.parseSymbolName()
		prop = core.StringValue(propName)
	} else {
		return nil, p.error(fmt.Sprintf("unexpected character '%c' after '.'", ch))
	}
	
	// Create the dot expression
	dotExpr := core.ListValue{
		core.SymbolValue("."),
		obj,
		prop,
	}
	
	// Check for method call or chained dot
	p.skipWhitespaceAndComments()
	if p.pos < len(p.input) {
		switch p.input[p.pos] {
		case '(':
			// Method call - parse arguments
			p.pos++ // consume '('
			
			// Parse arguments manually
			p.skipWhitespaceAndComments()
			for p.pos < len(p.input) && p.input[p.pos] != ')' {
				arg, err := p.parseExpr()
				if err != nil {
					return nil, err
				}
				dotExpr = append(dotExpr, arg)
				p.skipWhitespaceAndComments()
				
				// Handle comma-separated arguments
				if p.pos < len(p.input) && p.input[p.pos] == ',' {
					p.pos++ // consume comma
					p.skipWhitespaceAndComments()
				}
			}
			
			// Check for closing parenthesis
			if p.pos >= len(p.input) {
				return nil, p.error("unclosed method call")
			}
			p.pos++ // consume ')'
			
			// Check for further chaining
			p.skipWhitespaceAndComments()
			if p.pos < len(p.input) && p.input[p.pos] == '.' {
				p.pos++ // consume '.'
				return p.parseDotNotation(dotExpr)
			}
			return dotExpr, nil
			
		case '.':
			// Chained property access
			p.pos++ // consume '.'
			return p.parseDotNotation(dotExpr)
			
		default:
			// Just property access, check if it's the LHS of assignment
			// This will be handled by the caller
			return dotExpr, nil
		}
	}
	
	return dotExpr, nil
}

// tryParseDotNotation attempts to parse dot notation from the current position
// Returns nil if no dot notation is found
func (p *Parser) tryParseDotNotation(obj core.Value) (core.Value, bool, error) {
	// Check if next character is a dot
	p.skipWhitespaceAndComments()
	if p.pos >= len(p.input) || p.input[p.pos] != '.' {
		return obj, false, nil
	}
	
	// Make sure it's not a floating point number
	if num, ok := obj.(core.NumberValue); ok {
		// Check if this could be a decimal point
		if p.pos+1 < len(p.input) && isDigit(p.input[p.pos+1]) {
			// This is a decimal number, not dot notation
			return obj, false, nil
		}
		// Numbers can't have properties
		return nil, false, p.error(fmt.Sprintf("number %v has no properties", num))
	}
	
	// Consume the dot
	p.pos++
	
	// Parse the dot notation
	result, err := p.parseDotNotation(obj)
	if err != nil {
		return nil, false, err
	}
	
	return result, true, nil
}

// isValidPropertyName checks if a string is a valid property name
func isValidPropertyName(name string) bool {
	if len(name) == 0 {
		return false
	}
	
	// Check if it's a number (for index access)
	if _, err := strconv.Atoi(name); err == nil {
		return true
	}
	
	// Check if it's a valid identifier
	for i, ch := range name {
		if i == 0 {
			if !isLetter(byte(ch)) && ch != '_' {
				return false
			}
		} else {
			if !isLetter(byte(ch)) && !isDigit(byte(ch)) && ch != '_' && ch != '-' {
				return false
			}
		}
	}
	
	return true
}

// Helper to check if this is potentially a module.submodule.function pattern
func looksLikeModulePath(s string) bool {
	parts := strings.Split(s, ".")
	if len(parts) < 2 {
		return false
	}
	
	for _, part := range parts {
		if !isValidPropertyName(part) {
			return false
		}
	}
	
	return true
}
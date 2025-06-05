package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

// parseDotAccess handles parsing of dot notation after the initial dot
// It converts:
//
//	.prop -> builds (. base prop)
//	.method(args) -> builds (. base method args...)
func (p *Parser) parseDotAccess(base core.Value) (core.Value, error) {
	// Skip whitespace after dot
	p.skipWhitespaceAndComments()

	// Parse the property name
	if p.pos >= len(p.input) {
		return nil, p.error("unexpected end of input after '.'")
	}

	// Check what follows the dot
	ch := p.input[p.pos]

	var propName string

	if isDigit(ch) {
		// Numeric index like list.0
		start := p.pos
		for p.pos < len(p.input) && isDigit(p.input[p.pos]) {
			p.pos++
		}
		propName = p.input[start:p.pos]
	} else if isLetter(ch) || ch == '_' {
		// Regular property/method name
		propName = p.parseSymbolName()
	} else {
		return nil, p.error(fmt.Sprintf("unexpected character '%c' after '.'", ch))
	}

	// Check for method call
	// First, check if there's whitespace before the potential '('
	startPos := p.pos
	p.skipWhitespaceAndComments()
	hasWhitespace := p.pos > startPos

	// Only treat '(' as method call if there's no whitespace before it
	if p.pos < len(p.input) && p.input[p.pos] == '(' && !hasWhitespace {
		// It's a method call - parse arguments
		args, err := p.parseMethodArgs()
		if err != nil {
			return nil, err
		}

		// Build (. base method arg1 arg2...)
		// For method calls, we need to distinguish from property access
		// even when there are no args, so we add a special marker
		result := core.ListValue{
			core.SymbolValue("."),
			base,
			core.StringValue(propName),
		}
		// Add a method call marker to distinguish from property access
		if len(args) == 0 {
			// Empty args, but still a method call
			result = append(result, core.SymbolValue("__call__"))
		} else {
			result = append(result, args...)
		}
		return result, nil
	}

	// Not a method call - restore position to before whitespace skip
	p.pos = startPos

	// Just property access - build (. base prop)
	return core.ListValue{
		core.SymbolValue("."),
		base,
		core.StringValue(propName),
	}, nil
}

// parseMethodArgs parses the arguments of a method call
// Assumes the opening '(' has NOT been consumed
func (p *Parser) parseMethodArgs() ([]core.Value, error) {
	p.pos++ // consume '('

	var args []core.Value
	p.skipWhitespaceAndComments()

	// Empty args
	if p.pos < len(p.input) && p.input[p.pos] == ')' {
		p.pos++ // consume ')'
		return args, nil
	}

	// Parse comma-separated arguments
	for {
		arg, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)

		p.skipWhitespaceAndComments()
		if p.pos >= len(p.input) {
			return nil, p.error("unclosed method call")
		}

		if p.input[p.pos] == ')' {
			p.pos++ // consume ')'
			break
		}

		if p.input[p.pos] != ',' {
			return nil, p.error(fmt.Sprintf("expected ',' or ')' in method call, got '%c'", p.input[p.pos]))
		}
		p.pos++ // consume ','
		p.skipWhitespaceAndComments()
	}

	return args, nil
}

// DEPRECATED: tryParseDotNotation is kept for backward compatibility
// The new implementation uses parsePostfix and parseDotAccess
func (p *Parser) tryParseDotNotation(obj core.Value) (core.Value, bool, error) {
	// This function is no longer used but kept to avoid breaking changes
	return obj, false, nil
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

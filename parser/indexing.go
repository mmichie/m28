package parser

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// parseIndexAccess handles parsing of index access after '['
// It converts:
//   obj[key] -> (get-item obj key)
//   obj[start:end] -> (slice obj start end) [future]
func (p *Parser) parseIndexAccess(base core.Value) (core.Value, error) {
	// The '[' has already been consumed by parsePostfix
	p.skipWhitespaceAndComments()
	

	if p.pos >= len(p.input) {
		return nil, p.error("unexpected end of input after '['")
	}

	// Check for slice syntax (future feature)
	if p.input[p.pos] == ':' {
		return nil, p.error("slice syntax not yet implemented")
	}

	// Parse the index/key expression
	// We need to parse just the atom, not a full expression with postfix
	index, err := p.parseAtom()
	if err != nil {
		return nil, fmt.Errorf("error parsing index: %v", err)
	}
	
	// Skip whitespace and check for closing bracket
	p.skipWhitespaceAndComments()
	if p.pos >= len(p.input) {
		return nil, p.error("unclosed index access, expected ']'")
	}

	if p.input[p.pos] != ']' {
		// Check for slice syntax
		if p.input[p.pos] == ':' {
			return nil, p.error("slice syntax not yet implemented")
		}
		return nil, p.error(fmt.Sprintf("expected ']' after index, got '%c'", p.input[p.pos]))
	}
	p.pos++ // consume ']'

	// Build (get-item base index)
	return core.ListValue{
		core.SymbolValue("get-item"),
		base,
		index,
	}, nil
}
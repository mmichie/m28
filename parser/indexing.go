package parser

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// parseIndexAccess handles parsing of index access after '['
// It converts:
//
//	obj[key] -> (get-item obj key)
//	obj[start:end] -> (slice obj start end)
//	obj[start:end:step] -> (slice obj start end step)
func (p *Parser) parseIndexAccess(base core.Value) (core.Value, error) {
	// The '[' has already been consumed by parsePostfix
	p.skipWhitespaceAndComments()

	if p.pos >= len(p.input) {
		return nil, p.error("unexpected end of input after '['")
	}

	// Check for slice syntax with empty start [:end]
	if p.input[p.pos] == ':' {
		return p.parseSlice(base, nil)
	}

	// Parse the first expression (index or slice start)
	first, err := p.parseAtom()
	if err != nil {
		return nil, fmt.Errorf("error parsing index: %v", err)
	}

	// Skip whitespace and check what follows
	p.skipWhitespaceAndComments()
	if p.pos >= len(p.input) {
		return nil, p.error("unclosed index access, expected ']'")
	}

	if p.input[p.pos] == ']' {
		// Simple index access
		p.pos++ // consume ']'
		return core.ListValue{
			core.SymbolValue("get-item"),
			base,
			first,
		}, nil
	}

	if p.input[p.pos] == ':' {
		// Slice syntax
		return p.parseSlice(base, first)
	}

	return nil, p.error(fmt.Sprintf("expected ']' or ':' after index, got '%c'", p.input[p.pos]))
}

// parseSlice handles parsing slice syntax after the first ':'
// start may be nil for slices like [:end]
func (p *Parser) parseSlice(base core.Value, start core.Value) (core.Value, error) {
	// Consume the ':'
	p.pos++
	p.skipWhitespaceAndComments()

	var end core.Value
	var step core.Value

	// Check for empty end (e.g., [start:] or [start::step])
	if p.pos < len(p.input) && p.input[p.pos] != ']' && p.input[p.pos] != ':' {
		var err error
		end, err = p.parseAtom()
		if err != nil {
			return nil, fmt.Errorf("error parsing slice end: %v", err)
		}
		p.skipWhitespaceAndComments()
	}

	// Check for step
	if p.pos < len(p.input) && p.input[p.pos] == ':' {
		p.pos++ // consume second ':'
		p.skipWhitespaceAndComments()

		if p.pos < len(p.input) && p.input[p.pos] != ']' {
			var err error
			step, err = p.parseAtom()
			if err != nil {
				return nil, fmt.Errorf("error parsing slice step: %v", err)
			}
			p.skipWhitespaceAndComments()
		}
	}

	// Expect closing bracket
	if p.pos >= len(p.input) || p.input[p.pos] != ']' {
		return nil, p.error("expected ']' after slice")
	}
	p.pos++ // consume ']'

	// Build slice expression
	// Use nil for missing components
	if start == nil {
		start = core.None
	}
	if end == nil {
		end = core.None
	}
	if step == nil {
		step = core.None
	}

	return core.ListValue{
		core.SymbolValue("slice"),
		base,
		start,
		end,
		step,
	}, nil
}

package parser

import (
	"fmt"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// PythonParser implements a recursive descent parser for Python syntax
type PythonParser struct {
	tokens    []Token
	current   int
	errors    []error
	panic     bool   // panic mode for error recovery
	depth     int    // recursion depth tracking
	callCount int    // total parse calls (for detecting loops)
	maxDepth  int    // maximum recursion depth allowed
	maxCalls  int    // maximum parse calls allowed
	debugMode bool   // enable debug output
	filename  string // source filename for error reporting
	source    string // full source code for error context
}

// NewPythonParser creates a new Python parser from a token stream
func NewPythonParser(tokens []Token, filename string, source string) *PythonParser {
	return &PythonParser{
		tokens:    tokens,
		current:   0,
		errors:    []error{},
		panic:     false,
		depth:     0,
		callCount: 0,
		maxDepth:  500,    // Maximum recursion depth
		maxCalls:  100000, // Maximum total parse calls
		debugMode: false,  // Set to true to enable debug output
		filename:  filename,
		source:    source,
	}
}

// Parse parses the token stream into a list of AST nodes
func (p *PythonParser) Parse() ([]ast.ASTNode, error) {
	core.Log.Trace(core.SubsystemParser, "AST parsing started", "file", p.filename, "tokens", len(p.tokens))
	statements := []ast.ASTNode{}

	for !p.isAtEnd() {
		// Check for runaway parsing
		if p.callCount > p.maxCalls {
			err := fmt.Errorf("parser exceeded maximum call limit (%d calls) - possible infinite loop at token %d", p.maxCalls, p.current)
			core.Log.Error(core.SubsystemParser, "Parser exceeded call limit", "file", p.filename, "max_calls", p.maxCalls, "current_token", p.current)
			return nil, err
		}

		// Skip any leading newlines or semicolons
		for p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
			p.advance()
		}

		if p.isAtEnd() {
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			statements = append(statements, stmt)
			core.Log.Trace(core.SubsystemParser, "Statement parsed", "file", p.filename, "node_type", fmt.Sprintf("%T", stmt), "statement_count", len(statements))
		} else if len(p.errors) > 0 {
			// parseStatement returned nil and we have errors - stop parsing
			core.Log.Error(core.SubsystemParser, "Parse statement returned error", "file", p.filename, "error_count", len(p.errors))
			break
		}

		// If we hit an error, synchronize
		if p.panic {
			core.Log.Warn(core.SubsystemParser, "Parser in panic mode, synchronizing", "file", p.filename)
			p.synchronize()
		}
	}

	if len(p.errors) > 0 {
		core.Log.Error(core.SubsystemParser, "Parsing failed with errors", "file", p.filename, "error_count", len(p.errors), "statements_parsed", len(statements))
		return statements, fmt.Errorf("parse errors: %v", p.errors)
	}

	core.Log.Trace(core.SubsystemParser, "AST parsing completed successfully", "file", p.filename, "statement_count", len(statements))
	return statements, nil
}

// ============================================================================
// Parse Tracking (for detecting infinite loops)
// ============================================================================

// enterParse should be called at the start of each parse method
func (p *PythonParser) enterParse(name string) error {
	p.depth++
	p.callCount++

	if p.debugMode && p.callCount%100 == 0 {
		core.Log.Trace(core.SubsystemParser, "Parser progress",
			"call_count", p.callCount, "depth", p.depth,
			"token_pos", p.current, "total_tokens", len(p.tokens),
			"function", name)
	}

	if p.depth > p.maxDepth {
		return fmt.Errorf("parser exceeded maximum recursion depth (%d) in %s at token %d", p.maxDepth, name, p.current)
	}

	if p.callCount > p.maxCalls {
		return fmt.Errorf("parser exceeded maximum call limit (%d calls) in %s at token %d - infinite loop detected", p.maxCalls, name, p.current)
	}

	return nil
}

// exitParse should be called when leaving a parse method
func (p *PythonParser) exitParse() {
	p.depth--
}

// ============================================================================
// Token Navigation
// ============================================================================

// isAtEnd returns true if we've consumed all tokens
func (p *PythonParser) isAtEnd() bool {
	return p.current >= len(p.tokens) || p.peek().Type == TOKEN_EOF
}

// peek returns the current token without advancing
func (p *PythonParser) peek() Token {
	if p.current >= len(p.tokens) {
		return Token{Type: TOKEN_EOF}
	}
	return p.tokens[p.current]
}

// previous returns the previous token
func (p *PythonParser) previous() Token {
	if p.current == 0 {
		return Token{Type: TOKEN_EOF}
	}
	return p.tokens[p.current-1]
}

// advance consumes and returns the current token
func (p *PythonParser) advance() Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.previous()
}

// check returns true if the current token is of the given type
func (p *PythonParser) check(typ TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Type == typ
}

// checkAhead checks if the token at offset ahead matches the given type
func (p *PythonParser) checkAhead(typ TokenType, offset int) bool {
	idx := p.current + offset
	if idx >= len(p.tokens) {
		return false
	}
	return p.tokens[idx].Type == typ
}

// match advances if the current token matches any of the given types
func (p *PythonParser) match(types ...TokenType) bool {
	for _, typ := range types {
		if p.check(typ) {
			p.advance()
			return true
		}
	}
	return false
}

// expect consumes a token of the given type or reports an error
func (p *PythonParser) expect(typ TokenType) Token {
	if p.check(typ) {
		return p.advance()
	}

	p.error(fmt.Sprintf("Expected %v, got %v", typ, p.peek().Type))
	return Token{Type: TOKEN_ERROR}
}

// ============================================================================
// Error Handling
// ============================================================================

// error records a parse error
func (p *PythonParser) error(message string) {
	if p.panic {
		return // Don't cascade errors
	}

	p.panic = true
	tok := p.peek()
	err := &ParseError{
		Message:  message,
		Line:     tok.Line,
		Col:      tok.Col,
		Lexeme:   tok.Lexeme,
		Source:   p.source,
		Filename: p.filename,
	}
	p.errors = append(p.errors, err)
}

// synchronize recovers from a parse error by advancing to the next statement boundary
func (p *PythonParser) synchronize() {
	p.panic = false

	for !p.isAtEnd() {
		// Found a statement boundary (newline or dedent)
		if p.previous().Type == TOKEN_NEWLINE || p.previous().Type == TOKEN_DEDENT {
			return
		}

		// At the start of a new statement
		switch p.peek().Type {
		case TOKEN_DEF, TOKEN_CLASS, TOKEN_IF, TOKEN_FOR,
			TOKEN_WHILE, TOKEN_TRY, TOKEN_WITH, TOKEN_RETURN,
			TOKEN_BREAK, TOKEN_CONTINUE, TOKEN_PASS, TOKEN_RAISE,
			TOKEN_IMPORT, TOKEN_FROM:
			return
		}

		p.advance()
	}
}

// makeLocation creates a SourceLocation from a token
func (p *PythonParser) makeLocation(tok Token) *core.SourceLocation {
	return &core.SourceLocation{
		File:   p.filename,
		Line:   tok.Line,
		Column: tok.Col,
	}
}

// ============================================================================
// Statement Parsing
// ============================================================================

// parseStatement parses a single statement

// ============================================================================
// Parser Organization
// ============================================================================
//
// The Python parser is organized across multiple files for maintainability:
//
// - python_parser.go (this file): Core parser infrastructure
//   - PythonParser struct and constructor
//   - Parse() entry point
//   - Token utilities (peek, advance, check, match, expect, etc.)
//   - Error handling and synchronization
//   - Parse depth/call tracking
//
// - python_parser_statements.go: Statement parsing
//   - parseStatement (main dispatcher)
//   - Simple statements (return, yield, break, continue, pass, etc.)
//   - Import statements
//   - Expression statements and assignments
//
// - python_parser_expressions.go: Expression parsing
//   - Operator precedence climbing
//   - Primary expressions and literals
//   - Postfix expressions (calls, subscripts, attributes)
//   - Comprehensions and generators
//   - F-string parsing
//   - Lambda expressions
//
// - python_parser_compound.go: Compound statement parsing
//   - Block parsing and decorators
//   - Control flow (if, for, while, match)
//   - Function and class definitions
//   - Exception handling (try/except/finally)
//   - Context managers (with)
//

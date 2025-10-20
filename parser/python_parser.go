package parser

import (
	"fmt"
	"os"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// PythonParser implements a recursive descent parser for Python syntax
type PythonParser struct {
	tokens    []Token
	current   int
	errors    []error
	panic     bool // panic mode for error recovery
	depth     int  // recursion depth tracking
	callCount int  // total parse calls (for detecting loops)
	maxDepth  int  // maximum recursion depth allowed
	maxCalls  int  // maximum parse calls allowed
	debugMode bool // enable debug output
}

// NewPythonParser creates a new Python parser from a token stream
func NewPythonParser(tokens []Token) *PythonParser {
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
	}
}

// Parse parses the token stream into a list of AST nodes
func (p *PythonParser) Parse() ([]ast.ASTNode, error) {
	statements := []ast.ASTNode{}

	for !p.isAtEnd() {
		// Check for runaway parsing
		if p.callCount > p.maxCalls {
			return nil, fmt.Errorf("parser exceeded maximum call limit (%d calls) - possible infinite loop at token %d", p.maxCalls, p.current)
		}

		// Skip any leading newlines
		for p.check(TOKEN_NEWLINE) {
			p.advance()
		}

		if p.isAtEnd() {
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			statements = append(statements, stmt)
		} else if len(p.errors) > 0 {
			// parseStatement returned nil and we have errors - stop parsing
			break
		}

		// If we hit an error, synchronize
		if p.panic {
			p.synchronize()
		}
	}

	if len(p.errors) > 0 {
		return statements, fmt.Errorf("parse errors: %v", p.errors)
	}

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
		fmt.Printf("[PARSER] Call %d, Depth %d, Token %d/%d: %s\n",
			p.callCount, p.depth, p.current, len(p.tokens), name)
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
	err := fmt.Errorf("parse error at line %d, col %d: %s (near %q)",
		tok.Line, tok.Col, message, tok.Lexeme)
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
		File:   "<input>",
		Line:   tok.Line,
		Column: tok.Col,
	}
}

// ============================================================================
// Statement Parsing
// ============================================================================

// parseStatement parses a single statement
func (p *PythonParser) parseStatement() ast.ASTNode {
	if err := p.enterParse("parseStatement"); err != nil {
		p.errors = append(p.errors, err)
		return nil
	}
	defer p.exitParse()

	// Check for decorators first (can precede def or class)
	if p.check(TOKEN_AT) {
		decorators := p.parseDecorators()

		// Decorators must be followed by def, async def, or class
		isAsync := false
		if p.check(TOKEN_ASYNC) {
			isAsync = true
			p.advance()
		}

		if p.check(TOKEN_DEF) {
			return p.parseDefStatement(decorators, isAsync)
		} else if p.check(TOKEN_CLASS) {
			return p.parseClassStatement(decorators)
		} else {
			p.error("Decorators must precede 'def' or 'class'")
			return nil
		}
	}

	// Simple statements
	switch p.peek().Type {
	case TOKEN_ASYNC:
		// async def, async for, async with
		return p.parseAsyncStatement()
	case TOKEN_DEF:
		return p.parseDefStatement(nil, false)
	case TOKEN_CLASS:
		return p.parseClassStatement(nil)
	case TOKEN_IF:
		return p.parseIfStatement()
	case TOKEN_FOR:
		return p.parseForStatement()
	case TOKEN_WHILE:
		return p.parseWhileStatement()
	case TOKEN_TRY:
		return p.parseTryStatement()
	case TOKEN_WITH:
		return p.parseWithStatement()
	case TOKEN_IMPORT:
		return p.parseImportStatement()
	case TOKEN_FROM:
		return p.parseFromImportStatement()
	case TOKEN_RETURN:
		return p.parseReturnStatement()
	case TOKEN_YIELD:
		return p.parseYieldStatement()
	case TOKEN_BREAK:
		return p.parseBreakStatement()
	case TOKEN_CONTINUE:
		return p.parseContinueStatement()
	case TOKEN_PASS:
		return p.parsePassStatement()
	case TOKEN_RAISE:
		return p.parseRaiseStatement()
	case TOKEN_ASSERT:
		return p.parseAssertStatement()
	case TOKEN_GLOBAL:
		return p.parseGlobalStatement()
	case TOKEN_NONLOCAL:
		return p.parseNonlocalStatement()
	case TOKEN_DEL:
		return p.parseDelStatement()
	case TOKEN_IDENTIFIER:
		// Check for soft keywords (match)
		// Only treat "match" as keyword if not followed by assignment
		if p.peek().Lexeme == "match" {
			// Look ahead to see if this is a match statement or variable assignment
			// match statement: match expr:
			// variable: match = value
			if p.current+1 < len(p.tokens) && p.tokens[p.current+1].Type != TOKEN_ASSIGN {
				// Not an assignment, could be match statement
				// Parse and see what happens
				return p.parseMatchStatement()
			}
		}
		// Fall through to expression statement
		return p.parseExpressionStatement()
	default:
		// Expression statement (could be assignment)
		return p.parseExpressionStatement()
	}
}

// ============================================================================
// Simple Statement Parsing
// ============================================================================

// parseReturnStatement parses: return [expression[, expression]*]
func (p *PythonParser) parseReturnStatement() ast.ASTNode {
	tok := p.expect(TOKEN_RETURN)

	var value ast.ASTNode
	if !p.check(TOKEN_NEWLINE) && !p.isAtEnd() {
		// Parse first expression
		first := p.parseExpression()

		// Check for comma-separated expressions (implicit tuple)
		if p.check(TOKEN_COMMA) {
			values := []ast.ASTNode{first}

			for p.check(TOKEN_COMMA) {
				p.advance() // consume comma

				// Trailing comma is allowed
				if p.check(TOKEN_NEWLINE) || p.isAtEnd() {
					break
				}

				values = append(values, p.parseExpression())
			}

			// Create tuple: (tuple-literal elem1 elem2 ...)
			tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
			allElements := append([]ast.ASTNode{tupleSym}, values...)
			value = ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
		} else {
			value = first
		}
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewReturnForm(value, p.makeLocation(tok), ast.SyntaxPython)
}

// parseYieldStatement parses: yield [expression] or yield from expression
// Also handles implicit tuples: yield a, b, c
func (p *PythonParser) parseYieldStatement() ast.ASTNode {
	tok := p.expect(TOKEN_YIELD)

	// Check for "yield from"
	var yieldKeyword string
	if p.check(TOKEN_FROM) {
		p.advance() // consume FROM
		yieldKeyword = "yield-from"
	} else {
		yieldKeyword = "yield"
	}

	var args []ast.ASTNode
	if !p.check(TOKEN_NEWLINE) && !p.isAtEnd() {
		// Parse first expression
		first := p.parseExpression()

		// Check for implicit tuple: yield a, b, c
		if p.check(TOKEN_COMMA) {
			values := []ast.ASTNode{first}

			for p.check(TOKEN_COMMA) {
				p.advance()
				if p.check(TOKEN_NEWLINE) || p.isAtEnd() {
					break
				}
				values = append(values, p.parseExpression())
			}

			// Create tuple: (tuple-literal elem1 elem2 ...)
			tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
			allElements := append([]ast.ASTNode{tupleSym}, values...)
			args = append(args, ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython))
		} else {
			args = append(args, first)
		}
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	// Emit (yield expr) or (yield-from expr) form
	return ast.NewSExpr(append([]ast.ASTNode{
		ast.NewIdentifier(yieldKeyword, p.makeLocation(tok), ast.SyntaxPython),
	}, args...), p.makeLocation(tok), ast.SyntaxPython)
}

// parseBreakStatement parses: break
func (p *PythonParser) parseBreakStatement() ast.ASTNode {
	tok := p.expect(TOKEN_BREAK)

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewBreakForm(p.makeLocation(tok), ast.SyntaxPython)
}

// parseContinueStatement parses: continue
func (p *PythonParser) parseContinueStatement() ast.ASTNode {
	tok := p.expect(TOKEN_CONTINUE)

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewContinueForm(p.makeLocation(tok), ast.SyntaxPython)
}

// parsePassStatement parses: pass
func (p *PythonParser) parsePassStatement() ast.ASTNode {
	tok := p.expect(TOKEN_PASS)

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewPassForm(p.makeLocation(tok), ast.SyntaxPython)
}

// parseGlobalStatement parses: global name [, name2, ...]
func (p *PythonParser) parseGlobalStatement() ast.ASTNode {
	tok := p.expect(TOKEN_GLOBAL)

	// Parse comma-separated list of names
	names := []ast.ASTNode{
		ast.NewIdentifier("global", p.makeLocation(tok), ast.SyntaxPython),
	}

	// First name
	nameTok := p.expect(TOKEN_IDENTIFIER)
	names = append(names, ast.NewIdentifier(nameTok.Lexeme, p.makeLocation(nameTok), ast.SyntaxPython))

	// Additional names
	for p.check(TOKEN_COMMA) {
		p.advance()
		nameTok := p.expect(TOKEN_IDENTIFIER)
		names = append(names, ast.NewIdentifier(nameTok.Lexeme, p.makeLocation(nameTok), ast.SyntaxPython))
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewSExpr(names, p.makeLocation(tok), ast.SyntaxPython)
}

// parseNonlocalStatement parses: nonlocal name [, name2, ...]
func (p *PythonParser) parseNonlocalStatement() ast.ASTNode {
	tok := p.expect(TOKEN_NONLOCAL)

	// Parse comma-separated list of names
	names := []ast.ASTNode{
		ast.NewIdentifier("nonlocal", p.makeLocation(tok), ast.SyntaxPython),
	}

	// First name
	nameTok := p.expect(TOKEN_IDENTIFIER)
	names = append(names, ast.NewIdentifier(nameTok.Lexeme, p.makeLocation(nameTok), ast.SyntaxPython))

	// Additional names
	for p.check(TOKEN_COMMA) {
		p.advance()
		nameTok := p.expect(TOKEN_IDENTIFIER)
		names = append(names, ast.NewIdentifier(nameTok.Lexeme, p.makeLocation(nameTok), ast.SyntaxPython))
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewSExpr(names, p.makeLocation(tok), ast.SyntaxPython)
}

// parseRaiseStatement parses: raise [exception [from cause]]
func (p *PythonParser) parseRaiseStatement() ast.ASTNode {
	tok := p.expect(TOKEN_RAISE)

	var exception, cause ast.ASTNode

	if !p.check(TOKEN_NEWLINE) && !p.isAtEnd() {
		exception = p.parseExpression()

		// Check for 'from' clause
		if p.check(TOKEN_FROM) {
			p.advance()
			cause = p.parseExpression()
		}
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewRaiseForm(exception, cause, p.makeLocation(tok), ast.SyntaxPython)
}

// parseAssertStatement parses: assert condition [, message]
func (p *PythonParser) parseAssertStatement() ast.ASTNode {
	tok := p.expect(TOKEN_ASSERT)

	// Parse condition (required)
	condition := p.parseExpression()

	var message ast.ASTNode
	// Check for optional message after comma
	if p.check(TOKEN_COMMA) {
		p.advance()
		message = p.parseExpression()
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewAssertForm(condition, message, p.makeLocation(tok), ast.SyntaxPython)
}

// parseDelStatement parses: del target [, target2, ...]
func (p *PythonParser) parseDelStatement() ast.ASTNode {
	tok := p.expect(TOKEN_DEL)

	// Parse deletion targets (can be comma-separated)
	targets := []ast.ASTNode{p.parseExpression()}

	for p.check(TOKEN_COMMA) {
		p.advance()
		targets = append(targets, p.parseExpression())
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	// Create (del target1 target2 ...) form
	delSym := ast.NewIdentifier("del", p.makeLocation(tok), ast.SyntaxPython)
	allArgs := append([]ast.ASTNode{delSym}, targets...)
	return ast.NewSExpr(allArgs, p.makeLocation(tok), ast.SyntaxPython)
}

// parseImportStatement parses: import module [as alias] [, module2 [as alias2], ...]
func (p *PythonParser) parseImportStatement() ast.ASTNode {
	tok := p.expect(TOKEN_IMPORT)

	// Parse first module
	modules := []ast.ASTNode{p.parseImportModule(tok)}

	// Parse additional modules (comma-separated)
	for p.check(TOKEN_COMMA) {
		p.advance()
		modules = append(modules, p.parseImportModule(tok))
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	// If there's only one import, return it directly
	if len(modules) == 1 {
		return modules[0]
	}

	// Multiple imports: wrap in a block
	return ast.NewBlockForm(modules, p.makeLocation(tok), ast.SyntaxPython)
}

// parseImportModule parses a single module import: module [as alias]
// Returns an SExpr representing (import "module" [:as alias])
func (p *PythonParser) parseImportModule(tok Token) ast.ASTNode {
	// Parse module name (can be dotted: os.path)
	moduleName := p.parseModuleName()

	// Create import form: (import "module" ...)
	elements := []ast.ASTNode{
		ast.NewIdentifier("import", p.makeLocation(tok), ast.SyntaxPython),
		ast.NewLiteral(core.StringValue(moduleName), p.makeLocation(tok), ast.SyntaxPython),
	}

	// Check for 'as alias'
	if p.check(TOKEN_AS) {
		p.advance()
		aliasTok := p.expect(TOKEN_IDENTIFIER)

		// Add :as symbol and alias
		elements = append(elements,
			ast.NewIdentifier(":as", p.makeLocation(tok), ast.SyntaxPython),
			ast.NewIdentifier(aliasTok.Lexeme, p.makeLocation(aliasTok), ast.SyntaxPython),
		)
	}

	return ast.NewSExpr(elements, p.makeLocation(tok), ast.SyntaxPython)
}

// parseFromImportStatement parses: from module import name [as alias] [, name2 [as alias2], ...]
// or: from module import *
// Also handles relative imports: from . import, from .module import, from ..module import
func (p *PythonParser) parseFromImportStatement() ast.ASTNode {
	tok := p.expect(TOKEN_FROM)

	// Check for relative imports (dots)
	relativeLevel := 0
	for p.check(TOKEN_DOT) {
		p.advance()
		relativeLevel++
	}

	// Parse optional module name after dots (or required if no dots)
	var moduleName string
	if p.check(TOKEN_IDENTIFIER) {
		moduleName = p.parseModuleName()
	} else if relativeLevel == 0 {
		// No dots and no identifier means error
		p.error("Expected module name in import statement")
		return nil
	}

	// Expect 'import'
	p.expect(TOKEN_IMPORT)

	// Create base import form
	// For relative imports: (import "module" :from ... :level N)
	// For absolute imports: (import "module" :from ...)
	elements := []ast.ASTNode{
		ast.NewIdentifier("import", p.makeLocation(tok), ast.SyntaxPython),
		ast.NewLiteral(core.StringValue(moduleName), p.makeLocation(tok), ast.SyntaxPython),
		ast.NewIdentifier(":from", p.makeLocation(tok), ast.SyntaxPython),
	}

	// Check for '*' (import all)
	if p.check(TOKEN_STAR) {
		p.advance()
		elements = append(elements, ast.NewIdentifier("*", p.makeLocation(tok), ast.SyntaxPython))

		// Add relative level if it's a relative import
		if relativeLevel > 0 {
			elements = append(elements,
				ast.NewIdentifier(":level", p.makeLocation(tok), ast.SyntaxPython),
				ast.NewLiteral(core.NumberValue(relativeLevel), p.makeLocation(tok), ast.SyntaxPython),
			)
		}

		// Consume newline
		if p.check(TOKEN_NEWLINE) {
			p.advance()
		}

		return ast.NewSExpr(elements, p.makeLocation(tok), ast.SyntaxPython)
	}

	// Check for parenthesized import list (multi-line imports)
	hasParens := false
	if p.check(TOKEN_LPAREN) {
		hasParens = true
		p.advance()
		// Skip any newlines after opening paren
		for p.check(TOKEN_NEWLINE) {
			p.advance()
		}
	}

	// Parse import names (comma-separated)
	// Each name can be either:
	//   - just a name: symbol
	//   - name with alias: (list-literal name alias)
	names := []ast.ASTNode{}

	// First name
	nameTok := p.expect(TOKEN_IDENTIFIER)
	nameNode := p.parseImportName(nameTok, tok)
	names = append(names, nameNode)

	// Additional names
	for p.check(TOKEN_COMMA) {
		p.advance()
		// Skip newlines after comma (for multi-line imports)
		if hasParens {
			for p.check(TOKEN_NEWLINE) {
				p.advance()
			}
		}
		// Allow trailing comma
		if hasParens && p.check(TOKEN_RPAREN) {
			break
		}
		nameTok := p.expect(TOKEN_IDENTIFIER)
		nameNode := p.parseImportName(nameTok, tok)
		names = append(names, nameNode)
	}

	// Close parentheses if we opened them
	if hasParens {
		// Skip any newlines before closing paren
		for p.check(TOKEN_NEWLINE) {
			p.advance()
		}
		p.expect(TOKEN_RPAREN)
	}

	// Create (list-literal name1 name2 ...) for the names
	listLiteral := append([]ast.ASTNode{ast.NewIdentifier("list-literal", p.makeLocation(tok), ast.SyntaxPython)}, names...)
	elements = append(elements, ast.NewSExpr(listLiteral, p.makeLocation(tok), ast.SyntaxPython))

	// Add relative level after the names if it's a relative import
	if relativeLevel > 0 {
		elements = append(elements,
			ast.NewIdentifier(":level", p.makeLocation(tok), ast.SyntaxPython),
			ast.NewLiteral(core.NumberValue(relativeLevel), p.makeLocation(tok), ast.SyntaxPython),
		)
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewSExpr(elements, p.makeLocation(tok), ast.SyntaxPython)
}

// parseModuleName parses a module name (can be dotted: os.path.join)
func (p *PythonParser) parseModuleName() string {
	nameTok := p.expect(TOKEN_IDENTIFIER)
	name := nameTok.Lexeme

	// Handle dotted module names (os.path, etc.)
	for p.check(TOKEN_DOT) {
		p.advance()
		nextTok := p.expect(TOKEN_IDENTIFIER)
		name += "." + nextTok.Lexeme
	}

	return name
}

// parseImportName parses a single import name with optional alias
// Returns either:
//   - symbol (for name without alias)
//   - (list-literal name alias) (for name with alias)
func (p *PythonParser) parseImportName(nameTok Token, baseTok Token) ast.ASTNode {
	// Check for 'as alias'
	if p.check(TOKEN_AS) {
		p.advance()
		aliasTok := p.expect(TOKEN_IDENTIFIER)

		// Return (list-literal name alias)
		elements := []ast.ASTNode{
			ast.NewIdentifier("list-literal", p.makeLocation(baseTok), ast.SyntaxPython),
			ast.NewIdentifier(nameTok.Lexeme, p.makeLocation(nameTok), ast.SyntaxPython),
			ast.NewIdentifier(aliasTok.Lexeme, p.makeLocation(aliasTok), ast.SyntaxPython),
		}
		return ast.NewSExpr(elements, p.makeLocation(nameTok), ast.SyntaxPython)
	}

	// No alias, just return the symbol
	return ast.NewIdentifier(nameTok.Lexeme, p.makeLocation(nameTok), ast.SyntaxPython)
}

// parseExpressionStatement parses an expression or assignment statement
func (p *PythonParser) parseExpressionStatement() ast.ASTNode {
	// Check if this starts with star unpacking: *a, b = ...
	var expr ast.ASTNode
	if p.check(TOKEN_STAR) && p.current+1 < len(p.tokens) && p.tokens[p.current+1].Type == TOKEN_IDENTIFIER {
		// Star unpacking at start
		starTok := p.advance()
		varName := p.expect(TOKEN_IDENTIFIER)
		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("*unpack", p.makeLocation(starTok), ast.SyntaxPython),
			ast.NewIdentifier(varName.Lexeme, p.makeLocation(varName), ast.SyntaxPython),
		}, p.makeLocation(starTok), ast.SyntaxPython)
	} else {
		expr = p.parseExpression()
	}

	// Check for comma (tuple formation on left side of assignment)
	// e.g., x, y = 1, 2 or a, *b, c = [1, 2, 3, 4]
	if p.check(TOKEN_COMMA) {
		// Collect all comma-separated expressions (with possible star unpacking)
		elements := []ast.ASTNode{expr}
		for p.check(TOKEN_COMMA) {
			p.advance() // consume comma
			if p.check(TOKEN_ASSIGN) || p.check(TOKEN_NEWLINE) {
				// Trailing comma before assignment or end of line
				break
			}

			// Check for star unpacking: *rest
			if p.check(TOKEN_STAR) {
				starTok := p.advance()
				varName := p.expect(TOKEN_IDENTIFIER)
				// Create a marker for star unpacking: (*unpack varname)
				starExpr := ast.NewSExpr([]ast.ASTNode{
					ast.NewIdentifier("*unpack", p.makeLocation(starTok), ast.SyntaxPython),
					ast.NewIdentifier(varName.Lexeme, p.makeLocation(varName), ast.SyntaxPython),
				}, p.makeLocation(starTok), ast.SyntaxPython)
				elements = append(elements, starExpr)
			} else {
				elements = append(elements, p.parseExpression())
			}
		}

		// If more than one element, it's a tuple pattern for unpacking
		// Don't use tuple-literal here - just a plain list of targets
		if len(elements) > 1 {
			expr = ast.NewSExpr(elements, p.makeLocation(p.peek()), ast.SyntaxPython)
		}
	}

	// Check for assignment (including chained assignment like x = y = z = 0)
	if p.check(TOKEN_ASSIGN) {
		tok := p.advance()

		// Collect all assignment targets
		// For x = y = z = 0, we collect [x, y, z] and then parse 0
		targets := []ast.ASTNode{expr}

		// Keep collecting targets while we see = followed by another expression
		for {
			nextExpr := p.parseExpression()

			// Check for comma on right side (tuple formation)
			if p.check(TOKEN_COMMA) {
				elements := []ast.ASTNode{nextExpr}
				for p.check(TOKEN_COMMA) {
					p.advance() // consume comma
					if p.check(TOKEN_ASSIGN) || p.check(TOKEN_NEWLINE) {
						break
					}
					elements = append(elements, p.parseExpression())
				}

				// Create tuple for right side
				if len(elements) > 1 {
					tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
					allElements := append([]ast.ASTNode{tupleSym}, elements...)
					nextExpr = ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
				}
			}

			if p.check(TOKEN_ASSIGN) {
				// Another assignment, collect this target
				targets = append(targets, nextExpr)
				p.advance() // consume the =
			} else {
				// This is the final value
				// Build nested assignments right-to-left
				// x = y = z = 0 becomes (= x (= y (= z 0)))
				result := ast.NewAssignForm(targets[len(targets)-1], nextExpr, p.makeLocation(tok), ast.SyntaxPython)

				// Work backwards through remaining targets
				for i := len(targets) - 2; i >= 0; i-- {
					result = ast.NewAssignForm(targets[i], result, p.makeLocation(tok), ast.SyntaxPython)
				}

				// Consume newline
				if p.check(TOKEN_NEWLINE) {
					p.advance()
				}

				return result
			}
		}
	}

	// Check for augmented assignment (+=, -=, etc.)
	if p.match(TOKEN_PLUS_ASSIGN, TOKEN_MINUS_ASSIGN, TOKEN_STAR_ASSIGN,
		TOKEN_SLASH_ASSIGN, TOKEN_DOUBLESLASH_ASSIGN, TOKEN_PERCENT_ASSIGN,
		TOKEN_PIPE_ASSIGN, TOKEN_AMPERSAND_ASSIGN, TOKEN_CARET_ASSIGN,
		TOKEN_LSHIFT_ASSIGN, TOKEN_RSHIFT_ASSIGN) {
		tok := p.previous()
		value := p.parseExpression()

		// Convert augmented assignment to regular assignment
		// x += 1 → x = x + 1
		// d |= x → d = d | x
		var op string
		switch tok.Type {
		case TOKEN_PLUS_ASSIGN:
			op = "+"
		case TOKEN_MINUS_ASSIGN:
			op = "-"
		case TOKEN_STAR_ASSIGN:
			op = "*"
		case TOKEN_SLASH_ASSIGN:
			op = "/"
		case TOKEN_DOUBLESLASH_ASSIGN:
			op = "//"
		case TOKEN_PERCENT_ASSIGN:
			op = "%"
		case TOKEN_PIPE_ASSIGN:
			op = "|"
		case TOKEN_AMPERSAND_ASSIGN:
			op = "&"
		case TOKEN_CARET_ASSIGN:
			op = "^"
		case TOKEN_LSHIFT_ASSIGN:
			op = "<<"
		case TOKEN_RSHIFT_ASSIGN:
			op = ">>"
		}

		opCall := ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
			expr,
			value,
		}, p.makeLocation(tok), ast.SyntaxPython)

		// Consume newline
		if p.check(TOKEN_NEWLINE) {
			p.advance()
		}

		return ast.NewAssignForm(expr, opCall, p.makeLocation(tok), ast.SyntaxPython)
	}

	// Bare expression statement
	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return expr
}

// ============================================================================
// Expression Parsing (Precedence Climbing)
// ============================================================================

// parseExpression is the entry point for expression parsing
func (p *PythonParser) parseExpression() ast.ASTNode {
	if err := p.enterParse("parseExpression"); err != nil {
		p.errors = append(p.errors, err)
		return nil
	}
	defer p.exitParse()

	// Lambda has lowest precedence, handle it first
	if p.check(TOKEN_LAMBDA) {
		return p.parseLambda()
	}
	return p.parseNamedExpr()
}

// parseNamedExpr parses: ternary [:= ternary]
// Python: (x := expr)  -- walrus operator / assignment expression
func (p *PythonParser) parseNamedExpr() ast.ASTNode {
	expr := p.parseTernary()

	if p.check(TOKEN_COLONEQUAL) {
		tok := p.advance()

		// Left side must be an identifier
		ident, ok := expr.(*ast.Identifier)
		if !ok {
			// Error: walrus operator requires identifier on left
			return nil
		}

		// Parse the value
		value := p.parseTernary()

		// Create (:= var value) form
		// This should both assign and return the value
		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(":=", p.makeLocation(tok), ast.SyntaxPython),
			ident,
			value,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseTernary parses: or (if or else or)?
// Python: x if condition else y
func (p *PythonParser) parseTernary() ast.ASTNode {
	expr := p.parseOr()

	if p.check(TOKEN_IF) {
		p.advance() // consume 'if'
		condition := p.parseOr()

		if !p.check(TOKEN_ELSE) {
			return nil // Error: expected 'else' after 'if' in ternary
		}
		p.advance() // consume 'else'

		elseExpr := p.parseTernary() // Allow nested ternaries

		// Create (if condition expr elseExpr)
		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("if", p.makeLocation(p.previous()), ast.SyntaxPython),
			condition,
			expr,
			elseExpr,
		}, p.makeLocation(p.previous()), ast.SyntaxPython)
	}

	return expr
}

// parseLambda parses: lambda [params]: expression
func (p *PythonParser) parseLambda() ast.ASTNode {
	tok := p.expect(TOKEN_LAMBDA)

	// Parse parameters (no parentheses)
	params := []ast.ASTNode{}

	// If there's a colon immediately, no parameters
	if !p.check(TOKEN_COLON) {
		// Parse comma-separated parameter list
		for {
			if p.check(TOKEN_COLON) {
				break
			}

			paramTok := p.expect(TOKEN_IDENTIFIER)
			params = append(params, ast.NewIdentifier(paramTok.Lexeme, p.makeLocation(paramTok), ast.SyntaxPython))

			if !p.check(TOKEN_COMMA) {
				break
			}
			p.advance() // consume comma
		}
	}

	p.expect(TOKEN_COLON)

	// Parse the body expression (everything after the colon)
	// Use parseExpression to allow nested lambdas
	body := p.parseExpression()

	// Create parameter list S-expr
	paramList := ast.NewSExpr(params, p.makeLocation(tok), ast.SyntaxPython)

	// Create (lambda (params...) body) S-expr
	return ast.NewSExpr([]ast.ASTNode{
		ast.NewIdentifier("lambda", p.makeLocation(tok), ast.SyntaxPython),
		paramList,
		body,
	}, p.makeLocation(tok), ast.SyntaxPython)
}

// parseOr parses: and_expr (or and_expr)*
func (p *PythonParser) parseOr() ast.ASTNode {
	expr := p.parseAnd()

	for p.check(TOKEN_OR) {
		tok := p.advance()
		right := p.parseAnd()
		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("or", p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseAnd parses: not_expr (and not_expr)*
func (p *PythonParser) parseAnd() ast.ASTNode {
	expr := p.parseNot()

	for p.check(TOKEN_AND) {
		tok := p.advance()
		right := p.parseNot()
		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("and", p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseNot parses: not not_expr | comparison
func (p *PythonParser) parseNot() ast.ASTNode {
	if p.check(TOKEN_NOT) {
		tok := p.advance()
		expr := p.parseNot()
		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("not", p.makeLocation(tok), ast.SyntaxPython),
			expr,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return p.parseComparison()
}

// parseComparison parses: bitwise_or (comp_op bitwise_or)*
func (p *PythonParser) parseComparison() ast.ASTNode {
	expr := p.parseBitwiseOr()

	for p.match(TOKEN_EQUALEQUAL, TOKEN_NOTEQUAL, TOKEN_LESS, TOKEN_LESSEQUAL,
		TOKEN_GREATER, TOKEN_GREATEREQUAL, TOKEN_IN, TOKEN_NOT_IN, TOKEN_IS, TOKEN_IS_NOT) {
		tok := p.previous()
		right := p.parseBitwiseOr()

		var op string
		switch tok.Type {
		case TOKEN_EQUALEQUAL:
			op = "=="
		case TOKEN_NOTEQUAL:
			op = "!="
		case TOKEN_LESS:
			op = "<"
		case TOKEN_LESSEQUAL:
			op = "<="
		case TOKEN_GREATER:
			op = ">"
		case TOKEN_GREATEREQUAL:
			op = ">="
		case TOKEN_IN:
			op = "in"
		case TOKEN_NOT_IN:
			op = "not in"
		case TOKEN_IS:
			op = "is"
		case TOKEN_IS_NOT:
			op = "is not"
		}

		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseBitwiseOr parses: xor (| xor)*
func (p *PythonParser) parseBitwiseOr() ast.ASTNode {
	expr := p.parseBitwiseXor()

	for p.match(TOKEN_PIPE) {
		tok := p.previous()
		right := p.parseBitwiseXor()

		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("|", p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseBitwiseXor parses: and (^ and)*
func (p *PythonParser) parseBitwiseXor() ast.ASTNode {
	expr := p.parseBitwiseAnd()

	for p.match(TOKEN_CARET) {
		tok := p.previous()
		right := p.parseBitwiseAnd()

		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("^", p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseBitwiseAnd parses: shift (& shift)*
func (p *PythonParser) parseBitwiseAnd() ast.ASTNode {
	expr := p.parseShift()

	for p.match(TOKEN_AMPERSAND) {
		tok := p.previous()
		right := p.parseShift()

		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("&", p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseShift parses: addition ((<<|>>) addition)*
func (p *PythonParser) parseShift() ast.ASTNode {
	expr := p.parseAddition()

	for p.match(TOKEN_LSHIFT, TOKEN_RSHIFT) {
		tok := p.previous()
		right := p.parseAddition()

		var op string
		if tok.Type == TOKEN_LSHIFT {
			op = "<<"
		} else {
			op = ">>"
		}

		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseAddition parses: multiplication ((+|-) multiplication)*
func (p *PythonParser) parseAddition() ast.ASTNode {
	expr := p.parseMultiplication()

	for p.match(TOKEN_PLUS, TOKEN_MINUS) {
		tok := p.previous()
		right := p.parseMultiplication()

		var op string
		if tok.Type == TOKEN_PLUS {
			op = "+"
		} else {
			op = "-"
		}

		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseMultiplication parses: factor ((*|/|//|%) factor)*
func (p *PythonParser) parseMultiplication() ast.ASTNode {
	expr := p.parseFactor()

	for p.match(TOKEN_STAR, TOKEN_SLASH, TOKEN_DOUBLESLASH, TOKEN_PERCENT) {
		tok := p.previous()
		right := p.parseFactor()

		var op string
		switch tok.Type {
		case TOKEN_STAR:
			op = "*"
		case TOKEN_SLASH:
			op = "/"
		case TOKEN_DOUBLESLASH:
			op = "//"
		case TOKEN_PERCENT:
			op = "%"
		}

		expr = ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseFactor parses: (-|+|~) factor | power
// Python grammar: factor: ('+' | '-' | '~') factor | power
func (p *PythonParser) parseFactor() ast.ASTNode {
	// Handle await expression
	if p.match(TOKEN_AWAIT) {
		tok := p.previous()
		expr := p.parseFactor()

		// For now, await just evaluates the expression (no async runtime)
		// Emit: (await expr)
		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("await", p.makeLocation(tok), ast.SyntaxPython),
			expr,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	// Handle yield expression (used in expression context, not statement)
	// Example: (yield), (yield x), foo((yield x))
	if p.match(TOKEN_YIELD) {
		tok := p.previous()

		// Check for "yield from"
		var yieldKeyword string
		if p.check(TOKEN_FROM) {
			p.advance() // consume FROM
			yieldKeyword = "yield-from"
		} else {
			yieldKeyword = "yield"
		}

		var args []ast.ASTNode
		// Parse optional value (not if at end of expression context)
		if !p.check(TOKEN_RPAREN) && !p.check(TOKEN_COMMA) && !p.check(TOKEN_RBRACKET) &&
			!p.check(TOKEN_RBRACE) && !p.check(TOKEN_NEWLINE) && !p.isAtEnd() {
			args = append(args, p.parseFactor())
		}

		// Emit (yield expr) or (yield-from expr) form
		return ast.NewSExpr(append([]ast.ASTNode{
			ast.NewIdentifier(yieldKeyword, p.makeLocation(tok), ast.SyntaxPython),
		}, args...), p.makeLocation(tok), ast.SyntaxPython)
	}

	if p.match(TOKEN_MINUS, TOKEN_PLUS, TOKEN_TILDE) {
		tok := p.previous()
		expr := p.parseFactor()

		var op string
		switch tok.Type {
		case TOKEN_MINUS:
			op = "-"
		case TOKEN_PLUS:
			op = "+"
		case TOKEN_TILDE:
			op = "~"
		}

		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
			expr,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return p.parsePower()
}

// parsePower parses: postfix (** factor)? (right-associative)
// Python grammar: power: atom_expr ['**' factor]
// Examples:
//
//	2**3**4 == 2**(3**4) = 512
//	-2**2 == -(2**2) = -4
//	2**-3 == 2**(-3) = 0.125
func (p *PythonParser) parsePower() ast.ASTNode {
	expr := p.parsePostfix()

	if p.match(TOKEN_DOUBLESTAR) {
		tok := p.previous()
		// Right-associative: exponent can have unary operators
		right := p.parseFactor()

		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("**", p.makeLocation(tok), ast.SyntaxPython),
			expr,
			right,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	return expr
}

// parseUnary is now parseFactor (see above)
// Keeping this for compatibility, but it now just calls parseFactor
func (p *PythonParser) parseUnary() ast.ASTNode {
	return p.parseFactor()
}

// parsePostfix parses: primary (call | subscript | attribute)*
func (p *PythonParser) parsePostfix() ast.ASTNode {
	expr := p.parsePrimary()

	for {
		if p.check(TOKEN_LPAREN) {
			// Function call
			expr = p.parseCall(expr)
		} else if p.check(TOKEN_LBRACKET) {
			// Subscript
			expr = p.parseSubscript(expr)
		} else if p.check(TOKEN_DOT) {
			// Attribute access
			expr = p.parseAttribute(expr)
		} else {
			break
		}
	}

	return expr
}

// parseCall parses: (args)
func (p *PythonParser) parseCall(callee ast.ASTNode) ast.ASTNode {
	tok := p.expect(TOKEN_LPAREN)

	args := []ast.ASTNode{callee}
	var kwargs []ast.ASTNode // keyword arguments as (keyword value) pairs

	if !p.check(TOKEN_RPAREN) {
		seenKeyword := false
		for {
			// Check for **kwargs unpacking
			if p.check(TOKEN_DOUBLESTAR) {
				p.advance() // consume **
				expr := p.parseExpression()
				// Mark as kwargs unpacking by adding **unpack marker
				args = append(args,
					ast.NewIdentifier("**unpack", p.makeLocation(tok), ast.SyntaxPython),
					expr,
				)
			} else if p.check(TOKEN_STAR) {
				// Check for *args unpacking
				p.advance() // consume *
				expr := p.parseExpression()
				// Mark as args unpacking by adding *unpack marker
				args = append(args,
					ast.NewIdentifier("*unpack", p.makeLocation(tok), ast.SyntaxPython),
					expr,
				)
			} else if p.check(TOKEN_IDENTIFIER) && p.current+1 < len(p.tokens) && p.tokens[p.current+1].Type == TOKEN_ASSIGN {
				// Keyword argument: IDENTIFIER = expression
				seenKeyword = true
				nameTok := p.advance()
				p.expect(TOKEN_ASSIGN)
				value := p.parseExpression()

				// Store as a keyword-value pair
				kwPair := ast.NewSExpr([]ast.ASTNode{
					ast.NewLiteral(core.StringValue(nameTok.Lexeme), p.makeLocation(nameTok), ast.SyntaxPython),
					value,
				}, p.makeLocation(nameTok), ast.SyntaxPython)
				kwargs = append(kwargs, kwPair)
			} else {
				if seenKeyword {
					p.error("Positional argument after keyword argument")
					return nil
				}
				expr := p.parseExpression()

				// Check if this is a generator expression: expr for var in iterable
				if p.check(TOKEN_FOR) {
					// Parse as generator expression (don't consume closing paren)
					genExpr := p.parseGeneratorExpressionNoParen(expr, tok)
					args = append(args, genExpr)
				} else {
					args = append(args, expr)
				}
			}

			if !p.check(TOKEN_COMMA) {
				break
			}
			p.advance()

			// Allow trailing comma before closing paren
			if p.check(TOKEN_RPAREN) {
				break
			}
		}
	}

	p.expect(TOKEN_RPAREN)

	// If we have keyword arguments, append them as a special **kwargs node
	if len(kwargs) > 0 {
		// Create a dict-literal for the keyword arguments
		dictSym := ast.NewIdentifier("dict-literal", p.makeLocation(tok), ast.SyntaxPython)
		dictArgs := append([]ast.ASTNode{dictSym}, kwargs...)
		kwDict := ast.NewSExpr(dictArgs, p.makeLocation(tok), ast.SyntaxPython)

		// Append **kwargs marker and the dict
		args = append(args,
			ast.NewIdentifier("**kwargs", p.makeLocation(tok), ast.SyntaxPython),
			kwDict,
		)
	}

	// Special handling: if callee is an attribute access (. obj "name"),
	// convert it to a method call form: (. obj "name" args...)
	// This handles: {}.keys() -> (. {} "keys" __call__) instead of ((. {} "keys"))
	if sexpr, ok := callee.(*ast.SExpr); ok {
		if len(sexpr.Elements) >= 3 {
			if ident, ok := sexpr.Elements[0].(*ast.Identifier); ok && ident.Name == "." {
				// It's an attribute access: (. obj "name")
				// Check if this is a simple method call (no unpacking markers)
				hasUnpacking := false
				for _, arg := range args[1:] {
					if id, ok := arg.(*ast.Identifier); ok {
						if id.Name == "**unpack" || id.Name == "*unpack" || id.Name == "**kwargs" {
							hasUnpacking = true
							break
						}
					}
				}

				// Only use dot notation for simple cases (no unpacking)
				if !hasUnpacking {
					// No args (empty call): convert to (. obj "name" __call__)
					if len(args) == 1 && len(kwargs) == 0 {
						return ast.NewSExpr(append(sexpr.Elements,
							ast.NewIdentifier("__call__", p.makeLocation(tok), ast.SyntaxPython)),
							p.makeLocation(tok), ast.SyntaxPython)
					}
					// Has simple args: convert to (. obj "name" arg1 arg2 ...)
					if len(args) > 1 {
						result := append(sexpr.Elements, args[1:]...)
						return ast.NewSExpr(result, p.makeLocation(tok), ast.SyntaxPython)
					}
				}
			}
		}
	}

	return ast.NewSExpr(args, p.makeLocation(tok), ast.SyntaxPython)
}

// parseSubscript parses: [index] or [start:stop:step]
func (p *PythonParser) parseSubscript(obj ast.ASTNode) ast.ASTNode {
	tok := p.expect(TOKEN_LBRACKET)

	// Check for slice syntax by looking ahead for colon
	// We need to handle: [:]  [:stop]  [start:]  [start:stop]  [start:stop:step]
	var start, stop, step ast.ASTNode

	// Parse start (optional)
	if !p.check(TOKEN_COLON) && !p.check(TOKEN_RBRACKET) {
		start = p.parseExpression()

		// Check for implicit tuple syntax: dict[a, b, c] means dict[(a, b, c)]
		if p.check(TOKEN_COMMA) {
			// Collect all comma-separated expressions into a tuple
			elements := []ast.ASTNode{start}
			for p.check(TOKEN_COMMA) {
				p.advance() // consume comma
				if p.check(TOKEN_RBRACKET) {
					break // trailing comma
				}
				elements = append(elements, p.parseExpression())
			}

			// Create implicit tuple: (tuple-literal elem1 elem2 ...)
			tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
			allElements := append([]ast.ASTNode{tupleSym}, elements...)
			start = ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
		}
	}

	// Check if this is a slice
	if p.check(TOKEN_COLON) {
		// It's a slice
		p.advance() // consume first colon

		// Parse stop (optional)
		if !p.check(TOKEN_COLON) && !p.check(TOKEN_RBRACKET) {
			stop = p.parseExpression()
		}

		// Parse step (optional)
		if p.check(TOKEN_COLON) {
			p.advance() // consume second colon
			if !p.check(TOKEN_RBRACKET) {
				step = p.parseExpression()
			}
		}

		p.expect(TOKEN_RBRACKET)

		// Create slice object: (slice start stop step)
		// Use None (as nil literal) for missing components
		if start == nil {
			start = ast.NewLiteral(core.NilValue{}, p.makeLocation(tok), ast.SyntaxPython)
		}
		if stop == nil {
			stop = ast.NewLiteral(core.NilValue{}, p.makeLocation(tok), ast.SyntaxPython)
		}
		if step == nil {
			step = ast.NewLiteral(core.NilValue{}, p.makeLocation(tok), ast.SyntaxPython)
		}

		// Create (get-item obj (slice start stop step))
		sliceObj := ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("slice", p.makeLocation(tok), ast.SyntaxPython),
			start,
			stop,
			step,
		}, p.makeLocation(tok), ast.SyntaxPython)

		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("get-item", p.makeLocation(tok), ast.SyntaxPython),
			obj,
			sliceObj,
		}, p.makeLocation(tok), ast.SyntaxPython)
	}

	// Not a slice, just regular indexing
	p.expect(TOKEN_RBRACKET)

	if start == nil {
		// Empty brackets []
		p.error("invalid syntax: empty brackets")
		return nil
	}

	return ast.NewSExpr([]ast.ASTNode{
		ast.NewIdentifier("get-item", p.makeLocation(tok), ast.SyntaxPython),
		obj,
		start,
	}, p.makeLocation(tok), ast.SyntaxPython)
}

// parseAttribute parses: .name
func (p *PythonParser) parseAttribute(obj ast.ASTNode) ast.ASTNode {
	tok := p.expect(TOKEN_DOT)
	name := p.expect(TOKEN_IDENTIFIER)

	return ast.NewSExpr([]ast.ASTNode{
		ast.NewIdentifier(".", p.makeLocation(tok), ast.SyntaxPython),
		obj,
		ast.NewLiteral(core.StringValue(name.Lexeme), p.makeLocation(name), ast.SyntaxPython),
	}, p.makeLocation(tok), ast.SyntaxPython)
}

// ============================================================================
// Primary Expression Parsing
// ============================================================================

// parsePrimary parses atoms: identifiers, literals, parens, lists, dicts, sets
func (p *PythonParser) parsePrimary() ast.ASTNode {
	tok := p.peek()

	switch tok.Type {
	case TOKEN_IDENTIFIER:
		p.advance()
		return ast.NewIdentifier(tok.Lexeme, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_NUMBER:
		p.advance()
		return ast.NewLiteral(tok.Value, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_STRING:
		p.advance()
		// Handle implicit string concatenation (adjacent string literals)
		// Python: "hello" "world" -> "helloworld"
		strValue, ok := tok.Value.(core.StringValue)
		if !ok {
			return ast.NewLiteral(tok.Value, p.makeLocation(tok), ast.SyntaxPython)
		}

		// Check if there are any adjacent strings (regular or f-strings)
		if !p.check(TOKEN_STRING) && !p.check(TOKEN_FSTRING) {
			// No concatenation needed
			return ast.NewLiteral(strValue, p.makeLocation(tok), ast.SyntaxPython)
		}

		// Collect all adjacent string literals (both STRING and FSTRING)
		parts := []ast.ASTNode{ast.NewLiteral(strValue, p.makeLocation(tok), ast.SyntaxPython)}

		for p.check(TOKEN_STRING) || p.check(TOKEN_FSTRING) {
			nextTok := p.advance()
			var nextNode ast.ASTNode

			if nextTok.Type == TOKEN_STRING {
				// Regular string
				nextNode = ast.NewLiteral(nextTok.Value, p.makeLocation(nextTok), ast.SyntaxPython)
			} else {
				// F-string
				nextFstring, err := p.parseFStringFromLexeme(nextTok.Lexeme)
				if err != nil {
					p.error(fmt.Sprintf("Error parsing f-string: %v", err))
					return nil
				}
				nextNode = p.convertValueToASTNode(nextFstring, nextTok)
			}
			parts = append(parts, nextNode)
		}

		// If all parts are simple strings, concatenate at compile time
		allSimple := true
		for _, part := range parts {
			if _, ok := part.(*ast.Literal); !ok {
				allSimple = false
				break
			}
		}

		if allSimple {
			// Concatenate all string literals
			concatenated := ""
			for _, part := range parts {
				lit := part.(*ast.Literal)
				if str, ok := lit.Value.(core.StringValue); ok {
					concatenated += string(str)
				}
			}
			return ast.NewLiteral(core.StringValue(concatenated), p.makeLocation(tok), ast.SyntaxPython)
		}

		// Mix of strings and f-strings - use runtime concatenation with +
		concatArgs := append([]ast.ASTNode{
			ast.NewIdentifier("+", p.makeLocation(tok), ast.SyntaxPython),
		}, parts...)
		return ast.NewSExpr(concatArgs, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_FSTRING:
		p.advance()
		// Parse f-string using M28's f-string parser
		fstringValue, err := p.parseFStringFromLexeme(tok.Lexeme)
		if err != nil {
			p.error(fmt.Sprintf("Error parsing f-string: %v", err))
			return nil
		}

		// Check for adjacent string/f-string concatenation
		firstNode := p.convertValueToASTNode(fstringValue, tok)

		// Collect any adjacent strings or f-strings
		parts := []ast.ASTNode{firstNode}
		for p.check(TOKEN_STRING) || p.check(TOKEN_FSTRING) {
			nextTok := p.advance()
			var nextNode ast.ASTNode

			if nextTok.Type == TOKEN_STRING {
				// Regular string
				nextNode = ast.NewLiteral(nextTok.Value, p.makeLocation(nextTok), ast.SyntaxPython)
			} else {
				// F-string
				nextFstring, err := p.parseFStringFromLexeme(nextTok.Lexeme)
				if err != nil {
					p.error(fmt.Sprintf("Error parsing f-string: %v", err))
					return nil
				}
				nextNode = p.convertValueToASTNode(nextFstring, nextTok)
			}
			parts = append(parts, nextNode)
		}

		// If we have multiple parts, concatenate them with +
		if len(parts) == 1 {
			return parts[0]
		}

		// Build concatenation: (+ part1 part2 part3 ...)
		concatArgs := append([]ast.ASTNode{
			ast.NewIdentifier("+", p.makeLocation(tok), ast.SyntaxPython),
		}, parts...)
		return ast.NewSExpr(concatArgs, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_TRUE:
		p.advance()
		return ast.NewLiteral(core.BoolValue(true), p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_FALSE:
		p.advance()
		return ast.NewLiteral(core.BoolValue(false), p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_NIL:
		p.advance()
		return ast.NewLiteral(core.None, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_ELLIPSIS:
		p.advance()
		return ast.NewLiteral(core.EllipsisValue{}, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_LPAREN:
		// Parenthesized expression or generator expression
		return p.parseParenthesized()

	case TOKEN_LBRACKET:
		// List literal or list comprehension
		return p.parseListLiteral()

	case TOKEN_LBRACE:
		// Dict literal, set literal, or comprehension
		return p.parseDictOrSetLiteral()

	default:
		p.error(fmt.Sprintf("Unexpected token in expression: %v", tok.Type))
		return nil
	}
}

// parseParenthesized parses: (expression) or tuple literal
func (p *PythonParser) parseParenthesized() ast.ASTNode {
	tok := p.expect(TOKEN_LPAREN)

	// Empty parens creates empty tuple
	if p.check(TOKEN_RPAREN) {
		p.advance()
		return ast.NewLiteral(core.TupleValue{}, p.makeLocation(tok), ast.SyntaxPython)
	}

	// Parse first element (could be star expression in tuple)
	first := p.parseListElement()

	// Check if it's a generator expression (star expressions not allowed)
	if p.check(TOKEN_FOR) {
		// Star expressions not allowed in generator expressions
		if sExpr, ok := first.(*ast.SExpr); ok {
			if len(sExpr.Elements) > 0 {
				if id, ok := sExpr.Elements[0].(*ast.Identifier); ok && id.Name == "*unpack-iter" {
					p.error("Iterable unpacking (*) cannot be used in generator expression")
					return nil
				}
			}
		}
		return p.parseGeneratorExpression(first, tok)
	}

	// Check for comma (tuple literal)
	if p.check(TOKEN_COMMA) {
		p.advance()

		elements := []ast.ASTNode{first}

		// Parse remaining elements (if any)
		for !p.check(TOKEN_RPAREN) && !p.isAtEnd() {
			elements = append(elements, p.parseListElement())

			if !p.check(TOKEN_COMMA) {
				break
			}
			p.advance()
		}

		// This is a tuple - create (tuple-literal elem1 elem2 ...)
		tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
		allElements := append([]ast.ASTNode{tupleSym}, elements...)
		p.expect(TOKEN_RPAREN)
		return ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
	}

	// No comma - just a parenthesized expression (or single-element with star, which is an error)
	p.expect(TOKEN_RPAREN)

	// Check if it's a star expression without comma - that's an error
	if sExpr, ok := first.(*ast.SExpr); ok {
		if len(sExpr.Elements) > 0 {
			if id, ok := sExpr.Elements[0].(*ast.Identifier); ok && id.Name == "*unpack-iter" {
				p.error("Can't use starred expression here")
				return nil
			}
		}
	}

	return first
}

// parseListElement parses a list/tuple element, which can include star expressions
// Returns the element AST node
func (p *PythonParser) parseListElement() ast.ASTNode {
	// Check for star expression: *iterable
	if p.check(TOKEN_STAR) {
		starTok := p.advance()
		expr := p.parseExpression()

		// Create (*unpack-iter expr) to indicate unpacking in literal context
		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("*unpack-iter", p.makeLocation(starTok), ast.SyntaxPython),
			expr,
		}, p.makeLocation(starTok), ast.SyntaxPython)
	}

	return p.parseExpression()
}

// parseListLiteral parses: [elements] or [expr for var in iter]
func (p *PythonParser) parseListLiteral() ast.ASTNode {
	tok := p.expect(TOKEN_LBRACKET)

	if p.check(TOKEN_RBRACKET) {
		// Empty list
		p.advance()
		return ast.NewLiteral(core.ListValue{}, p.makeLocation(tok), ast.SyntaxPython)
	}

	// Parse first element
	first := p.parseListElement()

	// Check if it's a comprehension (only if first element is not a star expression)
	if p.check(TOKEN_FOR) {
		// Star expressions not allowed in comprehensions
		if sExpr, ok := first.(*ast.SExpr); ok {
			if len(sExpr.Elements) > 0 {
				if id, ok := sExpr.Elements[0].(*ast.Identifier); ok && id.Name == "*unpack-iter" {
					p.error("Iterable unpacking (*) cannot be used in comprehension")
					return nil
				}
			}
		}
		return p.parseListComprehension(first, tok)
	}

	// Regular list
	elements := []ast.ASTNode{first}

	for p.check(TOKEN_COMMA) {
		p.advance()
		if p.check(TOKEN_RBRACKET) {
			break // Trailing comma
		}
		elements = append(elements, p.parseListElement())
	}

	p.expect(TOKEN_RBRACKET)

	// Create (list-literal elem1 elem2 ...) form
	// This is a special form that evaluates each element and returns a list
	listLiteralSym := ast.NewIdentifier("list-literal", p.makeLocation(tok), ast.SyntaxPython)
	allElements := append([]ast.ASTNode{listLiteralSym}, elements...)

	return ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
}

// parseDictOrSetLiteral parses: {k:v, ...} or {elem, ...}
func (p *PythonParser) parseDictOrSetLiteral() ast.ASTNode {
	tok := p.expect(TOKEN_LBRACE)

	if p.check(TOKEN_RBRACE) {
		// Empty dict (not set, since {} is dict in Python)
		p.advance()
		return ast.NewLiteral(core.NewDict(), p.makeLocation(tok), ast.SyntaxPython)
	}

	// Parse first element
	first := p.parseExpression()

	// Check if it's a dict (has colon) or set (no colon)
	if p.check(TOKEN_COLON) {
		return p.parseDictLiteral(first, tok)
	} else {
		return p.parseSetLiteral(first, tok)
	}
}

// parseDictLiteral parses the rest of: {k: v, ...}
func (p *PythonParser) parseDictLiteral(firstKey ast.ASTNode, tok Token) ast.ASTNode {
	p.expect(TOKEN_COLON)
	firstValue := p.parseExpression()

	// Check if it's a dict comprehension
	if p.check(TOKEN_FOR) {
		return p.parseDictComprehension(firstKey, firstValue, tok)
	}

	// Regular dict - collect key-value pairs as AST nodes
	keyValuePairs := []ast.ASTNode{firstKey, firstValue}

	for p.check(TOKEN_COMMA) {
		p.advance()
		if p.check(TOKEN_RBRACE) {
			break // Trailing comma
		}

		key := p.parseExpression()
		p.expect(TOKEN_COLON)
		value := p.parseExpression()
		keyValuePairs = append(keyValuePairs, key, value)
	}

	p.expect(TOKEN_RBRACE)

	// Create (dict-literal key1 value1 key2 value2 ...) form
	dictLiteralSym := ast.NewIdentifier("dict-literal", p.makeLocation(tok), ast.SyntaxPython)
	allElements := append([]ast.ASTNode{dictLiteralSym}, keyValuePairs...)

	return ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
}

// parseSetLiteral parses the rest of: {elem, ...}
func (p *PythonParser) parseSetLiteral(first ast.ASTNode, tok Token) ast.ASTNode {
	// Check if it's a set comprehension
	if p.check(TOKEN_FOR) {
		return p.parseSetComprehension(first, tok)
	}

	// Regular set - collect elements as AST nodes
	elements := []ast.ASTNode{first}

	for p.check(TOKEN_COMMA) {
		p.advance()
		if p.check(TOKEN_RBRACE) {
			break // Trailing comma
		}
		elements = append(elements, p.parseExpression())
	}

	p.expect(TOKEN_RBRACE)

	// Create (set (list-literal elem1 elem2 ...)) form
	listLiteralSym := ast.NewIdentifier("list-literal", p.makeLocation(tok), ast.SyntaxPython)
	listElements := append([]ast.ASTNode{listLiteralSym}, elements...)
	listLiteral := ast.NewSExpr(listElements, p.makeLocation(tok), ast.SyntaxPython)

	setSym := ast.NewIdentifier("set", p.makeLocation(tok), ast.SyntaxPython)
	return ast.NewSExpr([]ast.ASTNode{setSym, listLiteral}, p.makeLocation(tok), ast.SyntaxPython)
}

// ============================================================================
// Comprehension Parsing (stub - to be implemented)
// ============================================================================

// parseLoopVariables parses the loop variable(s) in a comprehension.
// Handles:
//   - Single variable: for x in ...
//   - Parenthesized tuple: for (x, y) in ...
//   - Unparenthesized tuple: for x, y in ...
//   - Nested tuples: for key,(begin,end) in ...
//
// Returns the variable string in the format the evaluator expects.
func (p *PythonParser) parseLoopVariables() string {
	// Parse a single loop variable element (can be identifier or nested tuple)
	parseVarElement := func() string {
		if p.check(TOKEN_LPAREN) {
			// Parenthesized tuple: (x, y) or (x, (y, z))
			p.advance() // consume (

			parts := []string{}
			parts = append(parts, p.parseLoopVariableElement())

			for p.check(TOKEN_COMMA) {
				p.advance()
				if p.check(TOKEN_RPAREN) {
					break // Trailing comma
				}
				parts = append(parts, p.parseLoopVariableElement())
			}

			p.expect(TOKEN_RPAREN)

			// Format as "(x, y, z)"
			result := "(" + parts[0]
			for i := 1; i < len(parts); i++ {
				result += ", " + parts[i]
			}
			result += ")"
			return result
		}

		// Simple identifier
		varTok := p.expect(TOKEN_IDENTIFIER)
		return varTok.Lexeme
	}

	// Parse first element
	first := parseVarElement()

	// Check if this is a comma-separated list: for x, y in ... or for key,(begin,end) in ...
	if p.check(TOKEN_COMMA) && !p.checkAhead(TOKEN_IN, 1) {
		parts := []string{first}

		for p.check(TOKEN_COMMA) && !p.checkAhead(TOKEN_IN, 1) {
			p.advance()
			parts = append(parts, parseVarElement())
		}

		// Format as "(x, y, z)" or "(key, (begin, end))"
		result := "(" + parts[0]
		for i := 1; i < len(parts); i++ {
			result += ", " + parts[i]
		}
		result += ")"
		return result
	}

	// Single variable or already parenthesized
	return first
}

// parseLoopVariableElement parses a single element of loop variables (can be nested)
func (p *PythonParser) parseLoopVariableElement() string {
	if p.check(TOKEN_LPAREN) {
		// Nested tuple: (x, y)
		p.advance() // consume (

		parts := []string{}
		parts = append(parts, p.parseLoopVariableElement())

		for p.check(TOKEN_COMMA) {
			p.advance()
			if p.check(TOKEN_RPAREN) {
				break // Trailing comma
			}
			parts = append(parts, p.parseLoopVariableElement())
		}

		p.expect(TOKEN_RPAREN)

		// Format as "(x, y)"
		result := "(" + parts[0]
		for i := 1; i < len(parts); i++ {
			result += ", " + parts[i]
		}
		result += ")"
		return result
	}

	// Simple identifier
	varTok := p.expect(TOKEN_IDENTIFIER)
	return varTok.Lexeme
}

func (p *PythonParser) parseListComprehension(element ast.ASTNode, tok Token) ast.ASTNode {
	// [expr for var in iter if condition]
	// [expr for var1 in iter1 for var2 in iter2 if condition]

	// Parse all for clauses
	clauses := []ast.ComprehensionClause{}

	for p.check(TOKEN_FOR) {
		p.advance() // consume FOR

		variable := p.parseLoopVariables()

		p.expect(TOKEN_IN)
		// Use parseOr instead of parseExpression to avoid parsing 'if' as ternary operator
		iterable := p.parseOr()

		var condition ast.ASTNode
		if p.check(TOKEN_IF) {
			p.advance()
			// Condition can use full expression parsing (including ternary if needed)
			condition = p.parseOr()

			// Debug: print the condition AST
			if debugComp := os.Getenv("DEBUG_COMP_PARSE"); debugComp != "" {
				fmt.Fprintf(os.Stderr, "[DEBUG_COMP_PARSE] Parsed condition AST: %#v\n", condition)
				fmt.Fprintf(os.Stderr, "[DEBUG_COMP_PARSE] Condition string: %s\n", condition.String())
			}
		}

		clauses = append(clauses, ast.ComprehensionClause{
			Variable:  variable,
			Iterable:  iterable,
			Condition: condition,
		})
	}

	p.expect(TOKEN_RBRACKET)

	// Use the multi-clause constructor
	return ast.NewComprehensionFormMulti(
		ast.ListComp,
		element, // Element expression
		nil,     // KeyExpr (not used for list comp)
		nil,     // ValueExpr (not used for list comp)
		clauses,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

func (p *PythonParser) parseDictComprehension(key, value ast.ASTNode, tok Token) ast.ASTNode {
	// {k: v for var in iter if condition}
	// {k: v for var1 in iter1 for var2 in iter2 if condition}

	// Parse all for clauses
	clauses := []ast.ComprehensionClause{}

	for p.check(TOKEN_FOR) {
		p.advance() // consume FOR

		variable := p.parseLoopVariables()

		p.expect(TOKEN_IN)
		// Use parseOr instead of parseExpression to avoid parsing 'if' as ternary operator
		iterable := p.parseOr()

		var condition ast.ASTNode
		if p.check(TOKEN_IF) {
			p.advance()
			condition = p.parseOr()
		}

		clauses = append(clauses, ast.ComprehensionClause{
			Variable:  variable,
			Iterable:  iterable,
			Condition: condition,
		})
	}

	p.expect(TOKEN_RBRACE)

	return ast.NewComprehensionFormMulti(
		ast.DictComp,
		nil,   // Element (not used for dict comp)
		key,   // KeyExpr
		value, // ValueExpr
		clauses,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

func (p *PythonParser) parseSetComprehension(element ast.ASTNode, tok Token) ast.ASTNode {
	// {expr for var in iter if condition}
	// {expr for var1 in iter1 for var2 in iter2 if condition}

	// Parse all for clauses
	clauses := []ast.ComprehensionClause{}

	for p.check(TOKEN_FOR) {
		p.advance() // consume FOR

		variable := p.parseLoopVariables()

		p.expect(TOKEN_IN)
		// Use parseOr instead of parseExpression to avoid parsing 'if' as ternary operator
		iterable := p.parseOr()

		var condition ast.ASTNode
		if p.check(TOKEN_IF) {
			p.advance()
			condition = p.parseOr()
		}

		clauses = append(clauses, ast.ComprehensionClause{
			Variable:  variable,
			Iterable:  iterable,
			Condition: condition,
		})
	}

	p.expect(TOKEN_RBRACE)

	return ast.NewComprehensionFormMulti(
		ast.SetComp,
		element, // Element expression
		nil,     // KeyExpr (not used for set comp)
		nil,     // ValueExpr (not used for set comp)
		clauses,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

func (p *PythonParser) parseGeneratorExpression(element ast.ASTNode, tok Token) ast.ASTNode {
	// (expr for var in iter if condition)
	// (expr for var1 in iter1 for var2 in iter2)

	// Parse all for clauses
	clauses := []ast.ComprehensionClause{}

	for p.check(TOKEN_FOR) {
		p.advance() // consume FOR

		variable := p.parseLoopVariables()

		p.expect(TOKEN_IN)
		// Use parseOr instead of parseExpression to avoid parsing 'if' as ternary operator
		iterable := p.parseOr()

		var condition ast.ASTNode
		if p.check(TOKEN_IF) {
			p.advance()
			condition = p.parseOr()
		}

		clauses = append(clauses, ast.ComprehensionClause{
			Variable:  variable,
			Iterable:  iterable,
			Condition: condition,
		})
	}

	p.expect(TOKEN_RPAREN)

	return ast.NewComprehensionFormMulti(
		ast.GeneratorComp,
		element, // Element expression
		nil,     // KeyExpr (not used for generator)
		nil,     // ValueExpr (not used for generator)
		clauses,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

// parseGeneratorExpressionNoParen parses a generator expression without consuming closing paren
// Used when generator is inside function call: func(expr for var in iter)
func (p *PythonParser) parseGeneratorExpressionNoParen(element ast.ASTNode, tok Token) ast.ASTNode {
	// expr for var in iter if condition (no closing paren)
	// or: expr for (var1, var2) in iter
	// or: expr for var1, var2 in iter
	// or: expr for var1 in iter1 for var2 in iter2 (multiple for clauses)

	// Parse all for clauses
	clauses := []ast.ComprehensionClause{}

	for p.check(TOKEN_FOR) {
		p.advance() // consume FOR

		variable := p.parseLoopVariables()

		p.expect(TOKEN_IN)
		// Use parseOr instead of parseExpression to avoid parsing 'if' as ternary operator
		iterable := p.parseOr()

		var condition ast.ASTNode
		if p.check(TOKEN_IF) {
			p.advance()
			condition = p.parseOr()
		}

		clauses = append(clauses, ast.ComprehensionClause{
			Variable:  variable,
			Iterable:  iterable,
			Condition: condition,
		})
	}

	// Don't consume closing paren - let the caller handle it

	return ast.NewComprehensionFormMulti(
		ast.GeneratorComp,
		element, // Element expression
		nil,     // KeyExpr (not used for generator)
		nil,     // ValueExpr (not used for generator)
		clauses,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

// ============================================================================
// Block and Decorator Parsing
// ============================================================================

// parseBlock parses a colon-delimited block: COLON NEWLINE INDENT statements DEDENT
func (p *PythonParser) parseBlock() []ast.ASTNode {
	p.expect(TOKEN_COLON)
	p.expect(TOKEN_NEWLINE)
	p.expect(TOKEN_INDENT)

	statements := []ast.ASTNode{}

	for !p.check(TOKEN_DEDENT) && !p.isAtEnd() {
		// Skip empty lines
		for p.check(TOKEN_NEWLINE) {
			p.advance()
		}

		if p.check(TOKEN_DEDENT) {
			break
		}

		stmt := p.parseStatement()
		if stmt != nil {
			statements = append(statements, stmt)
		} else if len(p.errors) > 0 {
			// parseStatement returned nil and we have errors - stop parsing
			break
		}

		// If we hit an error, synchronize
		if p.panic {
			p.synchronize()
		}
	}

	p.expect(TOKEN_DEDENT)
	return statements
}

// parseDecorators parses: (@decorator_expr NEWLINE)*
func (p *PythonParser) parseDecorators() []ast.ASTNode {
	decorators := []ast.ASTNode{}

	for p.check(TOKEN_AT) {
		p.advance() // consume @

		// Parse decorator expression (identifier or call)
		decorator := p.parsePrimary()

		// Handle attribute access (e.g., @contextlib.contextmanager)
		for p.check(TOKEN_DOT) {
			decorator = p.parseAttribute(decorator)
		}

		// If followed by (, it's a decorator call
		if p.check(TOKEN_LPAREN) {
			decorator = p.parseCall(decorator)
		}

		decorators = append(decorators, decorator)
		p.expect(TOKEN_NEWLINE)
	}

	return decorators
}

// ============================================================================
// Compound Statement Parsing
// ============================================================================

// parseIfStatement parses: if expr: block (elif expr: block)* (else: block)?
func (p *PythonParser) parseIfStatement() ast.ASTNode {
	// Accept both if and elif (elif is used in recursive calls)
	tok := p.peek()
	if p.check(TOKEN_IF) {
		p.advance()
	} else if p.check(TOKEN_ELIF) {
		p.advance()
	} else {
		p.error(fmt.Sprintf("expected 'if' or 'elif', got %v", p.peek().Type))
		return nil
	}

	condition := p.parseExpression()
	thenBranch := p.parseBlock()

	var elseBranch []ast.ASTNode

	// Check for elif (treated as nested if)
	if p.check(TOKEN_ELIF) {
		elseIf := p.parseIfStatement() // Recursive - will handle elif
		elseBranch = []ast.ASTNode{elseIf}
	} else if p.check(TOKEN_ELSE) {
		p.advance()
		elseBranch = p.parseBlock()
	}

	// Convert blocks to single nodes
	var thenNode ast.ASTNode
	if len(thenBranch) == 1 {
		thenNode = thenBranch[0]
	} else {
		thenNode = ast.NewBlockForm(thenBranch, p.makeLocation(tok), ast.SyntaxPython)
	}

	var elseNode ast.ASTNode
	if len(elseBranch) == 1 {
		elseNode = elseBranch[0]
	} else if len(elseBranch) > 1 {
		elseNode = ast.NewBlockForm(elseBranch, p.makeLocation(tok), ast.SyntaxPython)
	}

	return ast.NewIfForm(condition, thenNode, elseNode, p.makeLocation(tok), ast.SyntaxPython)
}

// splitLoopVariables splits a loop variable string into individual variables
// Handles:
//   - "(x, y, z)" -> ["x", "y", "z"]
//   - "x" -> ["x"]
//   - "(x, (y, z))" -> ["x", "(y, z)"] // nested tuples preserved
func splitLoopVariables(varStr string) []string {
	varStr = strings.TrimSpace(varStr)

	// If no parentheses, return as-is
	if !strings.HasPrefix(varStr, "(") {
		return []string{varStr}
	}

	// Remove outer parentheses: "(x, y)" -> "x, y"
	if strings.HasPrefix(varStr, "(") && strings.HasSuffix(varStr, ")") {
		varStr = varStr[1 : len(varStr)-1]
	}

	// Split by comma, respecting nested parentheses
	variables := []string{}
	current := strings.Builder{}
	depth := 0

	for _, ch := range varStr {
		switch ch {
		case '(':
			depth++
			current.WriteRune(ch)
		case ')':
			depth--
			current.WriteRune(ch)
		case ',':
			if depth == 0 {
				// Top-level comma - split here
				variables = append(variables, strings.TrimSpace(current.String()))
				current.Reset()
			} else {
				current.WriteRune(ch)
			}
		default:
			current.WriteRune(ch)
		}
	}

	// Add last variable
	if current.Len() > 0 {
		variables = append(variables, strings.TrimSpace(current.String()))
	}

	return variables
}

// parseForStatement parses: for var[, var2, ...] in iterable: block (else: block)?
func (p *PythonParser) parseForStatement() ast.ASTNode {
	tok := p.expect(TOKEN_FOR)

	// Parse loop variables using parseLoopVariables to support:
	// - Single variable: for x in ...
	// - Tuple unpacking: for x, y in ...
	// - Nested tuples: for key,(begin,end) in ...
	variable := p.parseLoopVariables()

	p.expect(TOKEN_IN)

	// Parse iterable expression
	// Special handling for implicit tuple syntax: for x in 1, 2, 3:
	iterable := p.parseForIterable()

	// Parse body
	body := p.parseBlock()

	// Check for optional else clause
	var elseBody []ast.ASTNode
	if p.check(TOKEN_ELSE) {
		p.advance()
		elseBody = p.parseBlock()
	}

	// Split variable string into individual variables
	// e.g. "(index, name)" -> ["index", "name"]
	//      "x" -> ["x"]
	variables := splitLoopVariables(variable)

	if len(variables) == 1 {
		return ast.NewForForm(variables[0], iterable, body, elseBody, p.makeLocation(tok), ast.SyntaxPython)
	} else {
		return ast.NewForFormMulti(variables, iterable, body, elseBody, p.makeLocation(tok), ast.SyntaxPython)
	}
}

// parseForIterable parses the iterable expression in a for loop
// Handles implicit tuple syntax: for x in 1, 2, 3: means for x in (1, 2, 3):
// Parses comma-separated expressions until reaching ':'
func (p *PythonParser) parseForIterable() ast.ASTNode {
	startTok := p.peek()

	// Parse first expression
	exprs := []ast.ASTNode{p.parseExpression()}

	// Check for comma-separated values (implicit tuple)
	// Continue collecting expressions until we hit a colon
	for p.check(TOKEN_COMMA) {
		p.advance() // consume comma

		// If we hit a colon after the comma, we have a trailing comma - stop here
		if p.check(TOKEN_COLON) {
			break
		}

		exprs = append(exprs, p.parseExpression())
	}

	// If we got multiple expressions, create an implicit tuple
	if len(exprs) == 1 {
		return exprs[0]
	}

	// Create tuple-literal form (similar to parseExpressionStatement)
	tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(startTok), ast.SyntaxPython)
	allElements := append([]ast.ASTNode{tupleSym}, exprs...)
	return ast.NewSExpr(allElements, p.makeLocation(startTok), ast.SyntaxPython)
}

// parseWhileStatement parses: while expr: block (else: block)?
func (p *PythonParser) parseWhileStatement() ast.ASTNode {
	tok := p.expect(TOKEN_WHILE)

	// Parse condition
	condition := p.parseExpression()

	// Parse body
	body := p.parseBlock()

	// Check for optional else clause
	var elseBody []ast.ASTNode
	if p.check(TOKEN_ELSE) {
		p.advance()
		elseBody = p.parseBlock()
	}

	return ast.NewWhileForm(condition, body, elseBody, p.makeLocation(tok), ast.SyntaxPython)
}

// parseAsyncStatement parses: async (def|for|with)
func (p *PythonParser) parseAsyncStatement() ast.ASTNode {
	p.expect(TOKEN_ASYNC)

	switch p.peek().Type {
	case TOKEN_DEF:
		return p.parseDefStatement(nil, true)
	case TOKEN_FOR:
		// async for - for now, treat like regular for
		return p.parseForStatement()
	case TOKEN_WITH:
		// async with - for now, treat like regular with
		return p.parseWithStatement()
	default:
		p.error(fmt.Sprintf("Expected 'def', 'for', or 'with' after 'async', got %v", p.peek().Type))
		return nil
	}
}

// parseDefStatement parses: (@decorator)* (async)? def name(params) (-> type)?: block
func (p *PythonParser) parseDefStatement(decorators []ast.ASTNode, isAsync bool) ast.ASTNode {
	tok := p.expect(TOKEN_DEF)

	// Parse function name
	nameTok := p.expect(TOKEN_IDENTIFIER)
	name := nameTok.Lexeme

	// Parse parameters
	p.expect(TOKEN_LPAREN)
	params := p.parseParameters()
	p.expect(TOKEN_RPAREN)

	// Parse optional return type annotation
	var returnType *ast.TypeInfo
	if p.check(TOKEN_ARROW) {
		p.advance()
		returnType = p.parseTypeAnnotation()
	}

	// Parse body - can be either indented block or inline statement
	p.expect(TOKEN_COLON)

	var bodyNode ast.ASTNode
	if p.check(TOKEN_NEWLINE) {
		// Indented block
		p.advance() // consume newline
		p.expect(TOKEN_INDENT)

		statements := []ast.ASTNode{}
		for !p.check(TOKEN_DEDENT) && !p.isAtEnd() {
			// Skip empty lines
			for p.check(TOKEN_NEWLINE) {
				p.advance()
			}

			if p.check(TOKEN_DEDENT) {
				break
			}

			stmt := p.parseStatement()
			if stmt != nil {
				statements = append(statements, stmt)
			} else if len(p.errors) > 0 {
				break
			}

			if p.panic {
				p.synchronize()
			}
		}

		p.expect(TOKEN_DEDENT)

		if len(statements) == 1 {
			bodyNode = statements[0]
		} else {
			bodyNode = ast.NewBlockForm(statements, p.makeLocation(tok), ast.SyntaxPython)
		}
	} else {
		// Inline statement: def f(): pass
		bodyNode = p.parseStatement()
	}

	return ast.NewDefForm(name, params, bodyNode, returnType, decorators, isAsync, p.makeLocation(tok), ast.SyntaxPython)
}

// parseParameters parses: (param (: type)? (= default)?, ...)*
// Also handles *args and **kwargs
func (p *PythonParser) parseParameters() []ast.Parameter {
	params := []ast.Parameter{}

	if p.check(TOKEN_RPAREN) {
		return params // Empty parameter list
	}

	for {
		// Check for / (positional-only parameter separator)
		// In Python 3.8+: def f(a, b, /, c, d): means a, b are positional-only
		// For now, we just skip it and treat all parameters normally
		if p.check(TOKEN_SLASH) {
			p.advance() // consume /

			if !p.check(TOKEN_COMMA) {
				break
			}
			p.advance()

			if p.check(TOKEN_RPAREN) {
				break
			}
			continue
		}

		// Check for *args or keyword-only parameter separator (*)
		if p.check(TOKEN_STAR) {
			p.advance() // consume *

			// Check if this is just a keyword-only separator (bare *)
			// In that case, the next token is comma or rparen, not an identifier
			if p.check(TOKEN_COMMA) || p.check(TOKEN_RPAREN) {
				// Bare * - keyword-only parameter separator
				// All parameters after this must be keyword-only
				// For now, we don't enforce this, just skip the marker
				if p.check(TOKEN_COMMA) {
					p.advance()
				}
				if p.check(TOKEN_RPAREN) {
					break
				}
				continue
			}

			// *args case
			nameTok := p.expect(TOKEN_IDENTIFIER)
			param := ast.Parameter{
				Name:      "*" + nameTok.Lexeme, // Prefix with * to mark as varargs
				IsVarArgs: true,
			}
			params = append(params, param)

			if !p.check(TOKEN_COMMA) {
				break
			}
			p.advance()

			if p.check(TOKEN_RPAREN) {
				break
			}
			continue
		}

		// Check for **kwargs
		if p.check(TOKEN_DOUBLESTAR) {
			p.advance() // consume **
			nameTok := p.expect(TOKEN_IDENTIFIER)
			param := ast.Parameter{
				Name:     "**" + nameTok.Lexeme, // Prefix with ** to mark as kwargs
				IsKwargs: true,
			}
			params = append(params, param)

			if !p.check(TOKEN_COMMA) {
				break
			}
			p.advance()

			if p.check(TOKEN_RPAREN) {
				break
			}
			continue
		}

		// Parse regular parameter name
		nameTok := p.expect(TOKEN_IDENTIFIER)
		param := ast.Parameter{Name: nameTok.Lexeme}

		// Parse optional type annotation
		if p.check(TOKEN_COLON) {
			p.advance()
			param.Type = p.parseTypeAnnotation()
		}

		// Parse optional default value
		if p.check(TOKEN_ASSIGN) {
			p.advance()
			param.Default = p.parseExpression()
		}

		params = append(params, param)

		if !p.check(TOKEN_COMMA) {
			break
		}
		p.advance()

		// Allow trailing comma
		if p.check(TOKEN_RPAREN) {
			break
		}
	}

	return params
}

// parseTypeAnnotation parses a type name (simplified - no generics yet)
func (p *PythonParser) parseTypeAnnotation() *ast.TypeInfo {
	nameTok := p.expect(TOKEN_IDENTIFIER)
	return &ast.TypeInfo{Name: nameTok.Lexeme}
}

// parseClassStatement parses: (@decorator)* class name (bases)?: block
func (p *PythonParser) parseClassStatement(decorators []ast.ASTNode) ast.ASTNode {
	tok := p.expect(TOKEN_CLASS)

	// Parse class name
	nameTok := p.expect(TOKEN_IDENTIFIER)
	name := nameTok.Lexeme

	// Parse optional base classes and keyword arguments (like metaclass=)
	var bases []ast.ASTNode
	var keywords []ast.ASTNode // keyword arguments like metaclass=...
	if p.check(TOKEN_LPAREN) {
		p.advance()

		if !p.check(TOKEN_RPAREN) {
			for {
				// Check for keyword argument: IDENTIFIER = expression
				if p.check(TOKEN_IDENTIFIER) && p.current+1 < len(p.tokens) && p.tokens[p.current+1].Type == TOKEN_ASSIGN {
					nameTok := p.advance()
					p.expect(TOKEN_ASSIGN)
					value := p.parseExpression()

					// Store as a keyword-value pair
					kwPair := ast.NewSExpr([]ast.ASTNode{
						ast.NewLiteral(core.StringValue(nameTok.Lexeme), p.makeLocation(nameTok), ast.SyntaxPython),
						value,
					}, p.makeLocation(nameTok), ast.SyntaxPython)
					keywords = append(keywords, kwPair)
				} else {
					// Regular base class
					bases = append(bases, p.parseExpression())
				}

				if !p.check(TOKEN_COMMA) {
					break
				}
				p.advance()
			}
		}

		p.expect(TOKEN_RPAREN)
	}

	// Parse body
	body := p.parseBlock()

	return ast.NewClassForm(name, bases, body, decorators, keywords, p.makeLocation(tok), ast.SyntaxPython)
}

// parseTryStatement parses: try: block (except (type (as var))?: block)+ (else: block)? (finally: block)?
func (p *PythonParser) parseTryStatement() ast.ASTNode {
	tok := p.expect(TOKEN_TRY)

	// Parse try body
	tryBody := p.parseBlock()

	// Parse except clauses
	var exceptClauses []ast.ExceptClause

	for p.check(TOKEN_EXCEPT) {
		p.advance()

		var exceptType string
		var exceptVar string

		// Check if there's an exception type
		if !p.check(TOKEN_COLON) {
			// Exception type can be an identifier, parenthesized identifier, or tuple
			// For now, just get the string representation
			typeExpr := p.parseExpression()

			// Convert AST node to string for storage
			// This is a simplification - ideally we'd store the full AST node
			if ident, ok := typeExpr.(*ast.Identifier); ok {
				exceptType = ident.Name
			} else {
				// For complex types like (Exception) or (ValueError, TypeError),
				// just convert to string representation
				exceptType = typeExpr.String()
			}

			// Check for 'as variable'
			if p.check(TOKEN_AS) {
				p.advance()
				varTok := p.expect(TOKEN_IDENTIFIER)
				exceptVar = varTok.Lexeme
			}
		}

		// Parse except body
		exceptBody := p.parseBlock()

		exceptClauses = append(exceptClauses, ast.ExceptClause{
			ExceptionType: exceptType,
			Variable:      exceptVar,
			Body:          exceptBody,
		})
	}

	// Parse optional else clause
	var elseBody []ast.ASTNode
	if p.check(TOKEN_ELSE) {
		p.advance()
		elseBody = p.parseBlock()
	}

	// Parse optional finally clause
	var finallyBody []ast.ASTNode
	if p.check(TOKEN_FINALLY) {
		p.advance()
		finallyBody = p.parseBlock()
	}

	return ast.NewTryForm(tryBody, exceptClauses, elseBody, finallyBody, p.makeLocation(tok), ast.SyntaxPython)
}

// parseWithStatement parses: with expr (as var)?(, expr (as var)?)* : block
func (p *PythonParser) parseWithStatement() ast.ASTNode {
	tok := p.expect(TOKEN_WITH)

	// Parse context managers
	var items []ast.WithItem

	for {
		// Parse context expression
		context := p.parseExpression()

		var variable string
		if p.check(TOKEN_AS) {
			p.advance()
			varTok := p.expect(TOKEN_IDENTIFIER)
			variable = varTok.Lexeme
		}

		items = append(items, ast.WithItem{
			Context:  context,
			Variable: variable,
		})

		if !p.check(TOKEN_COMMA) {
			break
		}
		p.advance()
	}

	// Parse body
	body := p.parseBlock()

	return ast.NewWithForm(items, body, p.makeLocation(tok), ast.SyntaxPython)
}

// parseMatchStatement parses: match subject: case pattern: block ...
func (p *PythonParser) parseMatchStatement() ast.ASTNode {
	tok := p.expect(TOKEN_IDENTIFIER) // "match" as soft keyword
	if tok.Lexeme != "match" {
		p.error("Expected 'match'")
		return nil
	}

	// Parse subject expression
	subject := p.parseExpression()

	// Expect colon
	p.expect(TOKEN_COLON)

	// Parse block of case clauses
	var cases []ast.CaseClause

	// Expect NEWLINE and INDENT
	p.expect(TOKEN_NEWLINE)
	p.expect(TOKEN_INDENT)

	// Parse case clauses
	for p.check(TOKEN_IDENTIFIER) && p.peek().Lexeme == "case" {
		p.advance() // consume 'case'

		// Parse pattern
		// For now, simplified pattern parsing:
		// - Identifier: variable or class name
		// - Literal: number, string, etc.
		// - Call pattern: ClassName(args)
		// - Wildcard: _
		pattern := p.parsePattern()

		// Check for optional guard (if condition)
		var guard ast.ASTNode
		if p.check(TOKEN_IF) {
			p.advance()
			guard = p.parseExpression()
		}

		// Expect colon
		p.expect(TOKEN_COLON)

		// Parse case body
		body := p.parseBlock()

		cases = append(cases, ast.CaseClause{
			Pattern: pattern,
			Guard:   guard,
			Body:    body,
		})
	}

	// Expect DEDENT
	p.expect(TOKEN_DEDENT)

	return ast.NewMatchForm(subject, cases, p.makeLocation(tok), ast.SyntaxPython)
}

// parsePattern parses a pattern in a case clause
// Patterns can be:
// - Literal values: 1, "hello", True, None
// - Wildcard: _
// - Class patterns: Point(x, y), ast.Expr(expr)
// - Or patterns: pattern1 | pattern2
// For now, we handle simple patterns and class patterns
func (p *PythonParser) parsePattern() ast.ASTNode {
	// Check for literal patterns
	tok := p.peek()

	switch tok.Type {
	case TOKEN_NUMBER, TOKEN_STRING, TOKEN_TRUE, TOKEN_FALSE, TOKEN_NIL:
		p.advance()
		return ast.NewLiteral(tok.Value, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_IDENTIFIER:
		// Could be:
		// - Wildcard: _
		// - Variable binding
		// - Class pattern: ClassName(...)
		ident := p.advance()

		// Check for class pattern (function call syntax)
		if p.check(TOKEN_LPAREN) {
			// Parse as a call pattern (treat like function call)
			// ClassName(pattern1, pattern2, ...)
			return p.parseCall(ast.NewIdentifier(ident.Lexeme, p.makeLocation(ident), ast.SyntaxPython))
		}

		// Simple identifier or wildcard
		return ast.NewIdentifier(ident.Lexeme, p.makeLocation(ident), ast.SyntaxPython)

	case TOKEN_LPAREN:
		// Tuple pattern or grouped pattern
		return p.parseParenthesized()

	case TOKEN_LBRACKET:
		// List pattern
		return p.parseListLiteral()

	default:
		p.error(fmt.Sprintf("Unexpected token in pattern: %v", tok.Type))
		return nil
	}
}

// ============================================================================
// F-String Support
// ============================================================================

// parseFStringFromLexeme parses an f-string from its full lexeme (e.g., f"hello {x}")
// by delegating to M28's existing f-string parser
func (p *PythonParser) parseFStringFromLexeme(lexeme string) (core.Value, error) {
	// Find the quote character (after 'f')
	if len(lexeme) < 2 {
		return nil, fmt.Errorf("invalid f-string lexeme: %s", lexeme)
	}

	var quoteChar rune
	if lexeme[1] == '"' {
		quoteChar = '"'
	} else if lexeme[1] == '\'' {
		quoteChar = '\''
	} else {
		return nil, fmt.Errorf("invalid f-string lexeme: %s", lexeme)
	}

	// Create a temporary M28 parser to handle the f-string
	m28Parser := NewParser()
	m28Parser.input = lexeme
	m28Parser.pos = 0
	m28Parser.line = 1
	m28Parser.col = 1

	// Parse using M28's existing f-string logic
	return m28Parser.parseFStringEnhancedSimple(quoteChar)
}

// convertValueToASTNode converts a core.Value to an ASTNode
// This is needed for f-strings which return IR values
func (p *PythonParser) convertValueToASTNode(value core.Value, tok Token) ast.ASTNode {
	switch v := value.(type) {
	case core.StringValue:
		// Simple string result - return as literal
		return ast.NewLiteral(v, p.makeLocation(tok), ast.SyntaxPython)

	case core.ListValue:
		// F-string with interpolations returns a list like (format ...)
		// Convert to SExpr
		nodes := make([]ast.ASTNode, len(v))
		for i, elem := range v {
			nodes[i] = p.convertValueToASTNode(elem, tok)
		}
		return ast.NewSExpr(nodes, p.makeLocation(tok), ast.SyntaxPython)

	case core.SymbolValue:
		return ast.NewIdentifier(string(v), p.makeLocation(tok), ast.SyntaxPython)

	default:
		// For other types, wrap in literal
		return ast.NewLiteral(v, p.makeLocation(tok), ast.SyntaxPython)
	}
}

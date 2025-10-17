package parser

import (
	"fmt"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// PythonParser implements a recursive descent parser for Python syntax
type PythonParser struct {
	tokens  []Token
	current int
	errors  []error
	panic   bool // panic mode for error recovery
}

// NewPythonParser creates a new Python parser from a token stream
func NewPythonParser(tokens []Token) *PythonParser {
	return &PythonParser{
		tokens:  tokens,
		current: 0,
		errors:  []error{},
		panic:   false,
	}
}

// Parse parses the token stream into a list of AST nodes
func (p *PythonParser) Parse() ([]ast.ASTNode, error) {
	statements := []ast.ASTNode{}

	for !p.isAtEnd() {
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
	// Check for decorators first (can precede def or class)
	if p.check(TOKEN_AT) {
		decorators := p.parseDecorators()

		// Decorators must be followed by def or class
		if p.check(TOKEN_DEF) {
			return p.parseDefStatement(decorators)
		} else if p.check(TOKEN_CLASS) {
			return p.parseClassStatement(decorators)
		} else {
			p.error("Decorators must precede 'def' or 'class'")
			return nil
		}
	}

	// Simple statements
	switch p.peek().Type {
	case TOKEN_DEF:
		return p.parseDefStatement(nil)
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
	case TOKEN_BREAK:
		return p.parseBreakStatement()
	case TOKEN_CONTINUE:
		return p.parseContinueStatement()
	case TOKEN_PASS:
		return p.parsePassStatement()
	case TOKEN_RAISE:
		return p.parseRaiseStatement()
	default:
		// Expression statement (could be assignment)
		return p.parseExpressionStatement()
	}
}

// ============================================================================
// Simple Statement Parsing
// ============================================================================

// parseReturnStatement parses: return [expression]
func (p *PythonParser) parseReturnStatement() ast.ASTNode {
	tok := p.expect(TOKEN_RETURN)

	var value ast.ASTNode
	if !p.check(TOKEN_NEWLINE) && !p.isAtEnd() {
		value = p.parseExpression()
	}

	// Consume newline
	if p.check(TOKEN_NEWLINE) {
		p.advance()
	}

	return ast.NewReturnForm(value, p.makeLocation(tok), ast.SyntaxPython)
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
func (p *PythonParser) parseFromImportStatement() ast.ASTNode {
	tok := p.expect(TOKEN_FROM)

	// Parse module name
	moduleName := p.parseModuleName()

	// Expect 'import'
	p.expect(TOKEN_IMPORT)

	// Create base import form: (import "module" :from ...)
	elements := []ast.ASTNode{
		ast.NewIdentifier("import", p.makeLocation(tok), ast.SyntaxPython),
		ast.NewLiteral(core.StringValue(moduleName), p.makeLocation(tok), ast.SyntaxPython),
		ast.NewIdentifier(":from", p.makeLocation(tok), ast.SyntaxPython),
	}

	// Check for '*' (import all)
	if p.check(TOKEN_STAR) {
		p.advance()
		elements = append(elements, ast.NewIdentifier("*", p.makeLocation(tok), ast.SyntaxPython))

		// Consume newline
		if p.check(TOKEN_NEWLINE) {
			p.advance()
		}

		return ast.NewSExpr(elements, p.makeLocation(tok), ast.SyntaxPython)
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
		nameTok := p.expect(TOKEN_IDENTIFIER)
		nameNode := p.parseImportName(nameTok, tok)
		names = append(names, nameNode)
	}

	// Create (list-literal name1 name2 ...) for the names
	listLiteral := append([]ast.ASTNode{ast.NewIdentifier("list-literal", p.makeLocation(tok), ast.SyntaxPython)}, names...)
	elements = append(elements, ast.NewSExpr(listLiteral, p.makeLocation(tok), ast.SyntaxPython))

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
	expr := p.parseExpression()

	// Check for comma (tuple formation on left side of assignment)
	// e.g., x, y = 1, 2
	if p.check(TOKEN_COMMA) {
		// Collect all comma-separated expressions
		elements := []ast.ASTNode{expr}
		for p.check(TOKEN_COMMA) {
			p.advance() // consume comma
			if p.check(TOKEN_ASSIGN) || p.check(TOKEN_NEWLINE) {
				// Trailing comma before assignment or end of line
				break
			}
			elements = append(elements, p.parseExpression())
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
		TOKEN_SLASH_ASSIGN, TOKEN_DOUBLESLASH_ASSIGN, TOKEN_PERCENT_ASSIGN) {
		tok := p.previous()
		value := p.parseExpression()

		// Convert augmented assignment to regular assignment
		// x += 1 → x = x + 1
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
	// Lambda has lowest precedence, handle it first
	if p.check(TOKEN_LAMBDA) {
		return p.parseLambda()
	}
	return p.parseOr()
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

// parseComparison parses: addition (comp_op addition)*
func (p *PythonParser) parseComparison() ast.ASTNode {
	expr := p.parseAddition()

	for p.match(TOKEN_EQUALEQUAL, TOKEN_NOTEQUAL, TOKEN_LESS, TOKEN_LESSEQUAL,
		TOKEN_GREATER, TOKEN_GREATEREQUAL, TOKEN_IN, TOKEN_IS) {
		tok := p.previous()
		right := p.parseAddition()

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
		case TOKEN_IS:
			op = "is"
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

// parseMultiplication parses: unary ((*|/|//|%) unary)*
func (p *PythonParser) parseMultiplication() ast.ASTNode {
	expr := p.parseUnary()

	for p.match(TOKEN_STAR, TOKEN_SLASH, TOKEN_DOUBLESLASH, TOKEN_PERCENT) {
		tok := p.previous()
		right := p.parseUnary()

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

// parseUnary parses: (-|+|~) unary | postfix
func (p *PythonParser) parseUnary() ast.ASTNode {
	if p.match(TOKEN_MINUS, TOKEN_PLUS, TOKEN_TILDE) {
		tok := p.previous()
		expr := p.parseUnary()

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

	return p.parsePostfix()
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

	if !p.check(TOKEN_RPAREN) {
		for {
			args = append(args, p.parseExpression())

			if !p.check(TOKEN_COMMA) {
				break
			}
			p.advance()
		}
	}

	p.expect(TOKEN_RPAREN)

	return ast.NewSExpr(args, p.makeLocation(tok), ast.SyntaxPython)
}

// parseSubscript parses: [index]
func (p *PythonParser) parseSubscript(obj ast.ASTNode) ast.ASTNode {
	tok := p.expect(TOKEN_LBRACKET)
	index := p.parseExpression()
	p.expect(TOKEN_RBRACKET)

	return ast.NewSExpr([]ast.ASTNode{
		ast.NewIdentifier("get-item", p.makeLocation(tok), ast.SyntaxPython),
		obj,
		index,
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

	case TOKEN_NUMBER, TOKEN_STRING:
		p.advance()
		return ast.NewLiteral(tok.Value, p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_FSTRING:
		p.advance()
		// Parse f-string using M28's f-string parser
		fstringValue, err := p.parseFStringFromLexeme(tok.Lexeme)
		if err != nil {
			p.error(fmt.Sprintf("Error parsing f-string: %v", err))
			return nil
		}
		// Return as SExpr to be evaluated (like dict-literal, etc.)
		return p.convertValueToASTNode(fstringValue, tok)

	case TOKEN_TRUE:
		p.advance()
		return ast.NewLiteral(core.BoolValue(true), p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_FALSE:
		p.advance()
		return ast.NewLiteral(core.BoolValue(false), p.makeLocation(tok), ast.SyntaxPython)

	case TOKEN_NIL:
		p.advance()
		return ast.NewLiteral(core.None, p.makeLocation(tok), ast.SyntaxPython)

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

	// Parse first expression
	elements := []ast.ASTNode{p.parseExpression()}

	// Check if it's a generator expression
	if p.check(TOKEN_FOR) {
		return p.parseGeneratorExpression(elements[0], tok)
	}

	// Check for comma (tuple literal)
	if p.check(TOKEN_COMMA) {
		p.advance()

		// Parse remaining elements (if any)
		for !p.check(TOKEN_RPAREN) && !p.isAtEnd() {
			elements = append(elements, p.parseExpression())

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

	// No comma - just a parenthesized expression
	p.expect(TOKEN_RPAREN)
	return elements[0]
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
	first := p.parseExpression()

	// Check if it's a comprehension
	if p.check(TOKEN_FOR) {
		return p.parseListComprehension(first, tok)
	}

	// Regular list
	elements := []ast.ASTNode{first}

	for p.check(TOKEN_COMMA) {
		p.advance()
		if p.check(TOKEN_RBRACKET) {
			break // Trailing comma
		}
		elements = append(elements, p.parseExpression())
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

func (p *PythonParser) parseListComprehension(element ast.ASTNode, tok Token) ast.ASTNode {
	// [expr for var in iter if condition]
	p.expect(TOKEN_FOR)

	varTok := p.expect(TOKEN_IDENTIFIER)
	variable := varTok.Lexeme

	p.expect(TOKEN_IN)
	iterable := p.parseExpression()

	var condition ast.ASTNode
	if p.check(TOKEN_IF) {
		p.advance()
		condition = p.parseExpression()
	}

	p.expect(TOKEN_RBRACKET)

	return ast.NewComprehensionForm(
		ast.ListComp,
		element, // Element expression
		nil,     // KeyExpr (not used for list comp)
		nil,     // ValueExpr (not used for list comp)
		variable,
		iterable,
		condition,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

func (p *PythonParser) parseDictComprehension(key, value ast.ASTNode, tok Token) ast.ASTNode {
	// {k: v for var in iter if condition}
	p.expect(TOKEN_FOR)

	varTok := p.expect(TOKEN_IDENTIFIER)
	variable := varTok.Lexeme

	p.expect(TOKEN_IN)
	iterable := p.parseExpression()

	var condition ast.ASTNode
	if p.check(TOKEN_IF) {
		p.advance()
		condition = p.parseExpression()
	}

	p.expect(TOKEN_RBRACE)

	return ast.NewComprehensionForm(
		ast.DictComp,
		nil,   // Element (not used for dict comp)
		key,   // KeyExpr
		value, // ValueExpr
		variable,
		iterable,
		condition,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

func (p *PythonParser) parseSetComprehension(element ast.ASTNode, tok Token) ast.ASTNode {
	// {expr for var in iter if condition}
	p.expect(TOKEN_FOR)

	varTok := p.expect(TOKEN_IDENTIFIER)
	variable := varTok.Lexeme

	p.expect(TOKEN_IN)
	iterable := p.parseExpression()

	var condition ast.ASTNode
	if p.check(TOKEN_IF) {
		p.advance()
		condition = p.parseExpression()
	}

	p.expect(TOKEN_RBRACE)

	return ast.NewComprehensionForm(
		ast.SetComp,
		element, // Element expression
		nil,     // KeyExpr (not used for set comp)
		nil,     // ValueExpr (not used for set comp)
		variable,
		iterable,
		condition,
		p.makeLocation(tok),
		ast.SyntaxPython,
	)
}

func (p *PythonParser) parseGeneratorExpression(element ast.ASTNode, tok Token) ast.ASTNode {
	// (expr for var in iter if condition)
	p.expect(TOKEN_FOR)

	varTok := p.expect(TOKEN_IDENTIFIER)
	variable := varTok.Lexeme

	p.expect(TOKEN_IN)
	iterable := p.parseExpression()

	var condition ast.ASTNode
	if p.check(TOKEN_IF) {
		p.advance()
		condition = p.parseExpression()
	}

	p.expect(TOKEN_RPAREN)

	return ast.NewComprehensionForm(
		ast.GeneratorComp,
		element, // Element expression
		nil,     // KeyExpr (not used for generator)
		nil,     // ValueExpr (not used for generator)
		variable,
		iterable,
		condition,
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

// parseForStatement parses: for var in iterable: block (else: block)?
func (p *PythonParser) parseForStatement() ast.ASTNode {
	tok := p.expect(TOKEN_FOR)

	// Parse loop variable
	varTok := p.expect(TOKEN_IDENTIFIER)
	variable := varTok.Lexeme

	p.expect(TOKEN_IN)

	// Parse iterable
	iterable := p.parseExpression()

	// Parse body
	body := p.parseBlock()

	// Check for optional else clause
	var elseBody []ast.ASTNode
	if p.check(TOKEN_ELSE) {
		p.advance()
		elseBody = p.parseBlock()
	}

	return ast.NewForForm(variable, iterable, body, elseBody, p.makeLocation(tok), ast.SyntaxPython)
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

// parseDefStatement parses: (@decorator)* def name(params) (-> type)?: block
func (p *PythonParser) parseDefStatement(decorators []ast.ASTNode) ast.ASTNode {
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

	// Parse body
	body := p.parseBlock()

	// Convert body to single node
	var bodyNode ast.ASTNode
	if len(body) == 1 {
		bodyNode = body[0]
	} else {
		bodyNode = ast.NewBlockForm(body, p.makeLocation(tok), ast.SyntaxPython)
	}

	return ast.NewDefForm(name, params, bodyNode, returnType, decorators, p.makeLocation(tok), ast.SyntaxPython)
}

// parseParameters parses: (param (: type)? (= default)?, ...)*
func (p *PythonParser) parseParameters() []ast.Parameter {
	params := []ast.Parameter{}

	if p.check(TOKEN_RPAREN) {
		return params // Empty parameter list
	}

	for {
		// Parse parameter name
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

	// Parse optional base classes
	var bases []ast.ASTNode
	if p.check(TOKEN_LPAREN) {
		p.advance()

		if !p.check(TOKEN_RPAREN) {
			for {
				bases = append(bases, p.parseExpression())

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

	return ast.NewClassForm(name, bases, body, decorators, p.makeLocation(tok), ast.SyntaxPython)
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
			typeTok := p.expect(TOKEN_IDENTIFIER)
			exceptType = typeTok.Lexeme

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

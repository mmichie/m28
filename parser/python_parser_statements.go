package parser

import (
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// This file contains statement parsing methods for the Python parser.
// It includes:
// - parseStatement (main statement dispatcher)
// - Simple statements (return, yield, break, continue, pass, global, nonlocal, raise, assert, del)
// - Import statements (import, from...import)
// - Expression statements and assignments (including annotated assignments)

func (p *PythonParser) parseStatement() ast.ASTNode {
	if err := p.enterParse("parseStatement"); err != nil {
		p.errors = append(p.errors, err)
		return nil
	}
	defer p.exitParse()

	// Check for decorators first (can precede def or class)
	// Note: @= is TOKEN_AT_EQ, not TOKEN_AT, so this won't match augmented assignment
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
			// variable: match = value or match, rest = value (tuple unpacking)
			if p.current+1 < len(p.tokens) {
				nextToken := p.tokens[p.current+1].Type
				// If next token is = or , (tuple unpacking), it's an assignment, not match statement
				if nextToken != TOKEN_ASSIGN && nextToken != TOKEN_COMMA {
					// Not an assignment, could be match statement
					// Parse and see what happens
					return p.parseMatchStatement()
				}
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
		// Parse first expression (may include star unpacking)
		first := p.parseListElement()

		// Check for comma-separated expressions (implicit tuple)
		if p.check(TOKEN_COMMA) {
			values := []ast.ASTNode{first}

			for p.check(TOKEN_COMMA) {
				p.advance() // consume comma

				// Trailing comma is allowed
				if p.check(TOKEN_NEWLINE) || p.isAtEnd() {
					break
				}

				// Parse each element, supporting star unpacking
				values = append(values, p.parseListElement())
			}

			// Create tuple: (tuple-literal elem1 elem2 ...)
			tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
			allElements := append([]ast.ASTNode{tupleSym}, values...)
			value = ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
		} else {
			value = first
		}
	}

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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
		// Parse first expression (may include star unpacking)
		first := p.parseListElement()

		// Check for implicit tuple: yield a, b, c or yield 1, *rest
		if p.check(TOKEN_COMMA) {
			values := []ast.ASTNode{first}

			for p.check(TOKEN_COMMA) {
				p.advance()
				if p.check(TOKEN_NEWLINE) || p.isAtEnd() {
					break
				}
				// Parse each element, supporting star unpacking
				values = append(values, p.parseListElement())
			}

			// Create tuple: (tuple-literal elem1 elem2 ...)
			tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
			allElements := append([]ast.ASTNode{tupleSym}, values...)
			args = append(args, ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython))
		} else {
			args = append(args, first)
		}
	}

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
		p.advance()
	}

	return ast.NewBreakForm(p.makeLocation(tok), ast.SyntaxPython)
}

// parseContinueStatement parses: continue
func (p *PythonParser) parseContinueStatement() ast.ASTNode {
	tok := p.expect(TOKEN_CONTINUE)

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
		p.advance()
	}

	return ast.NewContinueForm(p.makeLocation(tok), ast.SyntaxPython)
}

// parsePassStatement parses: pass
func (p *PythonParser) parsePassStatement() ast.ASTNode {
	tok := p.expect(TOKEN_PASS)

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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
		p.advance() // consume comma

		// Trailing comma is allowed (e.g., del x, y,)
		if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) || p.isAtEnd() {
			break
		}

		targets = append(targets, p.parseExpression())
	}

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

		// Consume newline or semicolon
		if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
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

	// Check for annotated assignment: target : type [= value]
	// Target can be: identifier, attribute (obj.attr), or subscript (obj[key])
	if p.check(TOKEN_COLON) {
		return p.parseAnnotatedAssignment(expr)
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

		// Log Pythonic assignment desugaring
		core.Log.Trace(core.SubsystemParser, "Desugaring Pythonic assignment", "file", p.filename, "line", tok.Line, "col", tok.Col)

		// Collect all assignment targets
		// For x = y = z = 0, we collect [x, y, z] and then parse 0
		targets := []ast.ASTNode{expr}

		// Keep collecting targets while we see = followed by another expression
		for {
			nextExpr := p.parseExpression()

			// Check for comma on right side (tuple formation)
			if p.check(TOKEN_COMMA) {
				elements := []ast.ASTNode{nextExpr}
				hasTrailingComma := false
				for p.check(TOKEN_COMMA) {
					p.advance() // consume comma
					if p.check(TOKEN_ASSIGN) || p.check(TOKEN_NEWLINE) {
						// Trailing comma before assignment or end of line
						hasTrailingComma = true
						break
					}
					elements = append(elements, p.parseExpression())
				}

				// Create tuple for right side
				// Python rule: comma creates tuple even with single element if there's a trailing comma
				// e.g., x = 'win', creates tuple ('win',)
				if len(elements) > 1 || hasTrailingComma {
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

				// Consume newline or semicolon
				if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
					p.advance()
				}

				return result
			}
		}
	}

	// Check for augmented assignment (+=, -=, etc.)
	if p.match(TOKEN_PLUS_ASSIGN, TOKEN_MINUS_ASSIGN, TOKEN_STAR_ASSIGN,
		TOKEN_SLASH_ASSIGN, TOKEN_DOUBLESLASH_ASSIGN, TOKEN_PERCENT_ASSIGN,
		TOKEN_DOUBLESTAR_ASSIGN, TOKEN_PIPE_ASSIGN, TOKEN_AMPERSAND_ASSIGN,
		TOKEN_CARET_ASSIGN, TOKEN_LSHIFT_ASSIGN, TOKEN_RSHIFT_ASSIGN, TOKEN_AT_EQ) {
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
		case TOKEN_DOUBLESTAR_ASSIGN:
			op = "**"
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
		case TOKEN_AT_EQ:
			op = "@"
		}

		opCall := ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
			expr,
			value,
		}, p.makeLocation(tok), ast.SyntaxPython)

		// Consume newline or semicolon
		if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
			p.advance()
		}

		return ast.NewAssignForm(expr, opCall, p.makeLocation(tok), ast.SyntaxPython)
	}

	// Bare expression statement
	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
		p.advance()
	}

	return expr
}

// parseAnnotatedAssignment parses PEP 526 annotated assignments
// Syntax: target : type [= value]
// where target can be: identifier, attribute (obj.attr), or subscript (obj[key])
func (p *PythonParser) parseAnnotatedAssignment(target ast.ASTNode) ast.ASTNode {
	// Target has already been parsed by the caller
	// It can be an identifier, attribute access, or subscript

	// Expect colon
	colonTok := p.expect(TOKEN_COLON)

	// Parse type annotation
	annotation := p.parseTypeAnnotation()

	// Check for optional assignment
	if p.check(TOKEN_ASSIGN) {
		p.advance() // consume =
		value := p.parseExpression()

		// Consume newline or semicolon
		if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
			p.advance()
		}

		// Create annotated assignment: (annotated-assign target annotation value)
		return ast.NewAnnotatedAssignForm(target, annotation, value, p.makeLocation(colonTok), ast.SyntaxPython)
	}

	// No assignment - just a type annotation
	// In Python 3.6+, bare annotations only add to __annotations__, don't create variable
	// Consume newline or semicolon
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
		p.advance()
	}

	// Return annotated assignment without value: (annotated-assign target annotation)
	return ast.NewAnnotatedAssignForm(target, annotation, nil, p.makeLocation(colonTok), ast.SyntaxPython)
}

// ============================================================================
// Expression Parsing (Precedence Climbing)
// ============================================================================

// parseExpression is the entry point for expression parsing

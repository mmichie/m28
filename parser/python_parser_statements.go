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

	// Parse relative import level (count dots)
	relativeLevel := p.parseRelativeImportLevel()

	// Parse module name
	moduleName := p.parseFromImportModule(relativeLevel)

	// Expect 'import' keyword
	p.expect(TOKEN_IMPORT)

	// Create base import form elements
	elements := p.buildFromImportBase(moduleName, tok)

	// Handle star import (from module import *)
	if p.check(TOKEN_STAR) {
		return p.parseStarImport(elements, relativeLevel, tok)
	}

	// Parse import names list
	names := p.parseImportNamesList(tok)

	// Build final import form
	return p.buildFromImportForm(elements, names, relativeLevel, tok)
}

// parseRelativeImportLevel counts leading dots for relative imports
func (p *PythonParser) parseRelativeImportLevel() int {
	level := 0
	for p.check(TOKEN_DOT) {
		p.advance()
		level++
	}
	return level
}

// parseFromImportModule parses the module name after 'from'
func (p *PythonParser) parseFromImportModule(relativeLevel int) string {
	if p.check(TOKEN_IDENTIFIER) {
		return p.parseModuleName()
	}

	// If no identifier and no dots, it's an error
	if relativeLevel == 0 {
		p.error("Expected module name in import statement")
	}

	return ""
}

// buildFromImportBase creates the base elements for from-import
func (p *PythonParser) buildFromImportBase(moduleName string, tok Token) []ast.ASTNode {
	return []ast.ASTNode{
		ast.NewIdentifier("import", p.makeLocation(tok), ast.SyntaxPython),
		ast.NewLiteral(core.StringValue(moduleName), p.makeLocation(tok), ast.SyntaxPython),
		ast.NewIdentifier(":from", p.makeLocation(tok), ast.SyntaxPython),
	}
}

// parseStarImport handles 'from module import *'
func (p *PythonParser) parseStarImport(elements []ast.ASTNode, relativeLevel int, tok Token) ast.ASTNode {
	p.advance() // consume *

	elements = append(elements, ast.NewIdentifier("*", p.makeLocation(tok), ast.SyntaxPython))

	// Add relative level if needed
	if relativeLevel > 0 {
		elements = p.addRelativeLevel(elements, relativeLevel, tok)
	}

	p.consumeStatementTerminator()
	return ast.NewSExpr(elements, p.makeLocation(tok), ast.SyntaxPython)
}

// parseImportNamesList parses comma-separated import names with optional parens
func (p *PythonParser) parseImportNamesList(tok Token) []ast.ASTNode {
	// Check for parenthesized import list
	hasParens := p.match(TOKEN_LPAREN)
	if hasParens {
		p.skipNewlines()
	}

	// Parse first name
	names := []ast.ASTNode{p.parseImportName(p.expect(TOKEN_IDENTIFIER), tok)}

	// Parse additional names
	for p.check(TOKEN_COMMA) {
		p.advance()

		if hasParens {
			p.skipNewlines()
			// Allow trailing comma
			if p.check(TOKEN_RPAREN) {
				break
			}
		}

		names = append(names, p.parseImportName(p.expect(TOKEN_IDENTIFIER), tok))
	}

	// Close parens if needed
	if hasParens {
		p.skipNewlines()
		p.expect(TOKEN_RPAREN)
	}

	return names
}

// buildFromImportForm constructs the final from-import AST node
func (p *PythonParser) buildFromImportForm(elements, names []ast.ASTNode, relativeLevel int, tok Token) ast.ASTNode {
	// Create (list-literal name1 name2 ...) for the names
	listLiteral := append([]ast.ASTNode{ast.NewIdentifier("list-literal", p.makeLocation(tok), ast.SyntaxPython)}, names...)
	elements = append(elements, ast.NewSExpr(listLiteral, p.makeLocation(tok), ast.SyntaxPython))

	// Add relative level if needed
	if relativeLevel > 0 {
		elements = p.addRelativeLevel(elements, relativeLevel, tok)
	}

	p.consumeStatementTerminator()
	return ast.NewSExpr(elements, p.makeLocation(tok), ast.SyntaxPython)
}

// addRelativeLevel appends :level and level value to elements
func (p *PythonParser) addRelativeLevel(elements []ast.ASTNode, level int, tok Token) []ast.ASTNode {
	return append(elements,
		ast.NewIdentifier(":level", p.makeLocation(tok), ast.SyntaxPython),
		ast.NewLiteral(core.NumberValue(level), p.makeLocation(tok), ast.SyntaxPython),
	)
}

// skipNewlines skips any consecutive newline tokens
func (p *PythonParser) skipNewlines() {
	for p.check(TOKEN_NEWLINE) {
		p.advance()
	}
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
	// Parse initial expression (may include leading star unpacking)
	expr := p.parseInitialExpression()

	// Check for annotated assignment: target : type [= value]
	if p.check(TOKEN_COLON) {
		return p.parseAnnotatedAssignment(expr)
	}

	// Check for comma (tuple formation on left side of assignment)
	if p.check(TOKEN_COMMA) {
		expr = p.parseTuplePattern(expr)
	}

	// Check for assignment (including chained assignment)
	if p.check(TOKEN_ASSIGN) {
		return p.parseChainedAssignment(expr)
	}

	// Check for augmented assignment (+=, -=, etc.)
	if p.isAugmentedAssignment() {
		return p.parseAugmentedAssignment(expr)
	}

	// Bare expression statement
	p.consumeStatementTerminator()
	return expr
}

// parseInitialExpression parses the first expression, handling leading star unpacking
func (p *PythonParser) parseInitialExpression() ast.ASTNode {
	// Check if this starts with star unpacking: *a, b = ...
	if p.check(TOKEN_STAR) && p.current+1 < len(p.tokens) && p.tokens[p.current+1].Type == TOKEN_IDENTIFIER {
		starTok := p.advance()
		varName := p.expect(TOKEN_IDENTIFIER)
		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("*unpack", p.makeLocation(starTok), ast.SyntaxPython),
			ast.NewIdentifier(varName.Lexeme, p.makeLocation(varName), ast.SyntaxPython),
		}, p.makeLocation(starTok), ast.SyntaxPython)
	}
	return p.parseExpression()
}

// parseTuplePattern parses comma-separated expressions for tuple unpacking
// e.g., x, y = 1, 2 or a, *b, c = [1, 2, 3, 4] or x, = [1] (single-element unpacking)
func (p *PythonParser) parseTuplePattern(firstExpr ast.ASTNode) ast.ASTNode {
	elements := []ast.ASTNode{firstExpr}
	hasComma := false // Track if we saw at least one comma

	for p.check(TOKEN_COMMA) {
		hasComma = true
		p.advance() // consume comma

		// Check for trailing comma before assignment or end of line
		if p.check(TOKEN_ASSIGN) || p.check(TOKEN_NEWLINE) {
			break
		}

		// Parse next element (may be star unpacking or regular expression)
		elements = append(elements, p.parseTupleElement())
	}

	// If there was a comma, it's a tuple pattern for unpacking
	// This handles both multi-element (x, y = ...) and single-element (x, = ...) cases
	if hasComma {
		return ast.NewSExpr(elements, p.makeLocation(p.peek()), ast.SyntaxPython)
	}
	return firstExpr
}

// parseTupleElement parses a single element of a tuple, handling star unpacking
func (p *PythonParser) parseTupleElement() ast.ASTNode {
	if p.check(TOKEN_STAR) {
		starTok := p.advance()
		varName := p.expect(TOKEN_IDENTIFIER)
		return ast.NewSExpr([]ast.ASTNode{
			ast.NewIdentifier("*unpack", p.makeLocation(starTok), ast.SyntaxPython),
			ast.NewIdentifier(varName.Lexeme, p.makeLocation(varName), ast.SyntaxPython),
		}, p.makeLocation(starTok), ast.SyntaxPython)
	}
	return p.parseExpression()
}

// parseChainedAssignment parses assignment including chained assignments like x = y = z = 0
func (p *PythonParser) parseChainedAssignment(target ast.ASTNode) ast.ASTNode {
	tok := p.advance() // consume =

	core.Log.Trace(core.SubsystemParser, "Desugaring Pythonic assignment",
		"file", p.filename, "line", tok.Line, "col", tok.Col)

	// Validate the assignment target
	if errMsg := p.validateAssignmentTarget(target); errMsg != "" {
		p.error(errMsg)
		return nil
	}

	// Collect all assignment targets
	targets := []ast.ASTNode{target}

	// Keep collecting targets while we see = followed by another expression
	for {
		nextExpr := p.parseAssignmentValue(tok)

		if p.check(TOKEN_ASSIGN) {
			// Another assignment, validate and collect this target
			if errMsg := p.validateAssignmentTarget(nextExpr); errMsg != "" {
				p.error(errMsg)
				return nil
			}
			targets = append(targets, nextExpr)
			p.advance() // consume the =
		} else {
			// This is the final value - build nested assignments right-to-left
			result := p.buildNestedAssignments(targets, nextExpr, tok)
			p.consumeStatementTerminator()
			return result
		}
	}
}

// parseAssignmentValue parses the right-hand side of an assignment, handling tuple formation
func (p *PythonParser) parseAssignmentValue(tok Token) ast.ASTNode {
	expr := p.parseExpression()

	// Check for comma on right side (tuple formation)
	if !p.check(TOKEN_COMMA) {
		return expr
	}

	elements := []ast.ASTNode{expr}
	hasTrailingComma := false

	for p.check(TOKEN_COMMA) {
		p.advance() // consume comma

		if p.check(TOKEN_ASSIGN) || p.check(TOKEN_NEWLINE) {
			hasTrailingComma = true
			break
		}
		elements = append(elements, p.parseExpression())
	}

	// Create tuple for right side
	// Python rule: comma creates tuple even with single element if there's a trailing comma
	if len(elements) > 1 || hasTrailingComma {
		tupleSym := ast.NewIdentifier("tuple-literal", p.makeLocation(tok), ast.SyntaxPython)
		allElements := append([]ast.ASTNode{tupleSym}, elements...)
		return ast.NewSExpr(allElements, p.makeLocation(tok), ast.SyntaxPython)
	}
	return expr
}

// buildNestedAssignments constructs nested assignments right-to-left
// x = y = z = 0 becomes (= x (= y (= z 0)))
func (p *PythonParser) buildNestedAssignments(targets []ast.ASTNode, value ast.ASTNode, tok Token) ast.ASTNode {
	result := ast.NewAssignForm(targets[len(targets)-1], value, p.makeLocation(tok), ast.SyntaxPython)

	for i := len(targets) - 2; i >= 0; i-- {
		result = ast.NewAssignForm(targets[i], result, p.makeLocation(tok), ast.SyntaxPython)
	}

	return result
}

// isAugmentedAssignment checks if current token is an augmented assignment operator
func (p *PythonParser) isAugmentedAssignment() bool {
	return p.check(TOKEN_PLUS_ASSIGN) || p.check(TOKEN_MINUS_ASSIGN) ||
		p.check(TOKEN_STAR_ASSIGN) || p.check(TOKEN_SLASH_ASSIGN) ||
		p.check(TOKEN_DOUBLESLASH_ASSIGN) || p.check(TOKEN_PERCENT_ASSIGN) ||
		p.check(TOKEN_DOUBLESTAR_ASSIGN) || p.check(TOKEN_PIPE_ASSIGN) ||
		p.check(TOKEN_AMPERSAND_ASSIGN) || p.check(TOKEN_CARET_ASSIGN) ||
		p.check(TOKEN_LSHIFT_ASSIGN) || p.check(TOKEN_RSHIFT_ASSIGN) ||
		p.check(TOKEN_AT_EQ)
}

// parseAugmentedAssignment parses augmented assignments like +=, -=, etc.
func (p *PythonParser) parseAugmentedAssignment(target ast.ASTNode) ast.ASTNode {
	// Augmented assignment requires a single target - tuples/lists are not allowed
	// Check for tuple/list before general validation
	if p.isSequenceTarget(target) {
		p.error("'" + p.describeTargetForAugmented(target) + "' is an illegal expression for augmented assignment")
		return nil
	}

	// Validate the assignment target
	// CPython uses "illegal expression" for augmented assignment on invalid targets
	if errMsg := p.validateAssignmentTarget(target); errMsg != "" {
		p.error("'" + p.describeTargetForAugmented(target) + "' is an illegal expression for augmented assignment")
		return nil
	}

	p.match(TOKEN_PLUS_ASSIGN, TOKEN_MINUS_ASSIGN, TOKEN_STAR_ASSIGN,
		TOKEN_SLASH_ASSIGN, TOKEN_DOUBLESLASH_ASSIGN, TOKEN_PERCENT_ASSIGN,
		TOKEN_DOUBLESTAR_ASSIGN, TOKEN_PIPE_ASSIGN, TOKEN_AMPERSAND_ASSIGN,
		TOKEN_CARET_ASSIGN, TOKEN_LSHIFT_ASSIGN, TOKEN_RSHIFT_ASSIGN, TOKEN_AT_EQ)

	tok := p.previous()
	value := p.parseExpression()

	// Convert augmented assignment to regular assignment: x += 1 → x = x + 1
	op := p.getAugmentedOperator(tok.Type)
	opCall := ast.NewSExpr([]ast.ASTNode{
		ast.NewIdentifier(op, p.makeLocation(tok), ast.SyntaxPython),
		target,
		value,
	}, p.makeLocation(tok), ast.SyntaxPython)

	p.consumeStatementTerminator()
	return ast.NewAssignForm(target, opCall, p.makeLocation(tok), ast.SyntaxPython)
}

// getAugmentedOperator maps augmented assignment tokens to their base operators
func (p *PythonParser) getAugmentedOperator(tokType TokenType) string {
	switch tokType {
	case TOKEN_PLUS_ASSIGN:
		return "+"
	case TOKEN_MINUS_ASSIGN:
		return "-"
	case TOKEN_STAR_ASSIGN:
		return "*"
	case TOKEN_SLASH_ASSIGN:
		return "/"
	case TOKEN_DOUBLESLASH_ASSIGN:
		return "//"
	case TOKEN_PERCENT_ASSIGN:
		return "%"
	case TOKEN_DOUBLESTAR_ASSIGN:
		return "**"
	case TOKEN_PIPE_ASSIGN:
		return "|"
	case TOKEN_AMPERSAND_ASSIGN:
		return "&"
	case TOKEN_CARET_ASSIGN:
		return "^"
	case TOKEN_LSHIFT_ASSIGN:
		return "<<"
	case TOKEN_RSHIFT_ASSIGN:
		return ">>"
	case TOKEN_AT_EQ:
		return "@"
	default:
		return ""
	}
}

// consumeStatementTerminator consumes optional newline or semicolon
func (p *PythonParser) consumeStatementTerminator() {
	if p.check(TOKEN_NEWLINE) || p.check(TOKEN_SEMICOLON) {
		p.advance()
	}
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
// Assignment Target Validation
// ============================================================================

// describeTarget returns a description of an AST node for error messages
func (p *PythonParser) describeTarget(target ast.ASTNode) string {
	switch n := target.(type) {
	case *ast.Identifier:
		return n.Name
	case *ast.Literal:
		return "literal"
	case *ast.ComprehensionForm:
		switch n.Kind {
		case ast.DictComp:
			return "dict comprehension"
		case ast.ListComp:
			return "list comprehension"
		case ast.SetComp:
			return "set comprehension"
		case ast.GeneratorComp:
			return "generator expression"
		default:
			return "comprehension"
		}
	case *ast.SExpr:
		if len(n.Elements) > 0 {
			if ident, ok := n.Elements[0].(*ast.Identifier); ok {
				switch ident.Name {
				case "dict-comp":
					return "dict comprehension"
				case "list-comp":
					return "list comprehension"
				case "set-comp":
					return "set comprehension"
				case "generator":
					return "generator expression"
				case "call":
					return "function call result"
				case "lambda":
					return "lambda"
				case "if-expr", "ternary":
					return "conditional expression"
				}
			}
		}
		return "expression"
	default:
		return "expression"
	}
}

// isSequenceTarget checks if an AST node is a tuple or list (sequence unpacking target)
// These are valid for regular assignment but NOT for augmented assignment
func (p *PythonParser) isSequenceTarget(target ast.ASTNode) bool {
	switch n := target.(type) {
	case *ast.SExpr:
		if len(n.Elements) == 0 {
			return false
		}
		// Check the first element
		if ident, ok := n.Elements[0].(*ast.Identifier); ok {
			switch ident.Name {
			// Explicit tuple-literal or list-literal
			case "tuple-literal", "list-literal":
				return true
			// Special forms that are valid augmented assignment targets
			case "get-attr", ".", "get-item", "subscript":
				return false
			}
			// Check for implicit tuple: bare S-expression with multiple simple identifiers
			// e.g., (x b) from parsing "x, b"
			// This excludes things like (get-item self elem) where get-item is a special form
			if len(n.Elements) >= 2 {
				// If all elements are simple identifiers (not special forms), it's a tuple
				allSimpleIdents := true
				for _, elem := range n.Elements {
					if id, ok := elem.(*ast.Identifier); ok {
						// Check if it's a special form identifier
						switch id.Name {
						case "get-attr", ".", "get-item", "subscript", "+", "-", "*", "/",
							"call", "lambda", "if-expr", "ternary", "tuple-literal", "list-literal":
							allSimpleIdents = false
						}
					} else {
						allSimpleIdents = false
					}
					if !allSimpleIdents {
						break
					}
				}
				if allSimpleIdents {
					return true
				}
			}
		}
	}
	return false
}

// describeTargetForAugmented returns a description for augmented assignment errors
// Returns "tuple" or "list" for sequence targets
func (p *PythonParser) describeTargetForAugmented(target ast.ASTNode) string {
	switch n := target.(type) {
	case *ast.SExpr:
		if len(n.Elements) > 0 {
			if ident, ok := n.Elements[0].(*ast.Identifier); ok {
				switch ident.Name {
				case "tuple-literal":
					return "tuple"
				case "list-literal":
					return "list"
				}
			}
			// Check for implicit tuple (multiple identifiers)
			if len(n.Elements) >= 2 {
				allIdents := true
				for _, elem := range n.Elements {
					if _, ok := elem.(*ast.Identifier); !ok {
						allIdents = false
						break
					}
				}
				if allIdents {
					return "tuple"
				}
			}
		}
	}
	return p.describeTarget(target)
}

// validateAssignmentTarget checks if an AST node is a valid assignment target
// Returns an error message if invalid, empty string if valid
// Valid targets: identifiers, attribute access (obj.attr), subscript (obj[key]), tuple/list unpacking
// Invalid: comprehensions, literals, calls, binary operations
func (p *PythonParser) validateAssignmentTarget(target ast.ASTNode) string {
	switch n := target.(type) {
	case *ast.Identifier:
		// Simple variable names are valid targets
		return ""

	case *ast.Literal:
		// Literals cannot be assigned to
		return "cannot assign to literal here. Maybe you meant '==' instead of '='?"

	case *ast.ComprehensionForm:
		// Comprehensions cannot be assigned to
		switch n.Kind {
		case ast.DictComp:
			return "cannot assign to dict comprehension"
		case ast.ListComp:
			return "cannot assign to list comprehension"
		case ast.SetComp:
			return "cannot assign to set comprehension"
		case ast.GeneratorComp:
			return "cannot assign to generator expression"
		default:
			return "cannot assign to comprehension"
		}

	case *ast.SExpr:
		// Check what kind of S-expression this is
		elements := n.Elements
		if len(elements) == 0 {
			return "cannot assign to empty expression"
		}

		// Check the head of the S-expression
		if ident, ok := elements[0].(*ast.Identifier); ok {
			switch ident.Name {
			case "get-attr", ".":
				// Attribute access is a valid target
				return ""
			case "get-item", "subscript":
				// Subscript access is a valid target
				return ""
			case "tuple-literal", "list-literal":
				// Tuple/list unpacking - validate each element
				for _, elem := range elements[1:] {
					if errMsg := p.validateAssignmentTarget(elem); errMsg != "" {
						return errMsg
					}
				}
				return ""
			case "*unpack":
				// Star unpacking in assignment is valid
				return ""
			case "dict-comp":
				return "cannot assign to dict comprehension"
			case "list-comp":
				return "cannot assign to list comprehension"
			case "set-comp":
				return "cannot assign to set comprehension"
			case "generator":
				return "cannot assign to generator expression"
			case "call":
				return "cannot assign to function call"
			case "+", "-", "*", "/", "//", "%", "**",
				"&", "|", "^", "<<", ">>",
				"and", "or", "not",
				"==", "!=", "<", ">", "<=", ">=",
				"is", "is-not", "in", "not-in":
				return "cannot assign to expression"
			case "lambda":
				return "cannot assign to lambda"
			case "if-expr", "ternary":
				return "cannot assign to conditional expression"
			}
		}

		// For other S-expressions, check if it looks like a tuple for unpacking
		// This handles implicit tuples like (a, b)
		allValidTargets := true
		for _, elem := range elements {
			if errMsg := p.validateAssignmentTarget(elem); errMsg != "" {
				allValidTargets = false
				break
			}
		}
		if allValidTargets && len(elements) > 0 {
			return "" // Valid tuple unpacking
		}

		return "cannot assign to expression"

	default:
		// For any other node types, reject
		return "cannot assign to expression"
	}
}

// ============================================================================
// Expression Parsing (Precedence Climbing)
// ============================================================================

// parseExpression is the entry point for expression parsing

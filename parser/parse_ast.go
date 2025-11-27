package parser

import (
	"fmt"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
)

// ParseToAST parses input and returns an AST node with metadata
// This is the new AST-based parsing path (Phase 2)
func (p *Parser) ParseToAST(input string) (ast.ASTNode, *core.IRMetadata, error) {
	// Step 1: Tokenize the input
	tokenizer := NewTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return nil, nil, err // Tokenization error
	}

	// Step 2: Set up parser with tokens
	p.tokens = tokens
	p.tokenPos = 0
	p.input = input // Keep for backward compatibility

	// Step 3: Create metadata table
	metadata := core.NewIRMetadata()

	// Step 4: Parse the program to AST
	astNode, err := p.parseProgramToAST()
	if err != nil {
		return nil, nil, err
	}

	// Step 5: Populate metadata from AST
	// For now, just track the root node location
	if astNode != nil && astNode.Location() != nil {
		// We'll populate more metadata in later steps
		metadata.SetLocation(astNode, astNode.Location())
		metadata.SetSyntaxKind(astNode, int(astNode.SyntaxKind()))
		if len(astNode.Comments()) > 0 {
			metadata.SetComments(astNode, astNode.Comments())
		}
	}

	return astNode, metadata, nil
}

// parseProgramToAST parses a complete program into AST
func (p *Parser) parseProgramToAST() (ast.ASTNode, error) {
	var elements []ast.ASTNode

	startTok := p.currentToken()

	// Parse expressions until EOF
	for !p.matchToken(TOKEN_EOF) {
		expr, err := p.parseExprToAST()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)
	}

	// If there's only one expression, return it directly
	if len(elements) == 1 {
		return elements[0], nil
	}

	// Check if expressions form an infix pattern at top level
	// For now, wrap multiple expressions in an implicit (do ...) SExpr
	// We can add infix detection later

	// Create implicit do form
	doIdentifier := p.makeIdentifier("do", startTok)
	allElements := append([]ast.ASTNode{doIdentifier}, elements...)

	return p.makeSExpr(allElements, startTok), nil
}

// parseExprToAST parses a single expression to AST
func (p *Parser) parseExprToAST() (ast.ASTNode, error) {
	// Check for assignment syntax: identifier = expr
	if p.matchToken(TOKEN_IDENTIFIER) {
		nameTok := p.currentToken()

		// Look ahead for = (TOKEN_ASSIGN)
		if p.tokenPos+1 < len(p.tokens) {
			nextTok := p.tokens[p.tokenPos+1]

			if nextTok.Type == TOKEN_ASSIGN {
				// It's an assignment! Parse: name = value
				p.advanceToken() // consume identifier
				p.advanceToken() // consume =

				value, err := p.parseExprToAST()
				if err != nil {
					return nil, err
				}

				// Build AssignForm AST node
				target := p.makeIdentifier(nameTok.Lexeme, nameTok)
				return p.makeAssignForm(target, value, nameTok), nil
			}
		}
	}

	// Parse the primary expression (atom)
	base, err := p.parseAtomToAST()
	if err != nil {
		return nil, err
	}

	// Handle postfix operations (dots, calls, indexing)
	base, err = p.parsePostfixToAST(base)
	if err != nil {
		return nil, err
	}

	// TODO(M28-56d6): Handle infix operators
	// For now, just return the base expression

	return base, nil
}

// parseAtomToAST parses an atomic expression to AST
func (p *Parser) parseAtomToAST() (ast.ASTNode, error) {
	tok := p.currentToken()

	switch tok.Type {
	// Literals - already parsed by tokenizer!
	case TOKEN_NUMBER:
		p.advanceToken()
		return p.makeLiteral(tok.Value, tok), nil

	case TOKEN_STRING:
		p.advanceToken()
		return p.makeLiteral(tok.Value, tok), nil

	case TOKEN_TRUE:
		p.advanceToken()
		return p.makeLiteral(core.True, tok), nil

	case TOKEN_FALSE:
		p.advanceToken()
		return p.makeLiteral(core.False, tok), nil

	case TOKEN_NIL:
		p.advanceToken()
		return p.makeLiteral(core.None, tok), nil

	case TOKEN_IDENTIFIER:
		name := tok.Lexeme
		p.advanceToken()

		// Check for Pythonic def syntax: def name(params): expr
		if name == "def" {
			// TODO(M28-56d6): Implement Pythonic def parsing to AST
			// For now, just return as identifier
			return p.makeIdentifier(name, tok), nil
		}

		// Check for function call syntax: identifier(args)
		if p.matchToken(TOKEN_LPAREN) {
			prevTok := p.tokens[p.tokenPos-1]
			parenTok := p.currentToken()

			// No whitespace means it's a function call
			if prevTok.EndPos == parenTok.StartPos {
				// Parse as function call: name(args) -> (name args)
				args, err := p.parseMethodArgsToAST()
				if err != nil {
					return nil, err
				}

				// Build SExpr (name arg1 arg2 ...)
				elements := append([]ast.ASTNode{p.makeIdentifier(name, tok)}, args...)
				return p.makeSExpr(elements, tok), nil
			}
		}

		// Just an identifier
		return p.makeIdentifier(name, tok), nil

	// Delimiters - delegate to specialized parsers
	case TOKEN_LPAREN:
		return p.parseListToAST()

	case TOKEN_LBRACKET:
		return p.parseVectorLiteralToAST()

	case TOKEN_LBRACE:
		return p.parseDictLiteralToAST()

	// Special strings - for now, keep using existing parsing logic
	// We'll convert these later
	case TOKEN_FSTRING, TOKEN_SSTRING:
		// For now, parse to IR and wrap in Literal
		// TODO(M28-56d6): Create proper AST nodes for f-strings and s-strings
		p.advanceToken()
		return p.makeLiteral(tok.Value, tok), nil

	// Reader macros
	case TOKEN_BACKTICK:
		p.advanceToken()
		expr, err := p.parseExprToAST()
		if err != nil {
			return nil, p.tokenError(fmt.Sprintf("error parsing expression after backtick: %v", err), tok)
		}
		quasiquote := p.makeIdentifier("quasiquote", tok)
		return p.makeSExpr([]ast.ASTNode{quasiquote, expr}, tok), nil

	default:
		// Handle operator tokens as identifiers (they're symbols in Lisp)
		if tok.IsOperator() {
			name := tok.Lexeme
			p.advanceToken()
			return p.makeIdentifier(name, tok), nil
		}

		return nil, p.tokenError(fmt.Sprintf("unexpected token %v in AST parsing", tok.Type), tok)
	}
}

// parsePostfixToAST handles postfix operations (placeholder for now)
func (p *Parser) parsePostfixToAST(base ast.ASTNode) (ast.ASTNode, error) {
	// TODO(M28-56d6): Implement dot notation, method calls, indexing for AST
	// For now, just return the base
	return base, nil
}

// parseMethodArgsToAST parses method/function arguments to AST
func (p *Parser) parseMethodArgsToAST() ([]ast.ASTNode, error) {
	if !p.matchToken(TOKEN_LPAREN) {
		return nil, &ParseError{Message: "expected ( for method arguments"}
	}
	p.advanceToken() // consume (

	var args []ast.ASTNode

	for !p.matchToken(TOKEN_RPAREN) {
		if p.matchToken(TOKEN_EOF) {
			return nil, &ParseError{Message: "unclosed argument list"}
		}

		arg, err := p.parseExprToAST()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)

		// Skip optional commas
		if p.matchToken(TOKEN_COMMA) {
			p.advanceToken()
		}
	}

	if !p.matchToken(TOKEN_RPAREN) {
		return nil, &ParseError{Message: "expected ) to close argument list"}
	}
	p.advanceToken() // consume )

	return args, nil
}

// parseListToAST parses an S-expression (list) to AST
func (p *Parser) parseListToAST() (ast.ASTNode, error) {
	startTok := p.currentToken()
	p.advanceToken() // consume (

	var elements []ast.ASTNode

	for !p.matchToken(TOKEN_RPAREN) {
		if p.matchToken(TOKEN_EOF) {
			return nil, &ParseError{Message: "unclosed list"}
		}

		expr, err := p.parseExprToAST()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)

		// Skip optional commas (Python-style)
		if p.matchToken(TOKEN_COMMA) {
			p.advanceToken()
		}
	}

	if !p.matchToken(TOKEN_RPAREN) {
		return nil, &ParseError{Message: "expected ) to close list"}
	}
	p.advanceToken() // consume )

	return p.makeSExpr(elements, startTok), nil
}

// parseVectorLiteralToAST parses a vector literal [1, 2, 3] to AST
func (p *Parser) parseVectorLiteralToAST() (ast.ASTNode, error) {
	startTok := p.currentToken()
	p.advanceToken() // consume [

	var elements []ast.ASTNode

	for !p.matchToken(TOKEN_RBRACKET) {
		if p.matchToken(TOKEN_EOF) {
			return nil, &ParseError{Message: "unclosed vector literal"}
		}

		expr, err := p.parseExprToAST()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)

		// Skip optional commas
		if p.matchToken(TOKEN_COMMA) {
			p.advanceToken()
		}
	}

	if !p.matchToken(TOKEN_RBRACKET) {
		return nil, &ParseError{Message: "expected ] to close vector literal"}
	}
	p.advanceToken() // consume ]

	// Build (list-literal elem1 elem2 ...)
	listLiteral := p.makeIdentifier("list-literal", startTok)
	allElements := append([]ast.ASTNode{listLiteral}, elements...)

	return p.makeSExpr(allElements, startTok), nil
}

// parseDictLiteralToAST parses a dict literal {"key": value} to AST
func (p *Parser) parseDictLiteralToAST() (ast.ASTNode, error) {
	startTok := p.currentToken()
	p.advanceToken() // consume {

	var elements []ast.ASTNode

	// TODO(M28-56d6): Handle set literals vs dict literals
	// For now, assume dict literal

	for !p.matchToken(TOKEN_RBRACE) {
		if p.matchToken(TOKEN_EOF) {
			return nil, &ParseError{Message: "unclosed dict literal"}
		}

		// Parse key
		key, err := p.parseExprToAST()
		if err != nil {
			return nil, err
		}

		// Expect :
		if !p.matchToken(TOKEN_COLON) {
			return nil, &ParseError{Message: "expected : after dict key"}
		}
		p.advanceToken()

		// Parse value
		value, err := p.parseExprToAST()
		if err != nil {
			return nil, err
		}

		elements = append(elements, key, value)

		// Skip optional commas
		if p.matchToken(TOKEN_COMMA) {
			p.advanceToken()
		}
	}

	if !p.matchToken(TOKEN_RBRACE) {
		return nil, &ParseError{Message: "expected } to close dict literal"}
	}
	p.advanceToken() // consume }

	// Build (dict-literal key1 value1 key2 value2 ...)
	dictLiteral := p.makeIdentifier("dict-literal", startTok)
	allElements := append([]ast.ASTNode{dictLiteral}, elements...)

	return p.makeSExpr(allElements, startTok), nil
}

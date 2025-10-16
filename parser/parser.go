// Package parser provides a parser for the M28 programming language.
package parser

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"

	"github.com/mmichie/m28/core"
)

// Parser parses M28 code into expressions
type Parser struct {
	input    string // The input string
	pos      int    // Current position in the input
	line     int    // Current line number
	col      int    // Current column number
	filename string // Current filename for error reporting

	// Token-based parsing fields
	tokens   []Token // Token stream from tokenizer
	tokenPos int     // Current position in token stream
}

// NewParser creates a new parser
func NewParser() *Parser {
	return &Parser{
		line: 1,
		col:  1,
	}
}

// SetFilename sets the filename for error reporting
func (p *Parser) SetFilename(filename string) {
	p.filename = filename
}

// Parse parses a string into a value
func (p *Parser) Parse(input string) (core.Value, error) {
	// Step 1: Tokenize the input
	tokenizer := NewTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return nil, fmt.Errorf("tokenization error: %w", err)
	}

	// Step 2: Set up parser with tokens
	p.tokens = tokens
	p.tokenPos = 0
	p.input = input // Keep for backward compatibility

	// Step 3: Parse the program
	return p.parseProgram()
}

// parseProgram parses a complete program (multiple expressions)
func (p *Parser) parseProgram() (core.Value, error) {
	expressions := make(core.ListValue, 0)

	// Parse expressions until EOF
	for !p.matchToken(TOKEN_EOF) {
		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		expressions = append(expressions, expr)
	}

	// If there's only one expression, return it directly
	if len(expressions) == 1 {
		return expressions[0], nil
	}

	// Check if expressions form an infix pattern at top level
	// This makes "1 + 2" work naturally at REPL and in files
	// Example: "1 + 2" is parsed as [1, +, 2] and becomes (+ 1 2)
	if detectInfixPattern(expressions) {
		result, err := parseInfixExpressionSimple(expressions)
		if err == nil {
			return result, nil
		}
		// If infix parsing fails, fall through to wrap in do
		// This allows things like (reduce + 0 list) to still work
	}

	// Otherwise, wrap expressions in an implicit do form
	return core.ListValue(append(
		[]core.Value{core.SymbolValue("do")},
		expressions...,
	)), nil
}

// parseExpr parses a single expression
func (p *Parser) parseExpr() (core.Value, error) {
	// Parse the primary expression (atom, list, etc.)
	base, err := p.parseAtomFromToken()
	if err != nil {
		return nil, err
	}

	// Now handle postfix operations (dots, calls, indexing)
	base, err = p.parsePostfixFromToken(base)
	if err != nil {
		return nil, err
	}

	// Check for infix operators (no whitespace before operator)
	// This allows "1+2" to work inside lists like (= x 1+2)
	if p.matchToken(TOKEN_EOF) || p.matchToken(TOKEN_RPAREN) || p.matchToken(TOKEN_RBRACKET) || p.matchToken(TOKEN_RBRACE) || p.matchToken(TOKEN_COMMA) {
		return base, nil
	}

	// Check if next token is an operator with no whitespace
	prevTok := p.tokens[p.tokenPos-1]
	tok := p.currentToken()

	if tok.IsOperator() && prevTok.EndPos == tok.StartPos {
		// This is an infix expression - collect all elements
		elements := []core.Value{base}

		for {
			// Add the operator
			opTok := p.advanceToken()
			elements = append(elements, core.SymbolValue(opTok.Lexeme))

			// Parse the right operand
			right, err := p.parseAtomFromToken()
			if err != nil {
				return nil, err
			}
			right, err = p.parsePostfixFromToken(right)
			if err != nil {
				return nil, err
			}
			elements = append(elements, right)

			// Check if there's another operator
			if p.matchToken(TOKEN_EOF) || p.matchToken(TOKEN_RPAREN) || p.matchToken(TOKEN_RBRACKET) || p.matchToken(TOKEN_RBRACE) || p.matchToken(TOKEN_COMMA) {
				break
			}

			prevTok = p.tokens[p.tokenPos-1]
			tok = p.currentToken()

			// Only continue if next operator has no whitespace before it
			if !tok.IsOperator() || prevTok.EndPos < tok.StartPos {
				break
			}
		}

		// Parse as infix expression
		if detectInfixPattern(elements) {
			result, err := parseInfixExpressionSimple(elements)
			if err == nil {
				return result, nil
			}
		}
	}

	return base, nil
}

// parseAtomFromToken parses an atomic expression using tokens
func (p *Parser) parseAtomFromToken() (core.Value, error) {
	tok := p.currentToken()

	switch tok.Type {
	// Literals - already parsed by tokenizer!
	case TOKEN_NUMBER:
		p.advanceToken()
		return tok.Value, nil

	case TOKEN_STRING:
		p.advanceToken()
		return tok.Value, nil

	case TOKEN_TRUE:
		p.advanceToken()
		return core.True, nil

	case TOKEN_FALSE:
		p.advanceToken()
		return core.False, nil

	case TOKEN_NIL:
		p.advanceToken()
		return core.None, nil

	case TOKEN_IDENTIFIER:
		p.advanceToken()
		// Identifiers are symbols
		return core.SymbolValue(tok.Lexeme), nil

	// Delimiters - delegate to specialized parsers
	case TOKEN_LPAREN:
		return p.parseListFromToken()

	case TOKEN_LBRACKET:
		return p.parseVectorLiteralFromToken()

	case TOKEN_LBRACE:
		return p.parseDictLiteralFromToken()

	// Special strings - need interpolation, so delegate to existing logic
	case TOKEN_FSTRING:
		p.advanceToken()
		// Parse f-string from its lexeme using existing logic
		return p.parseFStringFromLexeme(tok.Lexeme)

	case TOKEN_SSTRING:
		p.advanceToken()
		// Parse s-string from its lexeme using existing logic
		return p.parseSStringFromLexeme(tok.Lexeme)

	// Reader macros
	case TOKEN_BACKTICK:
		p.advanceToken()
		expr, err := p.parseExpr()
		if err != nil {
			return nil, p.tokenError(fmt.Sprintf("error parsing expression after backtick: %v", err), tok)
		}
		return core.ListValue{core.SymbolValue("quasiquote"), expr}, nil

	case TOKEN_COMMA:
		p.advanceToken()
		// Check if next token is @ for unquote-splicing
		if p.matchToken(TOKEN_AT) {
			p.advanceToken()
			expr, err := p.parseExpr()
			if err != nil {
				return nil, p.tokenError(fmt.Sprintf("error parsing expression after ,@: %v", err), tok)
			}
			return core.ListValue{core.SymbolValue("unquote-splicing"), expr}, nil
		}
		// Regular unquote
		expr, err := p.parseExpr()
		if err != nil {
			return nil, p.tokenError(fmt.Sprintf("error parsing expression after comma: %v", err), tok)
		}
		return core.ListValue{core.SymbolValue("unquote"), expr}, nil

	case TOKEN_COMMA_AT:
		p.advanceToken()
		expr, err := p.parseExpr()
		if err != nil {
			return nil, p.tokenError(fmt.Sprintf("error parsing expression after ,@: %v", err), tok)
		}
		return core.ListValue{core.SymbolValue("unquote-splicing"), expr}, nil

	// Dot by itself is a symbol
	case TOKEN_DOT:
		p.advanceToken()
		return core.SymbolValue("."), nil

	default:
		// Special case: unary minus followed immediately by a number is a negative literal
		if tok.Type == TOKEN_MINUS && !p.isAtEnd() {
			nextTok := p.peekToken()
			// Check if next token is a number with no whitespace
			if nextTok.Type == TOKEN_NUMBER && tok.EndPos == nextTok.StartPos {
				p.advanceToken() // consume -
				p.advanceToken() // consume number
				// Create negative number
				if num, ok := nextTok.Value.(core.NumberValue); ok {
					return core.NumberValue(-float64(num)), nil
				}
			}
		}

		// Handle operators and keywords as symbols when used in prefix position
		// This allows (+ 1 2), (== a b), (and x y), etc.
		if tok.IsOperator() || tok.IsKeyword() {
			p.advanceToken()
			return core.SymbolValue(tok.Lexeme), nil
		}
		return nil, p.tokenError(fmt.Sprintf("unexpected token %v", tok.Type), tok)
	}
}

// isAtEnd checks if we're at the end of tokens
func (p *Parser) isAtEnd() bool {
	return p.matchToken(TOKEN_EOF)
}

// parseUnaryOpFromToken parses unary operator expressions
func (p *Parser) parseUnaryOpFromToken() (core.Value, error) {
	op := p.advanceToken()
	operand, err := p.parseAtomFromToken()
	if err != nil {
		return nil, err
	}

	// Map token types to symbol names
	var opSymbol string
	switch op.Type {
	case TOKEN_MINUS:
		opSymbol = "-"
	case TOKEN_PLUS:
		opSymbol = "+"
	case TOKEN_NOT:
		opSymbol = "not"
	case TOKEN_TILDE:
		opSymbol = "~"
	default:
		opSymbol = op.Lexeme
	}

	// Build (op operand)
	return core.ListValue{core.SymbolValue(opSymbol), operand}, nil
}

// parseListFromToken parses a list expression (...) using tokens
func (p *Parser) parseListFromToken() (core.Value, error) {
	// Consume opening parenthesis
	if _, err := p.expectToken(TOKEN_LPAREN); err != nil {
		return nil, err
	}

	elements := make(core.ListValue, 0)

	// Parse elements until closing parenthesis
	for !p.matchToken(TOKEN_RPAREN) {
		if p.matchToken(TOKEN_EOF) {
			return nil, fmt.Errorf("unclosed list")
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)

		// Skip optional comma separators
		if p.matchToken(TOKEN_COMMA) {
			p.advanceToken()
		}
	}

	// Consume closing parenthesis
	if _, err := p.expectToken(TOKEN_RPAREN); err != nil {
		return nil, err
	}

	// Check if this is an infix expression
	// Pattern: (x op y) where second element is an infix operator
	if detectInfixPattern(elements) {
		result, err := parseInfixExpressionSimple(elements)
		if err == nil {
			return result, nil
		}
		// Fall through to treat as prefix if infix parsing failed
	}

	// Check if this is a generator expression
	if p.isGeneratorExpression(elements) {
		return p.parseGeneratorExpression(elements)
	}

	return elements, nil
}

// parseVectorLiteralFromToken parses a vector literal [...] using tokens
func (p *Parser) parseVectorLiteralFromToken() (core.Value, error) {
	// Consume opening bracket
	if _, err := p.expectToken(TOKEN_LBRACKET); err != nil {
		return nil, err
	}

	elements := make(core.ListValue, 0)

	// Parse elements until closing bracket
	for !p.matchToken(TOKEN_RBRACKET) {
		if p.matchToken(TOKEN_EOF) {
			return nil, fmt.Errorf("unclosed vector")
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)

		// Skip optional comma separators
		if p.matchToken(TOKEN_COMMA) {
			p.advanceToken()
		}
	}

	// Consume closing bracket
	if _, err := p.expectToken(TOKEN_RBRACKET); err != nil {
		return nil, err
	}

	// Check if this is a list comprehension
	if p.isListComprehension(elements) {
		return p.parseListComprehension(elements)
	}

	// Return a list-literal form that evaluates its contents
	return core.ListValue(append([]core.Value{core.SymbolValue("list-literal")}, elements...)), nil
}

// parseDictLiteralFromToken parses a dictionary literal {...} using tokens
func (p *Parser) parseDictLiteralFromToken() (core.Value, error) {
	// Consume opening brace
	if _, err := p.expectToken(TOKEN_LBRACE); err != nil {
		return nil, err
	}

	// Check for empty dict/set
	if p.matchToken(TOKEN_RBRACE) {
		p.advanceToken()
		// Empty {} is a dict
		return core.ListValue([]core.Value{core.SymbolValue("dict-literal")}), nil
	}

	elements := make(core.ListValue, 0)
	hasColon := false
	colonPositions := make(map[int]bool)

	// Parse all elements
	for !p.matchToken(TOKEN_RBRACE) {
		if p.matchToken(TOKEN_EOF) {
			return nil, fmt.Errorf("unclosed dict or set")
		}

		expr, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, expr)

		// Check for colon (dictionary syntax)
		if p.matchToken(TOKEN_COLON) {
			hasColon = true
			colonPositions[len(elements)-1] = true
			p.advanceToken()

			// Parse the value after colon
			value, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			elements = append(elements, value)
		}

		// Skip optional comma separators
		if p.matchToken(TOKEN_COMMA) {
			p.advanceToken()
		}
	}

	// Consume closing brace
	if _, err := p.expectToken(TOKEN_RBRACE); err != nil {
		return nil, err
	}

	// Check for comprehensions
	if hasColon {
		// Check for dict comprehension: {k: v for ...}
		if p.isDictComprehension(elements, colonPositions) {
			return p.parseDictComprehension(elements, colonPositions)
		}
		// Regular dict literal
		return core.ListValue(append([]core.Value{core.SymbolValue("dict-literal")}, elements...)), nil
	} else {
		// Check for set comprehension: {x for ...}
		if p.isSetComprehension(elements) {
			return p.parseSetComprehension(elements)
		}
		// Regular set literal
		listLiteral := append([]core.Value{core.SymbolValue("list-literal")}, elements...)
		return core.ListValue([]core.Value{
			core.SymbolValue("set"),
			core.ListValue(listLiteral),
		}), nil
	}
}

// parsePostfixFromToken handles postfix operations using tokens
func (p *Parser) parsePostfixFromToken(base core.Value) (core.Value, error) {
	for {
		// Check for postfix operators
		if p.matchToken(TOKEN_EOF) {
			return base, nil
		}

		// Get previous token to check for whitespace
		prevTok := p.tokens[p.tokenPos-1]
		tok := p.currentToken()

		// Check if there's whitespace between previous token and current token
		// If so, no postfix operators (e.g., "tuple [1, 2]" is a call, not indexing)
		if prevTok.EndPos < tok.StartPos {
			return base, nil
		}

		switch tok.Type {
		case TOKEN_DOT:
			// Property access or method call
			p.advanceToken() // consume DOT
			var err error
			base, err = p.parseDotAccessFromToken(base)
			if err != nil {
				return nil, err
			}

		case TOKEN_LBRACKET:
			// Index access or slicing
			var err error
			base, err = p.parseIndexAccessFromToken(base)
			if err != nil {
				return nil, err
			}

		default:
			// No more postfix operators
			return base, nil
		}
	}
}

// parseDotAccessFromToken handles dot notation using tokens
func (p *Parser) parseDotAccessFromToken(base core.Value) (core.Value, error) {
	// Current token should be an identifier or number (for numeric index)
	tok := p.currentToken()

	var propName string

	if tok.Type == TOKEN_NUMBER {
		// Numeric index like list.0
		propName = tok.Lexeme
		p.advanceToken()
	} else if tok.Type == TOKEN_IDENTIFIER {
		// Regular property/method name
		propName = tok.Lexeme
		p.advanceToken()
	} else {
		return nil, p.tokenError(fmt.Sprintf("expected property name after '.', got %v", tok.Type), tok)
	}

	// Check for method call (opening parenthesis immediately after property name)
	// Only treat as method call if there's no whitespace before the (
	if p.matchToken(TOKEN_LPAREN) {
		// Check for whitespace between property name and (
		prevTok := p.tokens[p.tokenPos-1]
		parenTok := p.currentToken()

		// If there's whitespace, it's not a method call
		if prevTok.EndPos < parenTok.StartPos {
			// Property access, not method call
			return core.ListValue{
				core.SymbolValue("."),
				base,
				core.StringValue(propName),
			}, nil
		}

		// It's a method call - parse arguments
		args, err := p.parseMethodArgsFromToken()
		if err != nil {
			return nil, err
		}

		// Build (. base method arg1 arg2...)
		result := core.ListValue{
			core.SymbolValue("."),
			base,
			core.StringValue(propName),
		}
		if len(args) == 0 {
			// Empty args, but still a method call
			result = append(result, core.SymbolValue("__call__"))
		} else {
			result = append(result, args...)
		}
		return result, nil
	}

	// Just property access - build (. base prop)
	return core.ListValue{
		core.SymbolValue("."),
		base,
		core.StringValue(propName),
	}, nil
}

// parseMethodArgsFromToken parses method call arguments using tokens
func (p *Parser) parseMethodArgsFromToken() ([]core.Value, error) {
	// Consume opening parenthesis
	if _, err := p.expectToken(TOKEN_LPAREN); err != nil {
		return nil, err
	}

	var args []core.Value

	// Check for empty args
	if p.matchToken(TOKEN_RPAREN) {
		p.advanceToken()
		return args, nil
	}

	// Parse comma-separated arguments
	for {
		arg, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)

		// Check what's next
		if p.matchToken(TOKEN_RPAREN) {
			p.advanceToken()
			break
		}

		if !p.matchToken(TOKEN_COMMA) {
			tok := p.currentToken()
			return nil, p.tokenError(fmt.Sprintf("expected ',' or ')' in method call, got %v", tok.Type), tok)
		}
		p.advanceToken() // consume comma
	}

	return args, nil
}

// parseIndexAccessFromToken handles indexing and slicing using tokens
func (p *Parser) parseIndexAccessFromToken(base core.Value) (core.Value, error) {
	// Consume opening bracket
	if _, err := p.expectToken(TOKEN_LBRACKET); err != nil {
		return nil, err
	}

	// Check for slice syntax with empty start [:end]
	if p.matchToken(TOKEN_COLON) {
		return p.parseSliceFromToken(base, nil)
	}

	// Parse the first expression (index or slice start)
	// Handle unary minus for negative indices
	first, err := p.parseIndexOrSliceValue()
	if err != nil {
		return nil, fmt.Errorf("error parsing index: %v", err)
	}

	// Check what follows
	if p.matchToken(TOKEN_RBRACKET) {
		// Simple index access
		p.advanceToken()
		return core.ListValue{
			core.SymbolValue("get-item"),
			base,
			first,
		}, nil
	}

	if p.matchToken(TOKEN_COLON) {
		// Slice syntax
		return p.parseSliceFromToken(base, first)
	}

	tok := p.currentToken()
	return nil, p.tokenError(fmt.Sprintf("expected ']' or ':' after index, got %v", tok.Type), tok)
}

// parseIndexOrSliceValue parses a value in index/slice context, handling unary operators
func (p *Parser) parseIndexOrSliceValue() (core.Value, error) {
	tok := p.currentToken()

	// Handle unary minus for negative indices
	if tok.Type == TOKEN_MINUS {
		p.advanceToken()
		val, err := p.parseIndexOrSliceValue()
		if err != nil {
			return nil, err
		}
		// Build (- val)
		return core.ListValue{core.SymbolValue("-"), val}, nil
	}

	// Otherwise use standard atom parsing
	return p.parseAtomFromToken()
}

// parseSliceFromToken handles slice syntax using tokens
func (p *Parser) parseSliceFromToken(base core.Value, start core.Value) (core.Value, error) {
	// Consume colon
	p.advanceToken()

	var end core.Value
	var step core.Value

	// Check for end value
	if !p.matchToken(TOKEN_RBRACKET) && !p.matchToken(TOKEN_COLON) {
		var err error
		end, err = p.parseIndexOrSliceValue()
		if err != nil {
			return nil, fmt.Errorf("error parsing slice end: %v", err)
		}
	}

	// Check for step value
	if p.matchToken(TOKEN_COLON) {
		p.advanceToken()
		if !p.matchToken(TOKEN_RBRACKET) {
			var err error
			step, err = p.parseIndexOrSliceValue()
			if err != nil {
				return nil, fmt.Errorf("error parsing slice step: %v", err)
			}
		}
	}

	// Consume closing bracket
	if _, err := p.expectToken(TOKEN_RBRACKET); err != nil {
		return nil, err
	}

	// Build slice form: (__slice__ base start end step)
	// Always provide all 4 arguments (base, start, end, step)
	result := core.ListValue{core.SymbolValue("__slice__"), base}

	if start != nil {
		result = append(result, start)
	} else {
		result = append(result, core.None)
	}

	if end != nil {
		result = append(result, end)
	} else {
		result = append(result, core.None)
	}

	if step != nil {
		result = append(result, step)
	} else {
		result = append(result, core.None)
	}

	return result, nil
}

// parseFStringFromLexeme parses an f-string from its full lexeme (e.g., f"hello {x}")
func (p *Parser) parseFStringFromLexeme(lexeme string) (core.Value, error) {
	// Save current parser state
	savedInput := p.input
	savedPos := p.pos
	savedLine := p.line
	savedCol := p.col

	// Set up parser to parse the lexeme
	p.input = lexeme
	p.pos = 0
	p.line = 1
	p.col = 1

	// Determine quote character
	var quoteChar rune
	if len(lexeme) > 1 && lexeme[1] == '"' {
		quoteChar = '"'
	} else if len(lexeme) > 1 && lexeme[1] == '\'' {
		quoteChar = '\''
	} else {
		// Restore state
		p.input = savedInput
		p.pos = savedPos
		p.line = savedLine
		p.col = savedCol
		return nil, fmt.Errorf("invalid f-string lexeme: %s", lexeme)
	}

	// Parse using existing f-string logic
	result, err := p.parseFStringEnhancedSimple(quoteChar)

	// Restore parser state
	p.input = savedInput
	p.pos = savedPos
	p.line = savedLine
	p.col = savedCol

	return result, err
}

// parseSStringFromLexeme parses an s-string from its full lexeme (e.g., s"hello\nworld" or rs"raw\nstring")
func (p *Parser) parseSStringFromLexeme(lexeme string) (core.Value, error) {
	// Save current parser state
	savedInput := p.input
	savedPos := p.pos
	savedLine := p.line
	savedCol := p.col

	// Set up parser to parse the lexeme
	p.input = lexeme
	p.pos = 0
	p.line = 1
	p.col = 1

	// Check if it's raw s-string or regular s-string
	isRaw := false
	startPos := 0
	if len(lexeme) > 2 && lexeme[0] == 'r' && lexeme[1] == 's' {
		isRaw = true
		startPos = 2
	} else if len(lexeme) > 1 && lexeme[0] == 's' {
		startPos = 1
	} else {
		// Restore state
		p.input = savedInput
		p.pos = savedPos
		p.line = savedLine
		p.col = savedCol
		return nil, fmt.Errorf("invalid s-string lexeme: %s", lexeme)
	}

	// Determine quote character
	var quoteChar rune
	if startPos < len(lexeme) && lexeme[startPos] == '"' {
		quoteChar = '"'
	} else if startPos < len(lexeme) && lexeme[startPos] == '\'' {
		quoteChar = '\''
	} else {
		// Restore state
		p.input = savedInput
		p.pos = savedPos
		p.line = savedLine
		p.col = savedCol
		return nil, fmt.Errorf("invalid s-string lexeme: %s", lexeme)
	}

	// Parse using existing s-string logic
	result, err := p.parseSString(quoteChar, isRaw)

	// Restore parser state
	p.input = savedInput
	p.pos = savedPos
	p.line = savedLine
	p.col = savedCol

	return result, err
}

// parsePostfix handles postfix operations like dot notation, function calls, etc.
func (p *Parser) parsePostfix(base core.Value) (core.Value, error) {
	for {
		// Save current position to check if we moved past whitespace
		startPos := p.pos
		p.skipWhitespaceAndComments()

		if p.pos >= len(p.input) {
			return base, nil
		}

		// If we skipped any whitespace/comments, no postfix operators
		// This ensures "x [" is not treated as "x["
		if p.pos > startPos {
			return base, nil
		}

		switch p.input[p.pos] {
		case '.':
			// Check if it's a float continuation
			if num, ok := base.(core.NumberValue); ok {
				// This could be a decimal number continuation
				if p.pos+1 < len(p.input) && isDigit(p.input[p.pos+1]) {
					// This is actually a decimal number, not dot notation
					return base, nil
				}
				// Numbers can't have properties
				return nil, p.error(fmt.Sprintf("number %v has no properties", num))
			}

			// Property access or method call
			p.pos++ // consume '.'
			var err error
			base, err = p.parseDotAccess(base)
			if err != nil {
				return nil, err
			}

		// Note: Direct function calls on expressions not yet supported
		// case '(':
		// 	// Direct function call on the result
		// 	base, err = p.parseFunctionCall(base)
		// 	if err != nil {
		// 		return nil, err
		// 	}

		case '[':
			// Index access
			p.pos++ // consume '['
			var err error
			base, err = p.parseIndexAccess(base)
			if err != nil {
				return nil, err
			}

		default:
			return base, nil
		}
	}
}

// parseAtom parses a single atomic expression (without dot notation)
func (p *Parser) parseAtom() (core.Value, error) {
	p.skipWhitespaceAndComments()

	// Check for end of input
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unexpected end of input")
	}

	// Check for reader macros: backtick (quasiquote), comma (unquote/unquote-splicing)
	if p.input[p.pos] == '`' {
		return p.parseBacktickQuasiquote()
	}

	if p.input[p.pos] == ',' {
		return p.parseCommaUnquote()
	}

	// Check for f-string
	if p.pos+1 < len(p.input) && p.input[p.pos] == 'f' && (p.input[p.pos+1] == '"' || p.input[p.pos+1] == '\'') {
		// Use simpler enhanced f-string parser for now
		return p.parseFStringEnhancedSimple(rune(p.input[p.pos+1]))
	}

	// Check for s-string (s"..." or s'...')
	if p.pos+1 < len(p.input) && p.input[p.pos] == 's' && (p.input[p.pos+1] == '"' || p.input[p.pos+1] == '\'') {
		return p.parseSString(rune(p.input[p.pos+1]), false)
	}

	// Check for raw s-string (rs"..." or rs'...')
	if p.pos+2 < len(p.input) && p.input[p.pos] == 'r' && p.input[p.pos+1] == 's' && (p.input[p.pos+2] == '"' || p.input[p.pos+2] == '\'') {
		return p.parseSString(rune(p.input[p.pos+2]), true)
	}

	// Check what kind of expression we have
	switch p.input[p.pos] {
	case '(':
		return p.parseList()
	case '"', '\'':
		return p.parseString()
	case '[':
		return p.parseVectorLiteral()
	case '{':
		return p.parseDictLiteral()
	case '.':
		// Special handling for dot symbol
		p.advance()
		return core.SymbolValue("."), nil
	case '+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		if p.isNumberStart(p.input[p.pos]) {
			return p.parseNumber()
		}
		fallthrough
	default:
		// Try to parse a symbol or keyword
		return p.parseSymbolOrKeyword()
	}
}

// parseList parses a list expression (...) or generator expression
func (p *Parser) parseList() (core.Value, error) {
	// Skip opening parenthesis
	p.advance()

	elements := make(core.ListValue, 0)

	// Skip whitespace after the opening paren
	p.skipWhitespaceAndComments()

	// Parse elements until we hit the closing parenthesis
	for p.pos < len(p.input) && p.input[p.pos] != ')' {
		element, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		elements = append(elements, element)
		p.skipWhitespaceAndComments()

		// Skip optional comma as separator, but not if it's a reader macro
		// A comma followed immediately by non-whitespace (except closing delimiters) is a reader macro
		if p.pos < len(p.input) && p.input[p.pos] == ',' {
			nextPos := p.pos + 1
			// Skip comma if:
			// 1. Followed by whitespace (normal separator)
			// 2. Followed by closing delimiter ) ] } (trailing comma)
			// 3. At end of input
			if nextPos >= len(p.input) ||
				unicode.IsSpace(rune(p.input[nextPos])) ||
				p.input[nextPos] == ')' ||
				p.input[nextPos] == ']' ||
				p.input[nextPos] == '}' {
				p.advance()
				p.skipWhitespaceAndComments()
			}
			// Otherwise, don't skip it - let parseExpr() handle it as a reader macro (e.g., ,x or ,@x)
		}
	}

	// Check for unclosed list
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unclosed list")
	}

	// Skip closing parenthesis
	p.advance()

	// Check if this is an infix expression
	// Pattern: (x op y) where second element is an infix operator
	// If infix parsing fails, fall back to prefix (allows functions like (reduce + 0 lst))
	if detectInfixPattern(elements) {
		result, err := parseInfixExpressionSimple(elements)
		if err == nil {
			return result, nil
		}
		// Fall through to treat as prefix if infix parsing failed
	}

	// Check if this is a generator expression
	// Pattern: (expr for var in iterable) or (expr for var in iterable if condition)
	if p.isGeneratorExpression(elements) {
		return p.parseGeneratorExpression(elements)
	}

	return elements, nil
}

// parseVectorLiteral parses a vector literal [...] or list comprehension
func (p *Parser) parseVectorLiteral() (core.Value, error) {
	// Skip opening bracket
	p.advance()

	elements := make(core.ListValue, 0)

	// Skip whitespace after the opening bracket
	p.skipWhitespaceAndComments()

	// Parse elements until we hit the closing bracket
	for p.pos < len(p.input) && p.input[p.pos] != ']' {
		element, err := p.parseExpr()
		if err != nil {
			return nil, err
		}

		elements = append(elements, element)
		p.skipWhitespaceAndComments()

		// Skip optional comma as separator, but not if it's a reader macro
		if p.pos < len(p.input) && p.input[p.pos] == ',' {
			nextPos := p.pos + 1
			if nextPos >= len(p.input) ||
				unicode.IsSpace(rune(p.input[nextPos])) ||
				p.input[nextPos] == ')' ||
				p.input[nextPos] == ']' ||
				p.input[nextPos] == '}' {
				p.advance()
				p.skipWhitespaceAndComments()
			}
		}
	}

	// Check for unclosed vector
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unclosed vector")
	}

	// Skip closing bracket
	p.advance()

	// Check if this is a list comprehension
	// Pattern: [expr for var in iterable] or [expr for var in iterable if condition]
	if p.isListComprehension(elements) {
		// List comprehensions return an evaluable form, not a quoted literal
		return p.parseListComprehension(elements)
	}

	// Return a list-literal form that evaluates its contents
	// This allows expressions inside [...] to be evaluated
	return core.ListValue(append([]core.Value{core.SymbolValue("list-literal")}, elements...)), nil
}

// parseDictLiteral parses a dictionary literal {...}, set literal {1, 2, 3},
// dict comprehension {k: v for ...}, or set comprehension {x for ...}
func (p *Parser) parseDictLiteral() (core.Value, error) {
	// Skip opening brace
	p.advance()

	// Skip whitespace after the opening brace
	p.skipWhitespaceAndComments()

	// Check for empty dict/set
	if p.pos < len(p.input) && p.input[p.pos] == '}' {
		p.advance()
		// Empty {} is a dict
		return core.ListValue([]core.Value{core.SymbolValue("dict-literal")}), nil
	}

	// Parse all elements first to determine what kind of structure this is
	elements := make(core.ListValue, 0)
	hasColon := false
	colonPositions := make(map[int]bool) // Track which element positions have colons after them

	for p.pos < len(p.input) && p.input[p.pos] != '}' {
		element, err := p.parseExpr()
		if err != nil {
			return nil, err
		}
		elements = append(elements, element)

		p.skipWhitespaceAndComments()

		// Check for colon (dictionary syntax)
		if p.pos < len(p.input) && p.input[p.pos] == ':' {
			hasColon = true
			colonPositions[len(elements)-1] = true
			p.advance()
			p.skipWhitespaceAndComments()

			// Parse the value after colon
			value, err := p.parseExpr()
			if err != nil {
				return nil, err
			}
			elements = append(elements, value)
			p.skipWhitespaceAndComments()
		}

		// Skip optional comma as separator, but not if it's a reader macro
		if p.pos < len(p.input) && p.input[p.pos] == ',' {
			nextPos := p.pos + 1
			if nextPos >= len(p.input) ||
				unicode.IsSpace(rune(p.input[nextPos])) ||
				p.input[nextPos] == ')' ||
				p.input[nextPos] == ']' ||
				p.input[nextPos] == '}' {
				p.advance()
				p.skipWhitespaceAndComments()
			}
		}
	}

	// Check for unclosed brace
	if p.pos >= len(p.input) {
		return nil, fmt.Errorf("unclosed dict or set")
	}

	// Skip closing brace
	p.advance()

	// Check for comprehensions
	if hasColon {
		// Check for dict comprehension: {k: v for ...}
		if p.isDictComprehension(elements, colonPositions) {
			return p.parseDictComprehension(elements, colonPositions)
		}
		// Regular dict literal
		return core.ListValue(append([]core.Value{core.SymbolValue("dict-literal")}, elements...)), nil
	} else {
		// Check for set comprehension: {x for ...}
		if p.isSetComprehension(elements) {
			return p.parseSetComprehension(elements)
		}
		// Regular set literal
		// Return (set (list-literal elements...))
		listLiteral := append([]core.Value{core.SymbolValue("list-literal")}, elements...)
		return core.ListValue([]core.Value{
			core.SymbolValue("set"),
			core.ListValue(listLiteral),
		}), nil
	}
}

// parseString parses a string literal "..." or '...'
func (p *Parser) parseString() (core.Value, error) {
	// Remember the opening quote character
	quoteChar := p.input[p.pos]

	// Skip opening quote
	p.advance()

	var builder strings.Builder
	escaped := false

	// Parse characters until we hit the closing quote
	for p.pos < len(p.input) {
		ch := p.input[p.pos]

		if escaped {
			// Handle escaped characters
			switch ch {
			case 'n':
				builder.WriteByte('\n')
			case 't':
				builder.WriteByte('\t')
			case 'r':
				builder.WriteByte('\r')
			case '"':
				builder.WriteByte('"')
			case '\'':
				builder.WriteByte('\'')
			case '\\':
				builder.WriteByte('\\')
			case 'b':
				builder.WriteByte('\b')
			case 'f':
				builder.WriteByte('\f')
			case 'v':
				builder.WriteByte('\v')
			case '0':
				builder.WriteByte('\000')
			case 'u':
				// Unicode escape: \uXXXX
				if p.pos+4 < len(p.input) {
					hexStr := p.input[p.pos+1 : p.pos+5]
					if codePoint, err := strconv.ParseInt(hexStr, 16, 32); err == nil {
						builder.WriteRune(rune(codePoint))
						p.pos += 4 // Skip the 4 hex digits
					} else {
						// Invalid Unicode escape, just include literally
						builder.WriteByte(ch)
					}
				} else {
					// Not enough characters for Unicode escape
					builder.WriteByte(ch)
				}
			case 'U':
				// Unicode escape: \UXXXXXXXX
				if p.pos+8 < len(p.input) {
					hexStr := p.input[p.pos+1 : p.pos+9]
					if codePoint, err := strconv.ParseInt(hexStr, 16, 32); err == nil {
						builder.WriteRune(rune(codePoint))
						p.pos += 8 // Skip the 8 hex digits
					} else {
						// Invalid Unicode escape, just include literally
						builder.WriteByte(ch)
					}
				} else {
					// Not enough characters for Unicode escape
					builder.WriteByte(ch)
				}
			default:
				// For any other character, just include it literally
				// This allows for escaping any character
				builder.WriteByte(ch)
			}
			escaped = false
		} else {
			// Handle regular characters
			if ch == '\\' {
				escaped = true
			} else if ch == quoteChar {
				// End of string (matching quote)
				p.advance()
				return core.StringValue(builder.String()), nil
			} else {
				builder.WriteByte(ch)
			}
		}

		p.advance()
	}

	return nil, fmt.Errorf("unclosed string")
}

// parseFString parses an f-string literal
func (p *Parser) parseFString() (core.Value, error) {
	// Skip 'f'
	p.advance() // skip 'f'

	// Remember the quote character
	quoteChar := p.input[p.pos]
	p.advance() // skip quote

	var parts []core.Value
	var currentString strings.Builder
	escaped := false

	// Parse characters until closing quote
	for p.pos < len(p.input) {
		ch := p.input[p.pos]

		if escaped {
			// Handle escaped characters
			switch ch {
			case 'n':
				currentString.WriteByte('\n')
			case 't':
				currentString.WriteByte('\t')
			case 'r':
				currentString.WriteByte('\r')
			case '"':
				currentString.WriteByte('"')
			case '\'':
				currentString.WriteByte('\'')
			case '\\':
				currentString.WriteByte('\\')
			case '{':
				currentString.WriteByte('{')
			case '}':
				currentString.WriteByte('}')
			case 'b':
				currentString.WriteByte('\b')
			case 'f':
				currentString.WriteByte('\f')
			case 'v':
				currentString.WriteByte('\v')
			case '0':
				currentString.WriteByte('\000')
			case 'u':
				// Unicode escape: \uXXXX
				if p.pos+4 < len(p.input) {
					hexStr := p.input[p.pos+1 : p.pos+5]
					if codePoint, err := strconv.ParseInt(hexStr, 16, 32); err == nil {
						currentString.WriteRune(rune(codePoint))
						p.pos += 4 // Skip the 4 hex digits
					} else {
						// Invalid Unicode escape, just include literally
						currentString.WriteByte(ch)
					}
				} else {
					// Not enough characters for Unicode escape
					currentString.WriteByte(ch)
				}
			case 'U':
				// Unicode escape: \UXXXXXXXX
				if p.pos+8 < len(p.input) {
					hexStr := p.input[p.pos+1 : p.pos+9]
					if codePoint, err := strconv.ParseInt(hexStr, 16, 32); err == nil {
						currentString.WriteRune(rune(codePoint))
						p.pos += 8 // Skip the 8 hex digits
					} else {
						// Invalid Unicode escape, just include literally
						currentString.WriteByte(ch)
					}
				} else {
					// Not enough characters for Unicode escape
					currentString.WriteByte(ch)
				}
			default:
				// For any other character, just include it literally
				// This allows for escaping any character
				currentString.WriteByte(ch)
			}
			escaped = false
			p.advance()
		} else if ch == '\\' {
			escaped = true
			p.advance()
		} else if ch == quoteChar {
			// End of f-string
			if currentString.Len() > 0 {
				parts = append(parts, core.StringValue(currentString.String()))
			}
			p.advance()

			// Return a format expression
			if len(parts) == 0 {
				return core.StringValue(""), nil
			} else if len(parts) == 1 {
				return parts[0], nil
			}

			// Create (str-format part1 part2 ...)
			formatExpr := make(core.ListValue, 0, len(parts)+1)
			formatExpr = append(formatExpr, core.SymbolValue("str-format"))
			formatExpr = append(formatExpr, parts...)
			return formatExpr, nil
		} else if ch == '{' {
			// Start of expression
			if currentString.Len() > 0 {
				parts = append(parts, core.StringValue(currentString.String()))
				currentString.Reset()
			}

			// Skip '{'
			p.advance()

			// Parse expression until '}'
			expr, err := p.parseExpr()
			if err != nil {
				return nil, fmt.Errorf("error parsing f-string expression at line %d, column %d: %v", p.line, p.col, err)
			}

			// Skip whitespace before '}'
			p.skipWhitespaceAndComments()

			if p.pos >= len(p.input) || p.input[p.pos] != '}' {
				return nil, fmt.Errorf("unclosed f-string expression at line %d, column %d", p.line, p.col)
			}

			// Skip '}'
			p.advance()

			// Add expression to parts
			parts = append(parts, expr)
		} else {
			currentString.WriteByte(ch)
			p.advance()
		}
	}

	return nil, fmt.Errorf("unclosed f-string at line %d, column %d", p.line, p.col)
}

// isListComprehension checks if the elements form a list comprehension pattern
func (p *Parser) isListComprehension(elements core.ListValue) bool {
	// Need at least 5 elements: expr for var in iterable
	if len(elements) < 5 {
		return false
	}

	// Look for "for" and "in" keywords
	hasFor := false
	hasIn := false
	forIndex := -1

	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && !hasFor {
				hasFor = true
				forIndex = i
			} else if string(sym) == "in" && hasFor && i > forIndex {
				hasIn = true
			}
		}
	}

	// Must have both "for" and "in", and "for" must not be the first element
	return hasFor && hasIn && forIndex > 0
}

// parseListComprehension converts comprehension elements into a comprehension form
func (p *Parser) parseListComprehension(elements core.ListValue) (core.Value, error) {
	// Find the positions of "for" and "in"
	forIndex := -1
	inIndex := -1

	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && forIndex == -1 {
				forIndex = i
			} else if string(sym) == "in" && forIndex != -1 && inIndex == -1 {
				inIndex = i
			}
		}
	}

	// Extract parts
	// expr: elements[0:forIndex]
	// var: elements[forIndex+1:inIndex]
	// iterable: elements[inIndex+1:]

	if forIndex <= 0 || inIndex <= forIndex+1 {
		return nil, fmt.Errorf("invalid list comprehension syntax")
	}

	// Get the expression (everything before "for")
	var expr core.Value
	if forIndex == 1 {
		// Single element - check if it's a list that should be unquoted
		if list, ok := elements[0].(core.ListValue); ok && len(list) > 0 {
			// This was a parenthesized expression like (* x 2)
			// It's already a list, use it directly as an expression
			expr = list
		} else {
			expr = elements[0]
		}
	} else {
		// Multiple elements - wrap in a do block
		exprElements := make(core.ListValue, forIndex+1)
		exprElements[0] = core.SymbolValue("do")
		copy(exprElements[1:], elements[:forIndex])
		expr = exprElements
	}

	// Get the variable (between "for" and "in")
	if inIndex != forIndex+2 {
		return nil, fmt.Errorf("list comprehension variable must be a single symbol")
	}
	variable := elements[forIndex+1]

	// Get the iterable (everything after "in")
	var iterable core.Value
	remainingElements := elements[inIndex+1:]

	// Check for "if" condition
	ifIndex := -1
	for i, elem := range remainingElements {
		if sym, ok := elem.(core.SymbolValue); ok && string(sym) == "if" {
			ifIndex = i
			break
		}
	}

	var condition core.Value
	if ifIndex >= 0 {
		// Has condition
		if ifIndex == 0 {
			return nil, fmt.Errorf("missing iterable in list comprehension")
		}

		// Iterable is everything before "if"
		if ifIndex == 1 {
			// Check if it's a list that should be unquoted
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, ifIndex+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements[:ifIndex])
			iterable = iterElements
		}

		// Condition is everything after "if"
		condElements := remainingElements[ifIndex+1:]
		if len(condElements) == 0 {
			return nil, fmt.Errorf("missing condition after 'if' in list comprehension")
		} else if len(condElements) == 1 {
			// Check if it's a list that should be unquoted
			if list, ok := condElements[0].(core.ListValue); ok && len(list) > 0 {
				condition = list
			} else {
				condition = condElements[0]
			}
		} else {
			condExpr := make(core.ListValue, len(condElements)+1)
			condExpr[0] = core.SymbolValue("do")
			copy(condExpr[1:], condElements)
			condition = condExpr
		}
	} else {
		// No condition
		if len(remainingElements) == 1 {
			// Check if it's a list that should be unquoted
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, len(remainingElements)+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements)
			iterable = iterElements
		}
	}

	// Build the list-comp form
	// (list-comp expr var iterable) or (list-comp expr var iterable condition)
	result := make(core.ListValue, 0, 5)
	result = append(result, core.SymbolValue("list-comp"))
	result = append(result, expr)
	result = append(result, variable)
	result = append(result, iterable)
	if condition != nil {
		result = append(result, condition)
	}

	return result, nil
}

// isGeneratorExpression checks if the elements form a generator expression pattern
// Pattern: (expr for var in iterable) or (expr for var in iterable if condition)
func (p *Parser) isGeneratorExpression(elements core.ListValue) bool {
	// Need at least 5 elements: expr for var in iterable
	if len(elements) < 5 {
		return false
	}

	// Look for "for" and "in" keywords
	hasFor := false
	hasIn := false
	forIndex := -1

	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && !hasFor {
				hasFor = true
				forIndex = i
			} else if string(sym) == "in" && hasFor && i > forIndex {
				hasIn = true
			}
		}
	}

	// Must have both "for" and "in", and "for" must not be the first element
	return hasFor && hasIn && forIndex > 0
}

// parseGeneratorExpression converts comprehension elements into a generator expression form
// Pattern: (expr for var in iterable) or (expr for var in iterable if condition)
// Returns: (gen-expr expr var iterable) or (gen-expr expr var iterable condition)
func (p *Parser) parseGeneratorExpression(elements core.ListValue) (core.Value, error) {
	// Find the positions of "for" and "in"
	forIndex := -1
	inIndex := -1

	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && forIndex == -1 {
				forIndex = i
			} else if string(sym) == "in" && forIndex != -1 && inIndex == -1 {
				inIndex = i
			}
		}
	}

	if forIndex <= 0 || inIndex <= forIndex+1 {
		return nil, fmt.Errorf("invalid generator expression syntax")
	}

	// Get the expression (everything before "for")
	var expr core.Value
	if forIndex == 1 {
		// Single element
		if list, ok := elements[0].(core.ListValue); ok && len(list) > 0 {
			expr = list
		} else {
			expr = elements[0]
		}
	} else {
		// Multiple elements - wrap in a do block
		exprElements := make(core.ListValue, forIndex+1)
		exprElements[0] = core.SymbolValue("do")
		copy(exprElements[1:], elements[:forIndex])
		expr = exprElements
	}

	// Get the variable (between "for" and "in")
	if inIndex-forIndex != 2 {
		return nil, fmt.Errorf("generator expression requires single variable between 'for' and 'in'")
	}
	variable := elements[forIndex+1]

	// Everything after "in" is either the iterable or iterable + condition
	remainingElements := elements[inIndex+1:]

	// Look for "if" keyword
	var iterable core.Value
	ifIndex := -1
	for i, elem := range remainingElements {
		if sym, ok := elem.(core.SymbolValue); ok && string(sym) == "if" {
			ifIndex = i
			break
		}
	}

	var condition core.Value
	if ifIndex >= 0 {
		// Has condition
		if ifIndex == 0 {
			return nil, fmt.Errorf("missing iterable in generator expression")
		}

		// Iterable is everything before "if"
		if ifIndex == 1 {
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, ifIndex+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements[:ifIndex])
			iterable = iterElements
		}

		// Condition is everything after "if"
		condElements := remainingElements[ifIndex+1:]
		if len(condElements) == 0 {
			return nil, fmt.Errorf("missing condition after 'if' in generator expression")
		} else if len(condElements) == 1 {
			if list, ok := condElements[0].(core.ListValue); ok && len(list) > 0 {
				condition = list
			} else {
				condition = condElements[0]
			}
		} else {
			condExpr := make(core.ListValue, len(condElements)+1)
			condExpr[0] = core.SymbolValue("do")
			copy(condExpr[1:], condElements)
			condition = condExpr
		}
	} else {
		// No condition
		if len(remainingElements) == 1 {
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, len(remainingElements)+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements)
			iterable = iterElements
		}
	}

	// Build the gen-expr form
	// (gen-expr expr var iterable) or (gen-expr expr var iterable condition)
	result := make(core.ListValue, 0, 5)
	result = append(result, core.SymbolValue("gen-expr"))
	result = append(result, expr)
	result = append(result, variable)
	result = append(result, iterable)
	if condition != nil {
		result = append(result, condition)
	}

	return result, nil
}

// isSetComprehension checks if the elements form a set comprehension pattern
// Pattern: {expr for var in iterable} or {expr for var in iterable if condition}
func (p *Parser) isSetComprehension(elements core.ListValue) bool {
	// Need at least 5 elements: expr for var in iterable
	if len(elements) < 5 {
		return false
	}

	// Look for "for" and "in" keywords
	hasFor := false
	hasIn := false
	forIndex := -1

	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && !hasFor {
				hasFor = true
				forIndex = i
			} else if string(sym) == "in" && hasFor && i > forIndex {
				hasIn = true
			}
		}
	}

	// Must have both "for" and "in", and "for" must not be the first element
	return hasFor && hasIn && forIndex > 0
}

// parseSetComprehension converts set comprehension elements into a comprehension form
// Pattern: {expr for var in iterable} -> (set-comp expr var iterable)
// Pattern: {expr for var in iterable if condition} -> (set-comp expr var iterable condition)
func (p *Parser) parseSetComprehension(elements core.ListValue) (core.Value, error) {
	// Reuse the same logic as list comprehension parsing
	// Find the positions of "for" and "in"
	forIndex := -1
	inIndex := -1

	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && forIndex == -1 {
				forIndex = i
			} else if string(sym) == "in" && forIndex != -1 && inIndex == -1 {
				inIndex = i
			}
		}
	}

	if forIndex <= 0 || inIndex <= forIndex+1 {
		return nil, fmt.Errorf("invalid set comprehension syntax")
	}

	// Get the expression (everything before "for")
	var expr core.Value
	if forIndex == 1 {
		if list, ok := elements[0].(core.ListValue); ok && len(list) > 0 {
			expr = list
		} else {
			expr = elements[0]
		}
	} else {
		exprElements := make(core.ListValue, forIndex+1)
		exprElements[0] = core.SymbolValue("do")
		copy(exprElements[1:], elements[:forIndex])
		expr = exprElements
	}

	// Get the variable (between "for" and "in")
	if inIndex != forIndex+2 {
		return nil, fmt.Errorf("set comprehension variable must be a single symbol")
	}
	variable := elements[forIndex+1]

	// Get the iterable and optional condition (same as list comprehension)
	var iterable core.Value
	remainingElements := elements[inIndex+1:]

	// Check for "if" condition
	ifIndex := -1
	for i, elem := range remainingElements {
		if sym, ok := elem.(core.SymbolValue); ok && string(sym) == "if" {
			ifIndex = i
			break
		}
	}

	var condition core.Value
	if ifIndex >= 0 {
		if ifIndex == 0 {
			return nil, fmt.Errorf("missing iterable in set comprehension")
		}

		if ifIndex == 1 {
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, ifIndex+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements[:ifIndex])
			iterable = iterElements
		}

		condElements := remainingElements[ifIndex+1:]
		if len(condElements) == 0 {
			return nil, fmt.Errorf("missing condition after 'if' in set comprehension")
		} else if len(condElements) == 1 {
			if list, ok := condElements[0].(core.ListValue); ok && len(list) > 0 {
				condition = list
			} else {
				condition = condElements[0]
			}
		} else {
			condExpr := make(core.ListValue, len(condElements)+1)
			condExpr[0] = core.SymbolValue("do")
			copy(condExpr[1:], condElements)
			condition = condExpr
		}
	} else {
		if len(remainingElements) == 1 {
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, len(remainingElements)+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements)
			iterable = iterElements
		}
	}

	// Build the set-comp form
	result := make(core.ListValue, 0, 5)
	result = append(result, core.SymbolValue("set-comp"))
	result = append(result, expr)
	result = append(result, variable)
	result = append(result, iterable)
	if condition != nil {
		result = append(result, condition)
	}

	return result, nil
}

// isDictComprehension checks if elements form a dict comprehension pattern
// Pattern: {k: v for var in iterable} or {k: v for var in iterable if condition}
func (p *Parser) isDictComprehension(elements core.ListValue, colonPositions map[int]bool) bool {
	// Need at least 6 elements: key value for var in iterable
	// (colon is consumed during parsing, so it's not in elements)
	if len(elements) < 6 {
		return false
	}

	// Must have a colon at position 0 (after first element)
	if !colonPositions[0] {
		return false
	}

	// Look for "for" and "in" keywords after the first key:value pair
	hasFor := false
	hasIn := false
	forIndex := -1

	// Start looking after the first value (position 1)
	for i := 2; i < len(elements); i++ {
		if sym, ok := elements[i].(core.SymbolValue); ok {
			if string(sym) == "for" && !hasFor {
				hasFor = true
				forIndex = i
			} else if string(sym) == "in" && hasFor && i > forIndex {
				hasIn = true
			}
		}
	}

	return hasFor && hasIn && forIndex > 1
}

// parseDictComprehension converts dict comprehension elements into a comprehension form
// Pattern: {k: v for var in iterable} -> (dict-comp k v var iterable)
// Pattern: {k: v for var in iterable if condition} -> (dict-comp k v var iterable condition)
func (p *Parser) parseDictComprehension(elements core.ListValue, colonPositions map[int]bool) (core.Value, error) {
	// Find the positions of "for" and "in"
	forIndex := -1
	inIndex := -1

	for i, elem := range elements {
		if sym, ok := elem.(core.SymbolValue); ok {
			if string(sym) == "for" && forIndex == -1 {
				forIndex = i
			} else if string(sym) == "in" && forIndex != -1 && inIndex == -1 {
				inIndex = i
			}
		}
	}

	if forIndex <= 1 || inIndex <= forIndex+1 {
		return nil, fmt.Errorf("invalid dict comprehension syntax")
	}

	// Key is element[0], value is element[1]
	keyExpr := elements[0]
	valueExpr := elements[1]

	// Get the variable (between "for" and "in")
	if inIndex != forIndex+2 {
		return nil, fmt.Errorf("dict comprehension variable must be a single symbol")
	}
	variable := elements[forIndex+1]

	// Get the iterable and optional condition
	var iterable core.Value
	remainingElements := elements[inIndex+1:]

	// Check for "if" condition
	ifIndex := -1
	for i, elem := range remainingElements {
		if sym, ok := elem.(core.SymbolValue); ok && string(sym) == "if" {
			ifIndex = i
			break
		}
	}

	var condition core.Value
	if ifIndex >= 0 {
		if ifIndex == 0 {
			return nil, fmt.Errorf("missing iterable in dict comprehension")
		}

		if ifIndex == 1 {
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, ifIndex+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements[:ifIndex])
			iterable = iterElements
		}

		condElements := remainingElements[ifIndex+1:]
		if len(condElements) == 0 {
			return nil, fmt.Errorf("missing condition after 'if' in dict comprehension")
		} else if len(condElements) == 1 {
			if list, ok := condElements[0].(core.ListValue); ok && len(list) > 0 {
				condition = list
			} else {
				condition = condElements[0]
			}
		} else {
			condExpr := make(core.ListValue, len(condElements)+1)
			condExpr[0] = core.SymbolValue("do")
			copy(condExpr[1:], condElements)
			condition = condExpr
		}
	} else {
		if len(remainingElements) == 1 {
			if list, ok := remainingElements[0].(core.ListValue); ok && len(list) > 0 {
				iterable = list
			} else {
				iterable = remainingElements[0]
			}
		} else {
			iterElements := make(core.ListValue, len(remainingElements)+1)
			iterElements[0] = core.SymbolValue("do")
			copy(iterElements[1:], remainingElements)
			iterable = iterElements
		}
	}

	// Build the dict-comp form
	// (dict-comp key-expr value-expr var iterable) or (dict-comp key-expr value-expr var iterable condition)
	result := make(core.ListValue, 0, 6)
	result = append(result, core.SymbolValue("dict-comp"))
	result = append(result, keyExpr)
	result = append(result, valueExpr)
	result = append(result, variable)
	result = append(result, iterable)
	if condition != nil {
		result = append(result, condition)
	}

	return result, nil
}

// parseNumber parses a numeric literal
func (p *Parser) parseNumber() (core.Value, error) {
	start := p.pos

	// Skip sign if present
	if p.input[p.pos] == '+' || p.input[p.pos] == '-' {
		p.advance()
	}

	// Parse digits
	for p.pos < len(p.input) && isDigit(p.input[p.pos]) {
		p.advance()
	}

	// Check for decimal point
	if p.pos < len(p.input) && p.input[p.pos] == '.' {
		p.advance()

		// Parse fractional part
		for p.pos < len(p.input) && isDigit(p.input[p.pos]) {
			p.advance()
		}
	}

	// Parse the number
	numStr := p.input[start:p.pos]
	num, err := strconv.ParseFloat(numStr, 64)
	if err != nil {
		return nil, fmt.Errorf("invalid number: %s", numStr)
	}

	return core.NumberValue(num), nil
}

// parseSymbolOrKeyword parses a symbol or keyword
func (p *Parser) parseSymbolOrKeyword() (core.Value, error) {
	name := p.parseSymbolName()

	// Check for keywords (Python-style)
	switch name {
	case "True", "true":
		return core.True, nil
	case "False", "false":
		return core.False, nil
	case "None", "nil":
		return core.None, nil
	default:
		return core.SymbolValue(name), nil
	}
}

// parseSymbolName parses just the name part of a symbol
func (p *Parser) parseSymbolName() string {
	start := p.pos

	// Parse symbol characters
	for p.pos < len(p.input) && isSymbolChar(p.input[p.pos]) {
		p.advance()
	}

	// Get the symbol name
	return p.input[start:p.pos]
}

// parseBacktickQuasiquote parses backtick reader macro for quasiquote
// Transforms `expr into (quasiquote expr)
func (p *Parser) parseBacktickQuasiquote() (core.Value, error) {
	// Skip the backtick
	p.advance()

	// Parse the expression after the backtick
	expr, err := p.parseExpr()
	if err != nil {
		return nil, p.error(fmt.Sprintf("error parsing expression after backtick: %v", err))
	}

	// Wrap in (quasiquote expr)
	return core.ListValue{
		core.SymbolValue("quasiquote"),
		expr,
	}, nil
}

// parseCommaUnquote parses comma reader macro for unquote and unquote-splicing
// Transforms ,expr into (unquote expr)
// Transforms ,@expr into (unquote-splicing expr)
func (p *Parser) parseCommaUnquote() (core.Value, error) {
	// Skip the comma
	p.advance()

	// Check if this is unquote-splicing (,@)
	isSplicing := false
	if p.pos < len(p.input) && p.input[p.pos] == '@' {
		isSplicing = true
		p.advance() // skip @
	}

	// Parse the expression after the comma (and optional @)
	expr, err := p.parseExpr()
	if err != nil {
		if isSplicing {
			return nil, p.error(fmt.Sprintf("error parsing expression after ,@: %v", err))
		}
		return nil, p.error(fmt.Sprintf("error parsing expression after comma: %v", err))
	}

	// Wrap in (unquote expr) or (unquote-splicing expr)
	if isSplicing {
		return core.ListValue{
			core.SymbolValue("unquote-splicing"),
			expr,
		}, nil
	}
	return core.ListValue{
		core.SymbolValue("unquote"),
		expr,
	}, nil
}

// skipWhitespaceAndComments skips whitespace and comments
func (p *Parser) skipWhitespaceAndComments() {
	for p.pos < len(p.input) {
		if unicode.IsSpace(rune(p.input[p.pos])) {
			// Update line and column for newlines
			if p.input[p.pos] == '\n' {
				p.line++
				p.col = 1
			} else {
				p.col++
			}
			p.pos++
		} else if p.pos < len(p.input) && p.input[p.pos] == '#' {
			// Skip comment to end of line
			for p.pos < len(p.input) && p.input[p.pos] != '\n' {
				p.pos++
			}
		} else {
			break
		}
	}
}

// advance moves the parser position forward
func (p *Parser) advance() {
	if p.pos < len(p.input) {
		if p.input[p.pos] == '\n' {
			p.line++
			p.col = 1
		} else {
			p.col++
		}
		p.pos++
	}
}

// isDigit checks if a character is a digit
func isDigit(ch byte) bool {
	return ch >= '0' && ch <= '9'
}

// isLetter checks if a character is a letter
func isLetter(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
}

// isNumberStart checks if a character can start a number
func (p *Parser) isNumberStart(ch byte) bool {
	// Check if it's a digit
	if isDigit(ch) {
		return true
	}

	// + and - can only start a number if followed by a digit
	if (ch == '+' || ch == '-') && p.pos+1 < len(p.input) && isDigit(p.input[p.pos+1]) {
		return true
	}

	return false
}

// isSymbolChar checks if a character is valid in a symbol
func isSymbolChar(ch byte) bool {
	return !unicode.IsSpace(rune(ch)) &&
		ch != '(' && ch != ')' &&
		ch != '[' && ch != ']' &&
		ch != '{' && ch != '}' &&
		ch != '"' && ch != '\'' && ch != ';' &&
		ch != ':' && ch != '.' && // dot is not valid in symbols (used for dot notation)
		ch != ',' // comma is not valid in symbols (used for separating elements)
}

// error creates a parser error with current position info
func (p *Parser) error(msg string) error {
	return fmt.Errorf("%s at line %d, column %d", msg, p.line, p.col)
}

// Token navigation methods for token-based parsing

// currentToken returns the current token without advancing
func (p *Parser) currentToken() Token {
	if p.tokenPos >= len(p.tokens) {
		// Return EOF token
		return p.tokens[len(p.tokens)-1]
	}
	return p.tokens[p.tokenPos]
}

// peekToken returns the next token without advancing
func (p *Parser) peekToken() Token {
	if p.tokenPos+1 >= len(p.tokens) {
		return p.tokens[len(p.tokens)-1]
	}
	return p.tokens[p.tokenPos+1]
}

// advanceToken advances to the next token and returns the current one
func (p *Parser) advanceToken() Token {
	tok := p.currentToken()
	if p.tokenPos < len(p.tokens)-1 {
		p.tokenPos++
	}
	return tok
}

// expectToken expects a specific token type and advances
func (p *Parser) expectToken(typ TokenType) (Token, error) {
	tok := p.currentToken()
	if tok.Type != typ {
		return tok, p.tokenError(fmt.Sprintf("expected %v, got %v", typ, tok.Type), tok)
	}
	return p.advanceToken(), nil
}

// matchToken checks if current token matches any of the given types
func (p *Parser) matchToken(types ...TokenType) bool {
	for _, typ := range types {
		if p.currentToken().Type == typ {
			return true
		}
	}
	return false
}

// tokenError creates an error with token position info
func (p *Parser) tokenError(msg string, tok Token) error {
	return fmt.Errorf("%s at line %d, column %d", msg, tok.Line, tok.Col)
}

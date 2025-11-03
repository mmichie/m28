package parser

import (
	"fmt"
	"strconv"

	"github.com/mmichie/m28/core"
)

// Tokenizer performs lexical analysis on M28 source code
type Tokenizer struct {
	input    string  // Source code
	pos      int     // Current position in input
	line     int     // Current line (1-indexed)
	col      int     // Current column (1-indexed)
	filename string  // For error reporting
	tokens   []Token // Accumulated tokens
	errors   []error // Accumulated errors
}

// NewTokenizer creates a new tokenizer for the given input
func NewTokenizer(input string) *Tokenizer {
	return &Tokenizer{
		input:  input,
		pos:    0,
		line:   1,
		col:    1,
		tokens: make([]Token, 0, 256), // Pre-allocate
		errors: make([]error, 0),
	}
}

// Tokenize performs lexical analysis and returns all tokens
func (t *Tokenizer) Tokenize() ([]Token, error) {
	for !t.isAtEnd() {
		t.skipWhitespace()
		if t.isAtEnd() {
			break
		}

		tok := t.scanToken()
		t.tokens = append(t.tokens, tok)

		if tok.Type == TOKEN_ERROR {
			// Continue tokenizing to find more errors
			t.advance() // Skip the problematic character
		}
	}

	// Add EOF token
	t.tokens = append(t.tokens, Token{
		Type:     TOKEN_EOF,
		Lexeme:   "",
		Line:     t.line,
		Col:      t.col,
		StartPos: t.pos,
		EndPos:   t.pos,
	})

	if len(t.errors) > 0 {
		return t.tokens, fmt.Errorf("tokenization errors: %v", t.errors)
	}

	return t.tokens, nil
}

// scanToken scans and returns the next token
func (t *Tokenizer) scanToken() Token {
	start := t.pos
	startLine := t.line
	startCol := t.col

	ch := t.advance()

	// Numbers
	if isDigit(ch) {
		return t.scanNumber(start, startLine, startCol)
	}

	// Check for special string prefixes BEFORE identifiers
	// F-strings (f"..." or f'...')
	if ch == 'f' && !t.isAtEnd() && (t.peek() == '"' || t.peek() == '\'') {
		quote := t.advance()
		return t.scanFString(quote, start, startLine, startCol)
	}

	// S-strings (s"..." or s'...')
	if ch == 's' && !t.isAtEnd() && (t.peek() == '"' || t.peek() == '\'') {
		quote := t.advance()
		return t.scanSString(quote, start, startLine, startCol, false)
	}

	// Raw S-strings (rs"..." or rs'...')
	if ch == 'r' && !t.isAtEnd() && t.peek() == 's' {
		if t.pos+2 < len(t.input) {
			quote := t.input[t.pos+1]
			if quote == '"' || quote == '\'' {
				t.advance() // skip 's'
				t.advance() // skip quote
				return t.scanSString(byte(quote), start, startLine, startCol, true)
			}
		}
	}

	// Identifiers and keywords (allow ? for predicates like has-path?)
	if isLetter(ch) || ch == '_' || ch == '?' {
		return t.scanIdentifier(start, startLine, startCol)
	}

	// Strings
	if ch == '"' || ch == '\'' {
		return t.scanString(ch, start, startLine, startCol)
	}

	// Operators and punctuation
	switch ch {
	case '(':
		return t.makeToken(TOKEN_LPAREN, start, startLine, startCol)
	case ')':
		return t.makeToken(TOKEN_RPAREN, start, startLine, startCol)
	case '[':
		return t.makeToken(TOKEN_LBRACKET, start, startLine, startCol)
	case ']':
		return t.makeToken(TOKEN_RBRACKET, start, startLine, startCol)
	case '{':
		return t.makeToken(TOKEN_LBRACE, start, startLine, startCol)
	case '}':
		return t.makeToken(TOKEN_RBRACE, start, startLine, startCol)
	case ',':
		// Check for ,@ (unquote-splicing)
		if !t.isAtEnd() && t.peek() == '@' {
			t.advance()
			return t.makeToken(TOKEN_COMMA_AT, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_COMMA, start, startLine, startCol)
	case '.':
		return t.makeToken(TOKEN_DOT, start, startLine, startCol)
	case ':':
		// Check for := (walrus operator)
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_COLONEQUAL, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_COLON, start, startLine, startCol)
	case ';':
		return t.makeToken(TOKEN_SEMICOLON, start, startLine, startCol)
	case '`':
		return t.makeToken(TOKEN_BACKTICK, start, startLine, startCol)
	case '@':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_AT_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_AT, start, startLine, startCol)

	// Operators with potential multi-char variants
	case '+':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_PLUS_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_PLUS, start, startLine, startCol)

	case '-':
		// Check for -> (thread-first macro) or ->> (thread-last macro)
		if !t.isAtEnd() && t.peek() == '>' {
			t.advance()
			// Check for ->>
			if !t.isAtEnd() && t.peek() == '>' {
				t.advance()
				return Token{
					Type:     TOKEN_IDENTIFIER,
					Lexeme:   "->>",
					Value:    core.SymbolValue("->>"),
					Line:     startLine,
					Col:      startCol,
					StartPos: start,
					EndPos:   t.pos,
				}
			}
			// Just ->
			return Token{
				Type:     TOKEN_IDENTIFIER,
				Lexeme:   "->",
				Value:    core.SymbolValue("->"),
				Line:     startLine,
				Col:      startCol,
				StartPos: start,
				EndPos:   t.pos,
			}
		}
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_MINUS_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_MINUS, start, startLine, startCol)

	case '*':
		if !t.isAtEnd() && t.peek() == '*' {
			t.advance()
			if !t.isAtEnd() && t.peek() == '=' {
				t.advance()
				return t.makeToken(TOKEN_DOUBLE_STAR_EQ, start, startLine, startCol)
			}
			return t.makeToken(TOKEN_DOUBLE_STAR, start, startLine, startCol)
		}
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_STAR_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_STAR, start, startLine, startCol)

	case '/':
		if !t.isAtEnd() && t.peek() == '/' {
			t.advance()
			if !t.isAtEnd() && t.peek() == '=' {
				t.advance()
				return t.makeToken(TOKEN_DOUBLE_SLASH_EQ, start, startLine, startCol)
			}
			return t.makeToken(TOKEN_DOUBLE_SLASH, start, startLine, startCol)
		}
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_SLASH_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_SLASH, start, startLine, startCol)

	case '%':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_PERCENT_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_PERCENT, start, startLine, startCol)

	case '=':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_ASSIGN, start, startLine, startCol)

	case '!':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_NE, start, startLine, startCol)
		}
		return t.errorToken("unexpected character '!'", start, startLine, startCol)

	case '<':
		if !t.isAtEnd() && t.peek() == '<' {
			t.advance()
			if !t.isAtEnd() && t.peek() == '=' {
				t.advance()
				return t.makeToken(TOKEN_DOUBLE_LT_EQ, start, startLine, startCol)
			}
			return t.makeToken(TOKEN_DOUBLE_LT, start, startLine, startCol)
		}
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_LE, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_LT, start, startLine, startCol)

	case '>':
		if !t.isAtEnd() && t.peek() == '>' {
			t.advance()
			if !t.isAtEnd() && t.peek() == '=' {
				t.advance()
				return t.makeToken(TOKEN_DOUBLE_GT_EQ, start, startLine, startCol)
			}
			return t.makeToken(TOKEN_DOUBLE_GT, start, startLine, startCol)
		}
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_GE, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_GT, start, startLine, startCol)

	case '&':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_AMPERSAND_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_AMPERSAND, start, startLine, startCol)

	case '|':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_PIPE_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_PIPE, start, startLine, startCol)

	case '^':
		if !t.isAtEnd() && t.peek() == '=' {
			t.advance()
			return t.makeToken(TOKEN_CARET_EQ, start, startLine, startCol)
		}
		return t.makeToken(TOKEN_CARET, start, startLine, startCol)

	case '~':
		return t.makeToken(TOKEN_TILDE, start, startLine, startCol)

	case '#':
		// Comment - skip to end of line
		t.skipLineComment()
		t.skipWhitespace() // Skip whitespace (including newline) after comment
		if t.isAtEnd() {
			return t.makeToken(TOKEN_EOF, t.pos, t.line, t.col)
		}
		return t.scanToken() // Recurse to get next token

	default:
		return t.errorToken(fmt.Sprintf("unexpected character '%c'", ch), start, startLine, startCol)
	}
}

// scanNumber scans a number token
func (t *Tokenizer) scanNumber(start, startLine, startCol int) Token {
	// Scan integer part
	for !t.isAtEnd() && isDigit(t.peek()) {
		t.advance()
	}

	// Check for decimal point
	if !t.isAtEnd() && t.peek() == '.' {
		// Make sure it's not a method call like "42.times"
		if t.pos+1 < len(t.input) && isDigit(t.input[t.pos+1]) {
			t.advance() // Consume '.'
			for !t.isAtEnd() && isDigit(t.peek()) {
				t.advance()
			}
		}
	}

	// Check for scientific notation
	if !t.isAtEnd() && (t.peek() == 'e' || t.peek() == 'E') {
		t.advance()
		if !t.isAtEnd() && (t.peek() == '+' || t.peek() == '-') {
			t.advance()
		}
		if t.isAtEnd() || !isDigit(t.peek()) {
			return t.errorToken("invalid number: expected digit after 'e'", start, startLine, startCol)
		}
		for !t.isAtEnd() && isDigit(t.peek()) {
			t.advance()
		}
	}

	// Parse the number
	lexeme := t.input[start:t.pos]
	num, err := strconv.ParseFloat(lexeme, 64)
	if err != nil {
		return t.errorToken(fmt.Sprintf("invalid number: %v", err), start, startLine, startCol)
	}

	return Token{
		Type:     TOKEN_NUMBER,
		Lexeme:   lexeme,
		Value:    core.NumberValue(num),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanIdentifier scans an identifier or keyword
func (t *Tokenizer) scanIdentifier(start, startLine, startCol int) Token {
	// Scan identifier characters (including dashes for kebab-case like get-item)
	// Also allow '?' for predicate functions (Lisp/Scheme convention)
	for !t.isAtEnd() && (isLetter(t.peek()) || isDigit(t.peek()) || t.peek() == '_' || t.peek() == '-' || t.peek() == '?') {
		t.advance()
	}

	lexeme := t.input[start:t.pos]

	// Check for multi-word keywords
	savedPos, savedLine, savedCol := t.pos, t.line, t.col

	if lexeme == "not" {
		t.skipWhitespace()
		if !t.isAtEnd() && t.pos+2 <= len(t.input) && t.input[t.pos:t.pos+2] == "in" {
			// Check that "in" is a complete word
			if t.pos+2 >= len(t.input) || !isLetter(t.input[t.pos+2]) {
				t.advance()
				t.advance()
				return Token{
					Type:     TOKEN_NOT_IN,
					Lexeme:   "not in",
					Line:     startLine,
					Col:      startCol,
					StartPos: start,
					EndPos:   t.pos,
				}
			}
		}
		// Restore position if we didn't match "not in"
		t.pos, t.line, t.col = savedPos, savedLine, savedCol
	}

	if lexeme == "is" {
		t.skipWhitespace()
		if !t.isAtEnd() && t.pos+3 <= len(t.input) && t.input[t.pos:t.pos+3] == "not" {
			// Check that "not" is a complete word
			if t.pos+3 >= len(t.input) || !isLetter(t.input[t.pos+3]) {
				t.advance()
				t.advance()
				t.advance()
				return Token{
					Type:     TOKEN_IS_NOT,
					Lexeme:   "is not",
					Line:     startLine,
					Col:      startCol,
					StartPos: start,
					EndPos:   t.pos,
				}
			}
		}
		// Restore position if we didn't match "is not"
		t.pos, t.line, t.col = savedPos, savedLine, savedCol
	}

	// Check for single-word keywords
	tokType, isKeyword := keywords[lexeme]
	if isKeyword {
		var value core.Value
		switch tokType {
		case TOKEN_TRUE:
			value = core.True
		case TOKEN_FALSE:
			value = core.False
		case TOKEN_NIL:
			value = core.Nil
		}

		return Token{
			Type:     tokType,
			Lexeme:   lexeme,
			Value:    value,
			Line:     startLine,
			Col:      startCol,
			StartPos: start,
			EndPos:   t.pos,
		}
	}

	// It's an identifier
	return Token{
		Type:     TOKEN_IDENTIFIER,
		Lexeme:   lexeme,
		Value:    core.SymbolValue(lexeme),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanString scans a string literal
func (t *Tokenizer) scanString(quote byte, start, startLine, startCol int) Token {
	for !t.isAtEnd() && t.peek() != quote {
		if t.peek() == '\\' {
			t.advance() // Skip backslash
			if !t.isAtEnd() {
				t.advance() // Skip escaped character
			}
		} else if t.peek() == '\n' {
			t.line++
			t.col = 0
			t.advance()
		} else {
			t.advance()
		}
	}

	if t.isAtEnd() {
		return t.errorToken("unterminated string", start, startLine, startCol)
	}

	t.advance() // Consume closing quote

	// Extract string content (without quotes)
	content := t.input[start+1 : t.pos-1]

	// Parse escape sequences
	parsed, err := t.parseEscapes(content)
	if err != nil {
		return t.errorToken(fmt.Sprintf("invalid escape sequence: %v", err), start, startLine, startCol)
	}

	return Token{
		Type:     TOKEN_STRING,
		Lexeme:   t.input[start:t.pos],
		Value:    core.StringValue(parsed),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanFString scans an f-string (stored as opaque token, parser handles interpolation)
func (t *Tokenizer) scanFString(quote byte, start, startLine, startCol int) Token {
	// Skip the content, tracking brace depth for interpolation
	braceDepth := 0
	for !t.isAtEnd() {
		ch := t.peek()
		if ch == quote && braceDepth == 0 {
			break
		}
		if ch == '\\' {
			t.advance()
			if !t.isAtEnd() {
				t.advance()
			}
		} else if ch == '{' {
			braceDepth++
			t.advance()
		} else if ch == '}' {
			if braceDepth > 0 {
				braceDepth--
			}
			t.advance()
		} else {
			if ch == '\n' {
				t.line++
				t.col = 0
			}
			t.advance()
		}
	}

	if t.isAtEnd() {
		return t.errorToken("unterminated f-string", start, startLine, startCol)
	}

	t.advance() // Consume closing quote

	// Return as opaque token - parser will handle interpolation
	return Token{
		Type:     TOKEN_FSTRING,
		Lexeme:   t.input[start:t.pos],
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanSString scans an s-string (stored as opaque token, parser handles interpolation)
func (t *Tokenizer) scanSString(quote byte, start, startLine, startCol int, raw bool) Token {
	// Skip the content, tracking brace depth for interpolation
	braceDepth := 0
	for !t.isAtEnd() {
		ch := t.peek()
		if ch == quote && braceDepth == 0 {
			break
		}
		if !raw && ch == '\\' {
			t.advance()
			if !t.isAtEnd() {
				t.advance()
			}
		} else if ch == '{' {
			braceDepth++
			t.advance()
		} else if ch == '}' {
			if braceDepth > 0 {
				braceDepth--
			}
			t.advance()
		} else {
			if ch == '\n' {
				t.line++
				t.col = 0
			}
			t.advance()
		}
	}

	if t.isAtEnd() {
		return t.errorToken("unterminated s-string", start, startLine, startCol)
	}

	t.advance() // Consume closing quote

	// Return as opaque token - parser will handle interpolation
	return Token{
		Type:     TOKEN_SSTRING,
		Lexeme:   t.input[start:t.pos],
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// Helper functions

func (t *Tokenizer) advance() byte {
	if t.isAtEnd() {
		return 0
	}
	ch := t.input[t.pos]
	t.pos++
	t.col++
	return ch
}

func (t *Tokenizer) peek() byte {
	if t.isAtEnd() {
		return 0
	}
	return t.input[t.pos]
}

func (t *Tokenizer) peekAhead(n int) byte {
	if t.pos+n >= len(t.input) {
		return 0
	}
	return t.input[t.pos+n]
}

func (t *Tokenizer) isAtEnd() bool {
	return t.pos >= len(t.input)
}

func (t *Tokenizer) skipWhitespace() {
	for !t.isAtEnd() {
		ch := t.peek()
		if ch == ' ' || ch == '\t' || ch == '\r' {
			t.advance()
		} else if ch == '\n' {
			t.line++
			t.col = 0
			t.advance()
		} else {
			break
		}
	}
}

func (t *Tokenizer) skipLineComment() {
	for !t.isAtEnd() && t.peek() != '\n' {
		t.advance()
	}
}

func (t *Tokenizer) makeToken(typ TokenType, start, startLine, startCol int) Token {
	return Token{
		Type:     typ,
		Lexeme:   t.input[start:t.pos],
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

func (t *Tokenizer) errorToken(message string, start, startLine, startCol int) Token {
	err := fmt.Errorf("%s at line %d, column %d", message, startLine, startCol)
	t.errors = append(t.errors, err)

	return Token{
		Type:     TOKEN_ERROR,
		Lexeme:   t.input[start:t.pos],
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

func (t *Tokenizer) parseEscapes(s string) (string, error) {
	// Escape sequence parser with Unicode support
	result := make([]rune, 0, len(s))
	i := 0
	for i < len(s) {
		if s[i] == '\\' && i+1 < len(s) {
			switch s[i+1] {
			case 'n':
				result = append(result, '\n')
				i += 2
			case 't':
				result = append(result, '\t')
				i += 2
			case 'r':
				result = append(result, '\r')
				i += 2
			case '\\':
				result = append(result, '\\')
				i += 2
			case '"':
				result = append(result, '"')
				i += 2
			case '\'':
				result = append(result, '\'')
				i += 2
			case 'x':
				// \xHH - hex escape (2 hex digits)
				if i+3 < len(s) {
					hexStr := s[i+2 : i+4]
					var hexVal int
					if _, err := fmt.Sscanf(hexStr, "%x", &hexVal); err == nil {
						result = append(result, rune(hexVal))
						i += 4
					} else {
						result = append(result, rune(s[i+1]))
						i += 2
					}
				} else {
					result = append(result, rune(s[i+1]))
					i += 2
				}
			case 'u':
				// \uHHHH - unicode escape (4 hex digits)
				if i+5 < len(s) {
					hexStr := s[i+2 : i+6]
					var hexVal int
					if _, err := fmt.Sscanf(hexStr, "%x", &hexVal); err == nil {
						result = append(result, rune(hexVal))
						i += 6
					} else {
						result = append(result, rune(s[i+1]))
						i += 2
					}
				} else {
					result = append(result, rune(s[i+1]))
					i += 2
				}
			case 'U':
				// \UHHHHHHHH - long unicode escape (8 hex digits)
				if i+9 < len(s) {
					hexStr := s[i+2 : i+10]
					var hexVal int
					if _, err := fmt.Sscanf(hexStr, "%x", &hexVal); err == nil {
						result = append(result, rune(hexVal))
						i += 10
					} else {
						result = append(result, rune(s[i+1]))
						i += 2
					}
				} else {
					result = append(result, rune(s[i+1]))
					i += 2
				}
			default:
				result = append(result, rune(s[i+1]))
				i += 2
			}
		} else {
			result = append(result, rune(s[i]))
			i++
		}
	}
	return string(result), nil
}

// Keyword map
var keywords = map[string]TokenType{
	"True":  TOKEN_TRUE,
	"true":  TOKEN_TRUE,
	"False": TOKEN_FALSE,
	"false": TOKEN_FALSE,
	"None":  TOKEN_NIL,
	"nil":   TOKEN_NIL,
	"and":   TOKEN_AND,
	"or":    TOKEN_OR,
	"not":   TOKEN_NOT,
	"in":    TOKEN_IN,
	"is":    TOKEN_IS,
}

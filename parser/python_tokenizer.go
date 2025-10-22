package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

// PythonTokenizer performs lexical analysis on Python source code
// Key differences from M28 tokenizer:
// 1. Indentation-aware (generates INDENT/DEDENT tokens)
// 2. Newlines are significant
// 3. Tracks indentation levels with a stack
type PythonTokenizer struct {
	input       string  // Source code
	pos         int     // Current position in input
	line        int     // Current line (1-indexed)
	col         int     // Current column (1-indexed)
	filename    string  // For error reporting
	tokens      []Token // Accumulated tokens
	errors      []error // Accumulated errors
	indentStack []int   // Stack of indentation levels (number of spaces)
	atLineStart bool    // Are we at the start of a logical line?
	parenDepth  int     // Track nested parens/brackets/braces
}

// NewPythonTokenizer creates a new Python tokenizer
func NewPythonTokenizer(input string) *PythonTokenizer {
	return &PythonTokenizer{
		input:       input,
		pos:         0,
		line:        1,
		col:         1,
		tokens:      make([]Token, 0, 256),
		errors:      make([]error, 0),
		indentStack: []int{0}, // Start with base indentation level
		atLineStart: true,
		parenDepth:  0,
	}
}

// Tokenize performs lexical analysis and returns all tokens
func (t *PythonTokenizer) Tokenize() ([]Token, error) {
	for !t.isAtEnd() {
		// Handle indentation only at line start (when not inside parens)
		if t.atLineStart && t.parenDepth == 0 {
			t.handleIndentation()
		}

		// Skip whitespace (but not newlines)
		t.skipWhitespaceExceptNewlines()

		if t.isAtEnd() {
			break
		}

		// Check for comments
		if t.peek() == '#' {
			t.skipComment()
			continue
		}

		tok := t.scanToken()

		// Skip NEWLINE tokens that represent blank lines
		// (when we're at line start and just saw a newline)
		if tok.Type == TOKEN_NEWLINE && t.atLineStart {
			t.atLineStart = true
			continue
		}

		t.tokens = append(t.tokens, tok)

		// Track newlines
		if tok.Type == TOKEN_NEWLINE {
			t.atLineStart = true
		} else if tok.Type != TOKEN_COMMENT {
			t.atLineStart = false
		}

		if tok.Type == TOKEN_ERROR {
			t.advance() // Skip problematic character
		}
	}

	// Combine compound operators (not in, is not)
	t.combineCompoundOperators()

	// Emit remaining DEDENT tokens at end of file
	t.emitFinalDedents()

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

// handleIndentation processes indentation at the start of a line
func (t *PythonTokenizer) handleIndentation() {
	startPos := t.pos
	startLine := t.line
	startCol := t.col

	// Count leading spaces/tabs (Python prefers spaces)
	indent := 0
	for !t.isAtEnd() && (t.peek() == ' ' || t.peek() == '\t') {
		if t.peek() == ' ' {
			indent++
		} else {
			// Tab counts as 8 spaces (Python standard)
			indent += 8
		}
		t.advance()
	}

	// If line is blank or a comment, don't process indentation
	if t.isAtEnd() || t.peek() == '\n' || t.peek() == '#' {
		return
	}

	currentIndent := t.indentStack[len(t.indentStack)-1]

	if indent > currentIndent {
		// Increased indentation - emit INDENT
		t.indentStack = append(t.indentStack, indent)
		t.tokens = append(t.tokens, Token{
			Type:     TOKEN_INDENT,
			Lexeme:   "",
			Line:     startLine,
			Col:      startCol,
			StartPos: startPos,
			EndPos:   t.pos,
		})
	} else if indent < currentIndent {
		// Decreased indentation - emit DEDENT(s)
		for len(t.indentStack) > 0 && t.indentStack[len(t.indentStack)-1] > indent {
			t.indentStack = t.indentStack[:len(t.indentStack)-1]
			t.tokens = append(t.tokens, Token{
				Type:     TOKEN_DEDENT,
				Lexeme:   "",
				Line:     startLine,
				Col:      startCol,
				StartPos: startPos,
				EndPos:   t.pos,
			})
		}

		// Check for indentation error (dedent doesn't match any previous level)
		if len(t.indentStack) > 0 && t.indentStack[len(t.indentStack)-1] != indent {
			t.errors = append(t.errors, fmt.Errorf("indentation error at line %d", t.line))
		}
	}
	// If indent == currentIndent, no INDENT/DEDENT needed
}

// emitFinalDedents emits DEDENT tokens for remaining indentation levels
func (t *PythonTokenizer) emitFinalDedents() {
	for len(t.indentStack) > 1 {
		t.indentStack = t.indentStack[:len(t.indentStack)-1]
		t.tokens = append(t.tokens, Token{
			Type:     TOKEN_DEDENT,
			Lexeme:   "",
			Line:     t.line,
			Col:      t.col,
			StartPos: t.pos,
			EndPos:   t.pos,
		})
	}
}

// combineCompoundOperators combines adjacent tokens into compound operators
// "not" + "in" -> "not in"
// "is" + "not" -> "is not"
func (t *PythonTokenizer) combineCompoundOperators() {
	result := make([]Token, 0, len(t.tokens))
	i := 0

	for i < len(t.tokens) {
		current := t.tokens[i]

		// Check for "not in"
		if current.Type == TOKEN_NOT && i+1 < len(t.tokens) && t.tokens[i+1].Type == TOKEN_IN {
			next := t.tokens[i+1]
			// Combine into TOKEN_NOT_IN
			combined := Token{
				Type:     TOKEN_NOT_IN,
				Lexeme:   current.Lexeme + " " + next.Lexeme,
				Line:     current.Line,
				Col:      current.Col,
				StartPos: current.StartPos,
				EndPos:   next.EndPos,
			}
			result = append(result, combined)
			i += 2 // Skip both tokens
			continue
		}

		// Check for "is not"
		if current.Type == TOKEN_IS && i+1 < len(t.tokens) && t.tokens[i+1].Type == TOKEN_NOT {
			next := t.tokens[i+1]
			// Combine into TOKEN_IS_NOT
			combined := Token{
				Type:     TOKEN_IS_NOT,
				Lexeme:   current.Lexeme + " " + next.Lexeme,
				Line:     current.Line,
				Col:      current.Col,
				StartPos: current.StartPos,
				EndPos:   next.EndPos,
			}
			result = append(result, combined)
			i += 2 // Skip both tokens
			continue
		}

		// No combination, keep the token as-is
		result = append(result, current)
		i++
	}

	t.tokens = result
}

// scanToken scans and returns the next token
func (t *PythonTokenizer) scanToken() Token {
	start := t.pos
	startLine := t.line
	startCol := t.col

	ch := t.advance()

	// Handle backslash line continuation
	if ch == '\\' {
		// Check if next character is a newline
		if t.peek() == '\n' {
			t.advance() // consume the newline
			// Skip any whitespace on the next line and continue scanning
			t.skipWhitespaceExceptNewlines()
			return t.scanToken()
		}
		// If not followed by newline, it's an error
		t.errors = append(t.errors, fmt.Errorf("unexpected character '\\' at line %d", startLine))
		return Token{
			Type:     TOKEN_ERROR,
			Lexeme:   "\\",
			Line:     startLine,
			Col:      startCol,
			StartPos: start,
			EndPos:   t.pos,
		}
	}

	// Track parentheses depth
	switch ch {
	case '(', '[', '{':
		t.parenDepth++
	case ')', ']', '}':
		t.parenDepth--
	}

	// Newlines (only significant outside of parens)
	if ch == '\n' {
		if t.parenDepth == 0 {
			return Token{
				Type:     TOKEN_NEWLINE,
				Lexeme:   "\n",
				Line:     startLine,
				Col:      startCol,
				StartPos: start,
				EndPos:   t.pos,
			}
		}
		// Inside parens, skip newlines and any following whitespace, then continue
		t.skipWhitespaceExceptNewlines()
		return t.scanToken()
	}

	// Numbers
	if isDigit(ch) {
		return t.scanNumber(start, startLine, startCol)
	}

	// Strings
	if ch == '"' || ch == '\'' {
		return t.scanString(ch, start, startLine, startCol)
	}

	// F-strings
	if ch == 'f' && !t.isAtEnd() && (t.peek() == '"' || t.peek() == '\'') {
		quote := t.advance()
		return t.scanFString(quote, start, startLine, startCol)
	}

	// Raw bytes literals (br"..." or rb"..." or br'...' or rb'...')
	if (ch == 'b' || ch == 'r') && !t.isAtEnd() {
		next := t.peek()
		if (next == 'r' || next == 'b') && next != ch {
			// We have br or rb
			t.advance() // consume second prefix
			if !t.isAtEnd() && (t.peek() == '"' || t.peek() == '\'') {
				quote := t.advance()
				// Both br and rb are raw bytes strings
				return t.scanRawBytesString(quote, start, startLine, startCol)
			}
			// Not a string after br/rb, backtrack
			t.pos = start
			t.line = startLine
			t.col = startCol
		}
	}

	// Bytes literals (b"..." or b'...')
	if ch == 'b' && !t.isAtEnd() && (t.peek() == '"' || t.peek() == '\'') {
		quote := t.advance()
		return t.scanBytesString(quote, start, startLine, startCol)
	}

	// Raw strings (r"..." or r'...')
	if ch == 'r' && !t.isAtEnd() && (t.peek() == '"' || t.peek() == '\'') {
		quote := t.advance()
		return t.scanRawString(quote, start, startLine, startCol)
	}

	// Identifiers and keywords
	if pythonIsAlpha(ch) {
		return t.scanIdentifier(start, startLine, startCol)
	}

	// Comments
	if ch == '#' {
		t.skipComment()
		// After skipping comment, continue to next token
		return t.scanToken()
	}

	// Operators and delimiters
	return t.scanOperator(ch, start, startLine, startCol)
}

// scanIdentifier scans an identifier or keyword
func (t *PythonTokenizer) scanIdentifier(start, startLine, startCol int) Token {
	for !t.isAtEnd() && pythonIsAlphaNumeric(t.peek()) {
		t.advance()
	}

	lexeme := t.input[start:t.pos]
	tokenType := t.identifierType(lexeme)

	return Token{
		Type:     tokenType,
		Lexeme:   lexeme,
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// identifierType determines if an identifier is a keyword
func (t *PythonTokenizer) identifierType(lexeme string) TokenType {
	// Python keywords
	keywords := map[string]TokenType{
		"False":    TOKEN_FALSE,
		"None":     TOKEN_NIL,
		"True":     TOKEN_TRUE,
		"and":      TOKEN_AND,
		"as":       TOKEN_AS,
		"assert":   TOKEN_ASSERT,
		"async":    TOKEN_ASYNC,
		"await":    TOKEN_AWAIT,
		"break":    TOKEN_BREAK,
		"class":    TOKEN_CLASS,
		"continue": TOKEN_CONTINUE,
		"def":      TOKEN_DEF,
		"del":      TOKEN_DEL,
		"elif":     TOKEN_ELIF,
		"else":     TOKEN_ELSE,
		"except":   TOKEN_EXCEPT,
		"finally":  TOKEN_FINALLY,
		"for":      TOKEN_FOR,
		"from":     TOKEN_FROM,
		"global":   TOKEN_GLOBAL,
		"if":       TOKEN_IF,
		"import":   TOKEN_IMPORT,
		"in":       TOKEN_IN,
		"is":       TOKEN_IS,
		"lambda":   TOKEN_LAMBDA,
		"nonlocal": TOKEN_NONLOCAL,
		"not":      TOKEN_NOT,
		"or":       TOKEN_OR,
		"pass":     TOKEN_PASS,
		"raise":    TOKEN_RAISE,
		"return":   TOKEN_RETURN,
		"try":      TOKEN_TRY,
		"while":    TOKEN_WHILE,
		"with":     TOKEN_WITH,
		"yield":    TOKEN_YIELD,
	}

	if tokType, ok := keywords[lexeme]; ok {
		return tokType
	}

	return TOKEN_IDENTIFIER
}

// scanOperator scans operators and punctuation
func (t *PythonTokenizer) scanOperator(ch byte, start, startLine, startCol int) Token {
	makeToken := func(tokType TokenType) Token {
		return Token{
			Type:     tokType,
			Lexeme:   t.input[start:t.pos],
			Line:     startLine,
			Col:      startCol,
			StartPos: start,
			EndPos:   t.pos,
		}
	}

	switch ch {
	case '(':
		return makeToken(TOKEN_LPAREN)
	case ')':
		return makeToken(TOKEN_RPAREN)
	case '[':
		return makeToken(TOKEN_LBRACKET)
	case ']':
		return makeToken(TOKEN_RBRACKET)
	case '{':
		return makeToken(TOKEN_LBRACE)
	case '}':
		return makeToken(TOKEN_RBRACE)
	case ',':
		return makeToken(TOKEN_COMMA)
	case ';':
		return makeToken(TOKEN_SEMICOLON)
	case ':':
		if t.match('=') {
			return makeToken(TOKEN_COLONEQUAL)
		}
		return makeToken(TOKEN_COLON)
	case '@':
		return makeToken(TOKEN_AT)

	case '+':
		if t.match('=') {
			return makeToken(TOKEN_PLUS_ASSIGN)
		}
		return makeToken(TOKEN_PLUS)

	case '-':
		if t.match('=') {
			return makeToken(TOKEN_MINUS_ASSIGN)
		}
		if t.match('>') {
			return makeToken(TOKEN_ARROW)
		}
		return makeToken(TOKEN_MINUS)

	case '*':
		if t.match('*') {
			if t.match('=') {
				return makeToken(TOKEN_DOUBLESTAR_ASSIGN)
			}
			return makeToken(TOKEN_DOUBLESTAR)
		}
		if t.match('=') {
			return makeToken(TOKEN_STAR_ASSIGN)
		}
		return makeToken(TOKEN_STAR)

	case '/':
		if t.match('/') {
			if t.match('=') {
				return makeToken(TOKEN_DOUBLESLASH_ASSIGN)
			}
			return makeToken(TOKEN_DOUBLESLASH)
		}
		if t.match('=') {
			return makeToken(TOKEN_SLASH_ASSIGN)
		}
		return makeToken(TOKEN_SLASH)

	case '%':
		if t.match('=') {
			return makeToken(TOKEN_PERCENT_ASSIGN)
		}
		return makeToken(TOKEN_PERCENT)

	case '&':
		if t.match('=') {
			return makeToken(TOKEN_AMPERSAND_ASSIGN)
		}
		return makeToken(TOKEN_AMPERSAND)

	case '|':
		if t.match('=') {
			return makeToken(TOKEN_PIPE_ASSIGN)
		}
		return makeToken(TOKEN_PIPE)

	case '^':
		if t.match('=') {
			return makeToken(TOKEN_CARET_ASSIGN)
		}
		return makeToken(TOKEN_CARET)

	case '~':
		return makeToken(TOKEN_TILDE)

	case '<':
		if t.match('<') {
			if t.match('=') {
				return makeToken(TOKEN_LSHIFT_ASSIGN)
			}
			return makeToken(TOKEN_LSHIFT)
		}
		if t.match('=') {
			return makeToken(TOKEN_LESSEQUAL)
		}
		return makeToken(TOKEN_LESS)

	case '>':
		if t.match('>') {
			if t.match('=') {
				return makeToken(TOKEN_RSHIFT_ASSIGN)
			}
			return makeToken(TOKEN_RSHIFT)
		}
		if t.match('=') {
			return makeToken(TOKEN_GREATEREQUAL)
		}
		return makeToken(TOKEN_GREATER)

	case '=':
		if t.match('=') {
			return makeToken(TOKEN_EQUALEQUAL)
		}
		return makeToken(TOKEN_ASSIGN)

	case '!':
		if t.match('=') {
			return makeToken(TOKEN_NOTEQUAL)
		}
		return makeToken(TOKEN_BANG)

	case '.':
		// Check for ellipsis (...)
		if t.match('.') && t.match('.') {
			return makeToken(TOKEN_ELLIPSIS)
		}
		return makeToken(TOKEN_DOT)

	default:
		t.errors = append(t.errors, fmt.Errorf("unexpected character '%c' at line %d", ch, startLine))
		return Token{
			Type:     TOKEN_ERROR,
			Lexeme:   string(ch),
			Line:     startLine,
			Col:      startCol,
			StartPos: start,
			EndPos:   t.pos,
		}
	}
}

// scanNumber scans a numeric literal
func (t *PythonTokenizer) scanNumber(start, startLine, startCol int) Token {
	// Check for binary, octal, or hex prefix
	// The first character has already been consumed and is at t.input[start]
	if t.input[start] == '0' && !t.isAtEnd() {
		prefix := t.peek()

		// Binary: 0b1010 or 0B1010
		if prefix == 'b' || prefix == 'B' {
			t.advance() // consume 'b' or 'B'
			// Scan binary digits
			for !t.isAtEnd() && (t.peek() == '0' || t.peek() == '1' || t.peek() == '_') {
				t.advance()
			}
			lexeme := t.input[start:t.pos]
			// Remove underscores and parse
			cleanLexeme := strings.ReplaceAll(lexeme[2:], "_", "") // skip "0b" prefix
			if i, err := strconv.ParseInt(cleanLexeme, 2, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(i),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			return Token{Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(0),
				Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
		}

		// Octal: 0o755 or 0O755
		if prefix == 'o' || prefix == 'O' {
			t.advance() // consume 'o' or 'O'
			// Scan octal digits
			for !t.isAtEnd() && ((t.peek() >= '0' && t.peek() <= '7') || t.peek() == '_') {
				t.advance()
			}
			lexeme := t.input[start:t.pos]
			cleanLexeme := strings.ReplaceAll(lexeme[2:], "_", "") // skip "0o" prefix
			if i, err := strconv.ParseInt(cleanLexeme, 8, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(i),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			return Token{Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(0),
				Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
		}

		// Hexadecimal: 0xFF or 0XFF
		if prefix == 'x' || prefix == 'X' {
			t.advance() // consume 'x' or 'X'
			// Scan hex digits
			for !t.isAtEnd() && (isDigit(t.peek()) || (t.peek() >= 'a' && t.peek() <= 'f') ||
				(t.peek() >= 'A' && t.peek() <= 'F') || t.peek() == '_') {
				t.advance()
			}
			lexeme := t.input[start:t.pos]
			cleanLexeme := strings.ReplaceAll(lexeme[2:], "_", "") // skip "0x" prefix
			if i, err := strconv.ParseInt(cleanLexeme, 16, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(i),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			return Token{Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(0),
				Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
		}
	}

	// Regular decimal number
	// Scan integer part (allow underscores for readability)
	for !t.isAtEnd() && (isDigit(t.peek()) || t.peek() == '_') {
		t.advance()
	}

	// Check for decimal point
	if !t.isAtEnd() && t.peek() == '.' && t.pos+1 < len(t.input) && isDigit(t.input[t.pos+1]) {
		t.advance() // consume '.'
		for !t.isAtEnd() && (isDigit(t.peek()) || t.peek() == '_') {
			t.advance()
		}
	}

	// Check for scientific notation
	if !t.isAtEnd() && (t.peek() == 'e' || t.peek() == 'E') {
		t.advance()
		if !t.isAtEnd() && (t.peek() == '+' || t.peek() == '-') {
			t.advance()
		}
		for !t.isAtEnd() && (isDigit(t.peek()) || t.peek() == '_') {
			t.advance()
		}
	}

	// Check for complex number suffix 'j' or 'J'
	isComplex := false
	if !t.isAtEnd() && (t.peek() == 'j' || t.peek() == 'J') {
		t.advance() // consume 'j' or 'J'
		isComplex = true
	}

	lexeme := t.input[start:t.pos]
	// Remove underscores before parsing
	cleanLexeme := strings.ReplaceAll(lexeme, "_", "")
	if isComplex && (strings.HasSuffix(cleanLexeme, "j") || strings.HasSuffix(cleanLexeme, "J")) {
		cleanLexeme = cleanLexeme[:len(cleanLexeme)-1] // remove j/J suffix
	}

	// Parse the number
	var value core.Value
	if isComplex {
		// Complex number - imaginary part only (e.g., 2j, 3.5j)
		var imagPart float64
		if strings.Contains(cleanLexeme, ".") || strings.ContainsAny(cleanLexeme, "eE") {
			if f, err := strconv.ParseFloat(cleanLexeme, 64); err == nil {
				imagPart = f
			}
		} else {
			if i, err := strconv.ParseInt(cleanLexeme, 10, 64); err == nil {
				imagPart = float64(i)
			}
		}
		value = core.ComplexValue(complex(0, imagPart))
	} else if strings.Contains(cleanLexeme, ".") || strings.ContainsAny(cleanLexeme, "eE") {
		// Float
		if f, err := strconv.ParseFloat(cleanLexeme, 64); err == nil {
			value = core.NumberValue(f)
		} else {
			value = core.NumberValue(0.0)
		}
	} else {
		// Integer
		if i, err := strconv.ParseInt(cleanLexeme, 10, 64); err == nil {
			value = core.NumberValue(i)
		} else {
			value = core.NumberValue(0)
		}
	}

	return Token{
		Type:     TOKEN_NUMBER,
		Lexeme:   lexeme,
		Value:    value,
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanString scans a string literal
func (t *PythonTokenizer) scanString(quote byte, start, startLine, startCol int) Token {
	// Check for triple-quoted string
	isTriple := false
	if !t.isAtEnd() && t.peek() == quote {
		if t.pos+1 < len(t.input) && t.input[t.pos+1] == quote {
			isTriple = true
			t.advance() // second quote
			t.advance() // third quote
		}
	}

	var value strings.Builder
	for {
		if t.isAtEnd() {
			t.errors = append(t.errors, t.makeUnterminatedStringError(
				startLine, startCol, start, quote, isTriple, value.String()))
			break
		}

		ch := t.peek()

		// Check for end of string
		if !isTriple && ch == quote {
			t.advance()
			break
		}

		if isTriple {
			// Check for triple quote end
			if ch == quote && t.pos+2 < len(t.input) &&
				t.input[t.pos+1] == quote && t.input[t.pos+2] == quote {
				t.advance()
				t.advance()
				t.advance()
				break
			}
		}

		// Handle escape sequences
		if ch == '\\' && !isTriple {
			t.advance()
			if !t.isAtEnd() {
				escaped := t.advance()
				switch escaped {
				case 'n':
					value.WriteByte('\n')
				case 't':
					value.WriteByte('\t')
				case 'r':
					value.WriteByte('\r')
				case '\\':
					value.WriteByte('\\')
				case '\'':
					value.WriteByte('\'')
				case '"':
					value.WriteByte('"')
				default:
					value.WriteByte(escaped)
				}
			}
		} else {
			value.WriteByte(ch)
			t.advance()
		}
	}

	return Token{
		Type:     TOKEN_STRING,
		Lexeme:   t.input[start:t.pos],
		Value:    core.StringValue(value.String()),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanBytesString scans a bytes literal (b"..." or b'...')
func (t *PythonTokenizer) scanBytesString(quote byte, start, startLine, startCol int) Token {
	var value []byte

	for {
		if t.isAtEnd() {
			t.errors = append(t.errors, t.makeUnterminatedStringError(
				startLine, startCol, start, quote, false, "b"+string(value)))
			break
		}

		ch := t.peek()

		// Check for end of string
		if ch == quote {
			t.advance()
			break
		}

		// Handle escape sequences
		if ch == '\\' {
			t.advance()
			if !t.isAtEnd() {
				escaped := t.peek()
				t.advance()

				switch escaped {
				case 'n':
					value = append(value, '\n')
				case 't':
					value = append(value, '\t')
				case 'r':
					value = append(value, '\r')
				case '\\':
					value = append(value, '\\')
				case '\'':
					value = append(value, '\'')
				case '"':
					value = append(value, '"')
				case 'x':
					// Hex escape: \xNN
					if t.pos+1 < len(t.input) {
						hex1 := t.input[t.pos]
						hex2 := t.input[t.pos+1]
						if isHexDigit(hex1) && isHexDigit(hex2) {
							hexVal := hexDigitValue(hex1)*16 + hexDigitValue(hex2)
							value = append(value, byte(hexVal))
							t.pos += 2
						} else {
							// Invalid hex escape, just include the literal characters
							value = append(value, 'x')
						}
					} else {
						value = append(value, 'x')
					}
				case '0', '1', '2', '3', '4', '5', '6', '7':
					// Octal escape: \NNN (up to 3 digits)
					octalVal := int(escaped - '0')
					count := 1
					for count < 3 && t.pos < len(t.input) && t.input[t.pos] >= '0' && t.input[t.pos] <= '7' {
						octalVal = octalVal*8 + int(t.input[t.pos]-'0')
						t.pos++
						count++
					}
					if octalVal <= 255 {
						value = append(value, byte(octalVal))
					} else {
						// Invalid octal value, just use modulo
						value = append(value, byte(octalVal%256))
					}
				default:
					// Unknown escape, include backslash and character
					value = append(value, '\\', escaped)
				}
			}
		} else {
			value = append(value, ch)
			t.advance()
		}
	}

	return Token{
		Type:     TOKEN_STRING,
		Lexeme:   t.input[start:t.pos],
		Value:    core.BytesValue(value),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanRawString scans a raw string literal (r"..." or r'...')
// Raw strings don't process escape sequences except for the quote character
func (t *PythonTokenizer) scanRawString(quote byte, start, startLine, startCol int) Token {
	// Check for triple-quoted raw string
	isTriple := false
	if !t.isAtEnd() && t.peek() == quote {
		if t.pos+1 < len(t.input) && t.input[t.pos+1] == quote {
			isTriple = true
			t.advance() // second quote
			t.advance() // third quote
		}
	}

	var value strings.Builder
	for {
		if t.isAtEnd() {
			t.errors = append(t.errors, t.makeUnterminatedStringError(
				startLine, startCol, start, quote, isTriple, "r"+value.String()))
			break
		}

		ch := t.peek()

		// Check for end of string
		if !isTriple && ch == quote {
			t.advance()
			break
		}

		if isTriple {
			// Check for triple quote end
			if ch == quote && t.pos+2 < len(t.input) &&
				t.input[t.pos+1] == quote && t.input[t.pos+2] == quote {
				t.advance()
				t.advance()
				t.advance()
				break
			}
		}

		// In raw strings, backslashes are literal (no escape processing)
		// But we need to handle the special case where \ appears before the closing quote
		// Count consecutive backslashes to determine if the quote is escaped:
		// - r'\'  is unterminated (odd number of backslashes before quote)
		// - r'\\' is valid (even number of backslashes, quote closes string)

		// Check if we're at a quote that might close the string
		if !isTriple && ch == quote {
			// Count preceding backslashes
			backslashCount := 0
			checkPos := t.pos - 1
			for checkPos >= 0 && t.input[checkPos] == '\\' {
				backslashCount++
				checkPos--
			}

			// If odd number of backslashes, the quote is escaped (part of string content)
			if backslashCount%2 == 1 {
				// Quote is escaped - include it literally
				value.WriteByte(ch)
				t.advance()
				continue
			}

			// Even number of backslashes (including 0) - quote closes the string
			t.advance()
			break
		}

		// Regular character - include literally
		value.WriteByte(ch)
		t.advance()
	}

	return Token{
		Type:     TOKEN_STRING,
		Lexeme:   t.input[start:t.pos],
		Value:    core.StringValue(value.String()),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// scanRawBytesString scans a raw bytes literal (br"..." or rb"...")
// Combines raw string behavior (no escape processing) with bytes value return
func (t *PythonTokenizer) scanRawBytesString(quote byte, start, startLine, startCol int) Token {
	// Check for triple-quoted raw bytes string
	isTriple := false
	if !t.isAtEnd() && t.peek() == quote {
		if t.pos+1 < len(t.input) && t.input[t.pos+1] == quote {
			isTriple = true
			t.advance() // second quote
			t.advance() // third quote
		}
	}

	var value []byte
	for {
		if t.isAtEnd() {
			t.errors = append(t.errors, t.makeUnterminatedStringError(
				startLine, startCol, start, quote, isTriple, "br"+string(value)))
			break
		}

		ch := t.peek()

		// Check for end of string
		if !isTriple && ch == quote {
			t.advance()
			break
		}

		if isTriple {
			// Check for triple quote end
			if ch == quote && t.pos+2 < len(t.input) &&
				t.input[t.pos+1] == quote && t.input[t.pos+2] == quote {
				t.advance()
				t.advance()
				t.advance()
				break
			}
		}

		// In raw bytes strings, backslashes are literal (no escape processing)
		// But we need to handle the special case where \ appears before the closing quote
		// Count consecutive backslashes to determine if the quote is escaped:
		// - br'\'  is unterminated (odd number of backslashes before quote)
		// - br'\\' is valid (even number of backslashes, quote closes string)

		// Check if we're at a quote that might close the string
		if !isTriple && ch == quote {
			// Count preceding backslashes
			backslashCount := 0
			checkPos := t.pos - 1
			for checkPos >= 0 && t.input[checkPos] == '\\' {
				backslashCount++
				checkPos--
			}

			// If odd number of backslashes, the quote is escaped (part of string content)
			if backslashCount%2 == 1 {
				// Quote is escaped - include it literally
				value = append(value, ch)
				t.advance()
				continue
			}

			// Even number of backslashes (including 0) - quote closes the string
			t.advance()
			break
		}

		// Regular character - include literally
		value = append(value, ch)
		t.advance()
	}

	return Token{
		Type:     TOKEN_STRING,
		Lexeme:   t.input[start:t.pos],
		Value:    core.BytesValue(value),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// Helper functions for hex parsing
func isHexDigit(ch byte) bool {
	return (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')
}

func hexDigitValue(ch byte) int {
	if ch >= '0' && ch <= '9' {
		return int(ch - '0')
	}
	if ch >= 'a' && ch <= 'f' {
		return int(ch-'a') + 10
	}
	if ch >= 'A' && ch <= 'F' {
		return int(ch-'A') + 10
	}
	return 0
}

// Helper methods

func (t *PythonTokenizer) isAtEnd() bool {
	return t.pos >= len(t.input)
}

func (t *PythonTokenizer) peek() byte {
	if t.isAtEnd() {
		return 0
	}
	return t.input[t.pos]
}

func (t *PythonTokenizer) advance() byte {
	if t.isAtEnd() {
		return 0
	}
	ch := t.input[t.pos]
	t.pos++
	if ch == '\n' {
		t.line++
		t.col = 1
	} else {
		t.col++
	}
	return ch
}

func (t *PythonTokenizer) match(expected byte) bool {
	if t.isAtEnd() || t.peek() != expected {
		return false
	}
	t.advance()
	return true
}

func (t *PythonTokenizer) skipWhitespaceExceptNewlines() {
	for !t.isAtEnd() {
		ch := t.peek()
		if ch == ' ' || ch == '\t' || ch == '\r' {
			t.advance()
		} else {
			break
		}
	}
}

func (t *PythonTokenizer) skipComment() {
	// Skip until end of line
	for !t.isAtEnd() && t.peek() != '\n' {
		t.advance()
	}
}

// scanFString is a simplified version - delegates to existing implementation
func (t *PythonTokenizer) scanFString(quote byte, start, startLine, startCol int) Token {
	// For now, use the existing M28 f-string scanner
	// In a full implementation, this would need Python-specific f-string parsing
	var value strings.Builder
	for {
		if t.isAtEnd() {
			t.errors = append(t.errors, t.makeUnterminatedStringError(
				startLine, startCol, start, quote, false, "f"+value.String()))
			break
		}

		ch := t.peek()
		if ch == quote {
			t.advance()
			break
		}

		value.WriteByte(ch)
		t.advance()
	}

	return Token{
		Type:     TOKEN_FSTRING,
		Lexeme:   t.input[start:t.pos],
		Value:    core.StringValue(value.String()),
		Line:     startLine,
		Col:      startCol,
		StartPos: start,
		EndPos:   t.pos,
	}
}

// pythonIsAlpha checks if a character can start an identifier (Python style)
func pythonIsAlpha(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}

// pythonIsAlphaNumeric checks if a character can be in an identifier
func pythonIsAlphaNumeric(ch byte) bool {
	return pythonIsAlpha(ch) || isDigit(ch)
}

// makeUnterminatedStringError creates a detailed error message for unterminated strings
func (t *PythonTokenizer) makeUnterminatedStringError(
	startLine, startCol, startPos int, quote byte, isTriple bool, partialValue string) error {

	quoteChar := string(quote)
	stringType := "string"
	expectedEnd := quoteChar

	if isTriple {
		stringType = "triple-quoted string"
		expectedEnd = quoteChar + quoteChar + quoteChar
	}

	// Get a preview of the string content (first 50 chars)
	preview := partialValue
	if len(preview) > 50 {
		preview = preview[:50] + "..."
	}

	// Get context: last few tokens before this string
	contextTokens := ""
	if len(t.tokens) > 0 {
		startIdx := len(t.tokens) - 3
		if startIdx < 0 {
			startIdx = 0
		}
		recentTokens := []string{}
		for i := startIdx; i < len(t.tokens); i++ {
			tok := t.tokens[i]
			if tok.Type == TOKEN_NEWLINE {
				recentTokens = append(recentTokens, "NEWLINE")
			} else if len(tok.Lexeme) > 20 {
				recentTokens = append(recentTokens, tok.Lexeme[:20]+"...")
			} else {
				recentTokens = append(recentTokens, tok.Lexeme)
			}
		}
		if len(recentTokens) > 0 {
			contextTokens = fmt.Sprintf("\n  Context: previous tokens were: %v", recentTokens)
		}
	}

	// Show where we started and where we ended
	endLine := t.line
	endCol := t.col

	// Show opening quote(s) correctly
	openingQuotes := quoteChar
	if isTriple {
		openingQuotes = quoteChar + quoteChar + quoteChar
	}

	return fmt.Errorf(
		"unterminated %s: started at line %d, col %d with %s, reached EOF at line %d, col %d"+
			"\n  Expected closing: %s"+
			"\n  Parsed content: %q%s",
		stringType, startLine, startCol, openingQuotes, endLine, endCol,
		expectedEnd, preview, contextTokens)
}

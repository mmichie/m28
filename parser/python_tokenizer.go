package parser

import (
	"fmt"
	"strconv"
	"strings"
	"unicode/utf8"

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

// makeTokenizationError creates a TokenizationError with location and optional suggestion
func (t *PythonTokenizer) makeTokenizationError(message string, line, col int, suggestion string) error {
	return &TokenizationError{
		Message: message,
		Location: &core.SourceLocation{
			File:   t.filename,
			Line:   line,
			Column: col,
		},
		Suggestion: suggestion,
		Source:     t.input,
	}
}

// NewPythonTokenizer creates a new Python tokenizer
func NewPythonTokenizer(input string) *PythonTokenizer {
	return &PythonTokenizer{
		input:       input,
		pos:         0,
		line:        1,
		col:         1,
		filename:    "<eval>",
		tokens:      make([]Token, 0, 256),
		errors:      make([]error, 0),
		indentStack: []int{0}, // Start with base indentation level
		atLineStart: true,
		parenDepth:  0,
	}
}

// SetFilename sets the filename for error reporting
func (t *PythonTokenizer) SetFilename(filename string) {
	t.filename = filename
}

// Tokenize performs lexical analysis and returns all tokens
func (t *PythonTokenizer) Tokenize() ([]Token, error) {
	core.Log.Trace(core.SubsystemParser, "Tokenization started", "file", t.filename)

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

		// Log token at trace level
		core.Log.Trace(core.SubsystemParser, "Token scanned", "file", t.filename, "type", tok.Type.String(), "lexeme", tok.Lexeme, "line", tok.Line, "col", tok.Col)

		t.tokens = append(t.tokens, tok)

		// Track newlines
		if tok.Type == TOKEN_NEWLINE {
			t.atLineStart = true
		} else if tok.Type != TOKEN_COMMENT {
			t.atLineStart = false
		}

		if tok.Type == TOKEN_ERROR {
			core.Log.Error(core.SubsystemParser, "Tokenization error encountered", "file", t.filename, "line", tok.Line, "col", tok.Col, "lexeme", tok.Lexeme)
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
		core.Log.Error(core.SubsystemParser, "Tokenization failed with errors", "file", t.filename, "error_count", len(t.errors), "tokens_parsed", len(t.tokens))
		// Return the first error directly (it's already a TokenizationError with location)
		// If there are multiple errors, we'll report the first one
		// ErrorFormatter can then properly format it with source context
		return t.tokens, t.errors[0]
	}

	core.Log.Trace(core.SubsystemParser, "Tokenization completed successfully", "file", t.filename, "token_count", len(t.tokens))
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
			err := t.makeTokenizationError(
				"unindent does not match any outer indentation level",
				startLine,
				startCol,
				"Make sure your indentation is consistent (use spaces or tabs, not both)",
			)
			t.errors = append(t.errors, err)
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
		err := t.makeTokenizationError(
			"unexpected character '\\'",
			startLine,
			startCol,
			"Backslash must be followed by a newline for line continuation, or used inside a string",
		)
		t.errors = append(t.errors, err)
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

	// Raw f-strings (fr"..." or rf"...")
	if (ch == 'f' || ch == 'r') && !t.isAtEnd() {
		next := t.peek()
		if (next == 'r' || next == 'f') && next != ch {
			// We have fr or rf
			t.advance() // consume second prefix
			if !t.isAtEnd() && (t.peek() == '"' || t.peek() == '\'') {
				quote := t.advance()
				// Both fr and rf are raw f-strings - treat as regular f-strings for now
				return t.scanFString(quote, start, startLine, startCol)
			}
			// Not a string after fr/rf, backtrack
			t.pos = start
			t.line = startLine
			t.col = startCol
		}
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
		// Check for @= (matrix multiply assign)
		if t.match('=') {
			return makeToken(TOKEN_AT_EQ)
		}
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
		// Check for float literal starting with . (e.g., .995)
		if !t.isAtEnd() && isDigit(t.peek()) {
			return t.scanNumber(start, startLine, startCol)
		}
		return makeToken(TOKEN_DOT)

	default:
		suggestion := ""
		if ch < core.ASCIIPrintableMin || ch > core.ASCIIPrintableMaxVisible {
			suggestion = "Check for hidden control characters or use proper encoding"
		} else {
			suggestion = "This character is not valid Python syntax"
		}
		err := t.makeTokenizationError(
			fmt.Sprintf("unexpected character '%c'", ch),
			startLine,
			startCol,
			suggestion,
		)
		t.errors = append(t.errors, err)
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

			// Validate underscore placement
			if errMsg := validateUnderscores(lexeme); errMsg != "" {
				return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue(errMsg),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
			}

			// Remove underscores and parse
			cleanLexeme := strings.ReplaceAll(lexeme[2:], "_", "") // skip "0b" prefix
			if cleanLexeme == "" {
				// Invalid binary literal (just "0b" with no digits)
				return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue("invalid binary literal"),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
			}
			// Try parsing as signed int64 first
			if i, err := strconv.ParseInt(cleanLexeme, 2, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(i),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If that fails (overflow), try unsigned int64
			if u, err := strconv.ParseUint(cleanLexeme, 2, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(float64(u)),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If both fail, use arbitrary-precision BigInt (Python supports arbitrary-sized integers)
			if bigInt, err := core.NewBigIntFromString(cleanLexeme, 2); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: bigInt,
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If even BigInt fails, it's malformed (e.g., contains invalid characters)
			return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue("invalid binary literal"),
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

			// Validate underscore placement
			if errMsg := validateUnderscores(lexeme); errMsg != "" {
				return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue(errMsg),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
			}

			cleanLexeme := strings.ReplaceAll(lexeme[2:], "_", "") // skip "0o" prefix
			if cleanLexeme == "" {
				// Invalid octal literal (just "0o" with no digits)
				return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue("invalid octal literal"),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
			}
			// Try parsing as signed int64 first
			if i, err := strconv.ParseInt(cleanLexeme, 8, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(i),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If that fails (overflow), try unsigned int64
			if u, err := strconv.ParseUint(cleanLexeme, 8, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(float64(u)),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If both fail, use arbitrary-precision BigInt
			if bigInt, err := core.NewBigIntFromString(cleanLexeme, 8); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: bigInt,
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If even BigInt fails, it's malformed
			return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue("invalid octal literal"),
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

			// Validate underscore placement
			if errMsg := validateUnderscores(lexeme); errMsg != "" {
				return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue(errMsg),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
			}

			cleanLexeme := strings.ReplaceAll(lexeme[2:], "_", "") // skip "0x" prefix
			if cleanLexeme == "" {
				// Invalid hexadecimal literal (just "0x" with no digits)
				return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue("invalid hexadecimal literal"),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
			}
			// Try parsing as signed int64 first
			if i, err := strconv.ParseInt(cleanLexeme, 16, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(i),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If that fails (overflow), try unsigned int64
			if u, err := strconv.ParseUint(cleanLexeme, 16, 64); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: core.NumberValue(float64(u)),
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If both fail, use arbitrary-precision BigInt
			if bigInt, err := core.NewBigIntFromString(cleanLexeme, 16); err == nil {
				return Token{
					Type: TOKEN_NUMBER, Lexeme: lexeme, Value: bigInt,
					Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos,
				}
			}
			// If even BigInt fails, it's malformed
			return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue("invalid hexadecimal literal"),
				Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
		}
	}

	// Regular decimal number
	// Check if we're starting with a decimal point (e.g., .995)
	startsWithDot := t.input[start] == '.'

	if !startsWithDot {
		// Scan integer part (allow underscores for readability)
		for !t.isAtEnd() && (isDigit(t.peek()) || t.peek() == '_') {
			t.advance()
		}
	}

	// Check for decimal point (or we started with one)
	// Python allows both 314. and .314 as valid float literals
	hasDecimalPoint := startsWithDot
	if !startsWithDot && !t.isAtEnd() && t.peek() == '.' {
		// Look ahead to see if this is a float or dot notation
		nextPos := t.pos + 1
		if nextPos < len(t.input) {
			nextChar := t.input[nextPos]
			// It's a float if:
			// (1) next char is digit (1.5)
			// (2) next char is 'e' or 'E' for exponent (1.e5)
			// (3) next char is underscore followed by digit (1._4 - invalid but should be caught)
			// (4) next char is not a letter/underscore (trailing dot like 314.)
			isFloat := isDigit(nextChar) || nextChar == 'e' || nextChar == 'E' || !(isLetter(nextChar) || nextChar == '_')

			// Special case: underscore after dot followed by digit should be parsed as (invalid) number
			if nextChar == '_' && nextPos+1 < len(t.input) && isDigit(t.input[nextPos+1]) {
				isFloat = true
			}

			if isFloat {
				t.advance() // consume '.'
				hasDecimalPoint = true
				// Scan fractional part (may be empty for trailing dot like 314.)
				for !t.isAtEnd() && (isDigit(t.peek()) || t.peek() == '_') {
					t.advance()
				}
			}
		} else {
			// End of input after dot - it's a float like 314.
			t.advance()
			hasDecimalPoint = true
		}
	} else if startsWithDot {
		// Already consumed the dot, scan fractional part
		for !t.isAtEnd() && (isDigit(t.peek()) || t.peek() == '_') {
			t.advance()
		}
	}

	// Check for scientific notation
	// Must be: e/E followed by optional +/- and at least one digit
	if !t.isAtEnd() && (t.peek() == 'e' || t.peek() == 'E') {
		// Look ahead to verify this is valid scientific notation
		lookAhead := t.pos + 1
		if lookAhead < len(t.input) {
			ch := t.input[lookAhead]
			// Check for optional sign
			if ch == '+' || ch == '-' {
				lookAhead++
				if lookAhead >= len(t.input) {
					// e/E followed by sign but no digits - not scientific notation
					goto skipScientific
				}
				ch = t.input[lookAhead]
			}
			// Must have at least one digit after e/E (or e/E+/-)
			if !isDigit(ch) {
				// No digit - this is not scientific notation (e.g., "1Else")
				goto skipScientific
			}
		} else {
			// Nothing after e/E - not scientific notation
			goto skipScientific
		}

		// Valid scientific notation - consume it
		t.advance() // consume e/E
		if !t.isAtEnd() && (t.peek() == '+' || t.peek() == '-') {
			t.advance()
		}
		for !t.isAtEnd() && (isDigit(t.peek()) || t.peek() == '_') {
			t.advance()
		}
	}
skipScientific:

	// Check for complex number suffix 'j' or 'J'
	isComplex := false
	if !t.isAtEnd() && (t.peek() == 'j' || t.peek() == 'J') {
		t.advance() // consume 'j' or 'J'
		isComplex = true
	}

	// Check for invalid literal - number followed by letter (e.g., "1Else", "2abc")
	// In Python 3, this is a SyntaxError: invalid decimal literal
	if !t.isAtEnd() && pythonIsAlpha(t.peek()) {
		// Consume all following alphanumeric characters for error message
		for !t.isAtEnd() && pythonIsAlphaNumeric(t.peek()) {
			t.advance()
		}
		lexeme := t.input[start:t.pos]
		return Token{
			Type:     TOKEN_ERROR,
			Lexeme:   lexeme,
			Value:    core.StringValue("invalid decimal literal"),
			Line:     startLine,
			Col:      startCol,
			StartPos: start,
			EndPos:   t.pos,
		}
	}

	lexeme := t.input[start:t.pos]

	// Validate underscore placement for decimal numbers
	if errMsg := validateUnderscores(lexeme); errMsg != "" {
		return Token{Type: TOKEN_ERROR, Lexeme: lexeme, Value: core.StringValue(errMsg),
			Line: startLine, Col: startCol, StartPos: start, EndPos: t.pos}
	}

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
	} else if hasDecimalPoint || strings.ContainsAny(cleanLexeme, "eE") {
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
		} else if u, err := strconv.ParseUint(cleanLexeme, 10, 64); err == nil {
			value = core.NumberValue(float64(u))
		} else {
			// Use arbitrary-precision BigInt for very large integers
			if bigInt, err := core.NewBigIntFromString(cleanLexeme, 10); err == nil {
				value = bigInt
			} else {
				// Parse error - shouldn't happen if lexer scanned valid digits
				value = core.NumberValue(0)
			}
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

		// Handle escape sequences (applies to both single and triple-quoted strings)
		if ch == '\\' {
			t.advance()
			if !t.isAtEnd() {
				escaped := t.peek()
				// Line continuation: backslash followed by newline removes both
				if escaped == '\n' {
					t.advance() // consume the newline
					t.line++
					t.col = 1
					continue // skip both backslash and newline
				}
				// Handle \r\n line continuation (Windows line endings)
				if escaped == '\r' && t.pos+1 < len(t.input) && t.input[t.pos+1] == '\n' {
					t.advance() // consume \r
					t.advance() // consume \n
					t.line++
					t.col = 1
					continue // skip backslash and line ending
				}
				t.advance() // consume the escaped character
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
				case 'x':
					// Hex escape: \xNN - produces character with code point 0xNN
					if t.pos+1 < len(t.input) {
						hex1 := t.input[t.pos]
						hex2 := t.input[t.pos+1]
						if isHexDigit(hex1) && isHexDigit(hex2) {
							hexVal := hexDigitValue(hex1)*16 + hexDigitValue(hex2)
							// Write as a Unicode rune, not as a raw byte
							value.WriteRune(rune(hexVal))
							t.pos += 2
						} else {
							value.WriteByte('x')
						}
					} else {
						value.WriteByte('x')
					}
				case 'u':
					// Unicode escape: \uHHHH (4 hex digits)
					if t.pos+3 < len(t.input) {
						hex1 := t.input[t.pos]
						hex2 := t.input[t.pos+1]
						hex3 := t.input[t.pos+2]
						hex4 := t.input[t.pos+3]
						if isHexDigit(hex1) && isHexDigit(hex2) && isHexDigit(hex3) && isHexDigit(hex4) {
							hexVal := hexDigitValue(hex1)*4096 + hexDigitValue(hex2)*256 + hexDigitValue(hex3)*16 + hexDigitValue(hex4)
							// Convert rune to UTF-8 and write to string builder
							value.WriteRune(rune(hexVal))
							t.pos += 4
						} else {
							value.WriteByte('u')
						}
					} else {
						value.WriteByte('u')
					}
				case 'U':
					// Long unicode escape: \UHHHHHHHH (8 hex digits)
					if t.pos+7 < len(t.input) {
						hexVal := 0
						valid := true
						for i := 0; i < 8; i++ {
							h := t.input[t.pos+i]
							if isHexDigit(h) {
								hexVal = hexVal*16 + hexDigitValue(h)
							} else {
								valid = false
								break
							}
						}
						if valid && hexVal <= 0x10FFFF {
							// Convert rune to UTF-8 and write to string builder
							value.WriteRune(rune(hexVal))
							t.pos += 8
						} else {
							value.WriteByte('U')
						}
					} else {
						value.WriteByte('U')
					}
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
				case 'u':
					// Unicode escape: \uHHHH (4 hex digits)
					if t.pos+3 < len(t.input) {
						hex1 := t.input[t.pos]
						hex2 := t.input[t.pos+1]
						hex3 := t.input[t.pos+2]
						hex4 := t.input[t.pos+3]
						if isHexDigit(hex1) && isHexDigit(hex2) && isHexDigit(hex3) && isHexDigit(hex4) {
							hexVal := hexDigitValue(hex1)*4096 + hexDigitValue(hex2)*256 + hexDigitValue(hex3)*16 + hexDigitValue(hex4)
							// Convert rune to UTF-8 bytes
							var buf [4]byte
							n := utf8.EncodeRune(buf[:], rune(hexVal))
							value = append(value, buf[:n]...)
							t.pos += 4
						} else {
							// Invalid unicode escape
							value = append(value, 'u')
						}
					} else {
						value = append(value, 'u')
					}
				case 'U':
					// Long unicode escape: \UHHHHHHHH (8 hex digits)
					if t.pos+7 < len(t.input) {
						hexVal := 0
						valid := true
						for i := 0; i < 8; i++ {
							h := t.input[t.pos+i]
							if isHexDigit(h) {
								hexVal = hexVal*16 + hexDigitValue(h)
							} else {
								valid = false
								break
							}
						}
						if valid && hexVal <= 0x10FFFF {
							// Convert rune to UTF-8 bytes
							var buf [4]byte
							n := utf8.EncodeRune(buf[:], rune(hexVal))
							value = append(value, buf[:n]...)
							t.pos += 8
						} else {
							// Invalid unicode escape
							value = append(value, 'U')
						}
					} else {
						value = append(value, 'U')
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
					if octalVal <= core.MaxByteValue {
						value = append(value, byte(octalVal))
					} else {
						// Invalid octal value, just use modulo
						value = append(value, byte(octalVal%(core.MaxByteValue+1)))
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

// validateUnderscores checks for invalid underscore placement in numeric literals
// Returns an error message if invalid, empty string if valid
func validateUnderscores(lexeme string) string {
	// Find where digits start (skip prefix like 0b, 0o, 0x)
	digitStart := 0
	if len(lexeme) >= 2 && lexeme[0] == '0' {
		ch := lexeme[1]
		if ch == 'b' || ch == 'B' || ch == 'o' || ch == 'O' || ch == 'x' || ch == 'X' {
			digitStart = 2
		}
	}

	// Check for underscore immediately after prefix (0b_, 0x_, 0o_)
	if digitStart > 0 && digitStart < len(lexeme) && lexeme[digitStart] == '_' {
		return "invalid decimal literal"
	}

	// Check for trailing underscore
	if len(lexeme) > 0 && lexeme[len(lexeme)-1] == '_' {
		return "invalid decimal literal"
	}

	// Check for consecutive underscores and underscore around decimal point
	prevWasUnderscore := false
	for i := digitStart; i < len(lexeme); i++ {
		ch := lexeme[i]

		if ch == '_' {
			if prevWasUnderscore {
				return "invalid decimal literal"
			}
			prevWasUnderscore = true
		} else if ch == '.' {
			// Underscore before dot (1_.4)
			if prevWasUnderscore {
				return "invalid decimal literal"
			}
			// Underscore after dot (1._4)
			if i+1 < len(lexeme) && lexeme[i+1] == '_' {
				return "invalid decimal literal"
			}
			prevWasUnderscore = false
		} else if ch == 'e' || ch == 'E' {
			// Underscore before exponent (1_e10)
			if prevWasUnderscore {
				return "invalid decimal literal"
			}
			// Underscore after exponent (1e_10) - check next char
			if i+1 < len(lexeme) && lexeme[i+1] == '_' {
				return "invalid decimal literal"
			}
			// Handle optional sign after exponent
			if i+1 < len(lexeme) && (lexeme[i+1] == '+' || lexeme[i+1] == '-') {
				if i+2 < len(lexeme) && lexeme[i+2] == '_' {
					return "invalid decimal literal"
				}
			}
			prevWasUnderscore = false
		} else if ch == 'j' || ch == 'J' {
			// Underscore before complex suffix (1_j)
			if prevWasUnderscore {
				return "invalid decimal literal"
			}
			prevWasUnderscore = false
		} else {
			prevWasUnderscore = false
		}
	}

	return "" // Valid
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
	// Proper f-string scanner following PEP 498 and PEP 701
	// Handles nested quotes, expressions in braces, escape sequences

	var value strings.Builder
	isTriple := false

	// Check if this is a triple-quoted f-string
	if !t.isAtEnd() && t.peek() == quote {
		t.advance()
		if !t.isAtEnd() && t.peek() == quote {
			t.advance()
			isTriple = true
		} else {
			// Just two quotes - empty f-string
			return Token{
				Type:     TOKEN_FSTRING,
				Lexeme:   t.input[start:t.pos],
				Value:    core.StringValue(""),
				Line:     startLine,
				Col:      startCol,
				StartPos: start,
				EndPos:   t.pos,
			}
		}
	}

	for {
		if t.isAtEnd() {
			t.errors = append(t.errors, t.makeUnterminatedStringError(
				startLine, startCol, start, quote, isTriple, "f"+value.String()))
			break
		}

		ch := t.peek()

		// Check for closing quote(s)
		if ch == quote {
			if isTriple {
				// Need three consecutive quotes
				if t.pos+2 < len(t.input) && t.input[t.pos+1] == quote && t.input[t.pos+2] == quote {
					t.advance() // first quote
					t.advance() // second quote
					t.advance() // third quote
					break
				}
				// Single quote in triple-quoted string - just part of content
				value.WriteByte(ch)
				t.advance()
				continue
			} else {
				// Single-quoted f-string ending
				t.advance()
				break
			}
		}

		// Handle escape sequences in string portions
		// F-strings should interpret escape sequences just like regular strings
		if ch == '\\' {
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
				case 'a':
					value.WriteByte('\a') // bell
				case 'b':
					value.WriteByte('\b') // backspace
				case 'f':
					value.WriteByte('\f') // form feed
				case 'v':
					value.WriteByte('\v') // vertical tab
				case '0':
					value.WriteByte('\x00') // null
				default:
					// For unknown escapes, include the backslash and character
					// This matches Python's behavior for invalid escape sequences
					value.WriteByte('\\')
					value.WriteByte(escaped)
				}
			}
			continue
		}

		// Handle braces for expressions
		if ch == '{' {
			t.advance()
			// Check for escaped brace {{
			if !t.isAtEnd() && t.peek() == '{' {
				value.WriteByte('{')
				value.WriteByte('{')
				t.advance()
				continue
			}
			// Start of expression - scan until matching }
			value.WriteByte('{')
			t.scanFStringExpression(&value, quote)
			value.WriteByte('}')
			continue
		}

		// Handle closing brace }} (escaped)
		if ch == '}' {
			t.advance()
			if !t.isAtEnd() && t.peek() == '}' {
				value.WriteByte('}')
				value.WriteByte('}')
				t.advance()
				continue
			}
			// Single } in f-string literal is an error, but we'll allow it for now
			value.WriteByte('}')
			continue
		}

		// Regular character
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

// scanFStringExpression scans the expression portion inside { } in an f-string
// This allows nested quotes, nested braces, and any Python expression
func (t *PythonTokenizer) scanFStringExpression(value *strings.Builder, outerQuote byte) {
	braceDepth := 1 // We've already seen the opening {

	for braceDepth > 0 && !t.isAtEnd() {
		ch := t.peek()

		// Handle nested braces
		if ch == '{' {
			braceDepth++
			value.WriteByte(ch)
			t.advance()
			continue
		}

		if ch == '}' {
			braceDepth--
			if braceDepth > 0 {
				value.WriteByte(ch)
			}
			t.advance()
			continue
		}

		// Handle strings inside expressions - they can use any quote type
		if ch == '"' || ch == '\'' {
			quoteChar := ch
			value.WriteByte(ch)
			t.advance()

			// Scan the string inside the expression
			for !t.isAtEnd() {
				ch := t.peek()
				value.WriteByte(ch)

				if ch == '\\' {
					t.advance()
					if !t.isAtEnd() {
						value.WriteByte(t.peek())
						t.advance()
					}
					continue
				}

				if ch == quoteChar {
					t.advance()
					break
				}

				t.advance()
			}
			continue
		}

		// Regular character in expression
		value.WriteByte(ch)
		t.advance()
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

	message := fmt.Sprintf("unterminated %s", stringType)
	suggestion := fmt.Sprintf("Add closing %s at end of string", expectedEnd)

	return t.makeTokenizationError(message, startLine, startCol, suggestion)
}

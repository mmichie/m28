package parser

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// TokenType represents the type of a token
type TokenType int

const (
	// Literals
	TOKEN_NUMBER  TokenType = iota // 42, 3.14, 0x1A
	TOKEN_STRING                   // "hello", 'world'
	TOKEN_FSTRING                  // f"hello {name}"
	TOKEN_SSTRING                  // s"(+ {x} 1)", rs"..."
	TOKEN_TRUE                     // True, true
	TOKEN_FALSE                    // False, false
	TOKEN_NIL                      // None, nil

	// Identifiers/Symbols
	TOKEN_IDENTIFIER // foo, bar, my_var

	// Arithmetic Operators
	TOKEN_PLUS         // +
	TOKEN_MINUS        // -
	TOKEN_STAR         // *
	TOKEN_SLASH        // /
	TOKEN_DOUBLE_SLASH // //
	TOKEN_PERCENT      // %
	TOKEN_DOUBLE_STAR  // **

	// Comparison Operators
	TOKEN_EQ // ==
	TOKEN_NE // !=
	TOKEN_LT // <
	TOKEN_GT // >
	TOKEN_LE // <=
	TOKEN_GE // >=

	// Bitwise Operators
	TOKEN_AMPERSAND // &
	TOKEN_PIPE      // |
	TOKEN_CARET     // ^
	TOKEN_DOUBLE_LT // <<
	TOKEN_DOUBLE_GT // >>
	TOKEN_TILDE     // ~ (bitwise NOT)

	// Logical Operators (keywords)
	TOKEN_AND    // and
	TOKEN_OR     // or
	TOKEN_NOT    // not
	TOKEN_IN     // in
	TOKEN_NOT_IN // not in
	TOKEN_IS     // is
	TOKEN_IS_NOT // is not

	// Assignment Operators
	TOKEN_ASSIGN          // =
	TOKEN_PLUS_EQ         // +=
	TOKEN_MINUS_EQ        // -=
	TOKEN_STAR_EQ         // *=
	TOKEN_SLASH_EQ        // /=
	TOKEN_DOUBLE_SLASH_EQ // //=
	TOKEN_PERCENT_EQ      // %=
	TOKEN_DOUBLE_STAR_EQ  // **=
	TOKEN_AMPERSAND_EQ    // &=
	TOKEN_PIPE_EQ         // |=
	TOKEN_CARET_EQ        // ^=
	TOKEN_DOUBLE_LT_EQ    // <<=
	TOKEN_DOUBLE_GT_EQ    // >>=

	// Delimiters
	TOKEN_LPAREN    // (
	TOKEN_RPAREN    // )
	TOKEN_LBRACKET  // [
	TOKEN_RBRACKET  // ]
	TOKEN_LBRACE    // {
	TOKEN_RBRACE    // }
	TOKEN_COMMA     // ,
	TOKEN_DOT       // .
	TOKEN_COLON     // :
	TOKEN_SEMICOLON // ;

	// Special/Reader Macros
	TOKEN_BACKTICK      // ` (quasiquote)
	TOKEN_COMMA_UNQUOTE // , (when used as unquote in macro context)
	TOKEN_COMMA_AT      // ,@ (unquote-splicing)
	TOKEN_AT            // @ (decorators)

	// Meta tokens
	TOKEN_NEWLINE // Significant in REPL
	TOKEN_EOF     // End of file/input
	TOKEN_ERROR   // Lexical error
)

// Token represents a lexical token
type Token struct {
	Type     TokenType  // Token type
	Lexeme   string     // Original text
	Value    core.Value // Parsed value (for literals)
	Line     int        // Source line number (1-indexed)
	Col      int        // Source column number (1-indexed)
	StartPos int        // Absolute character position (start)
	EndPos   int        // Absolute character position (end)
}

// String returns a string representation of the token
func (t Token) String() string {
	if t.Value != nil {
		return fmt.Sprintf("%s(%q=%v) at %d:%d", t.Type, t.Lexeme, t.Value, t.Line, t.Col)
	}
	return fmt.Sprintf("%s(%q) at %d:%d", t.Type, t.Lexeme, t.Line, t.Col)
}

// IsOperator returns true if the token is an operator
func (t Token) IsOperator() bool {
	return t.Type >= TOKEN_PLUS && t.Type <= TOKEN_DOUBLE_GT_EQ
}

// IsLiteral returns true if the token is a literal value
func (t Token) IsLiteral() bool {
	return t.Type >= TOKEN_NUMBER && t.Type <= TOKEN_NIL
}

// IsKeyword returns true if the token is a keyword
func (t Token) IsKeyword() bool {
	return t.Type >= TOKEN_AND && t.Type <= TOKEN_IS_NOT
}

// String returns the name of the token type
func (tt TokenType) String() string {
	switch tt {
	case TOKEN_NUMBER:
		return "NUMBER"
	case TOKEN_STRING:
		return "STRING"
	case TOKEN_FSTRING:
		return "FSTRING"
	case TOKEN_SSTRING:
		return "SSTRING"
	case TOKEN_TRUE:
		return "TRUE"
	case TOKEN_FALSE:
		return "FALSE"
	case TOKEN_NIL:
		return "NIL"
	case TOKEN_IDENTIFIER:
		return "IDENTIFIER"
	case TOKEN_PLUS:
		return "PLUS"
	case TOKEN_MINUS:
		return "MINUS"
	case TOKEN_STAR:
		return "STAR"
	case TOKEN_SLASH:
		return "SLASH"
	case TOKEN_DOUBLE_SLASH:
		return "DOUBLE_SLASH"
	case TOKEN_PERCENT:
		return "PERCENT"
	case TOKEN_DOUBLE_STAR:
		return "DOUBLE_STAR"
	case TOKEN_EQ:
		return "EQ"
	case TOKEN_NE:
		return "NE"
	case TOKEN_LT:
		return "LT"
	case TOKEN_GT:
		return "GT"
	case TOKEN_LE:
		return "LE"
	case TOKEN_GE:
		return "GE"
	case TOKEN_AMPERSAND:
		return "AMPERSAND"
	case TOKEN_PIPE:
		return "PIPE"
	case TOKEN_CARET:
		return "CARET"
	case TOKEN_DOUBLE_LT:
		return "DOUBLE_LT"
	case TOKEN_DOUBLE_GT:
		return "DOUBLE_GT"
	case TOKEN_TILDE:
		return "TILDE"
	case TOKEN_AND:
		return "AND"
	case TOKEN_OR:
		return "OR"
	case TOKEN_NOT:
		return "NOT"
	case TOKEN_IN:
		return "IN"
	case TOKEN_NOT_IN:
		return "NOT_IN"
	case TOKEN_IS:
		return "IS"
	case TOKEN_IS_NOT:
		return "IS_NOT"
	case TOKEN_ASSIGN:
		return "ASSIGN"
	case TOKEN_PLUS_EQ:
		return "PLUS_EQ"
	case TOKEN_MINUS_EQ:
		return "MINUS_EQ"
	case TOKEN_STAR_EQ:
		return "STAR_EQ"
	case TOKEN_SLASH_EQ:
		return "SLASH_EQ"
	case TOKEN_DOUBLE_SLASH_EQ:
		return "DOUBLE_SLASH_EQ"
	case TOKEN_PERCENT_EQ:
		return "PERCENT_EQ"
	case TOKEN_DOUBLE_STAR_EQ:
		return "DOUBLE_STAR_EQ"
	case TOKEN_AMPERSAND_EQ:
		return "AMPERSAND_EQ"
	case TOKEN_PIPE_EQ:
		return "PIPE_EQ"
	case TOKEN_CARET_EQ:
		return "CARET_EQ"
	case TOKEN_DOUBLE_LT_EQ:
		return "DOUBLE_LT_EQ"
	case TOKEN_DOUBLE_GT_EQ:
		return "DOUBLE_GT_EQ"
	case TOKEN_LPAREN:
		return "LPAREN"
	case TOKEN_RPAREN:
		return "RPAREN"
	case TOKEN_LBRACKET:
		return "LBRACKET"
	case TOKEN_RBRACKET:
		return "RBRACKET"
	case TOKEN_LBRACE:
		return "LBRACE"
	case TOKEN_RBRACE:
		return "RBRACE"
	case TOKEN_COMMA:
		return "COMMA"
	case TOKEN_DOT:
		return "DOT"
	case TOKEN_COLON:
		return "COLON"
	case TOKEN_SEMICOLON:
		return "SEMICOLON"
	case TOKEN_BACKTICK:
		return "BACKTICK"
	case TOKEN_COMMA_UNQUOTE:
		return "COMMA_UNQUOTE"
	case TOKEN_COMMA_AT:
		return "COMMA_AT"
	case TOKEN_AT:
		return "AT"
	case TOKEN_NEWLINE:
		return "NEWLINE"
	case TOKEN_EOF:
		return "EOF"
	case TOKEN_ERROR:
		return "ERROR"
	default:
		return fmt.Sprintf("UNKNOWN(%d)", tt)
	}
}

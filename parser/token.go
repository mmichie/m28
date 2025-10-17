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

	// Definition keyword
	TOKEN_DEF // def

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
	TOKEN_LPAREN     // (
	TOKEN_RPAREN     // )
	TOKEN_LBRACKET   // [
	TOKEN_RBRACKET   // ]
	TOKEN_LBRACE     // {
	TOKEN_RBRACE     // }
	TOKEN_COMMA      // ,
	TOKEN_DOT        // .
	TOKEN_COLON      // :
	TOKEN_COLONEQUAL // := (walrus operator)
	TOKEN_SEMICOLON  // ;

	// Special/Reader Macros
	TOKEN_BACKTICK      // ` (quasiquote)
	TOKEN_COMMA_UNQUOTE // , (when used as unquote in macro context)
	TOKEN_COMMA_AT      // ,@ (unquote-splicing)
	TOKEN_AT            // @ (decorators)

	// Python-specific keywords
	TOKEN_CLASS    // class
	TOKEN_IF       // if
	TOKEN_ELIF     // elif
	TOKEN_ELSE     // else
	TOKEN_FOR      // for
	TOKEN_WHILE    // while
	TOKEN_BREAK    // break
	TOKEN_CONTINUE // continue
	TOKEN_RETURN   // return
	TOKEN_PASS     // pass
	TOKEN_IMPORT   // import
	TOKEN_FROM     // from
	TOKEN_AS       // as
	TOKEN_TRY      // try
	TOKEN_EXCEPT   // except
	TOKEN_FINALLY  // finally
	TOKEN_RAISE    // raise
	TOKEN_WITH     // with
	TOKEN_LAMBDA   // lambda
	TOKEN_YIELD    // yield
	TOKEN_ASYNC    // async
	TOKEN_AWAIT    // await
	TOKEN_ASSERT   // assert
	TOKEN_DEL      // del
	TOKEN_GLOBAL   // global
	TOKEN_NONLOCAL // nonlocal

	// Python-specific operators/tokens
	TOKEN_ARROW    // -> (type hint arrow)
	TOKEN_ELLIPSIS // ... (Ellipsis)
	TOKEN_BANG     // ! (not standard Python, but useful)

	// Indentation tokens (Python-specific)
	TOKEN_INDENT // Increase indentation level
	TOKEN_DEDENT // Decrease indentation level

	// Augmented assignment tokens (normalized names)
	TOKEN_PLUS_ASSIGN        // +=
	TOKEN_MINUS_ASSIGN       // -=
	TOKEN_STAR_ASSIGN        // *=
	TOKEN_SLASH_ASSIGN       // /=
	TOKEN_DOUBLESLASH        // //
	TOKEN_DOUBLESLASH_ASSIGN // //=
	TOKEN_PERCENT_ASSIGN     // %=
	TOKEN_DOUBLESTAR         // **
	TOKEN_DOUBLESTAR_ASSIGN  // **=
	TOKEN_AMPERSAND_ASSIGN   // &=
	TOKEN_PIPE_ASSIGN        // |=
	TOKEN_CARET_ASSIGN       // ^=
	TOKEN_LSHIFT             // <<
	TOKEN_LSHIFT_ASSIGN      // <<=
	TOKEN_RSHIFT             // >>
	TOKEN_RSHIFT_ASSIGN      // >>=

	// Comparison operators (normalized names)
	TOKEN_EQUALEQUAL   // ==
	TOKEN_NOTEQUAL     // !=
	TOKEN_LESS         // <
	TOKEN_GREATER      // >
	TOKEN_LESSEQUAL    // <=
	TOKEN_GREATEREQUAL // >=

	// Meta tokens
	TOKEN_NEWLINE // Significant in REPL and Python
	TOKEN_COMMENT // # comment
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
	return t.Type >= TOKEN_AND && t.Type <= TOKEN_DEF
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
	case TOKEN_DEF:
		return "DEF"
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
	case TOKEN_COLONEQUAL:
		return "COLONEQUAL"
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
	case TOKEN_CLASS:
		return "CLASS"
	case TOKEN_IF:
		return "IF"
	case TOKEN_ELIF:
		return "ELIF"
	case TOKEN_ELSE:
		return "ELSE"
	case TOKEN_FOR:
		return "FOR"
	case TOKEN_WHILE:
		return "WHILE"
	case TOKEN_BREAK:
		return "BREAK"
	case TOKEN_CONTINUE:
		return "CONTINUE"
	case TOKEN_RETURN:
		return "RETURN"
	case TOKEN_PASS:
		return "PASS"
	case TOKEN_IMPORT:
		return "IMPORT"
	case TOKEN_FROM:
		return "FROM"
	case TOKEN_AS:
		return "AS"
	case TOKEN_TRY:
		return "TRY"
	case TOKEN_EXCEPT:
		return "EXCEPT"
	case TOKEN_FINALLY:
		return "FINALLY"
	case TOKEN_RAISE:
		return "RAISE"
	case TOKEN_WITH:
		return "WITH"
	case TOKEN_LAMBDA:
		return "LAMBDA"
	case TOKEN_YIELD:
		return "YIELD"
	case TOKEN_ASYNC:
		return "ASYNC"
	case TOKEN_AWAIT:
		return "AWAIT"
	case TOKEN_ASSERT:
		return "ASSERT"
	case TOKEN_DEL:
		return "DEL"
	case TOKEN_GLOBAL:
		return "GLOBAL"
	case TOKEN_NONLOCAL:
		return "NONLOCAL"
	case TOKEN_ARROW:
		return "ARROW"
	case TOKEN_ELLIPSIS:
		return "ELLIPSIS"
	case TOKEN_BANG:
		return "BANG"
	case TOKEN_INDENT:
		return "INDENT"
	case TOKEN_DEDENT:
		return "DEDENT"
	case TOKEN_PLUS_ASSIGN:
		return "PLUS_ASSIGN"
	case TOKEN_MINUS_ASSIGN:
		return "MINUS_ASSIGN"
	case TOKEN_STAR_ASSIGN:
		return "STAR_ASSIGN"
	case TOKEN_SLASH_ASSIGN:
		return "SLASH_ASSIGN"
	case TOKEN_DOUBLESLASH:
		return "DOUBLESLASH"
	case TOKEN_DOUBLESLASH_ASSIGN:
		return "DOUBLESLASH_ASSIGN"
	case TOKEN_PERCENT_ASSIGN:
		return "PERCENT_ASSIGN"
	case TOKEN_DOUBLESTAR:
		return "DOUBLESTAR"
	case TOKEN_DOUBLESTAR_ASSIGN:
		return "DOUBLESTAR_ASSIGN"
	case TOKEN_AMPERSAND_ASSIGN:
		return "AMPERSAND_ASSIGN"
	case TOKEN_PIPE_ASSIGN:
		return "PIPE_ASSIGN"
	case TOKEN_CARET_ASSIGN:
		return "CARET_ASSIGN"
	case TOKEN_LSHIFT:
		return "LSHIFT"
	case TOKEN_LSHIFT_ASSIGN:
		return "LSHIFT_ASSIGN"
	case TOKEN_RSHIFT:
		return "RSHIFT"
	case TOKEN_RSHIFT_ASSIGN:
		return "RSHIFT_ASSIGN"
	case TOKEN_EQUALEQUAL:
		return "EQUALEQUAL"
	case TOKEN_NOTEQUAL:
		return "NOTEQUAL"
	case TOKEN_LESS:
		return "LESS"
	case TOKEN_GREATER:
		return "GREATER"
	case TOKEN_LESSEQUAL:
		return "LESSEQUAL"
	case TOKEN_GREATEREQUAL:
		return "GREATEREQUAL"
	case TOKEN_NEWLINE:
		return "NEWLINE"
	case TOKEN_COMMENT:
		return "COMMENT"
	case TOKEN_EOF:
		return "EOF"
	case TOKEN_ERROR:
		return "ERROR"
	default:
		return fmt.Sprintf("UNKNOWN(%d)", tt)
	}
}

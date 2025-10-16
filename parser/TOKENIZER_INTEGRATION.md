# Tokenizer Integration Guide

## Status

✅ **Tokenizer Complete**: Full lexical analysis working correctly
- Handles all M28 tokens: numbers, strings, operators, delimiters, keywords
- Correctly tokenizes `1+2` as `[NUMBER(1), PLUS, NUMBER(2)]`
- Correctly tokenizes `(-3)` as `[LPAREN, MINUS, NUMBER(3), RPAREN]`
- Tested and verified

⏳ **Parser Integration**: Needs implementation

## What's Been Built

### Files Created
1. **parser/token.go** (280 lines)
   - `TokenType` enum with 60+ token types
   - `Token` struct with lexeme, value, position info
   - Helper methods: `IsOperator()`, `IsLiteral()`, `IsKeyword()`

2. **parser/tokenizer.go** (650 lines)
   - `Tokenizer` struct with full lexical analysis
   - `Tokenize()` method returns token stream
   - Handles all M28 syntax:
     - Numbers (integers, floats, scientific notation)
     - Strings (with escape sequences)
     - F-strings and S-strings (as opaque tokens)
     - All operators: arithmetic, comparison, bitwise, logical
     - Multi-char operators: `**`, `//`, `==`, `!=`, `<=`, `>=`, `<<`, `>>`
     - Compound assignments: `+=`, `-=`, `*=`, etc.
     - Keywords: `and`, `or`, `not`, `in`, `is`, `not in`, `is not`
     - Multi-word keywords detected correctly
     - Comments handled
     - Line/column tracking for error messages

### Test Results
```bash
$ go run /tmp/tokenizer_test_main.go
Tokens for '1+2':
  NUMBER("1"=1) at 1:1
  PLUS("+") at 1:2
  NUMBER("2"=2) at 1:3

Tokens for '(-3)':
  LPAREN("(") at 1:1
  MINUS("-") at 1:2
  NUMBER("3"=3) at 1:3
  RPAREN(")") at 1:4

Tokens for '5 - (-3)':
  NUMBER("5"=5) at 1:1
  MINUS("-") at 1:3
  LPAREN("(") at 1:5
  MINUS("-") at 1:6
  NUMBER("3"=3) at 1:7
  RPAREN(")") at 1:8
```

## Parser Integration Steps

### Step 1: Add Token Fields to Parser (15 min)

```go
// In parser/parser.go, modify Parser struct:
type Parser struct {
    // Existing fields
    input    string
    pos      int
    line     int
    col      int
    filename string

    // New token-based fields
    tokens   []Token
    tokenPos int
}
```

### Step 2: Add Token Navigation Methods (20 min)

```go
// Add these methods to parser.go:

func (p *Parser) currentToken() Token {
    if p.tokenPos >= len(p.tokens) {
        return p.tokens[len(p.tokens)-1] // Return EOF
    }
    return p.tokens[p.tokenPos]
}

func (p *Parser) peekToken() Token {
    if p.tokenPos+1 >= len(p.tokens) {
        return p.tokens[len(p.tokens)-1]
    }
    return p.tokens[p.tokenPos+1]
}

func (p *Parser) advanceToken() Token {
    tok := p.currentToken()
    if p.tokenPos < len(p.tokens)-1 {
        p.tokenPos++
    }
    return tok
}

func (p *Parser) expectToken(typ TokenType) (Token, error) {
    tok := p.currentToken()
    if tok.Type != typ {
        return tok, p.tokenError(fmt.Sprintf("expected %v, got %v", typ, tok.Type), tok)
    }
    return p.advanceToken(), nil
}

func (p *Parser) matchToken(types ...TokenType) bool {
    for _, typ := range types {
        if p.currentToken().Type == typ {
            return true
        }
    }
    return false
}

func (p *Parser) tokenError(msg string, tok Token) error {
    return fmt.Errorf("%s at line %d, column %d", msg, tok.Line, tok.Col)
}
```

### Step 3: Modify Parse() to Tokenize First (10 min)

```go
func (p *Parser) Parse(input string) (core.Value, error) {
    // Step 1: Tokenize
    tokenizer := NewTokenizer(input)
    tokens, err := tokenizer.Tokenize()
    if err != nil {
        return nil, fmt.Errorf("tokenization error: %w", err)
    }

    // Step 2: Set up parser with tokens
    p.tokens = tokens
    p.tokenPos = 0
    p.input = input // Keep for error messages

    // Step 3: Parse program
    return p.parseProgram()
}

func (p *Parser) parseProgram() (core.Value, error) {
    expressions := make([]core.Value, 0)

    for !p.matchToken(TOKEN_EOF) {
        expr, err := p.parseExpr()
        if err != nil {
            return nil, err
        }
        expressions = append(expressions, expr)
    }

    if len(expressions) == 1 {
        return expressions[0], nil
    }

    // Check for infix pattern
    if detectInfixPattern(expressions) {
        result, err := parseInfixExpressionSimple(expressions)
        if err == nil {
            return result, nil
        }
    }

    // Wrap in (do ...)
    return core.ListValue(append(
        []core.Value{core.SymbolValue("do")},
        expressions...,
    )), nil
}
```

### Step 4: Create parseAtomFromToken() (30 min)

```go
func (p *Parser) parseExpr() (core.Value, error) {
    base, err := p.parseAtomFromToken()
    if err != nil {
        return nil, err
    }
    return p.parsePostfixFromToken(base)
}

func (p *Parser) parseAtomFromToken() (core.Value, error) {
    tok := p.currentToken()

    switch tok.Type {
    // Literals - already parsed!
    case TOKEN_NUMBER, TOKEN_STRING, TOKEN_TRUE, TOKEN_FALSE, TOKEN_NIL:
        p.advanceToken()
        return tok.Value, nil

    case TOKEN_IDENTIFIER:
        p.advanceToken()
        return tok.Value, nil

    // Delimiters
    case TOKEN_LPAREN:
        return p.parseListFromToken()

    case TOKEN_LBRACKET:
        return p.parseVectorLiteralFromToken()

    case TOKEN_LBRACE:
        return p.parseDictLiteralFromToken()

    // Special strings
    case TOKEN_FSTRING:
        p.advanceToken()
        // Use existing f-string interpolation logic
        return p.parseFStringFromLexeme(tok.Lexeme)

    case TOKEN_SSTRING:
        p.advanceToken()
        // Use existing s-string interpolation logic
        return p.parseSStringFromLexeme(tok.Lexeme)

    // Reader macros
    case TOKEN_BACKTICK:
        p.advanceToken()
        expr, err := p.parseExpr()
        if err != nil {
            return nil, err
        }
        return core.ListValue{core.SymbolValue("quasiquote"), expr}, nil

    case TOKEN_COMMA:
        return p.parseUnquoteFromToken()

    case TOKEN_COMMA_AT:
        p.advanceToken()
        expr, err := p.parseExpr()
        if err != nil {
            return nil, err
        }
        return core.ListValue{core.SymbolValue("unquote-splicing"), expr}, nil

    // Unary operators
    case TOKEN_MINUS, TOKEN_PLUS, TOKEN_NOT, TOKEN_TILDE:
        return p.parseUnaryOpFromToken()

    default:
        return nil, p.tokenError(fmt.Sprintf("unexpected token %v", tok.Type), tok)
    }
}

func (p *Parser) parseUnaryOpFromToken() (core.Value, error) {
    op := p.advanceToken()
    operand, err := p.parseAtomFromToken()
    if err != nil {
        return nil, err
    }
    // Build (op operand)
    return core.ListValue{core.SymbolValue(op.Lexeme), operand}, nil
}
```

### Step 5: Update parseList() for Tokens (20 min)

```go
func (p *Parser) parseListFromToken() (core.Value, error) {
    p.advanceToken() // consume LPAREN

    elements := make([]core.Value, 0)

    for !p.matchToken(TOKEN_RPAREN) {
        if p.matchToken(TOKEN_EOF) {
            return nil, fmt.Errorf("unclosed list")
        }

        expr, err := p.parseExpr()
        if err != nil {
            return nil, err
        }
        elements = append(elements, expr)

        // Skip optional commas
        if p.matchToken(TOKEN_COMMA) {
            p.advanceToken()
        }
    }

    p.expectToken(TOKEN_RPAREN) // consume RPAREN

    // Check for infix pattern
    if detectInfixPattern(elements) {
        return parseInfixExpressionSimple(elements)
    }

    return core.ListValue(elements), nil
}
```

### Step 6: Update Other Parse Methods (30 min)

Similar updates needed for:
- `parseVectorLiteralFromToken()`
- `parseDictLiteralFromToken()`
- `parsePostfixFromToken()`
- Update dot notation, indexing to use tokens

### Step 7: Testing (30 min)

After each step above, run:
```bash
make test
./test.sh
```

Fix any issues before proceeding to next step.

### Step 8: Verify Edge Cases (15 min)

Test these specific cases:
```bash
echo "1+2" | ./bin/m28        # Should return 3
echo "1-2" | ./bin/m28        # Should return -1
echo "(-3)" | ./bin/m28       # Should return -3
echo "5 - (-3)" | ./bin/m28   # Should return 8
echo "(+ 1 2)" | ./bin/m28    # Should return 3 (prefix still works)
```

## Estimated Time: 3-4 hours

The tokenizer is complete and working. Parser integration is systematic but requires careful attention to maintain backward compatibility.

## Benefits After Integration

1. ✅ `1+2` works (no spaces needed)
2. ✅ `(-3)` works correctly
3. ✅ Better error messages with line/column info
4. ✅ Cleaner parser code (token-based is simpler than character-based)
5. ✅ Foundation for future improvements (syntax highlighting, LSP, etc.)

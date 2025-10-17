package parser

import (
	"testing"

	"github.com/mmichie/m28/core"
)

func TestPythonTokenizer_BasicIndentation(t *testing.T) {
	input := `def foo():
    x = 1
    y = 2`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	expected := []TokenType{
		TOKEN_DEF,
		TOKEN_IDENTIFIER, // foo
		TOKEN_LPAREN,
		TOKEN_RPAREN,
		TOKEN_COLON,
		TOKEN_NEWLINE, // After colon
		TOKEN_INDENT,
		TOKEN_IDENTIFIER, // x
		TOKEN_ASSIGN,
		TOKEN_NUMBER, // 1
		TOKEN_NEWLINE,
		TOKEN_IDENTIFIER, // y
		TOKEN_ASSIGN,
		TOKEN_NUMBER, // 2
		TOKEN_DEDENT,
		TOKEN_EOF,
	}

	if len(tokens) != len(expected) {
		t.Fatalf("Expected %d tokens, got %d", len(expected), len(tokens))
	}

	for i, tok := range tokens {
		if tok.Type != expected[i] {
			t.Errorf("Token %d: expected %v, got %v (%q)", i, expected[i], tok.Type, tok.Lexeme)
		}
	}
}

func TestPythonTokenizer_NestedIndentation(t *testing.T) {
	input := `def outer():
    def inner():
        pass
    x = 1`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	expected := []TokenType{
		TOKEN_DEF,
		TOKEN_IDENTIFIER, // outer
		TOKEN_LPAREN,
		TOKEN_RPAREN,
		TOKEN_COLON,
		TOKEN_NEWLINE, // After outer's colon
		TOKEN_INDENT,
		TOKEN_DEF,
		TOKEN_IDENTIFIER, // inner
		TOKEN_LPAREN,
		TOKEN_RPAREN,
		TOKEN_COLON,
		TOKEN_NEWLINE, // After inner's colon
		TOKEN_INDENT,
		TOKEN_PASS,
		TOKEN_NEWLINE, // After pass
		TOKEN_DEDENT,
		TOKEN_IDENTIFIER, // x
		TOKEN_ASSIGN,
		TOKEN_NUMBER, // 1
		TOKEN_DEDENT,
		TOKEN_EOF,
	}

	if len(tokens) != len(expected) {
		t.Fatalf("Expected %d tokens, got %d", len(expected), len(tokens))
	}

	for i, tok := range tokens {
		if tok.Type != expected[i] {
			t.Errorf("Token %d: expected %v, got %v (%q)", i, expected[i], tok.Type, tok.Lexeme)
		}
	}
}

func TestPythonTokenizer_ParenthesisContinuation(t *testing.T) {
	input := `result = (1 +
          2 +
          3)`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	// Should NOT have INDENT/DEDENT inside parentheses
	for i, tok := range tokens {
		if tok.Type == TOKEN_INDENT || tok.Type == TOKEN_DEDENT {
			t.Errorf("Token %d: unexpected %v inside parentheses", i, tok.Type)
		}
	}

	// Check we have the expected tokens
	expected := []TokenType{
		TOKEN_IDENTIFIER, // result
		TOKEN_ASSIGN,
		TOKEN_LPAREN,
		TOKEN_NUMBER, // 1
		TOKEN_PLUS,
		TOKEN_NUMBER, // 2
		TOKEN_PLUS,
		TOKEN_NUMBER, // 3
		TOKEN_RPAREN,
		TOKEN_EOF,
	}

	if len(tokens) != len(expected) {
		t.Fatalf("Expected %d tokens, got %d", len(expected), len(tokens))
	}

	for i, tok := range tokens {
		if tok.Type != expected[i] {
			t.Errorf("Token %d: expected %v, got %v", i, expected[i], tok.Type)
		}
	}
}

func TestPythonTokenizer_BlankLinesAndComments(t *testing.T) {
	input := `def foo():
    # Comment

    x = 1  # inline comment`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	// Comments should be skipped, blank lines should not affect indentation
	expected := []TokenType{
		TOKEN_DEF,
		TOKEN_IDENTIFIER, // foo
		TOKEN_LPAREN,
		TOKEN_RPAREN,
		TOKEN_COLON,
		TOKEN_NEWLINE, // After colon
		TOKEN_INDENT,
		TOKEN_IDENTIFIER, // x
		TOKEN_ASSIGN,
		TOKEN_NUMBER, // 1
		TOKEN_DEDENT,
		TOKEN_EOF,
	}

	if len(tokens) != len(expected) {
		t.Fatalf("Expected %d tokens, got %d", len(expected), len(tokens))
	}

	for i, tok := range tokens {
		if tok.Type != expected[i] {
			t.Errorf("Token %d: expected %v, got %v", i, expected[i], tok.Type)
		}
	}
}

func TestPythonTokenizer_MultipleDeindents(t *testing.T) {
	input := `if True:
    if True:
        if True:
            pass
x = 1`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	// Count INDENT and DEDENT tokens
	indentCount := 0
	dedentCount := 0
	for _, tok := range tokens {
		if tok.Type == TOKEN_INDENT {
			indentCount++
		} else if tok.Type == TOKEN_DEDENT {
			dedentCount++
		}
	}

	// Should have 3 INDENTs and 3 DEDENTs
	if indentCount != 3 {
		t.Errorf("Expected 3 INDENTs, got %d", indentCount)
	}
	if dedentCount != 3 {
		t.Errorf("Expected 3 DEDENTs, got %d", dedentCount)
	}
}

func TestPythonTokenizer_PythonKeywords(t *testing.T) {
	input := `class MyClass:
    def __init__(self):
        pass

    async def method(self):
        await something()
        return None`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	// Check for specific keywords
	foundClass := false
	foundDef := false
	foundPass := false
	foundAsync := false
	foundAwait := false
	foundReturn := false
	foundNone := false

	for _, tok := range tokens {
		switch tok.Type {
		case TOKEN_CLASS:
			foundClass = true
		case TOKEN_DEF:
			foundDef = true
		case TOKEN_PASS:
			foundPass = true
		case TOKEN_ASYNC:
			foundAsync = true
		case TOKEN_AWAIT:
			foundAwait = true
		case TOKEN_RETURN:
			foundReturn = true
		case TOKEN_NIL: // None maps to TOKEN_NIL
			foundNone = true
		}
	}

	if !foundClass {
		t.Error("Expected to find 'class' keyword")
	}
	if !foundDef {
		t.Error("Expected to find 'def' keyword")
	}
	if !foundPass {
		t.Error("Expected to find 'pass' keyword")
	}
	if !foundAsync {
		t.Error("Expected to find 'async' keyword")
	}
	if !foundAwait {
		t.Error("Expected to find 'await' keyword")
	}
	if !foundReturn {
		t.Error("Expected to find 'return' keyword")
	}
	if !foundNone {
		t.Error("Expected to find 'None' keyword")
	}
}

func TestPythonTokenizer_OperatorsAndLiterals(t *testing.T) {
	input := `x += 5
y = [1, 2, 3]
z = {"key": "value"}
w = True and False or not None`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	// Check for specific operators
	foundPlusAssign := false
	foundLBracket := false
	foundLBrace := false
	foundAnd := false
	foundOr := false
	foundNot := false
	foundTrue := false
	foundFalse := false

	for _, tok := range tokens {
		switch tok.Type {
		case TOKEN_PLUS_ASSIGN:
			foundPlusAssign = true
		case TOKEN_LBRACKET:
			foundLBracket = true
		case TOKEN_LBRACE:
			foundLBrace = true
		case TOKEN_AND:
			foundAnd = true
		case TOKEN_OR:
			foundOr = true
		case TOKEN_NOT:
			foundNot = true
		case TOKEN_TRUE:
			foundTrue = true
		case TOKEN_FALSE:
			foundFalse = true
		}
	}

	if !foundPlusAssign {
		t.Error("Expected to find '+=' operator")
	}
	if !foundLBracket {
		t.Error("Expected to find '[' for list literal")
	}
	if !foundLBrace {
		t.Error("Expected to find '{' for dict literal")
	}
	if !foundAnd {
		t.Error("Expected to find 'and' operator")
	}
	if !foundOr {
		t.Error("Expected to find 'or' operator")
	}
	if !foundNot {
		t.Error("Expected to find 'not' operator")
	}
	if !foundTrue {
		t.Error("Expected to find 'True'")
	}
	if !foundFalse {
		t.Error("Expected to find 'False'")
	}
}

func TestPythonTokenizer_NumberParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected core.Value
	}{
		{"42", core.NumberValue(42)},
		{"3.14", core.NumberValue(3.14)},
		{"1e10", core.NumberValue(1e10)},
		{"2.5e-3", core.NumberValue(2.5e-3)},
	}

	for _, tt := range tests {
		tokenizer := NewPythonTokenizer(tt.input)
		tokens, err := tokenizer.Tokenize()
		if err != nil {
			t.Fatalf("Tokenize error for %q: %v", tt.input, err)
		}

		if len(tokens) < 2 {
			t.Fatalf("Expected at least 2 tokens for %q", tt.input)
		}

		tok := tokens[0]
		if tok.Type != TOKEN_NUMBER {
			t.Errorf("Expected NUMBER token for %q, got %v", tt.input, tok.Type)
		}

		if tok.Value != tt.expected {
			t.Errorf("Expected value %v for %q, got %v", tt.expected, tt.input, tok.Value)
		}
	}
}

func TestPythonTokenizer_StringLiterals(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{`"hello"`, "hello"},
		{`'world'`, "world"},
		{`"hello\nworld"`, "hello\nworld"},
		{`'it\'s'`, "it's"},
		{`"""triple
quoted"""`, "triple\nquoted"},
	}

	for _, tt := range tests {
		tokenizer := NewPythonTokenizer(tt.input)
		tokens, err := tokenizer.Tokenize()
		if err != nil {
			t.Fatalf("Tokenize error for %q: %v", tt.input, err)
		}

		if len(tokens) < 2 {
			t.Fatalf("Expected at least 2 tokens for %q", tt.input)
		}

		tok := tokens[0]
		if tok.Type != TOKEN_STRING {
			t.Errorf("Expected STRING token for %q, got %v", tt.input, tok.Type)
		}

		strVal, ok := tok.Value.(core.StringValue)
		if !ok {
			t.Errorf("Expected StringValue for %q, got %T", tt.input, tok.Value)
			continue
		}

		if string(strVal) != tt.expected {
			t.Errorf("Expected value %q for %q, got %q", tt.expected, tt.input, string(strVal))
		}
	}
}

func TestPythonTokenizer_ComparisonOperators(t *testing.T) {
	input := `x == y
a != b
c < d
e > f
g <= h
i >= j`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	operators := []TokenType{
		TOKEN_EQUALEQUAL,
		TOKEN_NOTEQUAL,
		TOKEN_LESS,
		TOKEN_GREATER,
		TOKEN_LESSEQUAL,
		TOKEN_GREATEREQUAL,
	}

	foundOps := make(map[TokenType]bool)
	for _, tok := range tokens {
		for _, op := range operators {
			if tok.Type == op {
				foundOps[op] = true
			}
		}
	}

	for _, op := range operators {
		if !foundOps[op] {
			t.Errorf("Expected to find operator %v", op)
		}
	}
}

func TestPythonTokenizer_ControlFlow(t *testing.T) {
	input := `if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")

for i in range(10):
    if i % 2 == 0:
        continue
    print(i)

while True:
    break`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	keywords := []TokenType{
		TOKEN_IF,
		TOKEN_ELIF,
		TOKEN_ELSE,
		TOKEN_FOR,
		TOKEN_IN,
		TOKEN_WHILE,
		TOKEN_BREAK,
		TOKEN_CONTINUE,
	}

	foundKeywords := make(map[TokenType]bool)
	for _, tok := range tokens {
		for _, kw := range keywords {
			if tok.Type == kw {
				foundKeywords[kw] = true
			}
		}
	}

	for _, kw := range keywords {
		if !foundKeywords[kw] {
			t.Errorf("Expected to find keyword %v", kw)
		}
	}
}

func TestPythonTokenizer_ExceptionHandling(t *testing.T) {
	input := `try:
    risky_operation()
except ValueError as e:
    print(e)
except:
    pass
finally:
    cleanup()`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	keywords := []TokenType{
		TOKEN_TRY,
		TOKEN_EXCEPT,
		TOKEN_AS,
		TOKEN_FINALLY,
	}

	foundKeywords := make(map[TokenType]bool)
	for _, tok := range tokens {
		for _, kw := range keywords {
			if tok.Type == kw {
				foundKeywords[kw] = true
			}
		}
	}

	for _, kw := range keywords {
		if !foundKeywords[kw] {
			t.Errorf("Expected to find keyword %v", kw)
		}
	}
}

func TestPythonTokenizer_LocationTracking(t *testing.T) {
	input := `x = 1
y = 2`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenize error: %v", err)
	}

	// Check that line numbers are tracked correctly
	line1Count := 0
	line2Count := 0
	for _, tok := range tokens {
		if tok.Line == 1 {
			line1Count++
		} else if tok.Line == 2 {
			line2Count++
		}
	}

	if line1Count == 0 {
		t.Error("Expected tokens on line 1")
	}
	if line2Count == 0 {
		t.Error("Expected tokens on line 2")
	}
}

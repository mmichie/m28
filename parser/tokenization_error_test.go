package parser

import (
	"strings"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestTokenizationError_Basic(t *testing.T) {
	err := &TokenizationError{
		Message: "unterminated string",
		Location: &core.SourceLocation{
			File:   "test.py",
			Line:   1,
			Column: 5,
		},
		Suggestion: "Add closing quote",
	}

	errMsg := err.Error()
	if !strings.Contains(errMsg, "test.py") {
		t.Errorf("Expected filename in error, got: %s", errMsg)
	}
	if !strings.Contains(errMsg, "1:5") {
		t.Errorf("Expected line:col in error, got: %s", errMsg)
	}
	if !strings.Contains(errMsg, "unterminated string") {
		t.Errorf("Expected message in error, got: %s", errMsg)
	}
}

func TestTokenizationError_WithFormatter(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := `x = "hello`

	ef.AddSource("test.py", source)

	err := &TokenizationError{
		Message: "unterminated string",
		Location: &core.SourceLocation{
			File:   "test.py",
			Line:   1,
			Column: 5,
		},
		Suggestion: "Add closing quote at end of string",
		Source:     source,
	}

	formatted := ef.FormatError(err)

	// Should show file and line
	if !strings.Contains(formatted, "test.py") {
		t.Errorf("Expected filename in formatted output, got: %s", formatted)
	}

	// Should show source line
	if !strings.Contains(formatted, `x = "hello`) {
		t.Errorf("Expected source line in formatted output, got: %s", formatted)
	}

	// Should show caret
	if !strings.Contains(formatted, "^") {
		t.Errorf("Expected caret in formatted output, got: %s", formatted)
	}

	// Should show suggestion
	if !strings.Contains(formatted, "Add closing quote") {
		t.Errorf("Expected suggestion in formatted output, got: %s", formatted)
	}
}

func TestPythonTokenizer_UnterminatedString(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		wantErrMsg string
		wantSugg   string
	}{
		{
			name:       "single quote string",
			input:      `x = 'unterminated`,
			wantErrMsg: "unterminated string",
			wantSugg:   "Add closing ' at end of string",
		},
		{
			name:       "double quote string",
			input:      `x = "unterminated`,
			wantErrMsg: "unterminated string",
			wantSugg:   "Add closing \" at end of string",
		},
		{
			name:       "triple quote string",
			input:      `x = """unterminated`,
			wantErrMsg: "unterminated triple-quoted string",
			wantSugg:   `Add closing """ at end of string`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.input)
			tokenizer.filename = "test.py"
			_, err := tokenizer.Tokenize()

			if err == nil {
				t.Fatal("Expected error for unterminated string")
			}

			errStr := err.Error()
			if !strings.Contains(errStr, tt.wantErrMsg) {
				t.Errorf("Expected error message %q, got: %s", tt.wantErrMsg, errStr)
			}

			// Check that we got a TokenizationError with suggestion
			if len(tokenizer.errors) == 0 {
				t.Fatal("Expected tokenization error to be recorded")
			}

			tokErr, ok := tokenizer.errors[0].(*TokenizationError)
			if !ok {
				t.Fatalf("Expected TokenizationError, got: %T", tokenizer.errors[0])
			}

			if !strings.Contains(tokErr.Suggestion, tt.wantSugg) {
				t.Errorf("Expected suggestion %q, got: %s", tt.wantSugg, tokErr.Suggestion)
			}
		})
	}
}

func TestPythonTokenizer_UnexpectedCharacter(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		wantErrMsg string
	}{
		{
			name:       "invalid character @",
			input:      "x @ y",
			wantErrMsg: "", // @ is actually valid in Python (matrix multiply)
		},
		{
			name:       "backslash not at EOL",
			input:      `x = \ 5`,
			wantErrMsg: "unexpected character '\\'",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.wantErrMsg == "" {
				return // Skip tests where we don't expect errors
			}

			tokenizer := NewPythonTokenizer(tt.input)
			tokenizer.filename = "test.py"
			_, err := tokenizer.Tokenize()

			if err == nil {
				t.Fatal("Expected error for invalid character")
			}

			errStr := err.Error()
			if !strings.Contains(errStr, tt.wantErrMsg) {
				t.Errorf("Expected error message %q, got: %s", tt.wantErrMsg, errStr)
			}

			// Check that we got a TokenizationError
			if len(tokenizer.errors) == 0 {
				t.Fatal("Expected tokenization error to be recorded")
			}

			_, ok := tokenizer.errors[0].(*TokenizationError)
			if !ok {
				t.Fatalf("Expected TokenizationError, got: %T", tokenizer.errors[0])
			}
		})
	}
}

func TestPythonTokenizer_IndentationError(t *testing.T) {
	input := `def foo():
    x = 1
   y = 2` // Bad dedent - doesn't match any previous level

	tokenizer := NewPythonTokenizer(input)
	tokenizer.filename = "test.py"
	_, err := tokenizer.Tokenize()

	if err == nil {
		t.Fatal("Expected indentation error")
	}

	errStr := err.Error()
	if !strings.Contains(errStr, "indentation") {
		t.Errorf("Expected indentation error message, got: %s", errStr)
	}

	// Check that we got a TokenizationError with suggestion
	if len(tokenizer.errors) == 0 {
		t.Fatal("Expected tokenization error to be recorded")
	}

	tokErr, ok := tokenizer.errors[0].(*TokenizationError)
	if !ok {
		t.Fatalf("Expected TokenizationError, got: %T", tokenizer.errors[0])
	}

	if !strings.Contains(tokErr.Suggestion, "consistent") {
		t.Errorf("Expected suggestion about consistent indentation, got: %s", tokErr.Suggestion)
	}
}

func TestM28Tokenizer_TokenizationError(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		wantErrMsg string
		wantSugg   string
	}{
		{
			name:       "unterminated string",
			input:      `(print "unterminated`,
			wantErrMsg: "unterminated string",
			wantSugg:   "Add closing quote",
		},
		{
			name:       "unexpected !",
			input:      `(if !x 1 0)`,
			wantErrMsg: "unexpected character '!'",
			wantSugg:   "Use 'not' for logical negation",
		},
		{
			name:       "invalid number with bad exponent",
			input:      `(= x 1.5e)`,
			wantErrMsg: "invalid number",
			wantSugg:   "Check number format",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewTokenizer(tt.input)
			tokenizer.filename = "test.m28"
			_, err := tokenizer.Tokenize()

			if err == nil {
				t.Fatal("Expected error")
			}

			errStr := err.Error()
			if !strings.Contains(errStr, tt.wantErrMsg) {
				t.Errorf("Expected error message %q, got: %s", tt.wantErrMsg, errStr)
			}

			// Check that we got a TokenizationError with suggestion
			if len(tokenizer.errors) == 0 {
				t.Fatal("Expected tokenization error to be recorded")
			}

			tokErr, ok := tokenizer.errors[0].(*TokenizationError)
			if !ok {
				t.Fatalf("Expected TokenizationError, got: %T", tokenizer.errors[0])
			}

			if !strings.Contains(tokErr.Suggestion, tt.wantSugg) {
				t.Errorf("Expected suggestion containing %q, got: %s", tt.wantSugg, tokErr.Suggestion)
			}
		})
	}
}

func TestTokenizationError_FormattedOutput(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := `def foo():
    x = "unterminated
    y = 2`

	ef.AddSource("test.py", source)

	err := &TokenizationError{
		Message: "unterminated string",
		Location: &core.SourceLocation{
			File:   "test.py",
			Line:   2,
			Column: 9,
		},
		Suggestion: "Add closing \" at end of string",
		Source:     source,
	}

	formatted := ef.FormatError(err)

	// Should show Python-style header
	if !strings.Contains(formatted, `File "test.py", line 2`) {
		t.Errorf("Expected Python-style header, got: %s", formatted)
	}

	// Should show source context
	lines := strings.Split(formatted, "\n")
	foundSourceLine := false
	foundCaret := false
	foundSuggestion := false

	for _, line := range lines {
		if strings.Contains(line, `x = "unterminated`) {
			foundSourceLine = true
		}
		if strings.Contains(line, "^") {
			foundCaret = true
		}
		if strings.Contains(line, "Add closing") {
			foundSuggestion = true
		}
	}

	if !foundSourceLine {
		t.Errorf("Expected source line in output, got: %s", formatted)
	}
	if !foundCaret {
		t.Errorf("Expected caret indicator in output, got: %s", formatted)
	}
	if !foundSuggestion {
		t.Errorf("Expected suggestion in output, got: %s", formatted)
	}
}

func TestTokenizationError_NoLocation(t *testing.T) {
	err := &TokenizationError{
		Message:    "generic error",
		Location:   nil,
		Suggestion: "fix it",
	}

	errMsg := err.Error()
	if errMsg != "generic error" {
		t.Errorf("Expected just the message, got: %s", errMsg)
	}
}

func TestPythonTokenizer_MixedQuoteTypes(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		shouldFail bool
	}{
		{
			name:       "single quotes inside double quotes",
			input:      `x = "it's working"`,
			shouldFail: false,
		},
		{
			name:       "double quotes inside single quotes",
			input:      `x = 'He said "hello"'`,
			shouldFail: false,
		},
		{
			name:       "mismatched quotes",
			input:      `x = "hello'`,
			shouldFail: true,
		},
		{
			name:       "triple single quotes",
			input:      `x = '''multiline'''`,
			shouldFail: false,
		},
		{
			name:       "triple double quotes",
			input:      `x = """multiline"""`,
			shouldFail: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.input)
			_, err := tokenizer.Tokenize()

			if tt.shouldFail && err == nil {
				t.Error("Expected tokenization to fail but it succeeded")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected tokenization to succeed but got error: %v", err)
			}
		})
	}
}

func TestPythonTokenizer_EscapeSequencesInUnterminatedString(t *testing.T) {
	input := `x = "hello\nworld`

	tokenizer := NewPythonTokenizer(input)
	tokenizer.filename = "test.py"
	_, err := tokenizer.Tokenize()

	if err == nil {
		t.Fatal("Expected error for unterminated string with escape sequence")
	}

	if !strings.Contains(err.Error(), "unterminated") {
		t.Errorf("Expected unterminated string error, got: %s", err.Error())
	}
}

func TestPythonTokenizer_StringAtEOF(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		shouldFail bool
	}{
		{
			name:       "complete string at EOF",
			input:      `x = "hello"`,
			shouldFail: false,
		},
		{
			name:       "unterminated string at EOF",
			input:      `x = "hello`,
			shouldFail: true,
		},
		{
			name:       "empty string at EOF",
			input:      `x = ""`,
			shouldFail: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.input)
			_, err := tokenizer.Tokenize()

			if tt.shouldFail && err == nil {
				t.Error("Expected error but got none")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected success but got error: %v", err)
			}
		})
	}
}

func TestPythonTokenizer_IndentationEdgeCases(t *testing.T) {
	tests := []struct {
		name       string
		input      string
		shouldFail bool
		errMsg     string
	}{
		{
			name: "all spaces - consistent",
			input: `def foo():
    x = 1
    y = 2`,
			shouldFail: false,
		},
		{
			name: "increasing indentation",
			input: `if True:
    if True:
        x = 1`,
			shouldFail: false,
		},
		{
			name: "dedent to previous level",
			input: `if True:
    x = 1
y = 2`,
			shouldFail: false,
		},
		{
			name: "dedent to middle of nowhere",
			input: `if True:
    if True:
        x = 1
   y = 2`,
			shouldFail: true,
			errMsg:     "indentation",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.input)
			_, err := tokenizer.Tokenize()

			if tt.shouldFail && err == nil {
				t.Errorf("Expected error containing %q but got none", tt.errMsg)
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected success but got error: %v", err)
			}
			if tt.shouldFail && err != nil && !strings.Contains(err.Error(), tt.errMsg) {
				t.Errorf("Expected error containing %q, got: %s", tt.errMsg, err.Error())
			}
		})
	}
}

func TestPythonTokenizer_EmptyInput(t *testing.T) {
	tokenizer := NewPythonTokenizer("")
	tokens, err := tokenizer.Tokenize()

	if err != nil {
		t.Errorf("Expected no error for empty input, got: %v", err)
	}

	// Should have at least EOF token
	if len(tokens) == 0 {
		t.Error("Expected at least EOF token for empty input")
	}
}

func TestPythonTokenizer_OnlyWhitespace(t *testing.T) {
	tests := []string{
		"   ",
		"\n\n\n",
		"\t\t\t",
		"  \n  \n  ",
	}

	for _, input := range tests {
		t.Run("whitespace", func(t *testing.T) {
			tokenizer := NewPythonTokenizer(input)
			tokens, err := tokenizer.Tokenize()

			if err != nil {
				t.Errorf("Expected no error for whitespace-only input, got: %v", err)
			}

			if len(tokens) == 0 {
				t.Error("Expected at least EOF token")
			}
		})
	}
}

func TestPythonTokenizer_UnicodeInStrings(t *testing.T) {
	input := `message = "Hello 世界 🌍"`

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()

	if err != nil {
		t.Errorf("Expected no error for unicode string, got: %v", err)
	}

	// Should successfully tokenize unicode
	if len(tokens) == 0 {
		t.Error("Expected tokens for unicode string")
	}
}

func TestPythonTokenizer_LongStringLiteral(t *testing.T) {
	// Create a very long string (1000+ characters)
	longString := `"` + strings.Repeat("a", 1000) + `"`
	input := "x = " + longString

	tokenizer := NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()

	if err != nil {
		t.Errorf("Expected no error for long string, got: %v", err)
	}

	if len(tokens) == 0 {
		t.Error("Expected tokens for long string")
	}
}

func TestTokenizationError_MultipleErrorsInSequence(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := `x = "unterminated1
y = "unterminated2
z = "unterminated3`

	ef.AddSource("test.py", source)

	// First error
	err1 := &TokenizationError{
		Message: "unterminated string",
		Location: &core.SourceLocation{
			File:   "test.py",
			Line:   1,
			Column: 5,
		},
		Source: source,
	}

	formatted1 := ef.FormatError(err1)

	if !strings.Contains(formatted1, "line 1") {
		t.Errorf("Expected line 1 in first error, got: %s", formatted1)
	}

	// Second error
	err2 := &TokenizationError{
		Message: "unterminated string",
		Location: &core.SourceLocation{
			File:   "test.py",
			Line:   2,
			Column: 5,
		},
		Source: source,
	}

	formatted2 := ef.FormatError(err2)

	if !strings.Contains(formatted2, "line 2") {
		t.Errorf("Expected line 2 in second error, got: %s", formatted2)
	}
}

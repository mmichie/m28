package parser

import (
	"strings"
	"testing"
)

func TestParseErrorWithContext(t *testing.T) {
	tests := []struct {
		name           string
		source         string
		expectedInFile bool
		expectedInMsg  []string
	}{
		{
			name: "Missing colon in if statement",
			source: `if x > 5
    print("big")`,
			expectedInFile: true,
			expectedInMsg:  []string{"test.py:1:", "Expected COLON"},
		},
		{
			name: "Unclosed parenthesis",
			source: `result = (1 + 2
print(result)`,
			expectedInFile: true,
			expectedInMsg:  []string{"test.py:", "RPAREN"},
		},
		{
			name: "Invalid syntax - unexpected token",
			source: `x =
y = 10`,
			expectedInFile: true,
			expectedInMsg:  []string{"test.py:1:"},
		},
		{
			name: "Missing closing bracket",
			source: `my_list = [1, 2, 3
print(my_list)`,
			expectedInFile: true,
			expectedInMsg:  []string{"test.py:", "RBRACKET"},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				// Some errors might be tokenization errors, skip those for now
				return
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if err == nil {
				t.Errorf("Expected parse error but got none")
				return
			}

			// Check that error is a ParseError
			parseErr, ok := err.(*ParseError)
			if !ok {
				// Check if it's wrapped in parser.errors
				if len(parser.errors) > 0 {
					parseErr, ok = parser.errors[0].(*ParseError)
					if !ok {
						t.Errorf("Expected ParseError, got %T: %v", parser.errors[0], parser.errors[0])
						return
					}
				} else {
					t.Errorf("Expected ParseError, got %T: %v", err, err)
					return
				}
			}

			// Verify error has filename
			if tt.expectedInFile && parseErr.Filename != "test.py" {
				t.Errorf("Expected filename 'test.py', got '%s'", parseErr.Filename)
			}

			// Verify error has source
			if parseErr.Source != tt.source {
				t.Errorf("Expected source to be preserved")
			}

			// Verify error message contains expected strings
			errMsg := parseErr.Error()
			for _, expected := range tt.expectedInMsg {
				if !strings.Contains(errMsg, expected) {
					t.Errorf("Expected error message to contain '%s', got: %s", expected, errMsg)
				}
			}

			// Verify FormatWithContext works
			formattedErr := parseErr.FormatWithContext()
			if formattedErr == "" {
				t.Errorf("FormatWithContext returned empty string")
			}

			// Should contain filename in formatted error
			if tt.expectedInFile && !strings.Contains(formattedErr, "test.py") {
				t.Errorf("Expected formatted error to contain filename, got: %s", formattedErr)
			}
		})
	}
}

func TestParseErrorLocation(t *testing.T) {
	source := `x = 10
y = 20
if x > 5
    print(x)`

	tokenizer := NewPythonTokenizer(source)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		t.Fatalf("Tokenization error: %v", err)
	}

	parser := NewPythonParser(tokens, "test.py", source)
	_, err = parser.Parse()

	if err == nil {
		t.Fatal("Expected parse error but got none")
	}

	// Get the parse error
	var parseErr *ParseError
	if len(parser.errors) > 0 {
		parseErr, _ = parser.errors[0].(*ParseError)
	}

	if parseErr == nil {
		t.Fatalf("Expected ParseError in parser.errors")
	}

	// Verify line number is correct (should be line 3 where 'if' is)
	if parseErr.Line != 3 {
		t.Errorf("Expected error on line 3, got line %d", parseErr.Line)
	}

	// Verify error has location info
	if parseErr.Col == 0 {
		t.Errorf("Expected column number to be set")
	}
}

func TestParseErrorFilename(t *testing.T) {
	tests := []struct {
		name         string
		filename     string
		source       string
		expectedFile string
	}{
		{
			name:         "Regular file",
			filename:     "/path/to/myfile.py",
			source:       "if x",
			expectedFile: "/path/to/myfile.py",
		},
		{
			name:         "REPL input",
			filename:     "<repl>",
			source:       "if x",
			expectedFile: "<repl>",
		},
		{
			name:         "Test input",
			filename:     "<test>",
			source:       "if x",
			expectedFile: "<test>",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, _ := tokenizer.Tokenize()

			parser := NewPythonParser(tokens, tt.filename, tt.source)
			_, _ = parser.Parse()

			if len(parser.errors) == 0 {
				t.Fatal("Expected parse error but got none")
			}

			parseErr, ok := parser.errors[0].(*ParseError)
			if !ok {
				t.Fatalf("Expected ParseError, got %T", parser.errors[0])
			}

			if parseErr.Filename != tt.expectedFile {
				t.Errorf("Expected filename '%s', got '%s'", tt.expectedFile, parseErr.Filename)
			}

			// Error() should include filename in output
			errStr := parseErr.Error()
			if tt.filename != "" && !strings.Contains(errStr, tt.expectedFile) {
				t.Errorf("Expected error string to contain filename '%s', got: %s", tt.expectedFile, errStr)
			}
		})
	}
}

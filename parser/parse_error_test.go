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
			if tt.expectedInFile && parseErr.Filename() != "test.py" {
				t.Errorf("Expected filename 'test.py', got '%s'", parseErr.Filename())
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
	if parseErr.Line() != 3 {
		t.Errorf("Expected error on line 3, got line %d", parseErr.Line())
	}

	// Verify error has location info
	if parseErr.Col() == 0 {
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

			if parseErr.Filename() != tt.expectedFile {
				t.Errorf("Expected filename '%s', got '%s'", tt.expectedFile, parseErr.Filename())
			}

			// Error() should include filename in output
			errStr := parseErr.Error()
			if tt.filename != "" && !strings.Contains(errStr, tt.expectedFile) {
				t.Errorf("Expected error string to contain filename '%s', got: %s", tt.expectedFile, errStr)
			}
		})
	}
}

func TestParseError_ComplexExpressions(t *testing.T) {
	tests := []struct {
		name       string
		source     string
		shouldFail bool
		errMsg     string
	}{
		{
			name:       "nested parentheses OK",
			source:     "result = ((1 + 2) * (3 + 4))",
			shouldFail: false,
		},
		{
			name:       "unclosed nested parenthesis",
			source:     "result = ((1 + 2) * (3 + 4)",
			shouldFail: true,
			errMsg:     "RPAREN",
		},
		{
			name:       "mismatched brackets and parens",
			source:     "result = [(1 + 2)]",
			shouldFail: false,
		},
		{
			name:       "wrong closing delimiter",
			source:     "result = [(1 + 2}]",
			shouldFail: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				if !tt.shouldFail {
					t.Fatalf("Tokenization failed: %v", err)
				}
				return
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if tt.shouldFail && err == nil {
				t.Error("Expected parse error but got none")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
			if tt.shouldFail && err != nil && tt.errMsg != "" {
				if !strings.Contains(err.Error(), tt.errMsg) {
					t.Errorf("Expected error containing %q, got: %s", tt.errMsg, err.Error())
				}
			}
		})
	}
}

func TestParseError_FunctionDefinitions(t *testing.T) {
	tests := []struct {
		name       string
		source     string
		shouldFail bool
		errMsg     string
	}{
		{
			name: "valid function",
			source: `def foo(x, y):
    return x + y`,
			shouldFail: false,
		},
		{
			name:       "missing colon",
			source:     "def foo(x, y)\n    return x + y",
			shouldFail: true,
			errMsg:     "COLON",
		},
		{
			name:       "missing params parentheses",
			source:     "def foo:\n    pass",
			shouldFail: true,
		},
		{
			name: "function with default args",
			source: `def foo(x, y=10):
    return x + y`,
			shouldFail: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenization failed: %v", err)
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if tt.shouldFail && err == nil {
				t.Error("Expected parse error but got none")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
		})
	}
}

func TestParseError_ClassDefinitions(t *testing.T) {
	tests := []struct {
		name       string
		source     string
		shouldFail bool
	}{
		{
			name: "valid class",
			source: `class MyClass:
    pass`,
			shouldFail: false,
		},
		{
			name:       "missing colon",
			source:     "class MyClass\n    pass",
			shouldFail: true,
		},
		{
			name: "class with methods",
			source: `class MyClass:
    def method(self):
        pass`,
			shouldFail: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenization failed: %v", err)
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if tt.shouldFail && err == nil {
				t.Error("Expected parse error but got none")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
		})
	}
}

func TestParseError_ControlFlow(t *testing.T) {
	tests := []struct {
		name       string
		source     string
		shouldFail bool
		errMsg     string
	}{
		{
			name: "if-elif-else OK",
			source: `if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")`,
			shouldFail: false,
		},
		{
			name:       "if without colon",
			source:     "if x > 0\n    print('x')",
			shouldFail: true,
			errMsg:     "COLON",
		},
		{
			name: "while loop OK",
			source: `while x > 0:
    x = x - 1`,
			shouldFail: false,
		},
		{
			name:       "while without colon",
			source:     "while True\n    break",
			shouldFail: true,
			errMsg:     "COLON",
		},
		{
			name: "for loop OK",
			source: `for i in range(10):
    print(i)`,
			shouldFail: false,
		},
		{
			name:       "for without colon",
			source:     "for i in range(10)\n    print(i)",
			shouldFail: true,
			errMsg:     "COLON",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenization failed: %v", err)
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if tt.shouldFail && err == nil {
				t.Error("Expected parse error but got none")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
			if tt.shouldFail && err != nil && tt.errMsg != "" {
				if !strings.Contains(err.Error(), tt.errMsg) {
					t.Errorf("Expected error containing %q, got: %s", tt.errMsg, err.Error())
				}
			}
		})
	}
}

func TestParseError_TryExcept(t *testing.T) {
	tests := []struct {
		name       string
		source     string
		shouldFail bool
	}{
		{
			name: "try-except OK",
			source: `try:
    x = 1 / 0
except ZeroDivisionError:
    print("error")`,
			shouldFail: false,
		},
		{
			name: "try-finally OK",
			source: `try:
    x = 1
finally:
    print("cleanup")`,
			shouldFail: false,
		},
		{
			name: "try-except-finally OK",
			source: `try:
    x = 1
except:
    pass
finally:
    print("done")`,
			shouldFail: false,
		},
		{
			name:       "try without body",
			source:     "try:",
			shouldFail: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				t.Fatalf("Tokenization failed: %v", err)
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if tt.shouldFail && err == nil {
				t.Error("Expected parse error but got none")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
		})
	}
}

func TestParseError_EmptyStatements(t *testing.T) {
	tests := []struct {
		name       string
		source     string
		shouldFail bool
	}{
		{
			name:       "empty if body",
			source:     "if True:",
			shouldFail: true,
		},
		{
			name: "if with pass",
			source: `if True:
    pass`,
			shouldFail: false,
		},
		{
			name:       "empty function",
			source:     "def foo():",
			shouldFail: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				if !tt.shouldFail {
					t.Fatalf("Tokenization failed: %v", err)
				}
				return
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if tt.shouldFail && err == nil {
				t.Error("Expected parse error but got none")
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected no error but got: %v", err)
			}
		})
	}
}

func TestParseError_InvalidAssignmentTargets(t *testing.T) {
	tests := []struct {
		name       string
		source     string
		shouldFail bool
		errMsg     string
	}{
		// Valid assignments - should not fail
		{
			name:       "simple variable assignment",
			source:     "x = 5",
			shouldFail: false,
		},
		{
			name:       "tuple unpacking",
			source:     "x, y = 1, 2",
			shouldFail: false,
		},
		{
			name:       "attribute assignment",
			source:     "obj.attr = 5",
			shouldFail: false,
		},
		{
			name:       "subscript assignment",
			source:     "arr[0] = 5",
			shouldFail: false,
		},
		{
			name:       "chained assignment",
			source:     "x = y = z = 0",
			shouldFail: false,
		},
		{
			name:       "star unpacking",
			source:     "a, *b, c = [1, 2, 3, 4]",
			shouldFail: false,
		},
		// Dict comprehension - should fail
		{
			name:       "dict comprehension assignment",
			source:     "{x: y for y, x in [(1, 2)]} = 5",
			shouldFail: true,
			errMsg:     "cannot assign to dict comprehension",
		},
		{
			name:       "dict comprehension augmented assignment",
			source:     "{x: y for y, x in [(1, 2)]} += 5",
			shouldFail: true,
			errMsg:     "illegal expression",
		},
		// List comprehension - should fail
		{
			name:       "list comprehension assignment",
			source:     "[x for x in range(5)] = [1, 2, 3]",
			shouldFail: true,
			errMsg:     "cannot assign to list comprehension",
		},
		{
			name:       "list comprehension augmented assignment",
			source:     "[x for x in range(5)] += [1]",
			shouldFail: true,
			errMsg:     "illegal expression",
		},
		// Set comprehension - should fail
		{
			name:       "set comprehension assignment",
			source:     "{x for x in range(5)} = {1, 2}",
			shouldFail: true,
			errMsg:     "cannot assign to set comprehension",
		},
		// Literals - should fail
		{
			name:       "number literal assignment",
			source:     "5 = x",
			shouldFail: true,
			errMsg:     "cannot assign to literal",
		},
		{
			name:       "string literal assignment",
			source:     `"hello" = x`,
			shouldFail: true,
			errMsg:     "cannot assign to literal",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tokenizer := NewPythonTokenizer(tt.source)
			tokens, err := tokenizer.Tokenize()
			if err != nil {
				if !tt.shouldFail {
					t.Fatalf("Tokenization failed: %v", err)
				}
				return
			}

			parser := NewPythonParser(tokens, "test.py", tt.source)
			_, err = parser.Parse()

			if tt.shouldFail && err == nil {
				t.Errorf("Expected parse error for %q but got none", tt.source)
			}
			if !tt.shouldFail && err != nil {
				t.Errorf("Expected no error for %q but got: %v", tt.source, err)
			}
			if tt.shouldFail && err != nil && tt.errMsg != "" {
				if !strings.Contains(err.Error(), tt.errMsg) {
					t.Errorf("Expected error containing %q for %q, got: %s", tt.errMsg, tt.source, err.Error())
				}
			}
		})
	}
}

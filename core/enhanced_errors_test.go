package core

import (
	"strings"
	"testing"
)

func TestSourceLocation_String(t *testing.T) {
	tests := []struct {
		name     string
		loc      *SourceLocation
		expected string
	}{
		{
			name:     "nil location",
			loc:      nil,
			expected: "<unknown>",
		},
		{
			name: "location with file and line",
			loc: &SourceLocation{
				File: "test.m28",
				Line: 42,
			},
			expected: "test.m28:42",
		},
		{
			name: "location with file, line, and column",
			loc: &SourceLocation{
				File:   "test.m28",
				Line:   42,
				Column: 15,
			},
			expected: "test.m28:42:15",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := tt.loc.String()
			if result != tt.expected {
				t.Errorf("SourceLocation.String() = %q, want %q", result, tt.expected)
			}
		})
	}
}

func TestNameError_WithLocation(t *testing.T) {
	err := &NameError{
		Name: "undefined_var",
		Location: &SourceLocation{
			File:   "test.m28",
			Line:   10,
			Column: 5,
		},
	}

	errMsg := err.Error()

	// Should contain the variable name
	if !strings.Contains(errMsg, "undefined_var") {
		t.Errorf("Error message should contain variable name, got: %s", errMsg)
	}

	// Should contain the location
	if !strings.Contains(errMsg, "test.m28:10:5") {
		t.Errorf("Error message should contain location, got: %s", errMsg)
	}
}

func TestTypeError_WithLocation(t *testing.T) {
	err := &TypeError{
		Message:  "expected number, got string",
		Expected: "number",
		Got:      "string",
		Location: &SourceLocation{
			File:   "test.m28",
			Line:   25,
			Column: 10,
		},
	}

	errMsg := err.Error()

	// Should contain the error message
	if !strings.Contains(errMsg, "expected number, got string") {
		t.Errorf("Error message should contain type mismatch, got: %s", errMsg)
	}

	// Should contain the location
	if !strings.Contains(errMsg, "test.m28:25:10") {
		t.Errorf("Error message should contain location, got: %s", errMsg)
	}
}

func TestIndexError_WithLocation(t *testing.T) {
	err := &IndexError{
		Index:  5,
		Length: 3,
		Location: &SourceLocation{
			File:   "test.m28",
			Line:   15,
			Column: 8,
		},
	}

	errMsg := err.Error()

	// Should contain index and length
	if !strings.Contains(errMsg, "index 5") || !strings.Contains(errMsg, "length 3") {
		t.Errorf("Error message should contain index and length, got: %s", errMsg)
	}

	// Should contain the location
	if !strings.Contains(errMsg, "test.m28:15:8") {
		t.Errorf("Error message should contain location, got: %s", errMsg)
	}
}

func TestKeyError_WithLocation(t *testing.T) {
	err := &KeyError{
		Key: StringValue("missing_key"),
		Location: &SourceLocation{
			File:   "test.m28",
			Line:   20,
			Column: 12,
		},
	}

	errMsg := err.Error()

	// Should contain the key
	if !strings.Contains(errMsg, "missing_key") {
		t.Errorf("Error message should contain key, got: %s", errMsg)
	}

	// Should contain the location
	if !strings.Contains(errMsg, "test.m28:20:12") {
		t.Errorf("Error message should contain location, got: %s", errMsg)
	}
}

func TestZeroDivisionError_WithLocation(t *testing.T) {
	err := &ZeroDivisionError{
		Location: &SourceLocation{
			File:   "test.m28",
			Line:   30,
			Column: 7,
		},
	}

	errMsg := err.Error()

	// Should contain "division by zero"
	if !strings.Contains(errMsg, "division by zero") {
		t.Errorf("Error message should mention division by zero, got: %s", errMsg)
	}

	// Should contain the location
	if !strings.Contains(errMsg, "test.m28:30:7") {
		t.Errorf("Error message should contain location, got: %s", errMsg)
	}
}

func TestEvalError_WithSyntaxKind(t *testing.T) {
	err := &EvalError{
		Type:       "NameError",
		Message:    "undefined variable",
		File:       "test.m28",
		Line:       5,
		Column:     10,
		SyntaxKind: 1, // Python syntax
	}

	// SyntaxKind should be accessible
	if err.SyntaxKind != 1 {
		t.Errorf("Expected SyntaxKind = 1, got %d", err.SyntaxKind)
	}
}

func TestGetLocationForValue_Helper(t *testing.T) {
	// Create a context with metadata
	ctx := NewContext(nil)

	// Create a value
	val := NumberValue(42)

	// Set location in metadata
	loc := &SourceLocation{
		File:   "test.m28",
		Line:   10,
		Column: 5,
	}
	ctx.Metadata.SetLocation(val, loc)

	// Retrieve using helper
	retrievedLoc := GetLocationForValue(ctx, val)

	if retrievedLoc == nil {
		t.Fatal("Expected location to be retrieved, got nil")
	}

	if retrievedLoc.File != "test.m28" || retrievedLoc.Line != 10 || retrievedLoc.Column != 5 {
		t.Errorf("Location mismatch: got %s, want test.m28:10:5", retrievedLoc.String())
	}
}

func TestGetSyntaxKindForValue_Helper(t *testing.T) {
	// Create a context with metadata
	ctx := NewContext(nil)

	// Create a value
	val := NumberValue(42)

	// Set syntax kind in metadata
	ctx.Metadata.SetSyntaxKind(val, 1) // Python syntax

	// Retrieve using helper
	kind := GetSyntaxKindForValue(ctx, val)

	if kind != 1 {
		t.Errorf("Expected SyntaxKind = 1, got %d", kind)
	}
}

func TestErrorsWithoutLocation(t *testing.T) {
	// Verify errors still work without locations (backwards compatibility)

	nameErr := &NameError{Name: "foo"}
	if !strings.Contains(nameErr.Error(), "foo") {
		t.Errorf("NameError without location should still show name")
	}

	typeErr := &TypeError{Message: "type mismatch"}
	if !strings.Contains(typeErr.Error(), "type mismatch") {
		t.Errorf("TypeError without location should still show message")
	}

	indexErr := &IndexError{Index: 5, Length: 3}
	if !strings.Contains(indexErr.Error(), "index 5") {
		t.Errorf("IndexError without location should still show index")
	}
}

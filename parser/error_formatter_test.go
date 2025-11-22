package parser

import (
	"strings"
	"testing"

	"github.com/mmichie/m28/core"
)

func TestErrorFormatter_Basic(t *testing.T) {
	ef := NewErrorFormatter(false) // No colors for testing

	err := &ParseError{
		Message:  "unexpected token",
		Line:     1,
		Col:      5,
		Filename: "test.py",
	}

	formatted := ef.FormatError(err)

	// Should contain file and line info
	if !strings.Contains(formatted, "test.py") {
		t.Errorf("Expected filename in output, got: %s", formatted)
	}
	if !strings.Contains(formatted, "line 1") {
		t.Errorf("Expected line number in output, got: %s", formatted)
	}
	if !strings.Contains(formatted, "unexpected token") {
		t.Errorf("Expected error message in output, got: %s", formatted)
	}
}

func TestErrorFormatter_WithSource(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := `x = 10
y = 20 30
z = 40`

	ef.AddSource("test.py", source)

	err := &ParseError{
		Message:  "invalid syntax",
		Line:     2,
		Col:      8,
		Filename: "test.py",
	}

	formatted := ef.FormatError(err)

	// Should show the error line
	if !strings.Contains(formatted, "y = 20 30") {
		t.Errorf("Expected source line in output, got: %s", formatted)
	}

	// Should show caret indicator
	if !strings.Contains(formatted, "^") {
		t.Errorf("Expected caret indicator in output, got: %s", formatted)
	}
}

func TestErrorFormatter_SourceContext(t *testing.T) {
	ef := NewErrorFormatter(false)
	ef.SetContextSize(1) // Show 1 line before and after

	source := `line1
line2
error_line
line4
line5`

	ef.AddSource("test.py", source)

	err := &ParseError{
		Message:  "syntax error",
		Line:     3,
		Col:      1,
		Filename: "test.py",
	}

	formatted := ef.FormatError(err)

	// Should show context lines
	if !strings.Contains(formatted, "line2") {
		t.Errorf("Expected context line before error, got: %s", formatted)
	}
	if !strings.Contains(formatted, "error_line") {
		t.Errorf("Expected error line, got: %s", formatted)
	}
	if !strings.Contains(formatted, "line4") {
		t.Errorf("Expected context line after error, got: %s", formatted)
	}

	// Should NOT show lines outside context
	if strings.Contains(formatted, "line1") {
		t.Errorf("Unexpected context line in output (outside window), got: %s", formatted)
	}
	if strings.Contains(formatted, "line5") {
		t.Errorf("Unexpected context line in output (outside window), got: %s", formatted)
	}
}

func TestErrorFormatter_EdgeCases(t *testing.T) {
	ef := NewErrorFormatter(false)

	tests := []struct {
		name     string
		err      error
		source   string
		filename string
		wantErr  bool
	}{
		{
			name:     "nil error",
			err:      nil,
			source:   "",
			filename: "",
			wantErr:  false,
		},
		{
			name: "missing source",
			err: &ParseError{
				Message:  "error",
				Line:     1,
				Col:      1,
				Filename: "missing.py",
			},
			source:   "",
			filename: "other.py",
			wantErr:  false,
		},
		{
			name: "invalid line number (too high)",
			err: &ParseError{
				Message:  "error",
				Line:     999,
				Col:      1,
				Filename: "test.py",
			},
			source:   "line1\nline2",
			filename: "test.py",
			wantErr:  false,
		},
		{
			name: "invalid line number (zero)",
			err: &ParseError{
				Message:  "error",
				Line:     0,
				Col:      1,
				Filename: "test.py",
			},
			source:   "line1\nline2",
			filename: "test.py",
			wantErr:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if tt.source != "" {
				ef.AddSource(tt.filename, tt.source)
			}

			// Should not panic
			formatted := ef.FormatError(tt.err)
			_ = formatted // Just ensure it doesn't crash
		})
	}
}

func TestErrorFormatter_CaretPositioning(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := `x = 10 + error_token`

	ef.AddSource("test.py", source)

	err := &ParseError{
		Message:  "undefined name",
		Line:     1,
		Col:      10, // Points to 'e' in 'error_token'
		Filename: "test.py",
	}

	formatted := ef.FormatError(err)

	// Count spaces before caret
	lines := strings.Split(formatted, "\n")
	var caretLine string
	for _, line := range lines {
		if strings.Contains(line, "^") {
			caretLine = line
			break
		}
	}

	if caretLine == "" {
		t.Fatal("No caret line found in output")
	}

	// Caret should be roughly at column 10 + prefix (3 for " -> " or "    ")
	leadingSpaces := 0
	for _, ch := range caretLine {
		if ch == ' ' {
			leadingSpaces++
		} else {
			break
		}
	}

	// Should be around 10 + 3 = 13 spaces
	if leadingSpaces < 10 || leadingSpaces > 15 {
		t.Errorf("Caret positioning seems wrong. Expected ~13 leading spaces, got %d. Line: %q", leadingSpaces, caretLine)
	}
}

func TestErrorFormatter_WithSuggestion(t *testing.T) {
	ef := NewErrorFormatter(false)

	err := &ParseError{
		Message:  "name 'x' is not defined",
		Line:     1,
		Col:      1,
		Filename: "test.py",
	}

	formatted := ef.FormatErrorWithSuggestion(err, "Did you mean 'y'?")

	if !strings.Contains(formatted, "Did you mean") {
		t.Errorf("Expected suggestion in output, got: %s", formatted)
	}
}

func TestErrorFormatter_ColorOutput(t *testing.T) {
	ef := NewErrorFormatter(true) // Enable colors

	err := &ParseError{
		Message:  "syntax error",
		Line:     1,
		Col:      1,
		Filename: "test.py",
	}

	formatted := ef.FormatError(err)

	// Should contain ANSI codes
	if !strings.Contains(formatted, "\033[") {
		t.Errorf("Expected ANSI color codes in colored output, got: %s", formatted)
	}

	// Test disabling colors
	ef.SetUseColor(false)
	formatted = ef.FormatError(err)

	// Should NOT contain ANSI codes
	if strings.Contains(formatted, "\033[") {
		t.Errorf("Expected no ANSI codes when colors disabled, got: %s", formatted)
	}
}

func TestErrorFormatter_MultilineError(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := `def foo():
    return (
        x +
        y
    )`

	ef.AddSource("test.py", source)

	err := &ParseError{
		Message:  "unexpected indent",
		Line:     3,
		Col:      9,
		Filename: "test.py",
	}

	formatted := ef.FormatError(err)

	// Should show the error line
	if !strings.Contains(formatted, "x +") {
		t.Errorf("Expected error line in output, got: %s", formatted)
	}
}

func TestErrorFormatter_GetSource(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := "test content"
	ef.AddSource("test.py", source)

	// Should retrieve registered source
	retrieved, ok := ef.GetSource("test.py")
	if !ok {
		t.Error("Expected to find registered source")
	}
	if retrieved != source {
		t.Errorf("Retrieved source doesn't match. Expected %q, got %q", source, retrieved)
	}

	// Should return false for missing source
	_, ok = ef.GetSource("missing.py")
	if ok {
		t.Error("Expected false for missing source")
	}
}

func TestErrorFormatter_ContextSize(t *testing.T) {
	ef := NewErrorFormatter(false)
	ef.SetContextSize(0) // Show only error line

	source := `line1
line2
error_line
line4
line5`

	ef.AddSource("test.py", source)

	err := &ParseError{
		Message:  "error",
		Line:     3,
		Col:      1,
		Filename: "test.py",
	}

	formatted := ef.FormatError(err)

	// Should show only error line
	if !strings.Contains(formatted, "error_line") {
		t.Errorf("Expected error line, got: %s", formatted)
	}

	// Should NOT show context
	if strings.Contains(formatted, "line2") || strings.Contains(formatted, "line4") {
		t.Errorf("Expected no context lines with contextSize=0, got: %s", formatted)
	}
}

func TestErrorFormatter_RangeHighlighting(t *testing.T) {
	ef := NewErrorFormatter(false)

	source := `result = undefined_variable + 10`

	ef.AddSource("test.py", source)

	// Simulate error with range (EndCol)
	loc := &core.SourceLocation{
		File:    "test.py",
		Line:    1,
		Column:  10,
		EndLine: 1,
		EndCol:  28, // Highlight "undefined_variable"
	}

	// For now, test that formatter handles EndCol
	// (Full range highlighting would be in formatCarets)
	carets := ef.formatCarets(source, loc)

	// Should have multiple carets for range
	caretCount := strings.Count(carets, "^")
	if caretCount < 2 {
		t.Errorf("Expected multiple carets for range, got: %s", carets)
	}
}

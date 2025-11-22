package parser

import (
	"fmt"
	"strings"
	"testing"
)

// TestErrorFormatter_VisualDemo demonstrates the error formatter output
// Run with: go test -v -run TestErrorFormatter_VisualDemo
func TestErrorFormatter_VisualDemo(t *testing.T) {
	// Create formatter with colors
	ef := NewErrorFormatter(true)

	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("ErrorFormatter Visual Demo")
	fmt.Println(strings.Repeat("=", 70) + "\n")

	// Example 1: Basic syntax error
	source1 := `x = 10
y = 20 30
z = 40`

	ef.AddSource("example.py", source1)

	err1 := &ParseError{
		Message:  "SyntaxError: invalid syntax",
		Line:     2,
		Col:      8,
		Filename: "example.py",
	}

	fmt.Println("Example 1: Basic Syntax Error")
	fmt.Println(strings.Repeat("-", 70))
	fmt.Print(ef.FormatError(err1))
	fmt.Println()

	// Example 2: With suggestion
	source2 := `def foo():
    return undefined_var`

	ef.AddSource("test.py", source2)

	err2 := &ParseError{
		Message:  "NameError: name 'undefined_var' is not defined",
		Line:     2,
		Col:      12,
		Filename: "test.py",
	}

	fmt.Println("Example 2: With Suggestion")
	fmt.Println(strings.Repeat("-", 70))
	fmt.Print(ef.FormatErrorWithSuggestion(err2, "Did you mean: 'undefined_variable', 'defined_var'?"))
	fmt.Println()

	// Example 3: Complex error with context
	source3 := `result = some_function(
    arg1,
    arg2,
    unexpected_token
)`

	ef.AddSource("complex.py", source3)

	err3 := &ParseError{
		Message:  "SyntaxError: unexpected identifier",
		Line:     4,
		Col:      5,
		Filename: "complex.py",
	}

	fmt.Println("Example 3: Context Showing")
	fmt.Println(strings.Repeat("-", 70))
	fmt.Print(ef.FormatError(err3))
	fmt.Println()

	// Example 4: No color version
	efNoColor := NewErrorFormatter(false)
	efNoColor.AddSource("plain.py", "x = 10 + ")

	err4 := &ParseError{
		Message:  "SyntaxError: unexpected end of line",
		Line:     1,
		Col:      10,
		Filename: "plain.py",
	}

	fmt.Println("Example 4: No Color Output (for piping/logs)")
	fmt.Println(strings.Repeat("-", 70))
	fmt.Print(efNoColor.FormatError(err4))

	fmt.Println("\n" + strings.Repeat("=", 70))
	fmt.Println("Demo complete!")
	fmt.Println(strings.Repeat("=", 70))
}

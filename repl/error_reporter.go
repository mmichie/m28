package repl

import (
	"fmt"
	"io"
	"strings"
	
	"github.com/mmichie/m28/core"
)

// ErrorReporter provides enhanced error reporting
type ErrorReporter struct {
	sourceCode map[string][]string // filename -> lines
}

// NewErrorReporter creates a new error reporter
func NewErrorReporter() *ErrorReporter {
	return &ErrorReporter{
		sourceCode: make(map[string][]string),
	}
}

// AddSource adds source code for error reporting
func (er *ErrorReporter) AddSource(filename, source string) {
	lines := strings.Split(source, "\n")
	er.sourceCode[filename] = lines
}

// ReportError reports an error with context
func (er *ErrorReporter) ReportError(err error, ctx *core.Context, w io.Writer) {
	// Check if it's a structured error
	if evalErr, ok := err.(*core.EvalError); ok {
		er.reportEvalError(evalErr, ctx, w)
		return
	}
	
	// Check for name errors
	if nameErr, ok := err.(*core.NameError); ok {
		er.reportNameError(nameErr, ctx, w)
		return
	}
	
	// Check for type errors
	if typeErr, ok := err.(*core.TypeError); ok {
		er.reportTypeError(typeErr, ctx, w)
		return
	}
	
	// Default error reporting
	fmt.Fprintf(w, "Error: %s\n", err)
	
	// Add stack trace if available
	if ctx != nil && len(ctx.CallStack) > 0 {
		fmt.Fprint(w, ctx.FormatStackTrace())
	}
}

// reportEvalError reports an evaluation error with source context
func (er *ErrorReporter) reportEvalError(err *core.EvalError, ctx *core.Context, w io.Writer) {
	// Print error type and message
	fmt.Fprintf(w, "%s: %s\n", err.Type, err.Message)
	
	// Print stack trace
	if ctx != nil && len(ctx.CallStack) > 0 {
		fmt.Fprint(w, ctx.FormatStackTrace())
	}
	
	// Show source context if available
	if err.File != "" && err.Line > 0 {
		er.showSourceContext(err.File, err.Line, err.Column, w)
	}
	
	// Print suggestions if any
	if err.Suggestion != "" {
		fmt.Fprintf(w, "\nSuggestion: %s\n", err.Suggestion)
	}
}

// reportNameError reports a name error with suggestions
func (er *ErrorReporter) reportNameError(err *core.NameError, ctx *core.Context, w io.Writer) {
	fmt.Fprintf(w, "NameError: name '%s' is not defined\n", err.Name)
	
	// Show stack trace
	if ctx != nil && len(ctx.CallStack) > 0 {
		fmt.Fprint(w, ctx.FormatStackTrace())
	}
	
	// Find similar names for suggestions
	if ctx != nil {
		suggestions := er.findSimilarNames(err.Name, ctx)
		if len(suggestions) > 0 {
			fmt.Fprintf(w, "\nDid you mean: %s?\n", strings.Join(suggestions, ", "))
		}
	}
}

// reportTypeError reports a type error with details
func (er *ErrorReporter) reportTypeError(err *core.TypeError, ctx *core.Context, w io.Writer) {
	fmt.Fprintf(w, "TypeError: %s\n", err.Message)
	
	if err.Expected != "" && err.Got != "" {
		fmt.Fprintf(w, "  Expected: %s\n", err.Expected)
		fmt.Fprintf(w, "  Got: %s\n", err.Got)
	}
	
	// Show stack trace
	if ctx != nil && len(ctx.CallStack) > 0 {
		fmt.Fprint(w, ctx.FormatStackTrace())
	}
}

// showSourceContext displays the source code around an error
func (er *ErrorReporter) showSourceContext(filename string, line, column int, w io.Writer) {
	lines, ok := er.sourceCode[filename]
	if !ok {
		return
	}
	
	// Show 2 lines before and after
	start := line - 3
	if start < 0 {
		start = 0
	}
	end := line + 2
	if end >= len(lines) {
		end = len(lines) - 1
	}
	
	fmt.Fprintf(w, "\nFile \"%s\", line %d:\n", filename, line)
	
	for i := start; i <= end; i++ {
		lineNum := i + 1
		prefix := "  "
		if lineNum == line {
			prefix = "> "
		}
		
		fmt.Fprintf(w, "%s%4d | %s\n", prefix, lineNum, lines[i])
		
		// Show column indicator for the error line
		if lineNum == line && column > 0 {
			spaces := strings.Repeat(" ", column + 6)
			fmt.Fprintf(w, "%s^\n", spaces)
		}
	}
}

// findSimilarNames finds similar variable names for suggestions
func (er *ErrorReporter) findSimilarNames(name string, ctx *core.Context) []string {
	allNames := ctx.GetAllSymbols()
	var suggestions []string
	
	for _, candidate := range allNames {
		if er.isSimilar(name, candidate) {
			suggestions = append(suggestions, candidate)
		}
	}
	
	// Limit to top 3 suggestions
	if len(suggestions) > 3 {
		suggestions = suggestions[:3]
	}
	
	return suggestions
}

// isSimilar checks if two names are similar (simple edit distance)
func (er *ErrorReporter) isSimilar(s1, s2 string) bool {
	// Very simple similarity check
	// In a real implementation, use Levenshtein distance
	
	if len(s1) == 0 || len(s2) == 0 {
		return false
	}
	
	// Check if one is a substring of the other
	if strings.Contains(s1, s2) || strings.Contains(s2, s1) {
		return true
	}
	
	// Check if they differ by only one character
	if abs(len(s1)-len(s2)) > 1 {
		return false
	}
	
	differences := 0
	minLen := len(s1)
	if len(s2) < minLen {
		minLen = len(s2)
	}
	
	for i := 0; i < minLen; i++ {
		if s1[i] != s2[i] {
			differences++
		}
	}
	
	return differences <= 1
}

func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}
package repl

import (
	"fmt"
	"io"
	"strings"

	"github.com/mmichie/m28/common/suggestions"
	"github.com/mmichie/m28/core"
)

// ErrorReporter provides enhanced error reporting
type ErrorReporter struct {
	sourceCode   map[string][]string // filename -> lines
	colorManager *ColorManager
}

// NewErrorReporter creates a new error reporter
func NewErrorReporter(colorManager *ColorManager) *ErrorReporter {
	return &ErrorReporter{
		sourceCode:   make(map[string][]string),
		colorManager: colorManager,
	}
}

// AddSource adds source code for error reporting
func (er *ErrorReporter) AddSource(filename, source string) {
	lines := strings.Split(source, "\n")
	er.sourceCode[filename] = lines
}

// ReportError reports an error with context
func (er *ErrorReporter) ReportError(err error, ctx *core.Context, w io.Writer) {
	// Check if it's an Exception with chaining (import eval to avoid circular dependency)
	// We need to use type assertion carefully here
	if excErr, ok := err.(interface {
		Error() string
		ErrorWithChain() string
	}); ok {
		// Check if it has chaining (Cause or Context)
		chainedMsg := excErr.ErrorWithChain()
		if chainedMsg != excErr.Error() {
			// Has chaining, show full chain
			fmt.Fprintln(w, er.colorManager.ColorizeError(chainedMsg))
			if ctx != nil && len(ctx.CallStack) > 0 {
				fmt.Fprint(w, ctx.FormatStackTrace())
			}
			return
		}
	}

	// Check if it's a structured error
	if evalErr, ok := err.(*core.EvalError); ok {
		// Check if EvalError wraps a NameError
		if nameErr, ok := evalErr.Wrapped.(*core.NameError); ok {
			er.reportNameError(nameErr, ctx, w)
			return
		}
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
	msg := fmt.Sprintf("Error: %s", err)
	fmt.Fprintln(w, er.colorManager.ColorizeError(msg))

	// Add stack trace if available
	if ctx != nil && len(ctx.CallStack) > 0 {
		fmt.Fprint(w, ctx.FormatStackTrace())
	}
}

// reportEvalError reports an evaluation error with source context
func (er *ErrorReporter) reportEvalError(err *core.EvalError, ctx *core.Context, w io.Writer) {
	// Print error type and message
	msg := fmt.Sprintf("%s: %s", err.Type, err.Message)

	// Add syntax kind if available
	if err.SyntaxKind != 0 {
		syntaxName := er.getSyntaxKindName(err.SyntaxKind)
		if syntaxName != "" {
			msg = fmt.Sprintf("%s [%s syntax]", msg, syntaxName)
		}
	}

	fmt.Fprintln(w, er.colorManager.ColorizeError(msg))

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
	msg := fmt.Sprintf("NameError: name '%s' is not defined", err.Name)
	fmt.Fprintln(w, er.colorManager.ColorizeError(msg))

	// Show stack trace
	if ctx != nil && len(ctx.CallStack) > 0 {
		fmt.Fprint(w, ctx.FormatStackTrace())
	}

	// Show source context if location is available
	if err.Location != nil {
		er.showSourceContext(err.Location.File, err.Location.Line, err.Location.Column, w)
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
	msg := fmt.Sprintf("TypeError: %s", err.Message)
	fmt.Fprintln(w, er.colorManager.ColorizeError(msg))

	if err.Expected != "" && err.Got != "" {
		fmt.Fprintf(w, "  Expected: %s\n", err.Expected)
		fmt.Fprintf(w, "  Got: %s\n", err.Got)
	}

	// Show stack trace
	if ctx != nil && len(ctx.CallStack) > 0 {
		fmt.Fprint(w, ctx.FormatStackTrace())
	}

	// Show source context if location is available
	if err.Location != nil {
		er.showSourceContext(err.Location.File, err.Location.Line, err.Location.Column, w)
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
			spaces := strings.Repeat(" ", column+6)
			fmt.Fprintf(w, "%s^\n", spaces)
		}
	}
}

// findSimilarNames finds similar variable names for suggestions
func (er *ErrorReporter) findSimilarNames(name string, ctx *core.Context) []string {
	allNames := ctx.GetAllSymbols()
	// Use Levenshtein-based similarity matching with max distance 2, up to 3 suggestions
	return suggestions.FindSimilarNames(name, allNames, 2, 3)
}

// isSimilar checks if two names are similar (using Levenshtein distance)
// Deprecated: Use suggestions.LevenshteinDistance directly
func (er *ErrorReporter) isSimilar(s1, s2 string) bool {
	if len(s1) == 0 || len(s2) == 0 {
		return false
	}

	// Use Levenshtein distance - consider similar if within 2 edits
	distance := suggestions.LevenshteinDistance(s1, s2)
	return distance <= 2
}

// getSyntaxKindName returns a human-readable name for syntax kind
func (er *ErrorReporter) getSyntaxKindName(kind int) string {
	// Import ast.SyntaxKind constants
	// 0 = SyntaxLisp, 1 = SyntaxPython, 2 = SyntaxJSON, 3 = SyntaxCustom
	switch kind {
	case 0:
		return "Lisp"
	case 1:
		return "Python"
	case 2:
		return "JSON"
	case 3:
		return "Custom"
	default:
		return ""
	}
}

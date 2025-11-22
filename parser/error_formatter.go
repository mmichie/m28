package parser

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// ErrorFormatter provides Python-style error formatting with source context
type ErrorFormatter struct {
	sources     map[string]string // filename -> source code
	useColor    bool              // enable ANSI color codes
	contextSize int               // lines of context to show (default: 2)
}

// NewErrorFormatter creates a new error formatter
func NewErrorFormatter(useColor bool) *ErrorFormatter {
	return &ErrorFormatter{
		sources:     make(map[string]string),
		useColor:    useColor,
		contextSize: 2, // show 2 lines before and after
	}
}

// AddSource registers source code for error reporting
func (ef *ErrorFormatter) AddSource(filename, source string) {
	ef.sources[filename] = source
}

// GetSource retrieves registered source code
func (ef *ErrorFormatter) GetSource(filename string) (string, bool) {
	source, ok := ef.sources[filename]
	return source, ok
}

// FormatError formats an error with Python-style output
func (ef *ErrorFormatter) FormatError(err error) string {
	if err == nil {
		return ""
	}

	// Try to extract location from various error types
	loc := ef.extractLocation(err)
	if loc == nil {
		// No location info - return basic error
		return ef.colorError(err.Error())
	}

	var b strings.Builder

	// Python-style header: File "path", line N
	if loc.File != "" {
		b.WriteString(fmt.Sprintf("  File \"%s\", line %d\n", loc.File, loc.Line))
	} else {
		b.WriteString(fmt.Sprintf("  Line %d\n", loc.Line))
	}

	// Show source context if available
	if source, ok := ef.sources[loc.File]; ok {
		context := ef.formatSourceContext(source, loc)
		b.WriteString(context)
	}

	// Error message
	b.WriteString(ef.colorError(err.Error()))
	b.WriteString("\n")

	return b.String()
}

// FormatErrorWithSuggestion formats error with an optional suggestion
func (ef *ErrorFormatter) FormatErrorWithSuggestion(err error, suggestion string) string {
	formatted := ef.FormatError(err)
	if suggestion != "" {
		formatted += "\n" + ef.colorSuggestion(suggestion) + "\n"
	}
	return formatted
}

// extractLocation tries to extract SourceLocation from various error types
func (ef *ErrorFormatter) extractLocation(err error) *core.SourceLocation {
	// Try ParseError
	if parseErr, ok := err.(*ParseError); ok {
		return &core.SourceLocation{
			File:   parseErr.Filename,
			Line:   parseErr.Line,
			Column: parseErr.Col,
		}
	}

	// Try to find SourceLocation in other error types (extensible)
	// Add more type checks here as needed

	return nil
}

// formatSourceContext formats source code with visual indicators
func (ef *ErrorFormatter) formatSourceContext(source string, loc *core.SourceLocation) string {
	lines := strings.Split(source, "\n")
	if loc.Line < 1 || loc.Line > len(lines) {
		return "" // Invalid line number
	}

	var b strings.Builder

	// Calculate context window
	start := loc.Line - ef.contextSize - 1
	if start < 0 {
		start = 0
	}
	end := loc.Line + ef.contextSize - 1
	if end >= len(lines) {
		end = len(lines) - 1
	}

	// Show context lines
	for i := start; i <= end; i++ {
		lineNum := i + 1
		line := lines[i]

		// Prefix for error line vs context
		prefix := "    "
		if lineNum == loc.Line {
			prefix = " -> "
			if ef.useColor {
				prefix = ef.colorError(prefix)
			}
		}

		// Format line
		b.WriteString(fmt.Sprintf("%s%s\n", prefix, line))

		// Add caret indicator for the error line
		if lineNum == loc.Line && loc.Column > 0 {
			carets := ef.formatCarets(line, loc)
			b.WriteString(carets)
		}
	}

	return b.String()
}

// formatCarets creates the ^^^^ indicator under the error location
func (ef *ErrorFormatter) formatCarets(line string, loc *core.SourceLocation) string {
	// Calculate spaces before caret (accounting for prefix)
	spaces := strings.Repeat(" ", loc.Column+3) // 3 for " -> " or "    "

	// Determine caret length
	caretLen := 1
	if loc.EndCol > loc.Column {
		caretLen = loc.EndCol - loc.Column
	} else if loc.Column > 0 && loc.Column <= len(line) {
		// Highlight the token at the error position
		// Find end of current token (simple heuristic)
		rest := line[loc.Column-1:]
		for i, ch := range rest {
			if ch == ' ' || ch == '\t' || ch == '(' || ch == ')' || ch == '[' || ch == ']' {
				if i > 0 {
					caretLen = i
				}
				break
			}
		}
	}

	carets := strings.Repeat("^", caretLen)
	if ef.useColor {
		carets = ef.colorError(carets)
	}

	return fmt.Sprintf("%s%s\n", spaces, carets)
}

// Color helpers (ANSI codes)
const (
	colorReset   = "\033[0m"
	colorRed     = "\033[31m"
	colorYellow  = "\033[33m"
	colorBold    = "\033[1m"
	colorRedBold = "\033[1;31m"
)

func (ef *ErrorFormatter) colorError(text string) string {
	if !ef.useColor {
		return text
	}
	return colorRedBold + text + colorReset
}

func (ef *ErrorFormatter) colorSuggestion(text string) string {
	if !ef.useColor {
		return text
	}
	return colorYellow + text + colorReset
}

// SetContextSize sets the number of context lines to show
func (ef *ErrorFormatter) SetContextSize(size int) {
	if size >= 0 {
		ef.contextSize = size
	}
}

// SetUseColor enables or disables color output
func (ef *ErrorFormatter) SetUseColor(useColor bool) {
	ef.useColor = useColor
}

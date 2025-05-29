package repl

import (
	"fmt"
	"os"
	"strings"
)

// ANSI color codes
const (
	ColorReset     = "\033[0m"
	ColorBold      = "\033[1m"
	ColorDim       = "\033[2m"
	ColorUnderline = "\033[4m"
	
	// Regular colors
	ColorBlack   = "\033[30m"
	ColorRed     = "\033[31m"
	ColorGreen   = "\033[32m"
	ColorYellow  = "\033[33m"
	ColorBlue    = "\033[34m"
	ColorMagenta = "\033[35m"
	ColorCyan    = "\033[36m"
	ColorWhite   = "\033[37m"
	
	// Bright colors
	ColorBrightBlack   = "\033[90m"
	ColorBrightRed     = "\033[91m"
	ColorBrightGreen   = "\033[92m"
	ColorBrightYellow  = "\033[93m"
	ColorBrightBlue    = "\033[94m"
	ColorBrightMagenta = "\033[95m"
	ColorBrightCyan    = "\033[96m"
	ColorBrightWhite   = "\033[97m"
)

// ColorTheme defines colors for different syntax elements
type ColorTheme struct {
	Keyword       string
	String        string
	Number        string
	Comment       string
	Function      string
	Variable      string
	Operator      string
	Parenthesis   string
	Error         string
	Prompt        string
	Output        string
	OutputPrompt  string
}

// DefaultTheme returns the default color theme
func DefaultTheme() *ColorTheme {
	return &ColorTheme{
		Keyword:      ColorBlue,
		String:       ColorGreen,
		Number:       ColorCyan,
		Comment:      ColorBrightBlack,
		Function:     ColorYellow,
		Variable:     ColorWhite,
		Operator:     ColorBrightWhite,
		Parenthesis:  ColorBrightBlack,
		Error:        ColorRed,
		Prompt:       ColorBrightGreen,
		Output:       ColorWhite,
		OutputPrompt: ColorBrightYellow,
	}
}

// ColorManager manages syntax highlighting
type ColorManager struct {
	enabled bool
	theme   *ColorTheme
}

// NewColorManager creates a new color manager
func NewColorManager(enabled bool) *ColorManager {
	// Check if we should disable colors (e.g., when output is piped)
	if !isTerminal() {
		enabled = false
	}
	
	return &ColorManager{
		enabled: enabled,
		theme:   DefaultTheme(),
	}
}

// isTerminal checks if output is going to a terminal
func isTerminal() bool {
	// Simple check - in production, use golang.org/x/term
	fileInfo, _ := os.Stdout.Stat()
	return (fileInfo.Mode() & os.ModeCharDevice) != 0
}

// SetEnabled enables or disables colors
func (cm *ColorManager) SetEnabled(enabled bool) {
	cm.enabled = enabled
}

// IsEnabled returns whether colors are enabled
func (cm *ColorManager) IsEnabled() bool {
	return cm.enabled
}

// Colorize applies color to text if colors are enabled
func (cm *ColorManager) Colorize(text, color string) string {
	if !cm.enabled || color == "" {
		return text
	}
	return fmt.Sprintf("%s%s%s", color, text, ColorReset)
}

// ColorizePrompt colors the input prompt
func (cm *ColorManager) ColorizePrompt(prompt string) string {
	return cm.Colorize(prompt, cm.theme.Prompt)
}

// ColorizeOutputPrompt colors the output prompt
func (cm *ColorManager) ColorizeOutputPrompt(prompt string) string {
	return cm.Colorize(prompt, cm.theme.OutputPrompt)
}

// ColorizeError colors error messages
func (cm *ColorManager) ColorizeError(text string) string {
	return cm.Colorize(text, cm.theme.Error)
}

// ColorizeSyntax applies syntax highlighting to M28 code
func (cm *ColorManager) ColorizeSyntax(code string) string {
	if !cm.enabled {
		return code
	}
	
	// This is a simple syntax highlighter
	// In a real implementation, you'd use a proper lexer
	result := code
	
	// Keywords
	keywords := []string{
		"def", "lambda", "if", "elif", "else", "for", "while", "in",
		"try", "except", "finally", "raise", "return", "break", "continue",
		"class", "super", "import", "from", "as", "with", "yield",
		"async", "await", "channel", "select", "go", "true", "false", "nil",
	}
	
	for _, kw := range keywords {
		// Match whole words only
		pattern := fmt.Sprintf(`\b%s\b`, kw)
		colored := cm.Colorize(kw, cm.theme.Keyword)
		result = replaceWholeWord(result, pattern, colored)
	}
	
	// Numbers (simple pattern)
	result = colorizePattern(result, `\b\d+\.?\d*\b`, cm.theme.Number, cm)
	
	// Strings (simple pattern for double quotes)
	result = colorizePattern(result, `"[^"]*"`, cm.theme.String, cm)
	
	// Comments
	result = colorizePattern(result, `#.*$`, cm.theme.Comment, cm)
	
	// Parentheses
	for _, p := range []string{"(", ")", "[", "]", "{", "}"} {
		colored := cm.Colorize(p, cm.theme.Parenthesis)
		result = strings.ReplaceAll(result, p, colored)
	}
	
	return result
}

// replaceWholeWord replaces whole word matches
func replaceWholeWord(text, pattern, replacement string) string {
	// This is a simplified implementation
	// In production, use regexp
	words := strings.Fields(text)
	for i, word := range words {
		if word == pattern || strings.Trim(word, "()[]{}:,") == pattern {
			words[i] = strings.Replace(word, pattern, replacement, 1)
		}
	}
	return strings.Join(words, " ")
}

// colorizePattern applies color to pattern matches
func colorizePattern(text, pattern, color string, cm *ColorManager) string {
	// This is a simplified implementation
	// In production, use regexp to properly handle patterns
	return text
}
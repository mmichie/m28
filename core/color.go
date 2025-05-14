package core

import (
	"fmt"
	"os"
)

// Color constants for terminal output
const (
	ColorReset  = "\033[0m"
	ColorRed    = "\033[1;31m"
	ColorGreen  = "\033[1;32m"
	ColorYellow = "\033[1;33m"
	ColorBlue   = "\033[1;34m"
	ColorPurple = "\033[1;35m"
	ColorCyan   = "\033[1;36m"
	ColorWhite  = "\033[37m"
	ColorGray   = "\033[2;37m"
	ColorBold   = "\033[1m"

	// Bright colors
	ColorBrightRed    = "\033[91m"
	ColorBrightGreen  = "\033[92m"
	ColorBrightYellow = "\033[93m"
	ColorBrightBlue   = "\033[94m"
	ColorBrightPurple = "\033[95m"
	ColorBrightCyan   = "\033[96m"
	ColorBrightWhite  = "\033[97m"
)

// ColoredPrompt is the default prompt with color
var ColoredPrompt = fmt.Sprintf("%sm28>%s ", ColorBlue, ColorReset)

// ColorEnabled determines whether to use colored output
var ColorEnabled = true

// ColoredErrors determines whether to use colored error output
var ColoredErrors = true

// IsTerminal attempts to determine if the output is a terminal
// where color would be useful
func IsTerminal() bool {
	fileInfo, err := os.Stdout.Stat()
	if err != nil {
		return false
	}
	return (fileInfo.Mode() & os.ModeCharDevice) != 0
}

// DisableColors turns off colored output
func DisableColors() {
	ColorEnabled = false
	ColoredErrors = false
}

// EnableColors turns on colored output
func EnableColors() {
	ColorEnabled = true
	ColoredErrors = true
}

// GetColorCode returns either the ANSI color code or empty string based on settings
func GetColorCode(code string) string {
	if ColorEnabled && IsTerminal() {
		return code
	}
	return ""
}

// getColorCode is an alias for GetColorCode to maintain compatibility with existing code
func getColorCode(code string) string {
	return GetColorCode(code)
}

// ColorizeValue returns a colorized string representation of a value
func ColorizeValue(val LispValue) string {
	switch v := val.(type) {
	case float64:
		return fmt.Sprintf("%s%g%s", GetColorCode(ColorCyan), v, GetColorCode(ColorReset))
	case string:
		return fmt.Sprintf("%s%q%s", GetColorCode(ColorGreen), v, GetColorCode(ColorReset))
	case PythonicBool:
		if v {
			return fmt.Sprintf("%sTrue%s", GetColorCode(ColorYellow), GetColorCode(ColorReset))
		}
		return fmt.Sprintf("%sFalse%s", GetColorCode(ColorYellow), GetColorCode(ColorReset))
	case PythonicNone:
		return fmt.Sprintf("%sNone%s", GetColorCode(ColorYellow), GetColorCode(ColorReset))
	case *Lambda, BuiltinFunc:
		return fmt.Sprintf("%s#<function>%s", GetColorCode(ColorBlue), GetColorCode(ColorReset))
	default:
		return PrintValue(val)
	}
}

// Initialize color settings
func init() {
	// Auto-detect whether to use colors by default
	ColorEnabled = IsTerminal()
	ColoredErrors = ColorEnabled

	// Update the colored prompt
	ColoredPrompt = ColorBrightPurple + "m28> " + ColorReset
}

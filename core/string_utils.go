package core

import (
	"strings"
	"unicode/utf8"
)

// JoinStrings joins a slice of strings with a separator
func JoinStrings(strs []string, sep string) string {
	return strings.Join(strs, sep)
}

// String utility functions for use in adapters
func SplitString(s string, sep string) []string {
	return strings.Split(s, sep)
}

func TrimString(s string, cutset string) string {
	return strings.Trim(s, cutset)
}

func TrimSpaceString(s string) string {
	return strings.TrimSpace(s)
}

func ToUpperString(s string) string {
	return strings.ToUpper(s)
}

func ToLowerString(s string) string {
	return strings.ToLower(s)
}

func ContainsString(s string, substr string) bool {
	return strings.Contains(s, substr)
}

func HasPrefixString(s string, prefix string) bool {
	return strings.HasPrefix(s, prefix)
}

func HasSuffixString(s string, suffix string) bool {
	return strings.HasSuffix(s, suffix)
}

func ReplaceString(s string, old string, new string) string {
	return strings.Replace(s, old, new, -1)
}

func ReplaceAllString(s string, old string, new string) string {
	return strings.ReplaceAll(s, old, new)
}

// ProcessEscapeSequences replaces escape sequences in a string with their actual characters
// Currently supports:
// - \n - newline
// - \t - tab
// - \r - carriage return
// - \\ - backslash
// - \" - double quote
// - \' - single quote
func ProcessEscapeSequences(s string) string {
	var result strings.Builder
	i := 0
	for i < len(s) {
		if s[i] == '\\' && i+1 < len(s) {
			switch s[i+1] {
			case 'n':
				result.WriteRune('\n')
			case 't':
				result.WriteRune('\t')
			case 'r':
				result.WriteRune('\r')
			case '\\':
				result.WriteRune('\\')
			case '"':
				result.WriteRune('"')
			case '\'':
				result.WriteRune('\'')
			default:
				// Unrecognized escape sequence, keep the backslash and the character
				result.WriteRune('\\')
				r, width := utf8.DecodeRuneInString(s[i+1:])
				result.WriteRune(r)
				i += width - 1 // Adjust for the extra rune (we'll still add 2 below)
			}
			i += 2 // Skip the escape sequence
		} else if i < len(s) {
			r, width := utf8.DecodeRuneInString(s[i:])
			result.WriteRune(r)
			i += width
		} else {
			break
		}
	}
	return result.String()
}

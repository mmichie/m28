package core

import "strings"

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

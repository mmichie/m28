package m28

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

var (
	commentRegex = regexp.MustCompile(`(?m)^;.*$|;.*`)
	tokenRegex   = regexp.MustCompile(`(\(|\)|'|"(?:[^"\\]|\\.)*"|-?[0-9]*\.?[0-9]+|[^\s()]+)`)
)

func tokenize(input string) []string {
	// Remove all comments
	noComments := commentRegex.ReplaceAllString(input, "")

	// Then tokenize the remaining input
	tokens := tokenRegex.FindAllString(noComments, -1)

	var filteredTokens []string
	for _, token := range tokens {
		trimmed := strings.TrimSpace(token)
		if trimmed != "" {
			filteredTokens = append(filteredTokens, trimmed)
		}
	}
	return filteredTokens
}

func parse(tokens []string, index int) (LispValue, int, error) {
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("unexpected EOF")
	}

	token := tokens[index]
	index++

	switch token {
	case "(":
		return parseList(tokens, index)
	case ")":
		return nil, index, fmt.Errorf("unexpected closing parenthesis")
	case "'":
		return parseQuote(tokens, index)
	default:
		return parseAtom(token), index, nil
	}
}

func parseList(tokens []string, index int) (LispValue, int, error) {
	var list LispList
	for index < len(tokens) && tokens[index] != ")" {
		val, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		list = append(list, val)
		index = newIndex
	}
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("missing closing parenthesis")
	}
	return list, index + 1, nil // consume the closing parenthesis
}

func parseQuote(tokens []string, index int) (LispValue, int, error) {
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("unexpected EOF after quote")
	}
	quoted, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return LispList{LispSymbol("quote"), quoted}, newIndex, nil
}

func parseAtom(token string) LispValue {
	// Check if it's a boolean literal
	if token == "#f" {
		return false
	}
	if token == "#t" {
		return true
	}

	// Check if it's a string literal
	if strings.HasPrefix(token, "\"") && strings.HasSuffix(token, "\"") {
		unquoted, err := strconv.Unquote(token)
		if err == nil {
			return unquoted
		}
	}

	// Check if it's a number
	if num, err := strconv.ParseFloat(token, 64); err == nil {
		return num
	}

	// If it's not a boolean, string, or number, it's a symbol
	return LispSymbol(token)
}

package m28

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

func tokenize(input string) []string {
	// Remove all comments
	noComments := regexp.MustCompile(`(?m)^;.*$|;.*`).ReplaceAllString(input, "")

	// Then tokenize the remaining input
	re := regexp.MustCompile(`(\(|\)|'|"(?:[^"\\]|\\.)*"|-?[0-9]*\.?[0-9]+|[^\s()]+)`)
	tokens := re.FindAllString(noComments, -1)

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
	case ")":
		return nil, index, fmt.Errorf("unexpected closing parenthesis")
	case "'":
		if index >= len(tokens) {
			return nil, index, fmt.Errorf("unexpected EOF after quote")
		}
		quoted, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		return LispList{LispSymbol("quote"), quoted}, newIndex, nil
	default:
		atom, err := parseAtom(token)
		return atom, index, err
	}
}

func parseAtom(token string) (LispValue, error) {
	// Check if it's a boolean literal
	if token == "#f" {
		return false, nil
	}
	if token == "#t" {
		return true, nil
	}
	// Check if it's a string literal
	if strings.HasPrefix(token, "\"") && strings.HasSuffix(token, "\"") {
		unquoted, err := strconv.Unquote(token)
		if err != nil {
			return nil, fmt.Errorf("error parsing string literal: %v", err)
		}
		return unquoted, nil
	}
	// Check if it's a number
	if num, err := strconv.ParseFloat(token, 64); err == nil {
		return num, nil
	}
	// If it's not a boolean, string, or number, it's a symbol
	return LispSymbol(token), nil
}

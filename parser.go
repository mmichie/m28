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
	re := regexp.MustCompile(`(\(|\)|\[|\]|'|` + "`" + `|,@|,|"(?:[^"\\]|\\.)*"|-?[0-9]*\.?[0-9]+|[^\s()[\]'` + "`" + `,]+)`)
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
	case "(", "[":
		return parseList(tokens, index)
	case ")", "]":
		return nil, index, fmt.Errorf("unexpected closing parenthesis or bracket")
	case "'":
		return parseQuote(tokens, index)
	case "`":
		return parseQuasiquote(tokens, index)
	case ",":
		return parseUnquote(tokens, index)
	case ",@":
		return parseUnquoteSplicing(tokens, index)
	default:
		return parseAtom(token), index, nil
	}
}

func parseList(tokens []string, index int) (LispValue, int, error) {
	var list LispList
	for index < len(tokens) && tokens[index] != ")" && tokens[index] != "]" {
		val, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		list = append(list, val)
		index = newIndex
	}
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("missing closing parenthesis or bracket")
	}
	return list, index + 1, nil // consume the closing parenthesis or bracket
}

func parseQuote(tokens []string, index int) (LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return LispList{LispSymbol("quote"), expr}, newIndex, nil
}

func parseQuasiquote(tokens []string, index int) (LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return Quasiquote{Expr: expr}, newIndex, nil
}

func parseUnquote(tokens []string, index int) (LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return Unquote{Expr: expr}, newIndex, nil
}

func parseUnquoteSplicing(tokens []string, index int) (LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return UnquoteSplicing{Expr: expr}, newIndex, nil
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

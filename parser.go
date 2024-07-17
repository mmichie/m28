package m28

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

func tokenize(input string) []string {
	noComments := removeComments(input)
	rawTokens := splitIntoRawTokens(noComments)
	return filterAndTrimTokens(rawTokens)
}

func removeComments(input string) string {
	commentRegex := regexp.MustCompile(`(?m)^;.*$|;.*`)
	return commentRegex.ReplaceAllString(input, "")
}

func splitIntoRawTokens(input string) []string {
	tokenRegex := regexp.MustCompile(`(\(|\)|\[|\]|'|` + "`" + `|,@|,|"(?:[^"\\]|\\.)*"|-?[0-9]*\.?[0-9]+|[^\s()[\]'` + "`" + `,]+)`)
	return tokenRegex.FindAllString(input, -1)
}

func filterAndTrimTokens(tokens []string) []string {
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
	if isBooleanLiteral(token) {
		return parseBooleanLiteral(token)
	}
	if isStringLiteral(token) {
		return parseStringLiteral(token)
	}
	if isNumeric(token) {
		return parseNumeric(token)
	}
	return LispSymbol(token)
}

func isBooleanLiteral(token string) bool {
	return token == "#f" || token == "#t"
}

func parseBooleanLiteral(token string) bool {
	return token == "#t"
}

func isStringLiteral(token string) bool {
	return strings.HasPrefix(token, "\"") && strings.HasSuffix(token, "\"")
}

func parseStringLiteral(token string) string {
	unquoted, err := strconv.Unquote(token)
	if err == nil {
		return unquoted
	}
	return token // Return the original token if unquoting fails
}

func isNumeric(token string) bool {
	_, err := strconv.ParseFloat(token, 64)
	return err == nil
}

func parseNumeric(token string) float64 {
	num, _ := strconv.ParseFloat(token, 64)
	return num
}

package m28

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
)

func tokenize(input string) []string {
	var tokens []string
	var currentToken strings.Builder
	inString := false
	escaped := false

	for _, char := range input {
		if inString {
			if escaped {
				currentToken.WriteRune(char)
				escaped = false
			} else if char == '\\' {
				escaped = true
				currentToken.WriteRune(char)
			} else if char == '"' {
				currentToken.WriteRune(char)
				tokens = append(tokens, currentToken.String())
				currentToken.Reset()
				inString = false
			} else {
				currentToken.WriteRune(char)
			}
		} else if char == '"' {
			if currentToken.Len() > 0 {
				tokens = append(tokens, currentToken.String())
				currentToken.Reset()
			}
			currentToken.WriteRune(char)
			inString = true
		} else if unicode.IsSpace(char) {
			if currentToken.Len() > 0 {
				tokens = append(tokens, currentToken.String())
				currentToken.Reset()
			}
		} else if char == '(' || char == ')' {
			if currentToken.Len() > 0 {
				tokens = append(tokens, currentToken.String())
				currentToken.Reset()
			}
			tokens = append(tokens, string(char))
		} else {
			currentToken.WriteRune(char)
		}
	}

	if currentToken.Len() > 0 {
		tokens = append(tokens, currentToken.String())
	}

	return tokens
}

func parse(tokens *[]string) (LispValue, error) {
	if len(*tokens) == 0 {
		return nil, fmt.Errorf("unexpected EOF")
	}
	token := (*tokens)[0]
	*tokens = (*tokens)[1:]

	switch {
	case token == "(":
		var list LispList
		for len(*tokens) > 0 && (*tokens)[0] != ")" {
			val, err := parse(tokens)
			if err != nil {
				return nil, err
			}
			list = append(list, val)
		}
		if len(*tokens) == 0 {
			return nil, fmt.Errorf("missing closing parenthesis")
		}
		*tokens = (*tokens)[1:] // consume the closing parenthesis
		return list, nil
	case token == ")":
		return nil, fmt.Errorf("unexpected closing parenthesis")
	case token == "'":
		quoted, err := parse(tokens)
		if err != nil {
			return nil, err
		}
		return LispList{LispSymbol("quote"), quoted}, nil
	default:
		return parseAtom(token)
	}
}

func parseAtom(token string) (LispValue, error) {
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

	// If it's not a string or number, it's a symbol
	return LispSymbol(token), nil
}

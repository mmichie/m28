package parser

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

type Parser struct{}

func NewParser() *Parser {
	return &Parser{}
}

func (p *Parser) Parse(input string) (core.LispValue, error) {
	tokens := tokenize(input)
	result, err := parseMultiple(tokens)
	if err != nil {
		return nil, err
	}
	return result, nil
}

func tokenize(input string) []string {
	input = removeComments(input)
	return splitIntoRawTokens(input)
}

func removeComments(input string) string {
	commentRegex := regexp.MustCompile(`(?m)^;.*$|;.*`)
	return commentRegex.ReplaceAllString(input, "")
}

func splitIntoRawTokens(input string) []string {
	tokenRegex := regexp.MustCompile(`(\(|\)|'|` + "`" + `|,@|,|"(?:[^"\\]|\\.)*"|-?[0-9]*\.?[0-9]+|[^\s()]+)`)
	return tokenRegex.FindAllString(input, -1)
}

func parseMultiple(tokens []string) (core.LispValue, error) {
	var expressions core.LispList
	index := 0
	for index < len(tokens) {
		expr, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, err
		}
		expressions = append(expressions, expr)
		index = newIndex
	}
	if len(expressions) == 1 {
		return expressions[0], nil
	}
	return expressions, nil
}

func parse(tokens []string, index int) (core.LispValue, int, error) {
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

func parseList(tokens []string, index int) (core.LispValue, int, error) {
	var list core.LispList
	for index < len(tokens) && tokens[index] != ")" {
		if tokens[index] == "." {
			// This is a dotted pair
			index++
			if index >= len(tokens) {
				return nil, index, fmt.Errorf("unexpected end of input after dot")
			}
			lastElem, newIndex, err := parse(tokens, index)
			if err != nil {
				return nil, newIndex, err
			}
			if newIndex >= len(tokens) || tokens[newIndex] != ")" {
				return nil, newIndex, fmt.Errorf("expected ) after dotted pair")
			}
			if len(list) == 0 {
				return nil, newIndex, fmt.Errorf("invalid dotted pair syntax")
			}
			return append(list, lastElem), newIndex + 1, nil
		}

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
	return list, index + 1, nil
}

func parseQuote(tokens []string, index int) (core.LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return core.LispList{core.LispSymbol("quote"), expr}, newIndex, nil
}

func parseQuasiquote(tokens []string, index int) (core.LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return core.Quasiquote{Expr: expr}, newIndex, nil
}

func parseUnquote(tokens []string, index int) (core.LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return core.Unquote{Expr: expr}, newIndex, nil
}

func parseUnquoteSplicing(tokens []string, index int) (core.LispValue, int, error) {
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	return core.UnquoteSplicing{Expr: expr}, newIndex, nil
}

func parseAtom(token string) core.LispValue {
	if num, err := strconv.ParseFloat(token, 64); err == nil {
		return num
	}
	if strings.HasPrefix(token, "\"") && strings.HasSuffix(token, "\"") {
		unquoted, err := strconv.Unquote(token)
		if err == nil {
			return unquoted
		}
	}
	if token == "nil" {
		return core.Nil{}
	}
	return core.LispSymbol(token)
}

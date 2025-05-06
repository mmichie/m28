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
	return processDotNotation(splitIntoRawTokens(input))
}

func removeComments(input string) string {
	// Process comments line-by-line to handle them more accurately
	var result strings.Builder
	lines := strings.Split(input, "\n")
	
	for _, line := range lines {
		// Find the first "#" that's not inside a string literal
		inString := false
		commentPos := -1
		
		for i, ch := range line {
			if ch == '"' && (i == 0 || line[i-1] != '\\') {
				inString = !inString
			} else if ch == '#' && !inString {
				commentPos = i
				break
			}
		}
		
		// If a comment was found, remove it
		if commentPos >= 0 {
			line = line[:commentPos]
		}
		
		result.WriteString(line)
		result.WriteString("\n")
	}
	
	return result.String()
}

func splitIntoRawTokens(input string) []string {
	// Updated regex to also recognize curly braces for dict literals, square brackets for list literals, and commas
	tokenRegex := regexp.MustCompile(`(\(|\)|{|}|\[|\]|'|` + "`" + `|,@|,|:|"(?:[^"\\]|\\.)*"|-?[0-9]*\.?[0-9]+|[^\s(){}[\],:]+)`)
	return tokenRegex.FindAllString(input, -1)
}

// processDotNotation transforms dot notation tokens into lisp expressions
// For example: "object.method" becomes ["(" "." "object" "\"method\"" ")"]
func processDotNotation(tokens []string) []string {
	var result []string
	
	for _, token := range tokens {
		// Skip tokens that are not symbols or don't contain dots
		if token == "." || len(token) <= 1 || !strings.Contains(token, ".") {
			result = append(result, token)
			continue
		}
		
		// Don't process numeric literals with decimals (like 3.14)
		if _, err := strconv.ParseFloat(token, 64); err == nil {
			result = append(result, token)
			continue
		}
		
		// Don't process special symbols starting with dot (like .5)
		if token[0] == '.' {
			if len(token) > 1 && token[1] >= '0' && token[1] <= '9' {
				result = append(result, token)
				continue
			}
		}
		
		// Split on dots
		parts := strings.Split(token, ".")
		if len(parts) < 2 {
			result = append(result, token)
			continue
		}
		
		// Build a nested expression for the dot notation
		// For example: obj.prop.method becomes (. (. obj "prop") "method")
		
		// Start with the object name
		expr := parts[0]
		
		// For each property/method, wrap in a dot expression
		for i := 1; i < len(parts); i++ {
			// Create (. expr "part")
			expr = "( . " + expr + " \"" + parts[i] + "\" )"
		}
		
		// Split the resulting expression into tokens
		exprTokens := splitIntoRawTokens(expr)
		result = append(result, exprTokens...)
	}
	
	return result
}

func parseMultiple(tokens []string) (core.LispValue, error) {
	var expressions core.LispList
	index := 0
	
	// Skip whitespace and empty tokens
	for index < len(tokens) {
		if tokens[index] == "" || strings.TrimSpace(tokens[index]) == "" {
			index++
			continue
		}
		
		expr, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, fmt.Errorf("parse error at token %d (%s): %v", index, tokens[index], err)
		}
		expressions = append(expressions, expr)
		index = newIndex
	}
	
	// If we have exactly one expression, return just that
	if len(expressions) == 1 {
		return expressions[0], nil
	}
	
	// If empty, return an empty list
	if len(expressions) == 0 {
		return core.LispList{}, nil
	}
	
	// Otherwise return multiple expressions
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
		// Check if we might have a tuple by looking ahead for commas

		// Special case for empty tuple/list: "()"
		if index < len(tokens) && tokens[index] == ")" {
			// Empty list
			return core.LispList{}, index + 1, nil
		}

		// Check if there's a comma somewhere before the closing parenthesis
		isTuple := false
		searchIndex := index
		parenDepth := 0
		for searchIndex < len(tokens) {
			if tokens[searchIndex] == ")" && parenDepth == 0 {
				break
			} else if tokens[searchIndex] == "(" {
				parenDepth++
			} else if tokens[searchIndex] == ")" {
				parenDepth--
			} else if tokens[searchIndex] == "," && parenDepth == 0 {
				isTuple = true
				break
			}
			searchIndex++
		}

		if isTuple {
			return parseTuple(tokens, index)
		}

		// If no comma found at this level, parse as a regular list
		return parseList(tokens, index)
	case ")":
		return nil, index, fmt.Errorf("unexpected closing parenthesis")
	case "{":
		return parseDict(tokens, index)
	case "}":
		return nil, index, fmt.Errorf("unexpected closing curly brace")
	case "[":
		return parseListLiteral(tokens, index)
	case "]":
		return nil, index, fmt.Errorf("unexpected closing square bracket")
	case "'":
		return parseQuote(tokens, index)
	case "`":
		return parseQuasiquote(tokens, index)
	case ",":
		return parseUnquote(tokens, index)
	case ",@":
		return parseUnquoteSplicing(tokens, index)
	case ".":
		// Handle dot notation: object.method transforms to (. object method)
		if index >= len(tokens) {
			return nil, index, fmt.Errorf("unexpected end of input after dot operator")
		}
		
		// Get the attribute name
		attrName, ok := parseAtom(tokens[index]).(core.LispSymbol)
		if !ok {
			return nil, index, fmt.Errorf("expected attribute name after dot, got %v", tokens[index])
		}
		index++
		
		// If we're at the end or the next token is not an open parenthesis,
		// this is an attribute access, not a method call
		if index >= len(tokens) || tokens[index] != "(" {
			// Previous token should be the object
			return core.LispList{
				core.LispSymbol("."),
				core.LispList{core.LispSymbol("quote"), attrName},
			}, index, nil
		}
		
		// Otherwise, it's a method call with arguments
		// Parse the argument list
		argList, newIndex, err := parseList(tokens, index+1) // Skip the opening parenthesis
		if err != nil {
			return nil, newIndex, err
		}
		
		// Create the method call expression: (. object method args...)
		methodCall := core.LispList{
			core.LispSymbol("."),
			core.LispList{core.LispSymbol("quote"), attrName},
		}
		
		// Add arguments
		for _, arg := range argList.(core.LispList) {
			methodCall = append(methodCall, arg)
		}
		
		return methodCall, newIndex, nil
	default:
		return parseAtom(token), index, nil
	}
}

func parseList(tokens []string, index int) (core.LispValue, int, error) {
	var list core.LispList
	startIndex := index
	numOpenParens := 1  // We start with one open parenthesis
	
	for index < len(tokens) && tokens[index] != ")" {
		if tokens[index] == "(" {
			numOpenParens++
		}
		
		if tokens[index] == "." {
			// This is a dotted pair
			index++
			if index >= len(tokens) {
				return nil, index, fmt.Errorf("unexpected end of input after dot in list starting at token %d", startIndex)
			}
			lastElem, newIndex, err := parse(tokens, index)
			if err != nil {
				return nil, newIndex, err
			}
			if newIndex >= len(tokens) || tokens[newIndex] != ")" {
				return nil, newIndex, fmt.Errorf("expected ) after dotted pair in list starting at token %d", startIndex)
			}
			if len(list) == 0 {
				return nil, newIndex, fmt.Errorf("invalid dotted pair syntax in list starting at token %d", startIndex)
			}
			return append(list, lastElem), newIndex + 1, nil
		}

		val, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, newIndex, fmt.Errorf("error parsing list element at token %d: %v", index, err)
		}
		list = append(list, val)
		index = newIndex
	}
	
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("missing closing parenthesis for list starting at token %d (expected %d closing parentheses)", 
			startIndex, numOpenParens)
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

// parseDict parses a Python-style dictionary literal: {"key": value, ...}
func parseDict(tokens []string, index int) (core.LispValue, int, error) {
	dict := core.NewPythonicDict()

	// Handle empty dict
	if index < len(tokens) && tokens[index] == "}" {
		return dict, index + 1, nil
	}

	for index < len(tokens) && tokens[index] != "}" {
		// Parse key
		key, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		index = newIndex

		// Expect colon
		if index >= len(tokens) || tokens[index] != ":" {
			return nil, index, fmt.Errorf("expected ':' after dictionary key")
		}
		index++ // Skip colon

		// Parse value
		value, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		index = newIndex

		// Add key-value pair to dict
		dict.Set(key, value)

		// Skip comma if present
		if index < len(tokens) && tokens[index] == "," {
			index++
		}
	}

	if index >= len(tokens) {
		return nil, index, fmt.Errorf("missing closing curly brace for dictionary")
	}

	return dict, index + 1, nil
}

// parseListLiteral parses a Python-style list literal: [1 2 3 ...] or a list comprehension: [expr for var in iterable if condition]
func parseListLiteral(tokens []string, index int) (core.LispValue, int, error) {
	// Handle empty list
	if index < len(tokens) && tokens[index] == "]" {
		return core.LispListLiteral{}, index + 1, nil
	}

	// Check if this might be a list comprehension
	// First, save the starting position to backtrack if needed
	startIndex := index

	// Parse the first expression
	expr, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	index = newIndex

	// Look for the "for" keyword that indicates a list comprehension
	isComprehension := false
	if index < len(tokens) && tokens[index] == "for" {
		isComprehension = true
	}

	// If it's not a comprehension, parse as a regular list literal
	if !isComprehension {
		// Reset index to the beginning and parse as a normal list
		index = startIndex
		var list core.LispListLiteral

		for index < len(tokens) && tokens[index] != "]" {
			// Skip commas between elements if present
			if tokens[index] == "," {
				index++
				continue
			}

			// Parse list element
			value, newIndex, err := parse(tokens, index)
			if err != nil {
				return nil, newIndex, err
			}
			index = newIndex

			// Add value to list
			list = append(list, value)
		}

		if index >= len(tokens) {
			return nil, index, fmt.Errorf("missing closing square bracket for list")
		}

		return list, index + 1, nil
	}

	// Parse the list comprehension

	// Skip "for" keyword
	index++

	// Parse variable name
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("expected variable name after 'for' in list comprehension")
	}

	varName, ok := parseAtom(tokens[index]).(core.LispSymbol)
	if !ok {
		return nil, index, fmt.Errorf("expected symbol as variable name in list comprehension, got %v", tokens[index])
	}
	index++

	// The "in" keyword is optional in our syntax, but we'll skip it if it's there for compatibility
	if index < len(tokens) && tokens[index] == "in" {
		index++
	}

	// Parse iterable expression
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("expected iterable expression in list comprehension")
	}

	iterable, newIndex, err := parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	index = newIndex

	// Check for optional "if" condition
	var condition core.LispValue = nil
	if index < len(tokens) && tokens[index] == "if" {
		index++
		if index >= len(tokens) {
			return nil, index, fmt.Errorf("expected condition after 'if' in list comprehension")
		}

		condition, newIndex, err = parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		index = newIndex
	}

	// Expect closing bracket
	if index >= len(tokens) || tokens[index] != "]" {
		return nil, index, fmt.Errorf("missing closing square bracket for list comprehension")
	}
	index++

	// Create the comprehension
	comprehension := core.LispComprehension{
		Expression: expr,
		Variable:   varName,
		Iterable:   iterable,
		Condition:  condition,
	}

	return comprehension, index, nil
}

// parseTuple parses a tuple of the form (elem1, elem2, ..., elemN)
func parseTuple(tokens []string, index int) (core.LispValue, int, error) {
	var elements []core.LispValue
	hasComma := false

	// Parse all elements until closing parenthesis
	for index < len(tokens) && tokens[index] != ")" {
		// Skip commas between elements
		if tokens[index] == "," {
			hasComma = true
			index++
			continue
		}

		// Parse tuple element
		value, newIndex, err := parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		elements = append(elements, value)
		index = newIndex
	}

	if index >= len(tokens) {
		return nil, index, fmt.Errorf("missing closing parenthesis for tuple")
	}

	// Handle special case for single-element tuple: (x,)
	// It must have a comma to be a tuple, otherwise it's just a parenthesized expression
	if len(elements) == 1 && !hasComma {
		// If only one element and no comma, it's not a tuple but a parenthesized expression
		return elements[0], index + 1, nil
	}

	// Skip closing parenthesis
	index++

	return core.LispTuple(elements), index, nil
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
	switch token {
	case "True":
		return core.PythonicBool(true)
	case "False":
		return core.PythonicBool(false)
	case "None":
		return core.PythonicNone{}
	}
	return core.LispSymbol(token)
}

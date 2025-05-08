package parser

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/mmichie/m28/core"
)

// Token represents a token with its source location
type Token struct {
	Value  string
	Line   int
	Column int
}

// Parser represents a parser for the M28 language
type Parser struct {
	Filename string // Current file being parsed
}

func NewParser() *Parser {
	return &Parser{}
}

// SetFilename sets the current filename for error reporting
func (p *Parser) SetFilename(filename string) {
	p.Filename = filename
}

func (p *Parser) Parse(input string) (core.LispValue, error) {
	tokens, err := tokenize(input)
	if err != nil {
		return nil, err
	}
	result, err := p.parseMultiple(tokens)
	if err != nil {
		return nil, err
	}
	return result, nil
}

func tokenize(input string) ([]Token, error) {
	cleanedInput, lineMap := removeComments(input)
	rawTokens := splitIntoRawTokens(cleanedInput)

	// Compute line and column for each token
	tokens := make([]Token, 0, len(rawTokens))
	for _, match := range rawTokens {
		line := lineMap[match.StartPos]
		token := Token{
			Value:  match.Value,
			Line:   line,
			Column: match.Column,
		}
		tokens = append(tokens, token)
	}

	// Enable dot notation processing
	return processDotNotation(tokens)
}

// removeComments removes comments and returns the cleaned input and a mapping from character positions to line numbers
func removeComments(input string) (string, map[int]int) {
	// Process comments line-by-line to handle them more accurately
	var result strings.Builder
	lines := strings.Split(input, "\n")
	lineMap := make(map[int]int) // Maps character positions to line numbers

	charPos := 0
	for lineNum, line := range lines {
		// Find the first "#" that's not inside a string literal
		inString := false
		commentPos := -1

		for i, ch := range line {
			lineMap[charPos+i] = lineNum + 1 // Store line mapping (1-based line numbers)

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
		charPos += len(line) + 1 // +1 for the newline
	}

	return result.String(), lineMap
}

type TokenMatch struct {
	Value    string
	StartPos int
	Column   int
}

func splitIntoRawTokens(input string) []TokenMatch {
	// Updated regex to also recognize curly braces for dict literals, square brackets for list literals, and commas
	tokenRegex := regexp.MustCompile(`(\(|\)|{|}|\[|\]|'|` + "`" + `|,@|,|:|"(?:[^"\\]|\\.)*"|-?[0-9]*\.?[0-9]+|[^\s(){}[\],:]+)`)

	matches := tokenRegex.FindAllStringSubmatchIndex(input, -1)
	results := make([]TokenMatch, len(matches))

	for i, match := range matches {
		startPos := match[0]
		value := input[match[0]:match[1]]

		// Calculate column by finding the last newline before this token
		lastNewline := strings.LastIndex(input[:startPos], "\n")
		column := 0
		if lastNewline >= 0 {
			column = startPos - lastNewline
		} else {
			column = startPos + 1 // If no newline, column is position + 1
		}

		results[i] = TokenMatch{
			Value:    value,
			StartPos: startPos,
			Column:   column,
		}
	}

	return results
}

// Helper function to check if a token is a numeric literal
func isNumericLiteral(token string) bool {
	// Check if it's a valid float (handles cases like 3.14)
	if _, err := strconv.ParseFloat(token, 64); err == nil {
		return true
	}

	// Check if it starts with a dot followed by a digit (like .5)
	if token[0] == '.' && len(token) > 1 && token[1] >= '0' && token[1] <= '9' {
		return true
	}

	return false
}

// processDotNotation transforms dot notation tokens into lisp expressions
// For example: "object.method" becomes tokens representing ["(" "." "object" "\"method\"" ")"]
// Also handles nested properties like object.prop.method
func processDotNotation(tokens []Token) ([]Token, error) {
	var result []Token

	for i := 0; i < len(tokens); i++ {
		token := tokens[i]

		// Skip tokens that are string literals (enclosed in quotes)
		if strings.HasPrefix(token.Value, "\"") && strings.HasSuffix(token.Value, "\"") {
			result = append(result, token)
			continue
		}

		// Skip tokens that are not symbols or don't contain dots
		if token.Value == "." || len(token.Value) <= 1 || !strings.Contains(token.Value, ".") {
			result = append(result, token)
			continue
		}

		// Don't process numeric literals with decimals
		if isNumericLiteral(token.Value) {
			result = append(result, token)
			continue
		}

		// Split on dots
		parts := strings.Split(token.Value, ".")
		if len(parts) < 2 {
			result = append(result, token)
			continue
		}

		// Handle nested property access recursively
		line, column := token.Line, token.Column

		// Create tokens for: (. object "method")
		openParen := Token{Value: "(", Line: line, Column: column}
		dotToken := Token{Value: ".", Line: line, Column: column + 1}
		objToken := Token{Value: parts[0], Line: line, Column: column + 2}

		// Handle nested property access recursively
		if len(parts) == 2 {
			// Simple case: object.method becomes (. object "method")
			methodToken := Token{Value: "\"" + parts[1] + "\"", Line: line, Column: column + 2 + len(parts[0]) + 1}
			closeParen := Token{Value: ")", Line: line, Column: column + 2 + len(parts[0]) + 1 + len(parts[1]) + 2}

			result = append(result, openParen, dotToken, objToken, methodToken, closeParen)
		} else {
			// Complex case: object.prop.method
			// First handle object.prop
			propToken := Token{Value: "\"" + parts[1] + "\"", Line: line, Column: column + 2 + len(parts[0]) + 1}
			tempCloseParen := Token{Value: ")", Line: line, Column: column + 2 + len(parts[0]) + 1 + len(parts[1]) + 2}

			// Add initial tokens
			tempResult := []Token{openParen, dotToken, objToken, propToken, tempCloseParen}

			// Then handle the rest of the chain
			for j := 2; j < len(parts); j++ {
				// Create a new dot expression with the previous expression
				// Update column positions based on the previous tokens
				prevOpenParen := openParen
				prevDotToken := Token{Value: ".", Line: line, Column: prevOpenParen.Column + 1}

				// For the object, we need to create a nested expression
				nestedOpenParen := Token{Value: "(", Line: line, Column: prevDotToken.Column + 1}

				// Add the tokens for the nested expression
				innerExpr := append([]Token{}, tempResult...)

				// Nested closing paren
				nestedCloseParen := Token{Value: ")", Line: line, Column: innerExpr[len(innerExpr)-1].Column + 1}

				// The property token
				propToken := Token{Value: "\"" + parts[j] + "\"", Line: line, Column: nestedCloseParen.Column + 1}

				// Final closing paren
				outerCloseParen := Token{Value: ")", Line: line, Column: propToken.Column + len(parts[j]) + 2}

				// Update tempResult for next iteration
				tempResult = []Token{
					prevOpenParen, prevDotToken, nestedOpenParen,
				}
				tempResult = append(tempResult, innerExpr...)
				tempResult = append(tempResult, nestedCloseParen, propToken, outerCloseParen)
			}

			// Add the final expression to the result
			result = append(result, tempResult...)
		}
	}

	return result, nil
}

func (p *Parser) parseMultiple(tokens []Token) (core.LispValue, error) {
	var expressions core.LispList
	index := 0

	// Skip whitespace and empty tokens
	for index < len(tokens) {
		if tokens[index].Value == "" || strings.TrimSpace(tokens[index].Value) == "" {
			index++
			continue
		}

		expr, newIndex, err := p.parse(tokens, index)
		if err != nil {
			token := tokens[index]
			location := core.Location{
				Filename: p.Filename,
				Line:     token.Line,
				Column:   token.Column,
			}
			return nil, fmt.Errorf("parse error at %s (%s): %v", location.String(), token.Value, err)
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

func (p *Parser) parse(tokens []Token, index int) (core.LispValue, int, error) {
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("unexpected EOF")
	}

	token := tokens[index]
	index++

	// Get source location for this token
	location := core.Location{
		Filename: p.Filename,
		Line:     token.Line,
		Column:   token.Column,
	}

	switch token.Value {
	case "(":
		// Check if we might have a tuple by looking ahead for commas

		// Special case for empty tuple/list: "()"
		if index < len(tokens) && tokens[index].Value == ")" {
			// Empty list
			emptyList := core.LispList{}
			// Wrap in LocatedValue
			return core.LocatedValue{Value: emptyList, Location: location}, index + 1, nil
		}

		// Check if there's a comma somewhere before the closing parenthesis
		isTuple := false
		searchIndex := index
		parenDepth := 0
		for searchIndex < len(tokens) {
			if tokens[searchIndex].Value == ")" && parenDepth == 0 {
				break
			} else if tokens[searchIndex].Value == "(" {
				parenDepth++
			} else if tokens[searchIndex].Value == ")" {
				parenDepth--
			} else if tokens[searchIndex].Value == "," && parenDepth == 0 {
				isTuple = true
				break
			}
			searchIndex++
		}

		if isTuple {
			return p.parseTuple(tokens, index, location)
		}

		// Check if this is a dot notation call
		// Format: (. object property-or-method [args...])
		if index < len(tokens) && tokens[index].Value == "." {
			// Skip the dot token
			index++

			// This is a dot notation call
			// Parse the object expression
			if index >= len(tokens) {
				return nil, index, fmt.Errorf("unexpected end of input after dot operator")
			}

			obj, newIndex, err := p.parse(tokens, index)
			if err != nil {
				return nil, newIndex, err
			}
			index = newIndex

			// Parse the property/method name
			if index >= len(tokens) {
				return nil, index, fmt.Errorf("unexpected end of input after object in dot notation")
			}

			// The property name could be a string literal or a symbol
			propName, newIndex, err := p.parse(tokens, index)
			if err != nil {
				return nil, newIndex, err
			}
			index = newIndex

			// Create the base dot expression
			dotExpr := core.LispList{core.LispSymbol("."), obj, propName}

			// Parse any arguments (for method calls)
			// Collect all expressions until the closing parenthesis
			for index < len(tokens) && tokens[index].Value != ")" {
				arg, newIndex, err := p.parse(tokens, index)
				if err != nil {
					return nil, newIndex, err
				}
				dotExpr = append(dotExpr, arg)
				index = newIndex
			}

			// Skip the closing parenthesis
			if index < len(tokens) && tokens[index].Value == ")" {
				index++
			} else {
				return nil, index, fmt.Errorf("missing closing parenthesis for dot notation")
			}

			// Wrap the expression with location information
			return core.LocatedValue{Value: dotExpr, Location: location}, index, nil
		}

		// If no comma found at this level, parse as a regular list
		return p.parseList(tokens, index, location)

	case ")":
		return nil, index, fmt.Errorf("unexpected closing parenthesis")
	case "{":
		return p.parseDict(tokens, index, location)
	case "}":
		return nil, index, fmt.Errorf("unexpected closing curly brace")
	case "[":
		return p.parseListLiteral(tokens, index, location)
	case "]":
		return nil, index, fmt.Errorf("unexpected closing square bracket")
	case "'":
		return p.parseQuote(tokens, index, location)
	case "`":
		return p.parseQuasiquote(tokens, index, location)
	case ",":
		return p.parseUnquote(tokens, index, location)
	case ",@":
		return p.parseUnquoteSplicing(tokens, index, location)
	case ".":
		// When dot appears outside of a list, it's likely a syntax error
		// or part of a dot notation that's been broken up during tokenization
		return nil, index, fmt.Errorf("unexpected dot operator outside of list context")
	default:
		// Parse the atom with location information
		atom := p.parseAtom(token.Value, location)
		return atom, index, nil
	}
}

func (p *Parser) parseList(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	var list core.LispList
	numOpenParens := 1 // We start with one open parenthesis

	for index < len(tokens) && tokens[index].Value != ")" {
		if tokens[index].Value == "(" {
			numOpenParens++
		}

		// Check if the current token is '.' and it's being used as a dot operator or dotted pair
		if tokens[index].Value == "." {
			// Check the context to determine if this is a dot notation or a dotted pair
			if len(list) == 1 && index > 0 && tokens[index-1].Value == "(" {
				// This is a dot notation call - the list only has "." as the first element
				// Do nothing special, proceed to normal processing which adds the dot as a symbol
			} else if len(list) > 0 {
				// This is a dotted pair (traditional Lisp syntax)
				index++
				if index >= len(tokens) {
					return nil, index, fmt.Errorf("unexpected end of input after dot in list")
				}
				lastElem, newIndex, err := p.parse(tokens, index)
				if err != nil {
					return nil, newIndex, err
				}
				if newIndex >= len(tokens) || tokens[newIndex].Value != ")" {
					return nil, newIndex, fmt.Errorf("expected ) after dotted pair in list")
				}
				if len(list) == 0 {
					return nil, newIndex, fmt.Errorf("invalid dotted pair syntax in list")
				}
				result := append(list, lastElem)
				// Wrap the list with location information
				return core.LocatedValue{Value: result, Location: location}, newIndex + 1, nil
			}
		}

		val, newIndex, err := p.parse(tokens, index)
		if err != nil {
			return nil, newIndex, fmt.Errorf("error parsing list element: %v", err)
		}
		list = append(list, val)
		index = newIndex
	}

	if index >= len(tokens) {
		return nil, index, fmt.Errorf("missing closing parenthesis for list (expected %d closing parentheses)",
			numOpenParens)
	}

	// Wrap the list with location information
	return core.LocatedValue{Value: list, Location: location}, index + 1, nil
}

func (p *Parser) parseQuote(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	expr, newIndex, err := p.parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	quotedExpr := core.LispList{core.LispSymbol("quote"), expr}
	return core.LocatedValue{Value: quotedExpr, Location: location}, newIndex, nil
}

func (p *Parser) parseQuasiquote(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	expr, newIndex, err := p.parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	qqExpr := core.Quasiquote{Expr: expr}
	return core.LocatedValue{Value: qqExpr, Location: location}, newIndex, nil
}

func (p *Parser) parseUnquote(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	expr, newIndex, err := p.parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	unquoteExpr := core.Unquote{Expr: expr}
	return core.LocatedValue{Value: unquoteExpr, Location: location}, newIndex, nil
}

func (p *Parser) parseUnquoteSplicing(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	expr, newIndex, err := p.parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	spliceExpr := core.UnquoteSplicing{Expr: expr}
	return core.LocatedValue{Value: spliceExpr, Location: location}, newIndex, nil
}

// parseDict parses a Python-style dictionary literal: {"key": value, ...}
func (p *Parser) parseDict(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	dict := core.NewPythonicDict()

	// Handle empty dict
	if index < len(tokens) && tokens[index].Value == "}" {
		return core.LocatedValue{Value: dict, Location: location}, index + 1, nil
	}

	for index < len(tokens) && tokens[index].Value != "}" {
		// Parse key
		key, newIndex, err := p.parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		index = newIndex

		// Expect colon
		if index >= len(tokens) || tokens[index].Value != ":" {
			return nil, index, fmt.Errorf("expected ':' after dictionary key")
		}
		index++ // Skip colon

		// Parse value
		value, newIndex, err := p.parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		index = newIndex

		// Add key-value pair to dict
		dict.Set(key, value)

		// Skip comma if present
		if index < len(tokens) && tokens[index].Value == "," {
			index++
		}
	}

	if index >= len(tokens) {
		return nil, index, fmt.Errorf("missing closing curly brace for dictionary")
	}

	return core.LocatedValue{Value: dict, Location: location}, index + 1, nil
}

// parseListLiteral parses a Python-style list literal: [1 2 3 ...] or a list comprehension: [expr for var in iterable if condition]
func (p *Parser) parseListLiteral(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	// Handle empty list
	if index < len(tokens) && tokens[index].Value == "]" {
		emptyList := core.LispListLiteral{}
		return core.LocatedValue{Value: emptyList, Location: location}, index + 1, nil
	}

	// Check if this might be a list comprehension
	// First, save the starting position to backtrack if needed
	startIndex := index

	// Parse the first expression
	expr, newIndex, err := p.parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	index = newIndex

	// Look for the "for" keyword that indicates a list comprehension
	isComprehension := false
	if index < len(tokens) && tokens[index].Value == "for" {
		isComprehension = true
	}

	// If it's not a comprehension, parse as a regular list literal
	if !isComprehension {
		// Reset index to the beginning and parse as a normal list
		index = startIndex
		var list core.LispListLiteral

		for index < len(tokens) && tokens[index].Value != "]" {
			// Skip commas between elements if present
			if tokens[index].Value == "," {
				index++
				continue
			}

			// Parse list element
			value, newIndex, err := p.parse(tokens, index)
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

		return core.LocatedValue{Value: list, Location: location}, index + 1, nil
	}

	// Parse the list comprehension

	// Skip "for" keyword
	index++

	// Parse variable name
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("expected variable name after 'for' in list comprehension")
	}

	// Get token and create location info
	token := tokens[index]
	tokenLocation := core.Location{
		Filename: p.Filename,
		Line:     token.Line,
		Column:   token.Column,
	}

	// Parse with location information
	atomValue := p.parseAtom(token.Value, tokenLocation)

	// Extract symbol from LocatedValue if needed
	var varValue core.LispValue
	if located, isLocated := atomValue.(core.LocatedValue); isLocated {
		varValue = located.Value
	} else {
		varValue = atomValue
	}

	// Check if it's a symbol
	varName, ok := varValue.(core.LispSymbol)
	if !ok {
		return nil, index, fmt.Errorf("expected symbol as variable name in list comprehension, got %v", token.Value)
	}
	index++

	// The "in" keyword is optional in our syntax, but we'll skip it if it's there for compatibility
	if index < len(tokens) && tokens[index].Value == "in" {
		index++
	}

	// Parse iterable expression
	if index >= len(tokens) {
		return nil, index, fmt.Errorf("expected iterable expression in list comprehension")
	}

	iterable, newIndex, err := p.parse(tokens, index)
	if err != nil {
		return nil, newIndex, err
	}
	index = newIndex

	// Check for optional "if" condition
	var condition core.LispValue = nil
	if index < len(tokens) && tokens[index].Value == "if" {
		index++
		if index >= len(tokens) {
			return nil, index, fmt.Errorf("expected condition after 'if' in list comprehension")
		}

		condition, newIndex, err = p.parse(tokens, index)
		if err != nil {
			return nil, newIndex, err
		}
		index = newIndex
	}

	// Expect closing bracket
	if index >= len(tokens) || tokens[index].Value != "]" {
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

	return core.LocatedValue{Value: comprehension, Location: location}, index, nil
}

// parseTuple parses a tuple of the form (elem1, elem2, ..., elemN)
func (p *Parser) parseTuple(tokens []Token, index int, location core.Location) (core.LispValue, int, error) {
	var elements []core.LispValue
	hasComma := false

	// Parse all elements until closing parenthesis
	for index < len(tokens) && tokens[index].Value != ")" {
		// Skip commas between elements
		if tokens[index].Value == "," {
			hasComma = true
			index++
			continue
		}

		// Parse tuple element
		value, newIndex, err := p.parse(tokens, index)
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

	tupleValue := core.LispTuple(elements)
	return core.LocatedValue{Value: tupleValue, Location: location}, index, nil
}

func (p *Parser) parseAtom(token string, location core.Location) core.LispValue {
	var value core.LispValue

	if num, err := strconv.ParseFloat(token, 64); err == nil {
		value = num
	} else if strings.HasPrefix(token, "\"") && strings.HasSuffix(token, "\"") {
		unquoted, err := strconv.Unquote(token)
		if err == nil {
			// Successfully unquoted string becomes a string value
			value = unquoted
		} else {
			// If unquoting fails, use the raw token as a symbol
			// This should not happen with well-formed strings
			value = core.LispSymbol(token)
		}
	} else {
		switch token {
		case "True":
			value = core.PythonicBool(true)
		case "False":
			value = core.PythonicBool(false)
		case "None":
			value = core.PythonicNone{}
		default:
			value = core.LispSymbol(token)
		}
	}

	// Always wrap the value with location information
	return core.LocatedValue{
		Value:    value,
		Location: location,
	}
}

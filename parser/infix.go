package parser

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

// Operator precedence table (Python-compatible)
// Higher numbers = higher precedence
var operatorPrecedence = map[string]int{
	// Assignment (lowest precedence)
	"=":   1,
	"+=":  1,
	"-=":  1,
	"*=":  1,
	"/=":  1,
	"//=": 1,
	"%=":  1,
	"**=": 1,

	// Boolean OR
	"or": 4,

	// Boolean AND
	"and": 5,

	// Boolean NOT (unary)
	"not": 6,

	// Comparisons (all same precedence, non-associative)
	"in":     8,
	"not in": 8,
	"is":     8,
	"is not": 8,
	"<":      8,
	">":      8,
	"<=":     8,
	">=":     8,
	"!=":     8,
	"==":     8,

	// Bitwise OR
	"|": 10,

	// Bitwise XOR
	"^": 11,

	// Bitwise AND
	"&": 12,

	// Bitwise shifts
	"<<": 15,
	">>": 15,

	// Addition and subtraction
	"+": 20,
	"-": 20,

	// Multiplication, division, modulo
	"*":  30,
	"/":  30,
	"//": 30,
	"%":  30,

	// Exponentiation (highest precedence, right-associative)
	"**": 40,
}

// Right-associative operators
var rightAssociative = map[string]bool{
	"**":  true,
	"=":   true,
	"+=":  true,
	"-=":  true,
	"*=":  true,
	"/=":  true,
	"//=": true,
	"%=":  true,
	"**=": true,
}

// isInfixOperator checks if a symbol is an infix operator
func isInfixOperator(sym string) bool {
	_, exists := operatorPrecedence[sym]
	return exists
}

// isComparisonOperator checks if a symbol is a comparison operator
func isComparisonOperator(sym string) bool {
	return sym == "==" || sym == "!=" || sym == "<" || sym == ">" ||
		sym == "<=" || sym == ">=" || sym == "is" || sym == "is not" ||
		sym == "in" || sym == "not in"
}

// detectInfixPattern checks if elements form an infix pattern
// Returns true if second element is an infix operator
func detectInfixPattern(elements []core.Value) bool {
	if len(elements) < 3 {
		return false
	}

	// Check if second element is an operator symbol
	if sym, ok := elements[1].(core.SymbolValue); ok {
		return isInfixOperator(string(sym))
	}

	return false
}

// parseInfixExpression parses elements as an infix expression
// Uses Pratt parsing (operator precedence climbing)
func parseInfixExpression(elements []core.Value) (core.Value, error) {
	if len(elements) == 0 {
		return nil, &ParseError{Message: "empty infix expression"}
	}

	// Start parsing from the first element
	result, finalPos, err := parseInfixWithPrecedence(elements, 0, 0)
	if err != nil {
		return nil, err
	}

	// Check if we consumed all elements
	if finalPos < len(elements) {
		return nil, &ParseError{Message: "unexpected tokens after expression"}
	}

	return result, nil
}

// parseInfixWithPrecedence implements precedence climbing
// pos is the current position in the elements slice
// minPrec is the minimum precedence to consider
func parseInfixWithPrecedence(elements []core.Value, pos int, minPrec int) (core.Value, int, error) {
	if pos >= len(elements) {
		return nil, pos, &ParseError{Message: "unexpected end of expression"}
	}

	// Parse left operand (primary expression)
	left := elements[pos]
	pos++

	// Climb the precedence tree
	for pos < len(elements) {
		// Check if next element is an operator
		op, ok := elements[pos].(core.SymbolValue)
		if !ok {
			// Not an operator, we're done
			break
		}

		opStr := string(op)
		prec, isOp := operatorPrecedence[opStr]

		if !isOp || prec < minPrec {
			// Not an operator or precedence too low
			break
		}

		// Special handling for chained comparisons (Python-style)
		// a == b == c should become (and (== a b) (== b c))
		if isComparisonOperator(opStr) {
			return parseChainedComparison(elements, pos, left)
		}

		pos++ // consume operator

		if pos >= len(elements) {
			return nil, pos, &ParseError{Message: fmt.Sprintf("operator %s missing right operand", opStr), Line: 1, Col: 1}
		}

		// Determine next precedence level
		nextMinPrec := prec + 1
		if rightAssociative[opStr] {
			nextMinPrec = prec // Right-associative
		}

		// Parse right operand with higher precedence
		right, newPos, err := parseInfixWithPrecedence(elements, pos, nextMinPrec)
		if err != nil {
			return nil, newPos, err
		}
		pos = newPos

		// Build AST node: (op left right)
		left = core.NewList(op, left, right)
	}

	return left, pos, nil
}

// parseChainedComparison handles Python-style chained comparisons
// e.g., a == b == c becomes (and (== a b) (== b c))
func parseChainedComparison(elements []core.Value, startPos int, leftOperand core.Value) (core.Value, int, error) {
	var comparisons []core.Value
	pos := startPos
	left := leftOperand

	// Collect all chained comparisons
	for pos < len(elements) {
		// Get the operator
		if pos >= len(elements) {
			break
		}

		op, ok := elements[pos].(core.SymbolValue)
		if !ok || !isComparisonOperator(string(op)) {
			// Not a comparison operator, we're done
			break
		}

		pos++ // consume operator

		if pos >= len(elements) {
			return nil, pos, &ParseError{Message: fmt.Sprintf("comparison operator %s missing right operand", op), Line: 1, Col: 1}
		}

		// Get the right operand (just the next element, not a full expression)
		// We need to parse it at a higher precedence to avoid consuming more comparisons
		right := elements[pos]
		pos++

		// Create the comparison: (op left right)
		comparison := core.NewList(op, left, right)
		comparisons = append(comparisons, comparison)

		// The right operand becomes the left operand for the next comparison
		left = right
	}

	// If we only have one comparison, return it directly
	if len(comparisons) == 1 {
		return comparisons[0], pos, nil
	}

	// Build the chained and expression: (and comp1 comp2 comp3...)
	andExpr := make([]core.Value, len(comparisons)+1)
	andExpr[0] = core.SymbolValue("and")
	copy(andExpr[1:], comparisons)

	return core.NewList(andExpr...), pos, nil
}

// Helper function that returns just the value (for simpler API)
func parseInfixExpressionSimple(elements []core.Value) (core.Value, error) {
	result, finalPos, err := parseInfixWithPrecedence(elements, 0, 0)
	if err != nil {
		return nil, err
	}

	// Check if we consumed all elements
	if finalPos < len(elements) {
		return nil, &ParseError{Message: "unexpected tokens after expression"}
	}

	return result, nil
}

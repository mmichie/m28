package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
	"github.com/mmichie/m28/parser"
	"os"
)

// EvalString parses and evaluates a string in the given context
func EvalString(input string, ctx *core.Context) (core.Value, error) {
	// Auto-detect syntax: if it starts with '(', use S-expression parser
	// Otherwise, use Python parser
	trimmed := strings.TrimSpace(input)

	if strings.HasPrefix(trimmed, "(") {
		// Use M28 S-expression parser
		p := parser.NewParser()
		expr, err := p.Parse(input)
		if err != nil {
			return nil, err
		}
		return Eval(expr, ctx)
	}

	// Use Python parser (default)
	tokenizer := parser.NewPythonTokenizer(input)
	tokens, err := tokenizer.Tokenize()
	if err != nil {
		return nil, err
	}

	pythonParser := parser.NewPythonParser(tokens)
	nodes, err := pythonParser.Parse()
	if err != nil {
		// Check if this is a parse error - if so, convert to SyntaxError
		errMsg := err.Error()
		if strings.Contains(errMsg, "parse error") || strings.Contains(errMsg, "Unexpected token") {
			// Format as SyntaxError for proper exception handling
			return nil, fmt.Errorf("SyntaxError: %s", errMsg)
		}
		return nil, err
	}

	// Convert AST nodes to IR and evaluate
	return evalPythonNodes(nodes, ctx)
}

// evalPythonNodes evaluates Python AST nodes
func evalPythonNodes(nodes []ast.ASTNode, ctx *core.Context) (core.Value, error) {
	var result core.Value = core.Nil
	var err error

	for _, node := range nodes {
		ir := node.ToIR()

		result, err = Eval(ir, ctx)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

// ReadFile reads the content of a file and returns it as a string
func ReadFile(filename string) (string, error) {
	content, err := os.ReadFile(filename)
	if err != nil {
		return "", err
	}
	return string(content), nil
}

// Duplicate builtin functions have been removed.
// Use builtin.RegisterAllBuiltins from the builtin package instead.

// Helper functions for creating common errors

// ErrArgCount creates an error for incorrect argument count
func ErrArgCount(msg string) error {
	return ArgumentError{msg}
}

// ErrType creates an error for incorrect argument type
func ErrType(expected string, got core.Value) error {
	return TypeError{
		Expected: core.Type(expected),
		Got:      got.Type(),
	}
}

// ErrDivByZero creates a division by zero error
func ErrDivByZero() error {
	return ArithmeticError{"division by zero"}
}

// Error types

// ArgumentError represents an error with function arguments
type ArgumentError struct {
	Message string
}

func (e ArgumentError) Error() string {
	return e.Message
}

// TypeError represents a type error
type TypeError struct {
	Expected core.Type
	Got      core.Type
}

func (e TypeError) Error() string {
	return "expected " + string(e.Expected) + ", got " + string(e.Got)
}

// ArithmeticError represents an arithmetic error
type ArithmeticError struct {
	Message string
}

func (e ArithmeticError) Error() string {
	return "arithmetic error: " + e.Message
}

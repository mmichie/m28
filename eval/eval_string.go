package eval

import (
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/parser"
	"os"
)

// EvalString parses and evaluates a string in the given context
func EvalString(input string, ctx *core.Context) (core.Value, error) {
	// Create a new parser
	p := parser.NewParser()

	// Parse the input
	expr, err := p.Parse(input)
	if err != nil {
		return nil, err
	}

	// Evaluate the parsed expression
	return Eval(expr, ctx)
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

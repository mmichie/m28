package eval

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/core/ast"
	"github.com/mmichie/m28/parser"
	"os"
)

// EvalString parses and evaluates a string in the given context.
//
// M28 targets Python semantics, so the Python parser is tried first — even when
// the input begins with '('. Many valid Python expressions start with '(':
// (True), (1, 2), (1,), (a + b), and generator expressions. Routing every
// '('-prefixed input to the S-expression parser (as this once did) broke eval()
// of such expressions. Only when Python parsing fails do we fall back to the
// M28 S-expression parser, and only for '('-prefixed input — that preserves
// deliberately-non-Python forms like (print "x") and (+ 1 2), which are not
// valid Python and so fail the Python parse first.
//
// The fallback triggers on a Python *parse* error only; a successful Python
// parse is always used (so there is no double evaluation and runtime errors
// such as NameError are surfaced rather than masked by a second attempt).
func EvalString(input string, ctx *core.Context) (core.Value, error) {
	trimmed := strings.TrimSpace(input)

	tokens, tokErr := parser.NewPythonTokenizer(input).Tokenize()
	if tokErr == nil {
		nodes, parseErr := parser.NewPythonParser(tokens, "<eval>", input).Parse()
		if parseErr == nil {
			return evalPythonNodes(nodes, ctx)
		}

		// Python parsing failed: try the S-expression parser for '('-prefixed
		// input (genuine M28 S-expressions).
		if strings.HasPrefix(trimmed, "(") {
			if expr, sexprErr := parser.NewParser().Parse(input); sexprErr == nil {
				return Eval(expr, ctx)
			}
		}

		// Surface the original Python parse error as a SyntaxError.
		errMsg := parseErr.Error()
		if strings.Contains(errMsg, "parse error") || strings.Contains(errMsg, "Unexpected token") {
			return nil, fmt.Errorf("SyntaxError: %s", errMsg)
		}
		return nil, parseErr
	}

	// Python tokenization failed: fall back to S-expression parsing for
	// '('-prefixed input, otherwise surface the tokenizer error.
	if strings.HasPrefix(trimmed, "(") {
		expr, sexprErr := parser.NewParser().Parse(input)
		if sexprErr != nil {
			return nil, sexprErr
		}
		return Eval(expr, ctx)
	}
	return nil, tokErr
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

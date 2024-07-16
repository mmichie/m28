package m28

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
)

// Interpreter represents the M28 Lisp interpreter
type Interpreter struct {
	env *Environment
}

// New creates a new M28 Lisp interpreter
func New() *Interpreter {
	return &Interpreter{
		env: NewEnvironment(nil),
	}
}

// Eval evaluates a LispValue in the interpreter's environment
func (i *Interpreter) Eval(expr LispValue) (LispValue, error) {
	return EvalExpression(expr, i.env)
}

func (i *Interpreter) Parse(input string) (LispValue, error) {
	tokens := tokenize(input)
	return parseMultiple(tokens)
}

func parseMultiple(tokens []string) (LispValue, error) {
	var expressions LispList
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

// Execute parses and evaluates a M28 Lisp expression
func (i *Interpreter) Execute(input string) (string, error) {
	expr, err := i.Parse(input)
	if err != nil {
		return "", err
	}
	result, err := i.Eval(expr)
	if err != nil {
		return "", err
	}
	return PrintValue(result), nil
}

// ExecuteFile reads and executes M28 Lisp code from a file
func (i *Interpreter) ExecuteFile(filename string) error {
	// Check if the file has the .m28 extension
	if filepath.Ext(filename) != ".m28" {
		return fmt.Errorf("file must have .m28 extension")
	}

	// Read the file contents
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	// Parse the file content
	expressions, err := i.Parse(string(content))
	if err != nil {
		return fmt.Errorf("error parsing file: %v", err)
	}

	// Evaluate each expression
	var result LispValue
	switch expr := expressions.(type) {
	case LispList:
		for _, e := range expr {
			result, err = i.Eval(e)
			if err != nil {
				return fmt.Errorf("error evaluating expression: %v", err)
			}
			// Print non-nil results
			if result != nil {
				fmt.Println(PrintValue(result))
			}
		}
	default:
		result, err = i.Eval(expr)
		if err != nil {
			return fmt.Errorf("error evaluating expression: %v", err)
		}
		if result != nil {
			fmt.Println(PrintValue(result))
		}
	}

	return nil
}

package special_forms

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"

	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/parser"
)

func EvalLoad(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("load requires exactly one argument")
	}

	filenameExpr, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	filename, ok := filenameExpr.(string)
	if !ok {
		return nil, fmt.Errorf("load argument must evaluate to a string")
	}

	// Resolve the file path
	absPath, err := filepath.Abs(filename)
	if err != nil {
		return nil, fmt.Errorf("error resolving file path: %v", err)
	}

	// Read the file content
	content, err := ioutil.ReadFile(absPath)
	if err != nil {
		return nil, fmt.Errorf("error reading file: %v", err)
	}

	// Parse the file content
	p := parser.NewParser()
	expr, err := p.Parse(string(content))
	if err != nil {
		return nil, fmt.Errorf("error parsing file content: %v", err)
	}

	// Evaluate the parsed expressions
	var result core.LispValue
	exprs, ok := expr.(core.LispList)
	if !ok {
		exprs = core.LispList{expr}
	}

	for _, expr := range exprs {
		result, err = e.Eval(expr, env)
		if err != nil {
			return nil, fmt.Errorf("error evaluating expression from loaded file: %v", err)
		}
	}

	return result, nil
}

// EvalReadFile reads the contents of a file and returns it as a string
func EvalReadFile(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("read-file requires exactly one argument")
	}

	filenameExpr, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	filename, ok := filenameExpr.(string)
	if !ok {
		return nil, fmt.Errorf("read-file argument must evaluate to a string")
	}

	content, err := ioutil.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("error reading file: %v", err)
	}

	return string(content), nil
}

// EvalWriteFile writes a string to a file
func EvalWriteFile(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("write-file requires exactly two arguments")
	}

	filenameExpr, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	filename, ok := filenameExpr.(string)
	if !ok {
		return nil, fmt.Errorf("write-file first argument must evaluate to a string")
	}

	contentExpr, err := e.Eval(args[1], env)
	if err != nil {
		return nil, err
	}

	content, ok := contentExpr.(string)
	if !ok {
		return nil, fmt.Errorf("write-file second argument must evaluate to a string")
	}

	err = ioutil.WriteFile(filename, []byte(content), 0644)
	if err != nil {
		return nil, fmt.Errorf("error writing file: %v", err)
	}

	return core.LispSymbol("t"), nil
}

// EvalFileExists checks if a file exists
func EvalFileExists(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("file-exists? requires exactly one argument")
	}

	filenameExpr, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	filename, ok := filenameExpr.(string)
	if !ok {
		return nil, fmt.Errorf("file-exists? argument must evaluate to a string")
	}

	_, err = os.Stat(filename)
	return err == nil, nil
}

// EvalDeleteFile deletes a file
func EvalDeleteFile(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("delete-file requires exactly one argument")
	}

	filenameExpr, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	filename, ok := filenameExpr.(string)
	if !ok {
		return nil, fmt.Errorf("delete-file argument must evaluate to a string")
	}

	err = os.Remove(filename)
	if err != nil {
		return nil, fmt.Errorf("error deleting file: %v", err)
	}

	return core.LispSymbol("t"), nil
}

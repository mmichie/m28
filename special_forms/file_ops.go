package special_forms

import (
	"fmt"
	"io/ioutil"
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

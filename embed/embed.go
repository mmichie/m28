package embed

import (
	"fmt"
	"os"
	"os/exec"
	"strings"

	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
	"github.com/mmichie/m28/env"
	"github.com/mmichie/m28/eval"
	"github.com/mmichie/m28/parser"
	"github.com/mmichie/m28/special_forms"
)

// M28Engine provides the main interface for embedding M28 in other applications
type M28Engine struct {
	env       core.Environment
	evaluator core.Evaluator
	parser    *parser.Parser
	// Callback for shell commands
	ShellExecutor func(string) (string, error)
}

// NewM28Engine creates a new M28 interpreter with a clean environment
func NewM28Engine() *M28Engine {
	// Initialize the environment
	environment := env.NewEnvironment(nil)

	// Set up builtins in the environment
	environment.SetupBuiltins()

	// Create the evaluator
	evaluator := eval.NewEvaluator()

	// Set up the evaluator for builtins
	builtin.SetEvaluator(evaluator)

	// Create and register the module loader
	moduleLoader := special_forms.NewModuleLoader()
	moduleLoader.SetEvaluator(evaluator)
	core.SetModuleLoader(moduleLoader)

	// Register special forms in the environment
	special_forms.RegisterSpecialForms(environment)

	engine := &M28Engine{
		env:       environment,
		evaluator: evaluator,
		parser:    parser.NewParser(),
		// Default shell executor uses os/exec
		ShellExecutor: defaultShellExecutor,
	}

	// Register shell-specific functions
	engine.registerShellFunctions()

	return engine
}

// defaultShellExecutor executes a shell command and returns its output
func defaultShellExecutor(command string) (string, error) {
	cmd := exec.Command("sh", "-c", command)
	output, err := cmd.CombinedOutput()
	return string(output), err
}

// registerShellFunctions adds shell-specific functions to the environment
func (m *M28Engine) registerShellFunctions() {
	// Execute shell command and return output
	m.env.Define(core.LispSymbol("shell"), core.NewBuiltinFunction("shell", func(args ...core.LispValue) (core.LispValue, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("shell: expected 1 argument, got %d", len(args))
		}

		commandStr, ok := args[0].(core.LispString)
		if !ok {
			return nil, fmt.Errorf("shell: expected string argument, got %T", args[0])
		}

		output, err := m.ShellExecutor(string(commandStr))
		if err != nil {
			return nil, fmt.Errorf("shell command error: %v", err)
		}

		// Remove trailing newline if present
		output = strings.TrimSuffix(output, "\n")
		return core.LispString(output), nil
	}))

	// Get environment variable
	m.env.Define(core.LispSymbol("getenv"), core.NewBuiltinFunction("getenv", func(args ...core.LispValue) (core.LispValue, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("getenv: expected 1 argument, got %d", len(args))
		}

		varName, ok := args[0].(core.LispString)
		if !ok {
			return nil, fmt.Errorf("getenv: expected string argument, got %T", args[0])
		}

		value := os.Getenv(string(varName))
		return core.LispString(value), nil
	}))

	// Set environment variable
	m.env.Define(core.LispSymbol("setenv"), core.NewBuiltinFunction("setenv", func(args ...core.LispValue) (core.LispValue, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("setenv: expected 2 arguments, got %d", len(args))
		}

		varName, ok := args[0].(core.LispString)
		if !ok {
			return nil, fmt.Errorf("setenv: expected string for variable name, got %T", args[0])
		}

		varValue, ok := args[1].(core.LispString)
		if !ok {
			return nil, fmt.Errorf("setenv: expected string for value, got %T", args[1])
		}

		err := os.Setenv(string(varName), string(varValue))
		if err != nil {
			return nil, fmt.Errorf("setenv error: %v", err)
		}

		return core.PythonicNone{}, nil
	}))

	// Get current working directory
	m.env.Define(core.LispSymbol("pwd"), core.NewBuiltinFunction("pwd", func(args ...core.LispValue) (core.LispValue, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("pwd: expected 0 arguments, got %d", len(args))
		}

		dir, err := os.Getwd()
		if err != nil {
			return nil, fmt.Errorf("pwd error: %v", err)
		}

		return core.LispString(dir), nil
	}))

	// Change directory
	m.env.Define(core.LispSymbol("cd"), core.NewBuiltinFunction("cd", func(args ...core.LispValue) (core.LispValue, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("cd: expected 1 argument, got %d", len(args))
		}

		dirStr, ok := args[0].(core.LispString)
		if !ok {
			return nil, fmt.Errorf("cd: expected string argument, got %T", args[0])
		}

		err := os.Chdir(string(dirStr))
		if err != nil {
			return nil, fmt.Errorf("cd error: %v", err)
		}

		return core.PythonicNone{}, nil
	}))

	// Exit with status code
	m.env.Define(core.LispSymbol("exit"), core.NewBuiltinFunction("exit", func(args ...core.LispValue) (core.LispValue, error) {
		code := 0
		if len(args) > 0 {
			if numVal, ok := args[0].(core.LispNumber); ok {
				code = int(numVal)
			} else {
				return nil, fmt.Errorf("exit: expected number argument, got %T", args[0])
			}
		}

		os.Exit(code)
		return core.PythonicNone{}, nil // Never reached
	}))
}

// Evaluate evaluates M28 code and returns the result
func (m *M28Engine) Evaluate(code string) (core.LispValue, error) {
	expr, err := m.parser.Parse(code)
	if err != nil {
		return nil, fmt.Errorf("parse error: %v", err)
	}

	// Check if we got multiple expressions as a list
	if exprList, ok := expr.(core.LispList); ok && len(exprList) > 0 {
		// Execute each expression in the list
		var result core.LispValue = core.PythonicNone{}

		for _, subExpr := range exprList {
			result, err = m.evaluator.Eval(subExpr, m.env)
			if err != nil {
				return nil, err
			}
		}

		// Return the last result
		return result, nil
	}

	// Single expression or empty list case
	return m.evaluator.Eval(expr, m.env)
}

// ExecuteFile executes an M28 file
func (m *M28Engine) ExecuteFile(filename string) error {
	fileContent, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	content := string(fileContent)

	// Register source code in cache for better error reporting
	core.RegisterSourceCode(filename, content)

	m.parser.SetFilename(filename)

	expr, err := m.parser.Parse(content)
	if err != nil {
		return fmt.Errorf("parse error: %v", err)
	}

	// Check if we got multiple expressions as a list
	if exprList, ok := expr.(core.LispList); ok {
		for _, subExpr := range exprList {
			_, err = m.evaluator.Eval(subExpr, m.env)
			if err != nil {
				if ex, ok := err.(*core.Exception); ok {
					return ex
				}
				return fmt.Errorf("error executing file: %v", err)
			}
		}
		return nil
	}

	// Single expression case
	_, err = m.evaluator.Eval(expr, m.env)
	if err != nil {
		if ex, ok := err.(*core.Exception); ok {
			return ex
		}
		return fmt.Errorf("error executing file: %v", err)
	}

	return nil
}

// DefineValue defines a value in the M28 environment
func (m *M28Engine) DefineValue(name string, value core.LispValue) {
	m.env.Define(core.LispSymbol(name), value)
}

// DefineFunction defines a Go function in the M28 environment
func (m *M28Engine) DefineFunction(name string, function func(args ...core.LispValue) (core.LispValue, error)) {
	m.env.Define(core.LispSymbol(name), core.NewBuiltinFunction(name, function))
}

// GetValue gets a value from the M28 environment
func (m *M28Engine) GetValue(name string) (core.LispValue, bool) {
	return m.env.Get(core.LispSymbol(name))
}

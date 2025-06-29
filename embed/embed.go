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
)

// M28Engine provides the main interface for embedding M28 in other applications
type M28Engine struct {
	env    *env.Environment
	parser *parser.Parser
	// Callback for shell commands
	ShellExecutor func(string) (string, error)
}

// NewM28Engine creates a new M28 interpreter with a clean environment
func NewM28Engine() *M28Engine {
	// Initialize the environment
	environment := env.NewEnvironment(nil)

	// Register built-in functions to a core.Context
	ctx := &core.Context{
		Vars: make(map[string]core.Value),
	}
	builtin.RegisterAllBuiltins(ctx)

	// Create the engine
	engine := &M28Engine{
		env:    environment,
		parser: parser.NewParser(),
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
	m.env.Define("shell", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("shell: expected 1 argument, got %d", len(args))
		}

		commandStr, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("shell: expected string argument, got %T", args[0])
		}

		output, err := m.ShellExecutor(string(commandStr))
		if err != nil {
			return nil, fmt.Errorf("shell command error: %v", err)
		}

		// Remove trailing newline if present
		output = strings.TrimSuffix(output, "\n")
		return core.StringValue(output), nil
	}))

	// Get environment variable
	m.env.Define("getenv", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("getenv: expected 1 argument, got %d", len(args))
		}

		varName, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("getenv: expected string argument, got %T", args[0])
		}

		value := os.Getenv(string(varName))
		return core.StringValue(value), nil
	}))

	// Set environment variable
	m.env.Define("setenv", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 2 {
			return nil, fmt.Errorf("setenv: expected 2 arguments, got %d", len(args))
		}

		varName, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("setenv: expected string for variable name, got %T", args[0])
		}

		varValue, ok := args[1].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("setenv: expected string for value, got %T", args[1])
		}

		err := os.Setenv(string(varName), string(varValue))
		if err != nil {
			return nil, fmt.Errorf("setenv error: %v", err)
		}

		return core.Nil, nil
	}))

	// Get current working directory
	m.env.Define("pwd", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 0 {
			return nil, fmt.Errorf("pwd: expected 0 arguments, got %d", len(args))
		}

		dir, err := os.Getwd()
		if err != nil {
			return nil, fmt.Errorf("pwd error: %v", err)
		}

		return core.StringValue(dir), nil
	}))

	// Change directory
	m.env.Define("cd", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) != 1 {
			return nil, fmt.Errorf("cd: expected 1 argument, got %d", len(args))
		}

		dirStr, ok := args[0].(core.StringValue)
		if !ok {
			return nil, fmt.Errorf("cd: expected string argument, got %T", args[0])
		}

		err := os.Chdir(string(dirStr))
		if err != nil {
			return nil, fmt.Errorf("cd error: %v", err)
		}

		return core.Nil, nil
	}))

	// Exit with status code
	m.env.Define("exit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		code := 0
		if len(args) > 0 {
			if numVal, ok := args[0].(core.NumberValue); ok {
				code = int(numVal)
			} else {
				return nil, fmt.Errorf("exit: expected number argument, got %T", args[0])
			}
		}

		os.Exit(code)
		return core.Nil, nil // Never reached
	}))
}

// Evaluate evaluates M28 code and returns the result
func (m *M28Engine) Evaluate(code string) (core.Value, error) {
	expr, err := m.parser.Parse(code)
	if err != nil {
		return nil, fmt.Errorf("parse error: %v", err)
	}

	// Create context
	ctx := &core.Context{
		Vars: make(map[string]core.Value),
	}

	// Check if we got multiple expressions as a list
	if exprList, ok := expr.(core.ListValue); ok && len(exprList) > 0 {
		// Execute each expression in the list
		var result core.Value = core.Nil

		for _, subExpr := range exprList {
			result, err = eval.Eval(subExpr, ctx)
			if err != nil {
				return nil, err
			}
		}

		// Return the last result
		return result, nil
	}

	// Single expression or empty list case
	return eval.Eval(expr, ctx)
}

// ExecuteFile executes an M28 file
func (m *M28Engine) ExecuteFile(filename string) error {
	fileContent, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("error reading file: %v", err)
	}

	content := string(fileContent)

	// Set filename for parser
	m.parser.SetFilename(filename)

	expr, err := m.parser.Parse(content)
	if err != nil {
		return fmt.Errorf("parse error: %v", err)
	}

	// Create context
	ctx := &core.Context{
		Vars: make(map[string]core.Value),
	}

	// Check if we got multiple expressions as a list
	if exprList, ok := expr.(core.ListValue); ok {
		for _, subExpr := range exprList {
			_, err = eval.Eval(subExpr, ctx)
			if err != nil {
				return fmt.Errorf("error executing file: %v", err)
			}
		}
		return nil
	}

	// Single expression case
	_, err = eval.Eval(expr, ctx)
	if err != nil {
		return fmt.Errorf("error executing file: %v", err)
	}

	return nil
}

// DefineValue defines a value in the M28 environment
func (m *M28Engine) DefineValue(name string, value core.Value) {
	m.env.Define(name, value)
}

// DefineFunction defines a Go function in the M28 environment
func (m *M28Engine) DefineFunction(name string, function func(args []core.Value, ctx *core.Context) (core.Value, error)) {
	m.env.Define(name, core.NewBuiltinFunction(function))
}

// GetValue gets a value from the M28 environment
func (m *M28Engine) GetValue(name string) (core.Value, bool) {
	return m.env.Get(name)
}

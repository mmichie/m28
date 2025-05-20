package core

import (
	"fmt"
	"os"
	"path/filepath"
)

// DefaultModuleLoader is the default implementation of ModuleLoader
type DefaultModuleLoader struct {
	ctx *Context
	// evalFunc is a function that evaluates code in a context
	evalFunc func(expr Value, ctx *Context) (Value, error)
	// parseFunc is a function that parses a string into an expression
	parseFunc func(code string) (Value, error)
}

// NewDefaultModuleLoader creates a new DefaultModuleLoader
func NewDefaultModuleLoader(ctx *Context, evalFunc func(expr Value, ctx *Context) (Value, error), parseFunc func(code string) (Value, error)) *DefaultModuleLoader {
	return &DefaultModuleLoader{
		ctx:       ctx,
		evalFunc:  evalFunc,
		parseFunc: parseFunc,
	}
}

// SetContext sets the evaluation context
func (l *DefaultModuleLoader) SetContext(ctx *Context) {
	l.ctx = ctx
}

// GetContext returns the evaluation context
func (l *DefaultModuleLoader) GetContext() *Context {
	return l.ctx
}

// LoadModule loads a module by name and returns its content
func (l *DefaultModuleLoader) LoadModule(name string, ctx *Context) (*DictValue, error) {
	// First check if the module is already loaded
	registry := GetModuleRegistry()
	if module, found := registry.GetModule(name); found {
		return module, nil
	}

	// Resolve the module path
	path, err := registry.ResolveModulePath(name)
	if err != nil {
		return nil, fmt.Errorf("failed to resolve module path: %v", err)
	}

	// Load the module content
	content, err := l.loadModuleContent(path)
	if err != nil {
		return nil, fmt.Errorf("failed to load module content: %v", err)
	}

	// Create a new module context
	moduleCtx := NewContext(l.ctx.Global)

	// Define the special __name__ variable
	moduleCtx.Define("__name__", StringValue(name))
	moduleCtx.Define("__file__", StringValue(path))

	// Parse the module content
	expr, err := l.parseFunc(content)
	if err != nil {
		return nil, fmt.Errorf("failed to parse module: %v", err)
	}

	// Evaluate the module code
	_, err = l.evalFunc(expr, moduleCtx)
	if err != nil {
		return nil, fmt.Errorf("failed to evaluate module: %v", err)
	}

	// Create a dictionary for the module exports
	moduleDict := NewDict()

	// Add all module context variables to the module dictionary
	// (in a real implementation, we'd filter out internal vars or respect __all__)
	for name, value := range moduleCtx.Vars {
		// Skip special vars like __name__ and __file__
		if len(name) >= 2 && name[:2] == "__" && name[len(name)-2:] == "__" {
			continue
		}
		moduleDict.Set(name, value)
	}

	// Register the module in the registry
	registry.StoreModule(name, moduleDict, path, []string{})

	return moduleDict, nil
}

// loadModuleContent reads a module file and returns its content
func (l *DefaultModuleLoader) loadModuleContent(path string) (string, error) {
	// Check if the file exists
	info, err := os.Stat(path)
	if err != nil {
		return "", fmt.Errorf("file not found: %v", err)
	}

	// Check if it's a directory (package)
	if info.IsDir() {
		// Look for __init__.m28 in the directory
		initPath := filepath.Join(path, "__init__.m28")
		if _, err := os.Stat(initPath); err == nil {
			path = initPath
		} else {
			return "", fmt.Errorf("directory does not contain __init__.m28: %s", path)
		}
	}

	// Read the file content
	content, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("failed to read file: %v", err)
	}

	return string(content), nil
}
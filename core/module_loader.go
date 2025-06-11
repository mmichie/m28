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
	// Get the module registry
	registry := GetModuleRegistry()

	// Normalize the module name by removing .m28 extension for caching
	cacheName := name
	if filepath.Ext(cacheName) == ".m28" {
		cacheName = cacheName[:len(cacheName)-4]
	}

	// First check if the module is already loaded
	if module, found := registry.GetModule(cacheName); found {
		return module, nil
	}

	// Check if the module is currently being loaded (circular dependency)
	if registry.IsLoading(cacheName) {
		return nil, fmt.Errorf("circular import detected: module '%s' is already being loaded", name)
	}

	// Mark the module as being loaded
	registry.SetLoading(cacheName, true)
	defer registry.SetLoading(cacheName, false)

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
	moduleCtx.Define("__name__", StringValue(cacheName))
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

	// Check if __exports__ is defined
	if exportsVal, err := moduleCtx.Lookup("__exports__"); err == nil {
		// __exports__ is defined, only export listed names
		if exportsList, ok := exportsVal.(ListValue); ok {
			for _, item := range exportsList {
				if nameStr, ok := item.(StringValue); ok {
					name := string(nameStr)
					if val, err := moduleCtx.Lookup(name); err == nil {
						moduleDict.Set(name, val)
					}
				} else if nameSym, ok := item.(SymbolValue); ok {
					name := string(nameSym)
					if val, err := moduleCtx.Lookup(name); err == nil {
						moduleDict.Set(name, val)
					}
				}
			}
		}
	} else {
		// No __exports__, export all non-special variables
		for name, value := range moduleCtx.Vars {
			// Skip special vars like __name__ and __file__
			if len(name) >= 2 && name[:2] == "__" && name[len(name)-2:] == "__" {
				continue
			}
			// Skip private vars (starting with _)
			if len(name) > 0 && name[0] == '_' {
				continue
			}
			moduleDict.Set(name, value)
		}
	}

	// Register the module in the registry
	registry.StoreModule(cacheName, moduleDict, path, []string{})

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

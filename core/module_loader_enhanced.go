package core

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// ModuleLoaderEnhanced is an enhanced module loader that supports builtin Go modules
type ModuleLoaderEnhanced struct {
	DefaultModuleLoader
	// getBuiltinModule is a function that returns builtin modules
	getBuiltinModule func(name string) (*DictValue, bool)
}

// NewModuleLoaderEnhanced creates a new enhanced module loader
func NewModuleLoaderEnhanced(
	ctx *Context,
	evalFunc func(expr Value, ctx *Context) (Value, error),
	parseFunc func(code string) (Value, error),
	getBuiltinModule func(name string) (*DictValue, bool),
) *ModuleLoaderEnhanced {
	return &ModuleLoaderEnhanced{
		DefaultModuleLoader: DefaultModuleLoader{
			ctx:       ctx,
			evalFunc:  evalFunc,
			parseFunc: parseFunc,
		},
		getBuiltinModule: getBuiltinModule,
	}
}

// LoadModule loads a module by name and returns its content
func (l *ModuleLoaderEnhanced) LoadModule(name string, ctx *Context) (*DictValue, error) {
	registry := GetModuleRegistry()
	cacheName := normalizeCacheName(name)

	// Check cache and circular dependencies
	if module, err := l.checkModuleCache(registry, cacheName, name); module != nil || err != nil {
		return module, err
	}

	// Mark module as loading
	registry.SetLoading(cacheName, true)
	defer registry.SetLoading(cacheName, false)

	// Try to load as builtin module
	if module := l.tryLoadBuiltinModule(registry, cacheName); module != nil {
		return module, nil
	}

	// Check if it's an M28 module
	path, err := registry.ResolveModulePath(name)
	if err == nil {
		// It's an M28 module - create partial module for circular import support
		partialModule := NewDict()
		registry.StoreModule(cacheName, partialModule, "", []string{})
		fmt.Fprintf(os.Stderr, "[DEBUG] LoadModule: stored partial M28 module '%s' in registry\n", cacheName)
		return l.loadM28Module(registry, cacheName, path, partialModule)
	}

	// Not an M28 module, try Python
	// Python loader will create partial module if the Python file exists
	return l.tryLoadPythonModuleWithoutPartial(registry, name, cacheName, err)
}

// normalizeCacheName removes .m28 extension for caching
func normalizeCacheName(name string) string {
	if filepath.Ext(name) == ".m28" {
		return name[:len(name)-4]
	}
	return name
}

// checkModuleCache checks if module is already loaded or being loaded
func (l *ModuleLoaderEnhanced) checkModuleCache(registry *ModuleRegistry, cacheName, name string) (*DictValue, error) {
	// Check if already loaded
	if module, found := registry.GetModule(cacheName); found {
		return module, nil
	}

	// Check for circular dependency - return partial module
	// In Python, circular imports are allowed
	if registry.IsLoading(cacheName) {
		fmt.Fprintf(os.Stderr, "[DEBUG] Circular import detected for '%s', returning partial module\n", cacheName)
		// Return the partial module that's currently being loaded
		if partialModule, found := registry.GetModule(cacheName); found {
			fmt.Fprintf(os.Stderr, "[DEBUG] Found partial module in registry with %d keys\n", len(partialModule.Keys()))
			return partialModule, nil
		}
		// If not in registry yet, return empty dict (will be populated later)
		fmt.Fprintf(os.Stderr, "[DEBUG] Partial module not in registry, returning empty dict\n")
		return NewDict(), nil
	}

	return nil, nil
}

// tryLoadBuiltinModule attempts to load a builtin Go module
func (l *ModuleLoaderEnhanced) tryLoadBuiltinModule(registry *ModuleRegistry, cacheName string) *DictValue {
	if l.getBuiltinModule == nil {
		return nil
	}

	module, isBuiltin := l.getBuiltinModule(cacheName)
	if !isBuiltin {
		return nil
	}

	registry.StoreModule(cacheName, module, "<builtin>", []string{})
	return module
}

// loadM28Module loads an M28 module from the file system
func (l *ModuleLoaderEnhanced) loadM28Module(registry *ModuleRegistry, cacheName, path string, partialModule *DictValue) (*DictValue, error) {
	// Load module content
	content, err := l.loadModuleContent(path)
	if err != nil {
		return nil, fmt.Errorf("failed to load module content: %v", err)
	}

	// Create module context and execute
	moduleCtx := l.createModuleContext(cacheName, path)
	if err := l.parseAndEvaluateModule(content, moduleCtx); err != nil {
		return nil, err
	}

	// Extract module exports and populate the partial module
	exportModuleVars(partialModule, moduleCtx)

	// Update registry with complete module
	registry.StoreModule(cacheName, partialModule, path, []string{})

	return partialModule, nil
}

// tryLoadPythonModuleWithoutPartial attempts to load a Python module
// Python loader will create and store partial module if the file exists
func (l *ModuleLoaderEnhanced) tryLoadPythonModuleWithoutPartial(registry *ModuleRegistry, name, cacheName string, m28Err error) (*DictValue, error) {
	// Check if Python loader is available
	if pythonLoaderFunc == nil {
		// Python loader not available, return ImportError
		return nil, &ImportError{
			ModuleName: name,
			Message:    fmt.Sprintf("no module named '%s'", name),
		}
	}

	// Try to load as Python module
	// Python loader (LoadPythonModule in python_loader.go) will:
	// 1. Check if the Python file exists
	// 2. If yes, transpile and evaluate it
	// 3. Return the module dict (or error)
	// We don't create a partial module here because if the Python file doesn't exist,
	// we want to return an error, not an empty module
	moduleDict, err := pythonLoaderFunc(name, l.GetContext(), l.evalFunc)
	if err != nil {
		// If error mentions "not found", return ImportError
		if strings.Contains(err.Error(), "not found") {
			return nil, &ImportError{
				ModuleName: name,
				Message:    fmt.Sprintf("no module named '%s'", name),
			}
		}
		// Other Python loading errors (transpilation, C extension, etc.)
		return nil, err
	}

	// Successfully loaded Python module, register it
	registry.StoreModule(cacheName, moduleDict, fmt.Sprintf("<Python module '%s'>", name), []string{})

	return moduleDict, nil
}

// createModuleContext creates a new context for module execution
func (l *ModuleLoaderEnhanced) createModuleContext(cacheName, path string) *Context {
	moduleCtx := NewContext(l.ctx.Global)
	moduleCtx.Define("__name__", StringValue(cacheName))
	moduleCtx.Define("__file__", StringValue(path))
	return moduleCtx
}

// parseAndEvaluateModule parses and evaluates module code
func (l *ModuleLoaderEnhanced) parseAndEvaluateModule(content string, moduleCtx *Context) error {
	expr, err := l.parseFunc(content)
	if err != nil {
		return fmt.Errorf("failed to parse module: %v", err)
	}

	_, err = l.evalFunc(expr, moduleCtx)
	if err != nil {
		return fmt.Errorf("failed to evaluate module: %v", err)
	}

	return nil
}

// extractModuleExports extracts module exports from context
func extractModuleExports(moduleCtx *Context) *DictValue {
	moduleDict := NewDict()
	exportModuleVars(moduleDict, moduleCtx)
	return moduleDict
}

// exportModuleVars populates a module dict with exports from context
func exportModuleVars(moduleDict *DictValue, moduleCtx *Context) {
	// Check if __exports__ is defined
	if exportsVal, err := moduleCtx.Lookup("__exports__"); err == nil {
		exportNamedVars(moduleDict, moduleCtx, exportsVal)
	} else {
		exportAllPublicVars(moduleDict, moduleCtx)
	}
}

// exportNamedVars exports only variables listed in __exports__
func exportNamedVars(moduleDict *DictValue, moduleCtx *Context, exportsVal Value) {
	exportsList, ok := exportsVal.(*ListValue)
	if !ok {
		return
	}

	for _, item := range exportsList.Items() {
		name := getExportName(item)
		if name == "" {
			continue
		}

		if val, err := moduleCtx.Lookup(name); err == nil {
			moduleDict.Set(name, val)
		}
	}
}

// getExportName extracts name from export list item (string or symbol)
func getExportName(item Value) string {
	if nameStr, ok := item.(StringValue); ok {
		return string(nameStr)
	}
	if nameSym, ok := item.(SymbolValue); ok {
		return string(nameSym)
	}
	return ""
}

// exportAllPublicVars exports all non-private variables from module context
func exportAllPublicVars(moduleDict *DictValue, moduleCtx *Context) {
	for name, value := range moduleCtx.Vars {
		if isPrivateVar(name) {
			continue
		}
		moduleDict.Set(name, value)
	}
}

// isPrivateVar checks if a variable name is private (special or starts with _)
func isPrivateVar(name string) bool {
	// Skip special vars like __name__ and __file__
	if len(name) >= 2 && name[:2] == "__" && name[len(name)-2:] == "__" {
		return true
	}
	// Skip private vars (starting with _)
	if len(name) > 0 && name[0] == '_' {
		return true
	}
	return false
}

// loadModuleContent reads a module file and returns its content
func (l *ModuleLoaderEnhanced) loadModuleContent(path string) (string, error) {
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

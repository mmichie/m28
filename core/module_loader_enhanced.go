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

	// Check sys.modules dict before trying to load
	// This allows Python code to manually set sys.modules['importlib._bootstrap'] etc.
	DebugLog("[DEBUG] LoadModule: checking sys.modules for '%s'\n", name)
	if module := l.checkSysModules(name); module != nil {
		DebugLog("[DEBUG] LoadModule: found '%s' in sys.modules, returning it\n", name)
		// Cache it in registry for faster lookups next time
		registry.StoreModule(cacheName, module, "<from sys.modules>", []string{})
		return module, nil
	}
	DebugLog("[DEBUG] LoadModule: '%s' not in sys.modules, will try to load\n", name)

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
		DebugLog("[DEBUG] LoadModule: stored partial M28 module '%s' in registry\n", cacheName)
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
		DebugLog("[DEBUG] Circular import detected for '%s', returning partial module\n", cacheName)
		// Return the partial module that's currently being loaded
		if partialModule, found := registry.GetModule(cacheName); found {
			DebugLog("[DEBUG] Found partial module in registry with %d keys\n", len(partialModule.Keys()))
			return partialModule, nil
		}
		// If not in registry yet, return empty dict (will be populated later)
		DebugLog("[DEBUG] Partial module not in registry, returning empty dict\n")
		return NewDict(), nil
	}

	return nil, nil
}

// checkSysModules checks if module is in sys.modules dict
// This allows Python code to manually register modules like sys.modules['importlib._bootstrap']
func (l *ModuleLoaderEnhanced) checkSysModules(name string) *DictValue {
	// Get sys module from builtin registry
	sysModule, isBuiltin := l.getBuiltinModule("sys")
	if !isBuiltin {
		return nil
	}

	// Get sys.modules dict
	sysModulesKey := ValueToKey(StringValue("modules"))
	sysModulesVal, ok := sysModule.Get(sysModulesKey)
	if !ok {
		return nil
	}

	sysModulesDict, ok := sysModulesVal.(*DictValue)
	if !ok {
		return nil
	}

	// Check if module exists in sys.modules
	moduleKey := ValueToKey(StringValue(name))
	moduleVal, ok := sysModulesDict.Get(moduleKey)
	if !ok {
		return nil
	}

	// Module exists - verify it's a dict (module dict) or Module
	if moduleDict, ok := moduleVal.(*DictValue); ok {
		DebugLog("[DEBUG] Found module '%s' in sys.modules (DictValue)\n", name)
		return moduleDict
	}

	if module, ok := moduleVal.(*Module); ok {
		// Convert Module to DictValue by extracting exports
		DebugLog("[DEBUG] Found module '%s' in sys.modules (Module), converting to DictValue\n", name)
		dict := NewDict()
		for name, val := range module.Exports {
			dict.Set(name, val)
		}
		return dict
	}

	// Not a dict or Module - ignore it
	DebugLog("[DEBUG] Found '%s' in sys.modules but it's not a dict/module (type: %T)\n", name, moduleVal)
	return nil
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
	// Pass partialModule so context syncs definitions to it in real-time
	moduleCtx := l.createModuleContext(cacheName, path, partialModule)
	if err := l.parseAndEvaluateModule(content, moduleCtx); err != nil {
		return nil, err
	}

	// Note: No need to call exportModuleVars here anymore
	// The module dict was populated in real-time during evaluation via Context.Define()

	// Update registry with complete module
	registry.StoreModule(cacheName, partialModule, path, []string{})

	return partialModule, nil
}

// tryLoadPythonModuleWithoutPartial attempts to load a Python module
// Creates a partial module for circular import support before loading
func (l *ModuleLoaderEnhanced) tryLoadPythonModuleWithoutPartial(registry *ModuleRegistry, name, cacheName string, m28Err error) (*DictValue, error) {
	// Check if Python loader is available
	if pythonLoaderFunc == nil {
		// Python loader not available, return ImportError
		return nil, &ImportError{
			ModuleName: name,
			Message:    fmt.Sprintf("no module named '%s'", name),
		}
	}

	// Create partial module and store in registry BEFORE loading
	// This enables circular imports to access the partial module during evaluation
	partialModule := NewDict()
	registry.StoreModule(cacheName, partialModule, "", []string{})
	DebugLog("[DEBUG] LoadModule: stored partial Python module '%s' in registry\n", cacheName)

	// Try to load as Python module
	// Python loader will populate partialModule during evaluation via Context.ModuleDict
	moduleDict, err := pythonLoaderFunc(name, l.GetContext(), l.evalFunc, partialModule)
	if err != nil {
		// Loading failed - remove the partial module from registry
		registry.ReloadModule(cacheName)
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

	// Successfully loaded Python module, update registry with full path
	registry.StoreModule(cacheName, moduleDict, fmt.Sprintf("<Python module '%s'>", name), []string{})

	return moduleDict, nil
}

// createModuleContext creates a new context for module execution
// partialModule is the dict that will be populated during evaluation for circular import support
func (l *ModuleLoaderEnhanced) createModuleContext(cacheName, path string, partialModule *DictValue) *Context {
	moduleCtx := NewContext(l.ctx.Global)
	// Link the context to the partial module dict for real-time syncing
	moduleCtx.ModuleDict = partialModule
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
	// __all__ is a special variable that should be exported (defines public API)
	if name == "__all__" {
		return false
	}
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

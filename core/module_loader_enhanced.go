package core

import (
	"errors"
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
	Log.Info(SubsystemImport, "Import resolution started", "module", name)

	registry := GetModuleRegistry()
	cacheName := normalizeCacheName(name)

	Log.Debug(SubsystemImport, "Module cache name normalized", "module", name, "cache_name", cacheName)

	// Check cache and circular dependencies
	if module, err := l.checkModuleCache(registry, cacheName, name); module != nil || err != nil {
		if err != nil {
			Log.Warn(SubsystemImport, "Cache check returned error", "module", name, "error", err)
		} else {
			Log.Info(SubsystemImport, "Module found in cache", "module", name, "cache_status", "hit")
		}
		return module, err
	}

	// Check sys.modules dict before trying to load
	// This allows Python code to manually set sys.modules['importlib._bootstrap'] etc.
	Log.Debug(SubsystemImport, "Checking sys.modules", "module", name)
	if module := l.checkSysModules(name); module != nil {
		Log.Info(SubsystemImport, "Module found in sys.modules", "module", name, "cache_status", "sys.modules_hit")
		// Cache it in registry for faster lookups next time
		registry.StoreModule(cacheName, module, "<from sys.modules>", []string{})
		return module, nil
	}
	Log.Debug(SubsystemImport, "Module not in sys.modules, proceeding to load", "module", name)

	// Mark module as loading
	registry.SetLoading(cacheName, true)
	defer registry.SetLoading(cacheName, false)

	// Try to load as builtin module
	Log.Debug(SubsystemImport, "Attempting builtin module load", "module", name)
	if module := l.tryLoadBuiltinModule(registry, cacheName); module != nil {
		Log.Info(SubsystemImport, "Module loaded successfully", "module", name, "source_type", "builtin")
		return module, nil
	}

	// Check if it's an M28 module
	Log.Debug(SubsystemImport, "Attempting M28 module resolution", "module", name)
	path, err := registry.ResolveModulePath(name)
	if err == nil {
		Log.Info(SubsystemImport, "M28 module path resolved", "module", name, "path", path, "source_type", "m28")
		// It's an M28 module - create partial module for circular import support
		partialModule := NewDict()
		registry.StoreModule(cacheName, partialModule, "", []string{})
		Log.Debug(SubsystemImport, "Partial M28 module stored in registry", "module", cacheName)
		return l.loadM28Module(registry, cacheName, path, partialModule)
	}

	// Not an M28 module, try Python
	Log.Debug(SubsystemImport, "M28 module not found, attempting Python module load", "module", name, "m28_error", err.Error())
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
		Log.Debug(SubsystemImport, "Module found in module cache", "module", name, "cache_status", "hit")
		return module, nil
	}

	// Check for circular dependency - return partial module
	// In Python, circular imports are allowed
	if registry.IsLoading(cacheName) {
		Log.Warn(SubsystemImport, "Circular import detected", "module", cacheName)
		// Return the partial module that's currently being loaded
		if partialModule, found := registry.GetModule(cacheName); found {
			Log.Debug(SubsystemImport, "Returning partial module for circular import", "module", cacheName, "keys", len(partialModule.Keys()))
			return partialModule, nil
		}
		// If not in registry yet, return empty dict (will be populated later)
		Log.Debug(SubsystemImport, "Partial module not in registry, returning empty dict", "module", cacheName)
		return NewDict(), nil
	}

	Log.Debug(SubsystemImport, "Module not in cache", "module", name, "cache_status", "miss")
	return nil, nil
}

// registerInSysModules registers a module in sys.modules
// This is needed for both builtin and Python modules so Python code can find them
func (l *ModuleLoaderEnhanced) registerInSysModules(name string, module *DictValue) {
	// Skip if getBuiltinModule is not set (happens during early initialization)
	if l.getBuiltinModule == nil {
		return
	}

	// Get sys module
	sysModule, ok := l.getBuiltinModule("sys")
	if !ok {
		return // sys not available yet
	}

	// Get sys.modules dict
	sysModulesVal, ok := sysModule.Get("modules")
	if !ok {
		return // sys.modules not available
	}

	sysModulesDict, ok := sysModulesVal.(*DictValue)
	if !ok {
		return // sys.modules is not a dict
	}

	// Register the module with proper key format
	key := ValueToKey(StringValue(name))
	sysModulesDict.SetWithKey(key, StringValue(name), module)
	DebugLog("[DEBUG] Registered module '%s' in sys.modules\n", name)

	// For dotted module names (e.g., "test.test_bool"), set the submodule
	// as an attribute on the parent package module
	// This allows sys.modules['test'].test_bool to work
	lastDot := -1
	for i := len(name) - 1; i >= 0; i-- {
		if name[i] == '.' {
			lastDot = i
			break
		}
	}

	if lastDot > 0 {
		parentName := name[:lastDot]
		childName := name[lastDot+1:]

		// Try to get existing parent module
		parentKey := ValueToKey(StringValue(parentName))
		parentModule, parentExists := sysModulesDict.Get(parentKey)

		// Only set child on parent if parent already exists as a real module
		// Don't create placeholder parent modules - they block the real module from loading
		if parentExists {
			if parentDict, ok := parentModule.(*DictValue); ok {
				// Only set if the child doesn't already exist OR if it's also a dict (module)
				// This prevents overwriting attributes like 'collections.namedtuple' (a function)
				// with partial module dicts when 'collections.namedtuple' fails to load as a module
				existingChild, childExists := parentDict.Get(childName)
				if !childExists || isModuleLike(existingChild) {
					parentDict.Set(childName, module)
					DebugLog("[DEBUG] Set '%s' as attribute on parent module '%s'\n", childName, parentName)
				} else {
					DebugLog("[DEBUG] Skipped setting '%s' on parent '%s' (already exists as non-module: %T)\n", childName, parentName, existingChild)
				}
			}
		} else {
			DebugLog("[DEBUG] Parent module '%s' not loaded yet, will set '%s' attribute when parent loads\n", parentName, childName)
		}
	}
}

// isModuleLike checks if a value looks like a module (dict or Module object)
func isModuleLike(val Value) bool {
	switch val.(type) {
	case *DictValue, *Module:
		return true
	default:
		return false
	}
}

// checkSysModules checks if module is in sys.modules dict
// This allows Python code to manually register modules like sys.modules['importlib._bootstrap']
func (l *ModuleLoaderEnhanced) checkSysModules(name string) *DictValue {
	Log.Trace(SubsystemImport, "Checking sys.modules for module", "module", name)

	// Get sys module from builtin registry
	sysModule, isBuiltin := l.getBuiltinModule("sys")
	if !isBuiltin {
		Log.Debug(SubsystemImport, "sys module not available as builtin", "module", name)
		return nil
	}

	// Get sys.modules dict
	// Use direct string key "modules" not ValueToKey which would be "s:modules"
	sysModulesVal, ok := sysModule.Get("modules")
	if !ok {
		Log.Debug(SubsystemImport, "sys.modules not found in sys", "module", name)
		return nil
	}

	sysModulesDict, ok := sysModulesVal.(*DictValue)
	if !ok {
		Log.Warn(SubsystemImport, "sys.modules is not a DictValue", "module", name)
		return nil
	}

	// Check if module exists in sys.modules
	moduleKey := ValueToKey(StringValue(name))
	Log.Trace(SubsystemImport, "Looking up module key in sys.modules", "module", name, "key", moduleKey)
	moduleVal, ok := sysModulesDict.Get(moduleKey)
	if !ok {
		Log.Trace(SubsystemImport, "Module not found in sys.modules", "module", name)
		return nil
	}

	// Module exists - verify it's a dict (module dict) or Module
	if moduleDict, ok := moduleVal.(*DictValue); ok {
		Log.Debug(SubsystemImport, "Found module in sys.modules as DictValue", "module", name)
		return moduleDict
	}

	if module, ok := moduleVal.(*Module); ok {
		// Convert Module to DictValue by getting its __dict__
		// This includes exports, context bindings, and Dict values
		Log.Debug(SubsystemImport, "Found module in sys.modules as Module, converting to DictValue", "module", name)
		if dictVal, ok := module.GetAttr("__dict__"); ok {
			if dict, ok := dictVal.(*DictValue); ok {
				return dict
			}
		}
		// Fallback: just use exports if __dict__ doesn't work
		dict := NewDict()
		for name, val := range module.Exports {
			dict.Set(name, val)
		}
		return dict
	}

	// Not a dict or Module - ignore it
	Log.Warn(SubsystemImport, "Found module in sys.modules but not a dict/module", "module", name, "type", fmt.Sprintf("%T", moduleVal))
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

	// Register builtin modules in sys.modules so Python code can find them
	// This is essential for importlib._setup() which looks for _thread, _weakref, _warnings
	l.registerInSysModules(cacheName, module)

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
		Log.Warn(SubsystemImport, "Python loader not available", "module", name)
		// Python loader not available, return ModuleNotFoundError
		return nil, NewModuleNotFoundError(name)
	}

	// Create partial module and store in registry BEFORE loading
	// This enables circular imports to access the partial module during evaluation
	partialModule := NewDict()
	registry.StoreModule(cacheName, partialModule, "", []string{})
	Log.Debug(SubsystemImport, "Created partial Python module in registry", "module", cacheName)

	// Register partial module in sys.modules BEFORE evaluation
	// This allows circular imports to find the module via sys.modules
	// matching Python's behavior exactly
	l.registerInSysModules(name, partialModule)

	// Try to load as Python module
	// Python loader will populate partialModule during evaluation via Context.ModuleDict
	Log.Debug(SubsystemImport, "Invoking Python loader", "module", name)
	moduleDict, err := pythonLoaderFunc(name, l.GetContext(), l.evalFunc, partialModule)
	if err != nil {
		Log.Error(SubsystemImport, "Python loader failed", "module", name, "error", err)
		// Loading failed - remove the partial module from registry
		registry.ReloadModule(cacheName)
		// Check if error is ModuleNotFoundError or ImportError using errors.As
		var modNotFound *ModuleNotFoundError
		var impErr *ImportError
		if errors.As(err, &modNotFound) || errors.As(err, &impErr) || strings.Contains(err.Error(), "not found") {
			return nil, NewModuleNotFoundError(name)
		}
		// Other Python loading errors (transpilation, C extension, etc.)
		return nil, err
	}
	Log.Info(SubsystemImport, "Python module loaded successfully", "module", name, "source_type", "python")

	// Successfully loaded Python module, update registry with full path
	registry.StoreModule(cacheName, moduleDict, fmt.Sprintf("<Python module '%s'>", name), []string{})

	return moduleDict, nil
}

// createModuleContext creates a new context for module execution
// partialModule is the dict that will be populated during evaluation for circular import support
func (l *ModuleLoaderEnhanced) createModuleContext(cacheName, path string, partialModule *DictValue) *Context {
	moduleCtx := NewContext(l.ctx.Global)
	// Important: Set the module context's Global to itself, not the parent.
	// In Python, 'global' refers to the module's namespace, not builtins.
	moduleCtx.Global = moduleCtx
	// Link the context to the partial module dict for real-time syncing
	moduleCtx.ModuleDict = partialModule
	moduleCtx.Define("__name__", StringValue(cacheName))
	moduleCtx.Define("__file__", StringValue(path))
	// __doc__ is None by default, set by module docstring if present
	moduleCtx.Define("__doc__", None)
	// Initialize empty __annotations__ dict for PEP 526 support
	// Python automatically creates this dict for modules that use annotations
	moduleCtx.Define("__annotations__", NewDict())
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

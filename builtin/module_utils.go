package builtin

import (
	"fmt"
	"path/filepath"

	"github.com/mmichie/m28/core"
)

// RegisterModuleUtils registers utility functions for working with modules
func RegisterModuleUtils() {
	// Register module utility functions
	core.RegisterBuiltin("add_module_path", AddModulePathFunc)
	core.RegisterBuiltin("get_module_paths", GetModulePathsFunc)
	core.RegisterBuiltin("module_info", ModuleInfoFunc)
	core.RegisterBuiltin("reload_module", ReloadModuleFunc)
}

// AddModulePathFunc adds a path to the module search path
func AddModulePathFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("add_module_path requires exactly 1 argument")
	}

	// Get the path argument
	var path string
	switch arg := args[0].(type) {
	case string:
		path = arg
	default:
		return nil, fmt.Errorf("add_module_path requires a string argument")
	}

	// Get the module registry and add the path
	registry := core.GetModuleRegistry()
	registry.AddSearchPath(path)

	return core.PythonicNone{}, nil
}

// GetModulePathsFunc returns the current module search paths
func GetModulePathsFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("get_module_paths takes no arguments")
	}

	// Get the module registry and get the paths
	registry := core.GetModuleRegistry()
	paths := registry.GetSearchPaths()

	// Convert to a Lisp list
	result := make(core.LispList, len(paths))
	for i, path := range paths {
		result[i] = path
	}

	return result, nil
}

// ModuleInfoFunc returns information about a loaded module
func ModuleInfoFunc(args []core.LispValue, _ core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("module_info requires exactly 1 argument")
	}

	// Get the module name
	var moduleName string
	switch arg := args[0].(type) {
	case string:
		moduleName = arg
	case core.LispSymbol:
		moduleName = string(arg)
	default:
		return nil, fmt.Errorf("module_info requires a string or symbol argument")
	}

	// Get the module registry and look up the module
	registry := core.GetModuleRegistry()
	info, found := registry.GetModuleInfo(moduleName)
	if !found {
		return nil, fmt.Errorf("module not found: %s", moduleName)
	}

	// Create a dictionary with module information
	result := core.NewPythonicDict()
	result.Set("name", moduleName)
	result.Set("path", info.Path)
	result.Set("load_time", info.LoadTime.String())

	// Add dependencies as a list
	deps := make(core.LispList, len(info.Dependencies))
	for i, dep := range info.Dependencies {
		deps[i] = dep
	}
	result.Set("dependencies", deps)

	// Get the module's exports if available
	if exports, ok := info.Module.Get("__exports__"); ok {
		result.Set("exports", exports)
	}

	return result, nil
}

// ReloadModuleFunc reloads a module from disk
func ReloadModuleFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("reload_module requires exactly 1 argument")
	}

	// Get the module name
	var moduleName string
	switch arg := args[0].(type) {
	case string:
		moduleName = arg
	case core.LispSymbol:
		moduleName = string(arg)
	default:
		return nil, fmt.Errorf("reload_module requires a string or symbol argument")
	}

	// Get the module registry and reload the module
	registry := core.GetModuleRegistry()
	err := registry.ReloadModule(moduleName)
	if err != nil {
		return nil, fmt.Errorf("failed to reload module %s: %v", moduleName, err)
	}

	// Get the module loader
	moduleLoader := core.GetModuleLoader()
	if moduleLoader == nil {
		return nil, fmt.Errorf("no module loader registered")
	}

	// Get the evaluator
	evaluator := moduleLoader.GetEvaluator()
	if evaluator == nil {
		return nil, fmt.Errorf("failed to get evaluator for module reload")
	}

	// Re-import the module to update the environment
	module, err := moduleLoader.LoadModule(moduleName, evaluator)
	if err != nil {
		return nil, fmt.Errorf("failed to load reloaded module %s: %v", moduleName, err)
	}

	// Check if the module was already imported in the current environment
	if _, exists := env.Get(core.LispSymbol(filepath.Base(moduleName))); exists {
		moduleBaseName := filepath.Base(moduleName)
		moduleBaseName = filepath.Base(moduleBaseName)
		env.Define(core.LispSymbol(moduleBaseName), module)
	}

	return core.PythonicNone{}, nil
}

func init() {
	RegisterModuleUtils()
}

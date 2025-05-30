package core

import (
	"fmt"
	"sync"
	"time"
)

// ModuleInfo holds information about a loaded module
type ModuleInfo struct {
	Module       *DictValue // The actual module contents
	Path         string     // Full path to the module file
	LoadTime     time.Time  // When the module was loaded
	Dependencies []string   // List of modules this module depends on
}

// ModuleRegistry keeps track of loaded modules
type ModuleRegistry struct {
	modules    map[string]ModuleInfo // Modules indexed by name
	searchPath []string              // List of directories to search for modules
	mu         sync.RWMutex          // For thread safety
}

// NewModuleRegistry creates a new ModuleRegistry
func NewModuleRegistry() *ModuleRegistry {
	return &ModuleRegistry{
		modules:    make(map[string]ModuleInfo),
		searchPath: []string{}, // Will be set by SetModulePaths
	}
}

// Global module registry
var globalRegistry = NewModuleRegistry()

// GetModuleRegistry returns the global module registry
func GetModuleRegistry() *ModuleRegistry {
	return globalRegistry
}

// ModuleLoader interface defines methods for loading modules
type ModuleLoader interface {
	// LoadModule loads a module by name and returns its content
	LoadModule(name string, ctx *Context) (*DictValue, error)

	// SetContext sets the evaluation context
	SetContext(ctx *Context)

	// GetContext returns the evaluation context
	GetContext() *Context
}

// Global module loader
var globalModuleLoader ModuleLoader

// SetModuleLoader sets the global module loader
func SetModuleLoader(loader ModuleLoader) {
	globalModuleLoader = loader
}

// GetModuleLoader returns the global module loader
func GetModuleLoader() ModuleLoader {
	return globalModuleLoader
}

// GetSearchPaths returns a copy of the current search paths
func (r *ModuleRegistry) GetSearchPaths() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()

	// Return a copy to prevent modification of the internal slice
	paths := make([]string, len(r.searchPath))
	copy(paths, r.searchPath)
	return paths
}

// AddSearchPath adds a directory to the module search path
func (r *ModuleRegistry) AddSearchPath(path string) {
	r.mu.Lock()
	defer r.mu.Unlock()

	// Check if the path already exists
	for _, existingPath := range r.searchPath {
		if existingPath == path {
			return // Path already in the search path
		}
	}
	// Add the new path
	r.searchPath = append(r.searchPath, path)
}

// GetModuleInfo returns information about a loaded module
func (r *ModuleRegistry) GetModuleInfo(name string) (ModuleInfo, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	info, found := r.modules[name]
	return info, found
}

// StoreModule stores a module in the registry
func (r *ModuleRegistry) StoreModule(name string, module *DictValue, path string, dependencies []string) {
	r.mu.Lock()
	defer r.mu.Unlock()

	r.modules[name] = ModuleInfo{
		Module:       module,
		Path:         path,
		LoadTime:     time.Now(),
		Dependencies: dependencies,
	}
}

// GetModule returns a module from the registry
func (r *ModuleRegistry) GetModule(name string) (*DictValue, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	if info, found := r.modules[name]; found {
		return info.Module, true
	}
	return nil, false
}

// ResolveModulePath resolves a module name to its file path
func (r *ModuleRegistry) ResolveModulePath(name string) (string, error) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	// Use the centralized FindModule function
	path, err := FindModule(name)
	if err != nil {
		return "", fmt.Errorf("module %s not found", name)
	}

	return path, nil
}

// ReloadModule removes a module from the registry so it will be reloaded on next import
func (r *ModuleRegistry) ReloadModule(name string) error {
	r.mu.Lock()
	defer r.mu.Unlock()

	// Check if the module is loaded
	if _, exists := r.modules[name]; !exists {
		return fmt.Errorf("module %s is not loaded", name)
	}

	// Remove the module from the registry
	delete(r.modules, name)

	return nil
}

// ClearAllModules removes all modules from the registry
func (r *ModuleRegistry) ClearAllModules() {
	r.mu.Lock()
	defer r.mu.Unlock()

	r.modules = make(map[string]ModuleInfo)
}

// ListModules returns a list of all loaded module names
func (r *ModuleRegistry) ListModules() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()

	modules := make([]string, 0, len(r.modules))
	for name := range r.modules {
		modules = append(modules, name)
	}
	return modules
}

package core

import (
	"fmt"
	"os"
	"path/filepath"
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
	// Default search paths
	defaultPaths := []string{
		".",
		"./tests",
		"./modules",
		"./examples",
		"/usr/local/lib/m28/modules",
		"/usr/lib/m28/modules",
	}

	return &ModuleRegistry{
		modules:    make(map[string]ModuleInfo),
		searchPath: defaultPaths,
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

	// Handle case where name is already a valid file path
	if _, err := os.Stat(name); err == nil {
		// File exists with the exact path
		absPath, err := filepath.Abs(name)
		if err != nil {
			return name, nil // Fall back to the original path if abs fails
		}
		return absPath, nil
	}

	// Check if it's a .m28 path that needs to be normalized
	if filepath.Ext(name) == ".m28" {
		// Try to resolve it both as a relative path and in the search paths
		// First check if it's a relative path
		if absPath, err := filepath.Abs(name); err == nil {
			if _, err := os.Stat(absPath); err == nil {
				return absPath, nil
			}
		}
	}

	// Try with .m28 extension added if not already present
	nameWithExt := name
	if filepath.Ext(name) != ".m28" {
		nameWithExt = name + ".m28"
		// Check if the name with extension is a valid path
		if _, err := os.Stat(nameWithExt); err == nil {
			absPath, err := filepath.Abs(nameWithExt)
			if err != nil {
				return nameWithExt, nil
			}
			return absPath, nil
		}
	}

	// Look for the module in each of the search paths
	for _, path := range r.searchPath {
		// Try with .m28 extension
		fullPath := filepath.Join(path, nameWithExt)
		if _, err := os.Stat(fullPath); err == nil {
			// Found the file
			absPath, err := filepath.Abs(fullPath)
			if err != nil {
				return fullPath, nil // Fall back to the original path
			}
			return absPath, nil
		}

		// If we didn't find it with .m28 extension, try with original name
		if nameWithExt != name {
			fullPath = filepath.Join(path, name)
			if _, err := os.Stat(fullPath); err == nil {
				absPath, err := filepath.Abs(fullPath)
				if err != nil {
					return fullPath, nil
				}
				return absPath, nil
			}
		}

		// Check if it's a directory with __init__.m28 (package)
		initPath := filepath.Join(path, name, "__init__.m28")
		if _, err := os.Stat(initPath); err == nil {
			// Found a package
			absPath, err := filepath.Abs(initPath)
			if err != nil {
				return initPath, nil
			}
			return absPath, nil
		}
	}

	// Module not found in any of the search paths
	return "", fmt.Errorf("module %s not found", name)
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

// Package core provides the fundamental types and interfaces for the M28 language.
package core

import (
	"fmt"
	"os"
	"path/filepath"
	"sync"
)

// Module represents a module in the language
type Module struct {
	BaseObject
	name     string
	filename string
	loaded   bool
	exports  map[string]Value
}

// NewModule creates a new module
func NewModule(name string, filename string) *Module {
	return &Module{
		BaseObject: *NewBaseObject(ModuleType),
		name:       name,
		filename:   filename,
		loaded:     false,
		exports:    make(map[string]Value),
	}
}

// Type implements Value.Type
func (m *Module) Type() Type {
	return ModuleType
}

// String implements Value.String
func (m *Module) String() string {
	return fmt.Sprintf("<module '%s' from '%s'>", m.name, m.filename)
}

// GetAttr implements Object.GetAttr
func (m *Module) GetAttr(name string) (Value, bool) {
	// First check exports
	if value, ok := m.exports[name]; ok {
		return value, true
	}
	
	// Then check base object attributes
	return m.BaseObject.GetAttr(name)
}

// SetAttr implements Object.SetAttr
func (m *Module) SetAttr(name string, value Value) error {
	// Add to exports
	m.exports[name] = value
	return nil
}

// ModuleRegistry manages the available modules
type ModuleRegistry struct {
	modules     map[string]*Module
	searchPaths []string
	mu          sync.RWMutex // For thread safety
}

// NewModuleRegistry creates a new module registry
func NewModuleRegistry() *ModuleRegistry {
	registry := &ModuleRegistry{
		modules:     make(map[string]*Module),
		searchPaths: []string{"."},
	}
	
	return registry
}

// AddSearchPath adds a search path to the registry
func (mr *ModuleRegistry) AddSearchPath(path string) {
	mr.mu.Lock()
	defer mr.mu.Unlock()
	
	// Convert to absolute path
	absPath, err := filepath.Abs(path)
	if err == nil {
		path = absPath
	}
	
	// Check if path already exists
	for _, existingPath := range mr.searchPaths {
		if existingPath == path {
			return
		}
	}
	
	mr.searchPaths = append(mr.searchPaths, path)
}

// GetModule returns a module from the registry
func (mr *ModuleRegistry) GetModule(name string) (*Module, bool) {
	mr.mu.RLock()
	defer mr.mu.RUnlock()
	
	module, ok := mr.modules[name]
	return module, ok
}

// RegisterModule adds a module to the registry
func (mr *ModuleRegistry) RegisterModule(module *Module) {
	mr.mu.Lock()
	defer mr.mu.Unlock()
	
	mr.modules[module.name] = module
}

// FindModuleFile finds a module file in the search paths
func (mr *ModuleRegistry) FindModuleFile(name string) (string, error) {
	mr.mu.RLock()
	defer mr.mu.RUnlock()
	
	// Search for module file
	for _, dir := range mr.searchPaths {
		// Try with .m28 extension
		filename := filepath.Join(dir, name+".m28")
		_, err := os.Stat(filename)
		if err == nil {
			return filename, nil
		}
		
		// Try directory with __init__.m28
		filename = filepath.Join(dir, name, "__init__.m28")
		_, err = os.Stat(filename)
		if err == nil {
			return filename, nil
		}
	}
	
	return "", fmt.Errorf("module '%s' not found", name)
}

// LoadModule loads a module by name
func (mr *ModuleRegistry) LoadModule(name string, ctx *Context) (*Module, error) {
	// Check if module is already loaded
	if module, ok := mr.GetModule(name); ok && module.loaded {
		return module, nil
	}
	
	// Find module file
	filename, err := mr.FindModuleFile(name)
	if err != nil {
		return nil, err
	}
	
	// Create new module
	module := NewModule(name, filename)
	
	// Register module (even before loading)
	mr.RegisterModule(module)
	
	// Create a new context for the module
	moduleCtx := NewContext(ctx.Global)
	
	// Add module to the module context so exported items can reference the module itself
	moduleCtx.Define("__module__", module)
	
	// Add exports container
	exports := make(map[string]Value)
	moduleCtx.Define("__exports__", &DictValue{
		BaseObject: *NewBaseObject(DictType),
		entries:    exports,
	})
	
	// Read and execute module file
	content, err := os.ReadFile(filename)
	if err != nil {
		return nil, fmt.Errorf("error reading module file: %v", err)
	}
	
	// Execute module code (this requires access to the eval.EvalString function which we don't have here)
	// For now, we'll assume this is done by the caller
	
	// Mark module as loaded
	module.loaded = true
	
	return module, nil
}

// Default module registry
var DefaultModuleRegistry = NewModuleRegistry()

// Add standard library paths to the registry
func init() {
	// Current directory
	DefaultModuleRegistry.AddSearchPath(".")
	
	// Environment variable for module path
	if envPath := os.Getenv("M28_PATH"); envPath != "" {
		for _, path := range filepath.SplitList(envPath) {
			DefaultModuleRegistry.AddSearchPath(path)
		}
	}
	
	// Standard library path (relative to executable)
	execPath, err := os.Executable()
	if err == nil {
		stdlibPath := filepath.Join(filepath.Dir(execPath), "stdlib")
		DefaultModuleRegistry.AddSearchPath(stdlibPath)
	}
}
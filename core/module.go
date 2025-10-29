package core

import (
	"fmt"
	"strings"
	"sync"
	"time"
)

// Module represents a loaded module with export control
type Module struct {
	BaseObject
	Name    string           // Module name
	Path    string           // File path
	Context *Context         // Module's execution context
	Exports map[string]Value // Explicitly exported values
	All     []string         // Names to export with * import
	Dict    *DictValue       // Backing dict for dynamic attribute lookups (optional)
}

// NewModule creates a new module
func NewModule(name, path string) *Module {
	return &Module{
		BaseObject: *NewBaseObject(Type("module")),
		Name:       name,
		Path:       path,
		Exports:    make(map[string]Value),
		All:        []string{},
	}
}

// Type returns the module type
func (m *Module) Type() Type {
	return Type("module")
}

// String returns the string representation of the module
func (m *Module) String() string {
	return fmt.Sprintf("<module '%s' from '%s'>", m.Name, m.Path)
}

// Export adds a value to the module's exports
func (m *Module) Export(name string, value Value) {
	m.Exports[name] = value
	// Add to __all__ if not already there
	found := false
	for _, n := range m.All {
		if n == name {
			found = true
			break
		}
	}
	if !found {
		m.All = append(m.All, name)
	}
}

// GetExport retrieves an exported value
func (m *Module) GetExport(name string) (Value, bool) {
	val, ok := m.Exports[name]
	return val, ok
}

// GetAllExports returns all exported names
func (m *Module) GetAllExports() []string {
	if len(m.All) > 0 {
		return m.All
	}
	// If __all__ is not set, return all non-private exports
	exports := []string{}
	for name := range m.Exports {
		if !strings.HasPrefix(name, "_") {
			exports = append(exports, name)
		}
	}
	return exports
}

// SetAll sets the __all__ list for * imports
func (m *Module) SetAll(names []string) {
	m.All = names
}

// GetAttr implements object attribute access for modules
func (m *Module) GetAttr(name string) (Value, bool) {
	// First check backing dict for most up-to-date values
	// This allows Python's importlib._setup() to dynamically inject module globals
	// Dict takes priority over Exports to support dynamic attribute updates via setattr()
	if m.Dict != nil {
		key := ValueToKey(StringValue(name))
		if val, ok := m.Dict.Get(key); ok {
			return val, true
		}
	}

	// Then check exports (for modules without backing dict or non-exported names)
	if val, ok := m.Exports[name]; ok {
		return val, true
	}

	// Finally check base object attributes
	return m.BaseObject.GetAttr(name)
}

// ModuleRegistry enhancement for new module type
type EnhancedModuleInfo struct {
	Module       *Module   // The module object
	LoadTime     time.Time // When the module was loaded
	Dependencies []string  // List of modules this module depends on
}

// EnhancedModuleRegistry keeps track of loaded modules
type EnhancedModuleRegistry struct {
	modules    map[string]EnhancedModuleInfo // Modules indexed by name
	searchPath []string                      // List of directories to search for modules
	mu         sync.RWMutex                  // For thread safety
}

// NewEnhancedModuleRegistry creates a new enhanced module registry
func NewEnhancedModuleRegistry() *EnhancedModuleRegistry {
	// Default search paths
	defaultPaths := []string{
		".",
		"./tests",
		"./modules",
		"./examples",
		"/usr/local/lib/m28/modules",
		"/usr/lib/m28/modules",
	}

	return &EnhancedModuleRegistry{
		modules:    make(map[string]EnhancedModuleInfo),
		searchPath: defaultPaths,
	}
}

// StoreModule stores a module in the enhanced registry
func (r *EnhancedModuleRegistry) StoreModule(module *Module, dependencies []string) {
	r.mu.Lock()
	defer r.mu.Unlock()

	r.modules[module.Name] = EnhancedModuleInfo{
		Module:       module,
		LoadTime:     time.Now(),
		Dependencies: dependencies,
	}
}

// GetModule returns a module from the enhanced registry
func (r *EnhancedModuleRegistry) GetModule(name string) (*Module, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	if info, found := r.modules[name]; found {
		return info.Module, true
	}
	return nil, false
}

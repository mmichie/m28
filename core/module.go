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

// SetAttr implements attribute setting for modules
func (m *Module) SetAttr(name string, value Value) error {
	// If we have a shared dict (like __main__), set there
	if m.Dict != nil && m.Context != nil && m.Context.ModuleDict == m.Dict {
		key := ValueToKey(StringValue(name))
		m.Dict.Set(key, value)
		// Also set in context for consistency
		m.Context.Define(name, value)
		return nil
	}

	// For regular modules, set in exports
	m.Export(name, value)

	// Also set in Dict if it exists
	if m.Dict != nil {
		key := ValueToKey(StringValue(name))
		m.Dict.Set(key, value)
	}

	return nil
}

// GetAttr implements object attribute access for modules
func (m *Module) GetAttr(name string) (Value, bool) {
	// Special case: __dict__ returns the module's namespace
	if name == "__dict__" {
		// If we have a Dict set (like for __main__), return it directly
		// This ensures globals() and __main__.__dict__ return the same object
		if m.Dict != nil && m.Context != nil && m.Context.ModuleDict == m.Dict {
			// This is __main__ or a module with a shared dict
			// Make sure the dict is synchronized with the context
			for k, v := range m.Context.Vars {
				key := ValueToKey(StringValue(k))
				m.Dict.Set(key, v)
			}
			return m.Dict, true
		}

		// For regular modules, create a dict from the module's exports and Dict
		dict := NewDict()

		// Add all exports
		for k, v := range m.Exports {
			dict.Set(ValueToKey(StringValue(k)), v)
		}

		// Add all context bindings (for __main__ and dynamically executed modules)
		// Walk up the context chain to collect all bindings
		if m.Context != nil {
			ctx := m.Context
			for ctx != nil {
				for k, v := range ctx.Vars {
					key := ValueToKey(StringValue(k))
					// Only set if not already present (inner scopes take precedence)
					if _, exists := dict.Get(key); !exists {
						dict.Set(key, v)
					}
				}
				ctx = ctx.Outer
			}
		}

		// Override with Dict values if present (Dict has higher priority)
		if m.Dict != nil {
			for _, key := range m.Dict.Keys() {
				if val, ok := m.Dict.Get(key); ok {
					dict.Set(key, val)
				}
			}
		}

		return dict, true
	}

	// Special case: __doc__ returns the module's documentation
	if name == "__doc__" {
		// Check if __doc__ is in Dict or Exports
		if m.Dict != nil {
			key := ValueToKey(StringValue("__doc__"))
			if val, ok := m.Dict.Get(key); ok {
				return val, true
			}
		}
		if val, ok := m.Exports["__doc__"]; ok {
			return val, true
		}
		// Return None if no docstring is set
		return None, true
	}

	// Special case: __file__ returns the module's file path
	if name == "__file__" {
		// Check if __file__ is in Dict or Exports first
		if m.Dict != nil {
			key := ValueToKey(StringValue("__file__"))
			if val, ok := m.Dict.Get(key); ok {
				return val, true
			}
		}
		if val, ok := m.Exports["__file__"]; ok {
			return val, true
		}
		// Return the module's Path field
		if m.Path != "" {
			return StringValue(m.Path), true
		}
		// Return None if no path is set
		return None, true
	}

	// Special case: __name__ returns the module's name
	if name == "__name__" {
		// Check if __name__ is in Dict or Exports first
		if m.Dict != nil {
			key := ValueToKey(StringValue("__name__"))
			if val, ok := m.Dict.Get(key); ok {
				return val, true
			}
		}
		if val, ok := m.Exports["__name__"]; ok {
			return val, true
		}
		// Return the module's Name field
		return StringValue(m.Name), true
	}

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

	// Check the module's context (for __main__ and other dynamically executed modules)
	if m.Context != nil {
		if val, err := m.Context.Lookup(name); err == nil {
			return val, true
		}
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

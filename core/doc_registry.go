package core

import (
	"fmt"
	"sort"
	"strings"
	"sync"
)

// DocEntry stores documentation for a symbol
type DocEntry struct {
	// Name of the symbol
	Name string

	// Type of the symbol (e.g. "function", "special-form", "class", "method")
	Type string

	// Brief one-line description
	Brief string

	// Detailed description (can include multiline text)
	Description string

	// Parameters documentation (only for callables)
	Params []ParamDoc

	// Return value documentation (only for callables)
	Returns string

	// Example usage
	Examples []string

	// Related topics
	Related []string

	// Module where this is defined (if applicable)
	Module string
}

// ParamDoc documents a parameter
type ParamDoc struct {
	// Parameter name
	Name string

	// Parameter type (if known)
	Type string

	// Description of the parameter
	Description string

	// Whether the parameter is optional
	Optional bool

	// Default value (if optional)
	Default string
}

// DocRegistry is the global documentation registry
type DocRegistry struct {
	// Map from symbol name to documentation
	entries map[string]DocEntry

	// Map from module name to list of symbols in that module
	moduleSymbols map[string][]string

	// Map from documentation type to list of symbols
	typeSymbols map[string][]string

	// Mutex for concurrent access
	mu sync.RWMutex
}

// NewDocRegistry creates a new documentation registry
func NewDocRegistry() *DocRegistry {
	return &DocRegistry{
		entries:       make(map[string]DocEntry),
		moduleSymbols: make(map[string][]string),
		typeSymbols:   make(map[string][]string),
	}
}

// DocRegistryInstance is the global instance of the documentation registry
var DocRegistryInstance = NewDocRegistry()

// RegisterDoc registers documentation for a symbol
func RegisterDoc(entry DocEntry) {
	DocRegistryInstance.Register(entry)
}

// Register adds a documentation entry to the registry
func (r *DocRegistry) Register(entry DocEntry) {
	r.mu.Lock()
	defer r.mu.Unlock()

	// Add to main entries map
	r.entries[entry.Name] = entry

	// Add to module map
	if entry.Module != "" {
		r.moduleSymbols[entry.Module] = append(r.moduleSymbols[entry.Module], entry.Name)
	}

	// Add to type map
	if entry.Type != "" {
		r.typeSymbols[entry.Type] = append(r.typeSymbols[entry.Type], entry.Name)
	}
}

// GetDoc retrieves documentation for a symbol
func (r *DocRegistry) GetDoc(name string) (DocEntry, bool) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	entry, ok := r.entries[name]
	return entry, ok
}

// GetDoc is a convenience function for accessing the global registry
func GetDoc(name string) (DocEntry, bool) {
	return DocRegistryInstance.GetDoc(name)
}

// ListByType retrieves all symbols of a given type
func (r *DocRegistry) ListByType(typeName string) []DocEntry {
	r.mu.RLock()
	defer r.mu.RUnlock()

	symbols, ok := r.typeSymbols[typeName]
	if !ok {
		return []DocEntry{}
	}

	// Sort symbols alphabetically for consistent output
	sort.Strings(symbols)

	result := make([]DocEntry, 0, len(symbols))
	for _, name := range symbols {
		if entry, ok := r.entries[name]; ok {
			result = append(result, entry)
		}
	}

	return result
}

// ListByType is a convenience function for accessing the global registry
func ListByType(typeName string) []DocEntry {
	return DocRegistryInstance.ListByType(typeName)
}

// ListByModule retrieves all symbols from a given module
func (r *DocRegistry) ListByModule(moduleName string) []DocEntry {
	r.mu.RLock()
	defer r.mu.RUnlock()

	symbols, ok := r.moduleSymbols[moduleName]
	if !ok {
		return []DocEntry{}
	}

	// Sort symbols alphabetically for consistent output
	sort.Strings(symbols)

	result := make([]DocEntry, 0, len(symbols))
	for _, name := range symbols {
		if entry, ok := r.entries[name]; ok {
			result = append(result, entry)
		}
	}

	return result
}

// ListByModule is a convenience function for accessing the global registry
func ListByModule(moduleName string) []DocEntry {
	return DocRegistryInstance.ListByModule(moduleName)
}

// ListModules retrieves a list of all registered modules
func (r *DocRegistry) ListModules() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()

	modules := make([]string, 0, len(r.moduleSymbols))
	for module := range r.moduleSymbols {
		modules = append(modules, module)
	}

	// Sort modules alphabetically for consistent output
	sort.Strings(modules)

	return modules
}

// ListModules is a convenience function for accessing the global registry
func ListModules() []string {
	return DocRegistryInstance.ListModules()
}

// ListTypes retrieves a list of all registered symbol types
func (r *DocRegistry) ListTypes() []string {
	r.mu.RLock()
	defer r.mu.RUnlock()

	types := make([]string, 0, len(r.typeSymbols))
	for typeName := range r.typeSymbols {
		types = append(types, typeName)
	}

	// Sort types alphabetically for consistent output
	sort.Strings(types)

	return types
}

// ListTypes is a convenience function for accessing the global registry
func ListTypes() []string {
	return DocRegistryInstance.ListTypes()
}

// FormatDocEntry formats a DocEntry for display in the REPL
func FormatDocEntry(entry DocEntry) string {
	var sb strings.Builder

	// Header with name and type
	sb.WriteString(fmt.Sprintf("┌───────────────────────────────────────────────────────────────────┐\n"))
	sb.WriteString(fmt.Sprintf("│ %-52s [%s] │\n", entry.Name, entry.Type))
	sb.WriteString(fmt.Sprintf("├───────────────────────────────────────────────────────────────────┤\n"))

	// Brief description
	sb.WriteString(fmt.Sprintf("│ %-67s │\n", entry.Brief))
	sb.WriteString(fmt.Sprintf("│                                                                    │\n"))

	// Detailed description if available
	if entry.Description != "" {
		sb.WriteString(fmt.Sprintf("│ Description:                                                       │\n"))
		for _, line := range formatLongText(entry.Description, 65, "   ") {
			sb.WriteString(fmt.Sprintf("│ %-67s │\n", line))
		}
		sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
	}

	// Parameters
	if len(entry.Params) > 0 {
		sb.WriteString(fmt.Sprintf("│ Parameters:                                                        │\n"))
		for _, param := range entry.Params {
			name := param.Name
			if param.Optional {
				name = name + " (optional)"
			}

			if param.Default != "" {
				name = name + ", default=" + param.Default
			}

			sb.WriteString(fmt.Sprintf("│   %-65s │\n", name))

			for _, line := range formatLongText(param.Description, 61, "     ") {
				sb.WriteString(fmt.Sprintf("│ %-67s │\n", line))
			}
		}
		sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
	}

	// Return value
	if entry.Returns != "" {
		sb.WriteString(fmt.Sprintf("│ Returns:                                                           │\n"))
		for _, line := range formatLongText(entry.Returns, 65, "   ") {
			sb.WriteString(fmt.Sprintf("│ %-67s │\n", line))
		}
		sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
	}

	// Examples
	if len(entry.Examples) > 0 {
		sb.WriteString(fmt.Sprintf("│ Examples:                                                          │\n"))
		for _, example := range entry.Examples {
			sb.WriteString(fmt.Sprintf("│   %-65s │\n", example))
		}
		sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
	}

	// Related topics
	if len(entry.Related) > 0 {
		sb.WriteString(fmt.Sprintf("│ Related:                                                           │\n"))
		sb.WriteString(fmt.Sprintf("│   %-65s │\n", strings.Join(entry.Related, ", ")))
		sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
	}

	// Module
	if entry.Module != "" {
		sb.WriteString(fmt.Sprintf("│ Module:                                                            │\n"))
		sb.WriteString(fmt.Sprintf("│   %-65s │\n", entry.Module))
	}

	// Footer
	sb.WriteString(fmt.Sprintf("└───────────────────────────────────────────────────────────────────┘\n"))

	return sb.String()
}

// FormatDocList formats a list of DocEntries as a table
func FormatDocList(entries []DocEntry, title string) string {
	var sb strings.Builder

	// Title
	sb.WriteString(fmt.Sprintf("=== %s ===\n\n", title))

	// Table header
	sb.WriteString(fmt.Sprintf("%-20s | %-50s\n", "Name", "Description"))
	sb.WriteString(fmt.Sprintf("%-20s | %-50s\n", strings.Repeat("-", 20), strings.Repeat("-", 50)))

	// Table rows
	for _, entry := range entries {
		// Truncate description if needed
		description := entry.Brief
		if len(description) > 50 {
			description = description[:47] + "..."
		}

		sb.WriteString(fmt.Sprintf("%-20s | %-50s\n", entry.Name, description))
	}

	return sb.String()
}

// formatLongText breaks long text into multiple lines with specified width and prefix
func formatLongText(text string, width int, prefix string) []string {
	// Split text by paragraphs
	paragraphs := strings.Split(text, "\n\n")
	var result []string

	for _, paragraph := range paragraphs {
		// Split paragraph by lines
		lines := strings.Split(paragraph, "\n")
		for _, line := range lines {
			words := strings.Fields(line)
			var currentLine string

			for _, word := range words {
				if len(currentLine)+len(word)+1 <= width {
					if currentLine == "" {
						currentLine = word
					} else {
						currentLine += " " + word
					}
				} else {
					result = append(result, prefix+currentLine)
					currentLine = word
				}
			}

			if currentLine != "" {
				result = append(result, prefix+currentLine)
			}
		}
	}

	return result
}

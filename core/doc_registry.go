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

	formatHeader(&sb, entry.Name, entry.Type)
	formatBrief(&sb, entry.Brief)
	formatDescription(&sb, entry.Description)
	formatParameters(&sb, entry.Params)
	formatReturns(&sb, entry.Returns)
	formatExamples(&sb, entry.Examples)
	formatRelated(&sb, entry.Related)
	formatModule(&sb, entry.Module)
	formatFooter(&sb)

	return sb.String()
}

// formatHeader writes the header section with name and type
func formatHeader(sb *strings.Builder, name, docType string) {
	sb.WriteString(fmt.Sprintf("┌───────────────────────────────────────────────────────────────────┐\n"))
	sb.WriteString(fmt.Sprintf("│ %-52s [%s] │\n", name, docType))
	sb.WriteString(fmt.Sprintf("├───────────────────────────────────────────────────────────────────┤\n"))
}

// formatBrief writes the brief description section
func formatBrief(sb *strings.Builder, brief string) {
	sb.WriteString(fmt.Sprintf("│ %-67s │\n", brief))
	sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
}

// formatDescription writes the detailed description section if available
func formatDescription(sb *strings.Builder, description string) {
	if description == "" {
		return
	}

	sb.WriteString(fmt.Sprintf("│ Description:                                                       │\n"))
	for _, line := range formatLongText(description, 65, "   ") {
		sb.WriteString(fmt.Sprintf("│ %-67s │\n", line))
	}
	sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
}

// formatParameters writes the parameters section if available
func formatParameters(sb *strings.Builder, params []ParamDoc) {
	if len(params) == 0 {
		return
	}

	sb.WriteString(fmt.Sprintf("│ Parameters:                                                        │\n"))
	for _, param := range params {
		formatParameter(sb, param)
	}
	sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
}

// formatParameter writes a single parameter documentation
func formatParameter(sb *strings.Builder, param ParamDoc) {
	name := buildParameterName(param)
	sb.WriteString(fmt.Sprintf("│   %-65s │\n", name))

	for _, line := range formatLongText(param.Description, 61, "     ") {
		sb.WriteString(fmt.Sprintf("│ %-67s │\n", line))
	}
}

// buildParameterName constructs the parameter name with optional and default annotations
func buildParameterName(param ParamDoc) string {
	name := param.Name
	if param.Optional {
		name = name + " (optional)"
	}
	if param.Default != "" {
		name = name + ", default=" + param.Default
	}
	return name
}

// formatReturns writes the return value section if available
func formatReturns(sb *strings.Builder, returns string) {
	if returns == "" {
		return
	}

	sb.WriteString(fmt.Sprintf("│ Returns:                                                           │\n"))
	for _, line := range formatLongText(returns, 65, "   ") {
		sb.WriteString(fmt.Sprintf("│ %-67s │\n", line))
	}
	sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
}

// formatExamples writes the examples section if available
func formatExamples(sb *strings.Builder, examples []string) {
	if len(examples) == 0 {
		return
	}

	sb.WriteString(fmt.Sprintf("│ Examples:                                                          │\n"))
	for _, example := range examples {
		sb.WriteString(fmt.Sprintf("│   %-65s │\n", example))
	}
	sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
}

// formatRelated writes the related topics section if available
func formatRelated(sb *strings.Builder, related []string) {
	if len(related) == 0 {
		return
	}

	sb.WriteString(fmt.Sprintf("│ Related:                                                           │\n"))
	sb.WriteString(fmt.Sprintf("│   %-65s │\n", strings.Join(related, ", ")))
	sb.WriteString(fmt.Sprintf("│                                                                    │\n"))
}

// formatModule writes the module section if available
func formatModule(sb *strings.Builder, module string) {
	if module == "" {
		return
	}

	sb.WriteString(fmt.Sprintf("│ Module:                                                            │\n"))
	sb.WriteString(fmt.Sprintf("│   %-65s │\n", module))
}

// formatFooter writes the footer line
func formatFooter(sb *strings.Builder) {
	sb.WriteString(fmt.Sprintf("└───────────────────────────────────────────────────────────────────┘\n"))
}

// RegisterDocStrings registers a map of symbol names to documentation strings
func RegisterDocStrings(docs map[string]string) {
	for name, docString := range docs {
		// Simple doc entry from raw string
		entry := DocEntry{
			Name:        name,
			Type:        "function",
			Brief:       fmt.Sprintf("%s function", name),
			Description: docString,
		}
		RegisterDoc(entry)
	}
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

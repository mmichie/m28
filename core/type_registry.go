package core

// This file now only contains the init() function that calls the refactored type registry
// The old 1152-line InitializeTypeRegistry god function has been removed and split into:
// - type_registry_refactored.go (orchestrator)
// - type_registry_primitives.go (number, string, bool, nil, symbol)
// - type_registry_collections.go (list, dict, tuple, set)
// - type_registry_objects.go (class, module)
// - type_registry_file.go (file I/O)
// - type_registry_concurrent.go (task, channel, generator)

// Helper to initialize types on startup
func init() {
	// Switch to refactored version
	InitializeTypeRegistryRefactored()
	// Add additional string methods after types are registered
	InitStringMethods()
	// Add additional list methods after types are registered
	InitListMethods()
	// Add additional dict methods after types are registered
	InitDictMethods()
	// Add additional set methods after types are registered
	InitSetMethods()
}

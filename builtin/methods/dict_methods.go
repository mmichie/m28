package methods

import (
	"github.com/mmichie/m28/core"
)

// RegisterDictMethods registers dict-specific methods
// Note: These methods are actually registered via core.InitDictMethods()
// This function serves as documentation and ensures the methods are initialized
func RegisterDictMethods(ctx *core.Context) {
	// Dict methods are registered directly on the dict type descriptor
	// in core/dict_methods.go via InitDictMethods()
	// This is called during type system initialization

	// Methods included:
	// - __len__: Return number of key-value pairs
	// - keys: Return list of keys
	// - values: Return list of values
	// - items: Return list of (key, value) tuples
	// - get: Get value with default
	// - pop: Remove and return value
	// - update: Update with other dict
	// - clear: Remove all items
	// - copy: Return shallow copy
	// - setdefault: Set default value

	// Ensure dict methods are initialized
	core.InitDictMethods()
}

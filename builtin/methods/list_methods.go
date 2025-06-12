package methods

import (
	"github.com/mmichie/m28/core"
)

// RegisterListMethods registers list-specific methods
// Note: These methods are actually registered via core.InitListMethods()
// This function serves as documentation and ensures the methods are initialized
func RegisterListMethods(ctx *core.Context) {
	// List methods are registered directly on the list type descriptor
	// in core/list_methods.go via InitListMethods()
	// This is called during type system initialization

	// Methods included:
	// - __len__: Return list length
	// - append: Add item to end
	// - extend: Extend list with iterable
	// - insert: Insert item at index
	// - remove: Remove first occurrence
	// - pop: Remove and return item
	// - clear: Remove all items
	// - index: Find item index
	// - count: Count occurrences
	// - sort: Sort list in place
	// - reverse: Reverse list in place
	// - copy: Return shallow copy

	// Ensure list methods are initialized
	core.InitListMethods()
}

package methods

import (
	"github.com/mmichie/m28/core"
)

// RegisterSetMethods registers set-specific methods
// Note: These methods are actually registered via core.InitSetMethods()
// This function serves as documentation and ensures the methods are initialized
func RegisterSetMethods(ctx *core.Context) {
	// Set methods are registered directly on the set type descriptor
	// in core/set_methods.go via InitSetMethods()
	// This is called during type system initialization

	// Methods included:
	// - __len__: Return number of elements
	// - add: Add element to set
	// - remove: Remove element (error if not found)
	// - discard: Remove element (no error if not found)
	// - pop: Remove and return arbitrary element
	// - clear: Remove all elements
	// - copy: Return shallow copy
	// - union: Return union with other sets
	// - intersection: Return intersection with other sets
	// - difference: Return difference from other sets
	// - symmetric_difference: Return symmetric difference
	// - issubset: Check if subset
	// - issuperset: Check if superset
	// - isdisjoint: Check if disjoint
	// - update: Update with union
	// - intersection_update: Update with intersection
	// - difference_update: Update with difference
	// - symmetric_difference_update: Update with symmetric difference

	// Ensure set methods are initialized
	core.InitSetMethods()
}

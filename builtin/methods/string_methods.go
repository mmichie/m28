package methods

import (
	"github.com/mmichie/m28/core"
)

// RegisterStringMethods registers string-specific methods
// Note: These methods are actually registered via core.InitStringMethods()
// This function serves as documentation and ensures the methods are initialized
func RegisterStringMethods(ctx *core.Context) {
	// String methods are registered directly on the string type descriptor
	// in core/string_methods.go via InitStringMethods()
	// This is called during type system initialization

	// Methods included:
	// - __len__: Return character count
	// - replace: Replace occurrences
	// - startswith: Check prefix
	// - endswith: Check suffix
	// - find: Find substring
	// - join: Join strings
	// - capitalize: Capitalize first letter
	// - title: Title case
	// - count: Count occurrences
	// - format: Format string
	// - lstrip/rstrip/strip: Remove characters
	// - isdigit/isalpha/isspace: Character tests
	// - index: Find with error
	// - split: Split into list
	// - upper/lower: Case conversion

	// Ensure string methods are initialized
	core.InitStringMethods()
}

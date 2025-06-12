package modules

import (
	"github.com/mmichie/m28/core"
)

// RegisterJSONModule creates and registers the JSON module
func RegisterJSONModule(ctx *core.Context) {
	// Note: The actual implementation is in builtin/json.go
	// This function is called from the new registry system
	// TODO: Move the implementation here to avoid circular imports
}

package modules

import (
	"github.com/mmichie/m28/core"
)

// RegisterAsyncModule creates and registers async/concurrent functions
func RegisterAsyncModule(ctx *core.Context) {
	// Note: The actual implementation is in builtin/async.go
	// This function is called from the new registry system
	// TODO(M28-a357): Move the implementation here to avoid circular imports
}

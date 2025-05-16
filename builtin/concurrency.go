package builtin

import (
	"github.com/mmichie/m28/concurrency"
	"github.com/mmichie/m28/core"
)

// RegisterConcurrencyBuiltins registers concurrency-related functions
func RegisterConcurrencyBuiltins() {
	// Register context-related functions
	core.RegisterBuiltin("context-background", concurrency.ContextBackgroundFunc)
	core.RegisterBuiltin("context-with-cancel", concurrency.ContextWithCancelFunc)
	core.RegisterBuiltin("context-with-timeout", concurrency.ContextWithTimeoutFunc)
	core.RegisterBuiltin("context-done", concurrency.ContextDoneFunc)
	core.RegisterBuiltin("context-canceled?", concurrency.ContextCanceledFunc)
	core.RegisterBuiltin("context-error", concurrency.ContextErrorFunc)
}

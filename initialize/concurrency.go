// Package initialize provides initialization functions for M28 components
package initialize

import (
	"github.com/mmichie/m28/concurrency"
)

// InitializeConcurrency sets up all concurrency features
// This is called from main.go after the REPL is created
func InitializeConcurrency() {
	// Register concurrency builtin functions
	concurrency.RegisterConcurrencyFunctions()

	// Register concurrency form handlers
	RegisterConcurrencyFormHandlers()
}

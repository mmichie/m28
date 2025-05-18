// Package initialize provides initialization functions for M28 components.
// This package is used to break import cycles between packages.
package initialize

import "github.com/mmichie/m28/core"

// InitializeAll initializes all M28 components
func InitializeAll() {
	// Initialize concurrency package
	InitializeConcurrency()
}

// EnableDebugMode enables debug output in the interpreter
func EnableDebugMode() {
	core.Debug = true
}

// DisableDebugMode disables debug output in the interpreter
func DisableDebugMode() {
	core.Debug = false
}

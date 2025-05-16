// Package initialize provides initialization functions for M28 components.
// This package is used to break import cycles between packages.
package initialize

// InitializeAll initializes all M28 components
func InitializeAll() {
	// Initialize concurrency package
	InitializeConcurrency()
}

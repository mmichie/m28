package core

// Debug mode flag - set to true to enable debug logging
var Debug = false

// EnableDebug enables debug mode
func EnableDebug() {
	Debug = true
}

// DisableDebug disables debug mode
func DisableDebug() {
	Debug = false
}

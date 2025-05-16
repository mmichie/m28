package concurrency

// No imports needed for now

// RegisterConcurrencyFunctions registers all concurrency-related builtin functions
func RegisterConcurrencyFunctions() {
	// Register context functions only for now
	RegisterContextFunctions()

	// These will be implemented later
	// RegisterChannelFunctions()
	// RegisterMutexFunctions()
	// RegisterWaitGroupFunctions()
}

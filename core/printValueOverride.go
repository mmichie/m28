package core

// PrintValueProvider is an interface for types that can provide a custom string representation
// for the PrintValue function
type PrintValueProvider interface {
	PrintValue() string
}

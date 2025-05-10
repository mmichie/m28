package core

// Evaluable is the interface for values that can be evaluated
// This interface provides a unified protocol for evaluation across all types
type Evaluable interface {
	// Eval evaluates the value in the given environment with the evaluator
	Eval(e Evaluator, env Environment) (LispValue, error)
}

// Invokable is the interface for values that can be invoked as functions
// This separates the call behavior from evaluation
type Invokable interface {
	// Invoke calls the value with the given arguments
	Invoke(args []LispValue, e Evaluator, env Environment) (LispValue, error)
}

// ObjectMember represents a member (attribute or method) of an object
type ObjectMember interface {
	// Get retrieves the member value
	Get(obj LispValue, e Evaluator, env Environment) (LispValue, error)

	// Set sets the member value
	Set(obj LispValue, value LispValue, e Evaluator, env Environment) error

	// Call calls the member as a method
	Call(obj LispValue, args []LispValue, e Evaluator, env Environment) (LispValue, error)
}

// MemberAccessor provides unified access to object members
// This is a central protocol for accessing properties and methods
type MemberAccessor interface {
	// GetMember gets a member by name
	GetMember(name string, e Evaluator, env Environment) (ObjectMember, bool)

	// SetMember sets a member by name
	SetMember(name string, value LispValue, e Evaluator, env Environment) error
}

// EvaluatorAware defines an interface for objects that need access to the evaluator
type EvaluatorAware interface {
	// SetEvaluator provides the evaluator to the object
	SetEvaluator(e Evaluator)

	// GetEvaluator retrieves the current evaluator
	GetEvaluator() Evaluator

	// GetMember gets a member (attribute or method) with evaluator context
	// This is the preferred method for accessing object properties and methods
	GetMember(name string, eval Evaluator, env Environment) (LispValue, error)

	// SetMember sets a member value with evaluator context
	// This is the preferred method for setting object properties
	SetMember(name string, value LispValue, eval Evaluator, env Environment) error
}

// EvaluatorProvider is a simple struct that can be embedded to provide
// evaluator awareness to any struct
type EvaluatorProvider struct {
	Eval Evaluator
}

// SetEvaluator sets the evaluator
func (ep *EvaluatorProvider) SetEvaluator(e Evaluator) {
	ep.Eval = e
}

// GetEvaluator gets the current evaluator
func (ep *EvaluatorProvider) GetEvaluator() Evaluator {
	return ep.Eval
}

// DotAccessibleEx is an extended DotAccessible interface that includes evaluator context
type DotAccessibleEx interface {
	DotAccessible
	EvaluatorAware
}

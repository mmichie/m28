package core

import (
	"fmt"
	"strings"
)

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

// MappableEnvironment defines an interface for environments that can provide a map of their symbols
type MappableEnvironment interface {
	// GetSymbolMap returns a map of all symbols in the environment
	GetSymbolMap() map[LispSymbol]LispValue
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

// Make Lambda implement the Applicable interface
func (l *Lambda) Apply(e Evaluator, args []LispValue, callEnv Environment) (LispValue, error) {
	// Create a new environment for the function call
	lambdaEnv := callEnv.NewEnvironment(l.Env)

	// Process arguments (binding parameters to values)
	if !processLambdaArgs(l, args, lambdaEnv) {
		return nil, fmt.Errorf("parameter mismatch in lambda call")
	}

	// Evaluate the function body in the new environment
	result, err := e.Eval(l.Body, lambdaEnv)
	if err != nil {
		// Handle special flow control signals if implemented
		// The actual logic depends on how flow control is implemented in the language
		return nil, err
	}

	return result, nil
}

// processLambdaArgs binds function arguments to parameters in the lambda environment
// Returns true if successful, false if there's a parameter mismatch
func processLambdaArgs(lambda *Lambda, args []LispValue, lambdaEnv Environment) bool {
	// Extract keyword arguments and count positional arguments
	keywordArgs := make(map[string]LispValue)
	positionalArgs := make([]LispValue, 0, len(args))

	for _, arg := range args {
		// Check if it's a keyword argument (string in the form "name=value")
		if strArg, ok := arg.(string); ok && strings.Contains(strArg, "=") {
			// Parse the keyword argument
			parts := strings.SplitN(strArg, "=", 2)
			if len(parts) == 2 {
				keywordName := parts[0]
				keywordValue := parts[1]
				// Store as string for now, may need more complex parsing later
				keywordArgs[keywordName] = keywordValue
			}
		} else {
			// It's a positional argument
			positionalArgs = append(positionalArgs, arg)
		}
	}

	// Bind positional arguments to parameters
	paramCount := len(lambda.Params)
	argCount := len(positionalArgs)

	// Check if we have too many arguments
	if argCount > paramCount && !hasVariadicParam(lambda.Params) {
		return false
	}

	// Bind positional parameters
	for i, param := range lambda.Params {
		if i < argCount {
			// Bind argument to parameter
			lambdaEnv.Define(param, positionalArgs[i])
		} else if defaultVal, hasDefault := lambda.DefaultValues[param]; hasDefault {
			// Use default value
			lambdaEnv.Define(param, defaultVal)
		} else {
			// Parameter without argument or default value
			lambdaEnv.Define(param, PythonicNone{})
		}
	}

	// Apply keyword arguments (overriding positional bindings if applicable)
	for name, value := range keywordArgs {
		paramName := LispSymbol(name)
		// Check if this is a valid parameter name
		found := false
		for _, param := range lambda.Params {
			if param == paramName {
				found = true
				lambdaEnv.Define(paramName, value)
				break
			}
		}
		if !found {
			// Keyword argument doesn't match any parameter
			return false
		}
	}

	return true
}

// hasVariadicParam checks if the parameter list contains a variadic parameter
func hasVariadicParam(params []LispSymbol) bool {
	for _, param := range params {
		if strings.HasPrefix(string(param), "*") {
			return true
		}
	}
	return false
}

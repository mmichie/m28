package env

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/core"
)

// Debug flag for verbose logging
var Debug bool = false

// SymbolCollector is an interface for environments that can iterate over their symbols
type SymbolCollector interface {
	ForEachSymbol(func(symbol string, value core.Value))
}

// Environment represents a Lisp environment
type Environment struct {
	vars      map[string]core.Value
	outer     *Environment
	evaluator interface{} // Added evaluator reference for evaluator-aware objects
}

// NewEnvironment creates a new environment
func NewEnvironment(outer *Environment) *Environment {
	env := &Environment{
		vars:      make(map[string]core.Value),
		outer:     outer,
		evaluator: nil,
	}

	// Inherit evaluator from outer environment if available
	if outer != nil && outer.evaluator != nil {
		env.evaluator = outer.evaluator
	}

	env.Define("None", core.Nil)
	env.Define("True", core.BoolValue(true))
	env.Define("False", core.BoolValue(false))
	env.Define("__debug__", core.BoolValue(true))
	env.Define("Ellipsis", core.Ellipsis)

	// No need to register type constants in this version

	return env
}

// Get retrieves a value from the environment
func (e *Environment) Get(symbol string) (core.Value, bool) {
	value, ok := e.vars[symbol]
	if !ok && e.outer != nil {
		// Look in outer environment if not found in current
		return e.outer.Get(symbol)
	}
	return value, ok
}

// Set sets a value in the environment
func (e *Environment) Set(symbol string, value core.Value) {
	e.vars[symbol] = value
}

// SetEvaluator sets the evaluator for this environment
func (e *Environment) SetEvaluator(eval interface{}) {
	e.evaluator = eval
}

// GetOuter returns the outer environment
func (e *Environment) GetOuter() *Environment {
	return e.outer
}

// GetEvaluator gets the current evaluator
func (e *Environment) GetEvaluator() interface{} {
	return e.evaluator
}

// Define creates a new binding in the current environment
func (e *Environment) Define(symbol string, value core.Value) {
	e.vars[symbol] = value
}

// SetMutable sets a value in the environment, searching up the chain
func (e *Environment) SetMutable(symbol string, value core.Value) bool {
	if _, ok := e.vars[symbol]; ok {
		e.vars[symbol] = value
		return true
	}
	if e.outer != nil {
		return e.outer.SetMutable(symbol, value)
	}
	return false
}

func (e *Environment) NewEnvironment(outer *Environment) *Environment {
	newEnv := NewEnvironment(outer)
	return newEnv
}

func (e *Environment) String() string {
	return e.StringWithDepth(0)
}

func (e *Environment) StringWithDepth(depth int) string {
	if depth > 2 { // Limit the depth to 2 levels
		return "...\n"
	}

	var sb strings.Builder
	sb.WriteString(fmt.Sprintf("%sEnvironment contents:\n", strings.Repeat("  ", depth)))
	count := 0
	for symbol, value := range e.vars {
		if count >= 10 { // Limit to 10 entries per level
			sb.WriteString(fmt.Sprintf("%s... (%d more entries)\n", strings.Repeat("  ", depth+1), len(e.vars)-count))
			break
		}
		sb.WriteString(fmt.Sprintf("%s%v: %T\n", strings.Repeat("  ", depth+1), symbol, value))
		count++
	}
	if e.outer != nil {
		sb.WriteString(fmt.Sprintf("%sOuter environment:\n", strings.Repeat("  ", depth)))
		sb.WriteString(e.outer.StringWithDepth(depth + 1))
	}
	return sb.String()
}

// ForEachSymbol calls the provided function for each symbol in the environment
func (e *Environment) ForEachSymbol(fn func(symbol string, value core.Value)) {
	for symbol, value := range e.vars {
		fn(symbol, value)
	}
}

// GetSymbolMap returns a map of all symbols in the environment
func (e *Environment) GetSymbolMap() map[string]core.Value {
	// Create a copy of the symbol map to prevent modification
	symbolMap := make(map[string]core.Value)
	for symbol, value := range e.vars {
		symbolMap[symbol] = value
	}
	return symbolMap
}

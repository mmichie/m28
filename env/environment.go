package env

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/builtin"
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

	// No need to register type constants in this version

	// This is a reasonable default for most environment needs
	// This ensures builtins are always available
	env.SetupBuiltins()

	return env
}

// Get retrieves a value from the environment
func (e *Environment) Get(symbol string) (core.Value, bool) {
	// Debug the lookup
	if Debug {
		fmt.Printf("DEBUG Get: Looking up symbol '%s' in environment %p\n", symbol, e)
	}

	value, ok := e.vars[symbol]
	if !ok && e.outer != nil {
		// Look in outer environment if not found in current
		if Debug {
			fmt.Printf("DEBUG Get: Symbol '%s' not found in current environment, checking outer\n", symbol)
		}
		return e.outer.Get(symbol)
	}

	if ok {
		if Debug {
			fmt.Printf("DEBUG Get: Found symbol '%s' = %v (type %T)\n", symbol, value, value)
		}

		// No special handling needed for now
	} else {
		if Debug {
			fmt.Printf("DEBUG Get: Symbol '%s' not found\n", symbol)
		}
	}

	return value, ok
}

// Set sets a value in the environment
func (e *Environment) Set(symbol string, value core.Value) {
	if Debug {
		fmt.Printf("DEBUG Set: Setting symbol '%s' = %v (type %T) in environment %p\n", symbol, value, value, e)
	}

	// No special handling needed for now

	e.vars[symbol] = value

	// Verification
	if Debug {
		if storedValue, exists := e.vars[symbol]; exists {
			fmt.Printf("DEBUG Set: Verified symbol '%s' exists after setting, value: %v\n", symbol, storedValue)
		} else {
			fmt.Printf("DEBUG Set: CRITICAL - Symbol '%s' not found after setting\n", symbol)
		}
	}
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
	if Debug {
		fmt.Printf("DEBUG Define: Defining symbol '%s' = %v (type %T) in environment %p\n", symbol, value, value, e)
	}

	// No special handling needed for now

	e.vars[symbol] = value

	// Verification
	if Debug {
		if storedValue, exists := e.vars[symbol]; exists {
			fmt.Printf("DEBUG Define: Verified symbol '%s' exists after defining, value: %v\n", symbol, storedValue)
		} else {
			fmt.Printf("DEBUG Define: CRITICAL - Symbol '%s' not found after defining\n", symbol)
		}
	}
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

// SetupBuiltins initializes the environment with builtin functions
func (e *Environment) SetupBuiltins() {
	// Register builtin functions by calling the main registry function
	builtin.RegisterAllBuiltins(&core.Context{Vars: make(map[string]core.Value)})

	// Standard exceptions and type constants would be registered here
	// but they're now handled by the core.Context system

	// Register special form functions in builtins
	// Removed direct special_forms dependency to avoid import cycle
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

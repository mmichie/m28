package env

import (
	"fmt"
	"strings"

	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
)

// SymbolCollector is an interface for environments that can iterate over their symbols
type SymbolCollector interface {
	ForEachSymbol(func(symbol core.LispSymbol, value core.LispValue))
}

// Environment represents a Lisp environment
type Environment struct {
	vars  map[core.LispSymbol]core.LispValue
	outer core.Environment
}

// NewEnvironment creates a new environment
func NewEnvironment(outer core.Environment) *Environment {
	env := &Environment{
		vars:  make(map[core.LispSymbol]core.LispValue),
		outer: outer,
	}

	env.Define(core.LispSymbol("None"), core.PythonicNone{})
	env.Define(core.LispSymbol("True"), core.PythonicBool(true))
	env.Define(core.LispSymbol("False"), core.PythonicBool(false))

	// This is a reasonable default for most environment needs
	// This ensures builtins are always available
	env.SetupBuiltins()

	return env
}

// Get retrieves a value from the environment
func (e *Environment) Get(symbol core.LispSymbol) (core.LispValue, bool) {
	value, ok := e.vars[symbol]
	if !ok && e.outer != nil {
		return e.outer.Get(symbol)
	}
	if ok {
	} else {
	}
	return value, ok
}

// Set sets a value in the environment
func (e *Environment) Set(symbol core.LispSymbol, value core.LispValue) {
	e.vars[symbol] = value
}

// Define creates a new binding in the current environment
func (e *Environment) Define(symbol core.LispSymbol, value core.LispValue) {
	e.vars[symbol] = value
}

// SetMutable sets a value in the environment, searching up the chain
func (e *Environment) SetMutable(symbol core.LispSymbol, value core.LispValue) bool {
	if _, ok := e.vars[symbol]; ok {
		e.vars[symbol] = value
		return true
	}
	if e.outer != nil {
		return e.outer.SetMutable(symbol, value)
	}
	return false
}

func (e *Environment) NewEnvironment(outer core.Environment) core.Environment {
	newEnv := NewEnvironment(outer)
	return newEnv
}

// SetupBuiltins initializes the environment with builtin functions
func (e *Environment) SetupBuiltins() {
	builtin.RegisterArithmeticFuncs()
	builtin.RegisterPythonBuiltins()
	for name, fn := range core.BuiltinFuncs {
		e.Set(core.LispSymbol(name), fn)
	}

	// Register standard Python exceptions in the environment
	for name, exception := range core.StandardExceptions {
		e.Set(core.LispSymbol(name), exception)
	}

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
		if outerEnv, ok := e.outer.(*Environment); ok {
			sb.WriteString(outerEnv.StringWithDepth(depth + 1))
		} else {
			sb.WriteString("<non-Environment outer>\n")
		}
	}
	return sb.String()
}

// ForEachSymbol calls the provided function for each symbol in the environment
func (e *Environment) ForEachSymbol(fn func(symbol core.LispSymbol, value core.LispValue)) {
	for symbol, value := range e.vars {
		fn(symbol, value)
	}
}

// GetSymbolMap returns a map of all symbols in the environment
func (e *Environment) GetSymbolMap() map[core.LispSymbol]core.LispValue {
	// Create a copy of the symbol map to prevent modification
	symbolMap := make(map[core.LispSymbol]core.LispValue)
	for symbol, value := range e.vars {
		symbolMap[symbol] = value
	}
	return symbolMap
}

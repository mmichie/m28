package env

import (
	"github.com/mmichie/m28/builtin"
	"github.com/mmichie/m28/core"
)

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

	// Add nil to the environment
	env.Define(core.LispSymbol("nil"), core.Nil{})
	env.Define(core.LispSymbol("t"), true)

	return env
}

// Get retrieves a value from the environment
func (e *Environment) Get(symbol core.LispSymbol) (core.LispValue, bool) {
	value, ok := e.vars[symbol]
	if !ok && e.outer != nil {
		return e.outer.Get(symbol)
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

// NewEnvironment creates a new child environment
func (e *Environment) NewEnvironment(outer core.Environment) core.Environment {
	return NewEnvironment(outer)
}

// SetupBuiltins initializes the environment with builtin functions
func (e *Environment) SetupBuiltins() {
	builtin.RegisterArithmeticFuncs()
	builtin.RegisterStringOps()
	builtin.RegisterIOOps()
	builtin.RegisterTestOps()
	builtin.RegisterHashTableOps()
	builtin.RegisterSequenceOps()
	for name, fn := range core.BuiltinFuncs {
		e.Set(core.LispSymbol(name), fn)
	}
}

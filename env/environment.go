package env

import (
	"fmt"
	"strings"

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

	env.Define(core.LispSymbol("None"), core.PythonicNone{})
	env.Define(core.LispSymbol("True"), core.PythonicBool(true))
	env.Define(core.LispSymbol("False"), core.PythonicBool(false))

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

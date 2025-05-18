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
	vars      map[core.LispSymbol]core.LispValue
	outer     core.Environment
	evaluator core.Evaluator // Added evaluator reference for evaluator-aware objects
}

// NewEnvironment creates a new environment
func NewEnvironment(outer core.Environment) *Environment {
	env := &Environment{
		vars:      make(map[core.LispSymbol]core.LispValue),
		outer:     outer,
		evaluator: nil,
	}

	// Inherit evaluator from outer environment if available
	if outer != nil {
		if outerEnv, ok := outer.(*Environment); ok && outerEnv.evaluator != nil {
			env.evaluator = outerEnv.evaluator
		}
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
	// Debug the lookup
	fmt.Printf("DEBUG Get: Looking up symbol '%s' in environment %p\n", symbol, e)

	value, ok := e.vars[symbol]
	if !ok && e.outer != nil {
		// Look in outer environment if not found in current
		fmt.Printf("DEBUG Get: Symbol '%s' not found in current environment, checking outer\n", symbol)
		return e.outer.Get(symbol)
	}

	if ok {
		fmt.Printf("DEBUG Get: Found symbol '%s' = %v (type %T)\n", symbol, value, value)

		// Special handling for dictionaries to ensure evaluator is set
		if dict, isDictionary := value.(*core.PythonicDict); isDictionary && e.evaluator != nil {
			fmt.Printf("DEBUG Get: Setting evaluator on dictionary for symbol '%s'\n", symbol)
			dict.SetEvaluator(e.evaluator)
		}
	} else {
		fmt.Printf("DEBUG Get: Symbol '%s' not found\n", symbol)
	}

	return value, ok
}

// Set sets a value in the environment
func (e *Environment) Set(symbol core.LispSymbol, value core.LispValue) {
	fmt.Printf("DEBUG Set: Setting symbol '%s' = %v (type %T) in environment %p\n", symbol, value, value, e)

	// Special handling for evaluator-aware types
	if evalAware, ok := value.(core.EvaluatorAware); ok && e.evaluator != nil {
		fmt.Printf("DEBUG Set: Value is EvaluatorAware, setting evaluator for '%s'\n", symbol)
		evalAware.SetEvaluator(e.evaluator)
	}

	// Special handling for dictionaries to ensure they get the evaluator
	if dict, ok := value.(*core.PythonicDict); ok && e.evaluator != nil {
		fmt.Printf("DEBUG Set: Dictionary detected for symbol '%s', ensuring evaluator is set\n", symbol)
		dict.SetEvaluator(e.evaluator)
	}

	e.vars[symbol] = value

	// Verification
	if storedValue, exists := e.vars[symbol]; exists {
		fmt.Printf("DEBUG Set: Verified symbol '%s' exists after setting, value: %v\n", symbol, storedValue)
	} else {
		fmt.Printf("DEBUG Set: CRITICAL - Symbol '%s' not found after setting\n", symbol)
	}
}

// SetEvaluator sets the evaluator for this environment
func (e *Environment) SetEvaluator(eval core.Evaluator) {
	e.evaluator = eval
}

// GetEvaluator gets the current evaluator
func (e *Environment) GetEvaluator() core.Evaluator {
	return e.evaluator
}

// Define creates a new binding in the current environment
func (e *Environment) Define(symbol core.LispSymbol, value core.LispValue) {
	fmt.Printf("DEBUG Define: Defining symbol '%s' = %v (type %T) in environment %p\n", symbol, value, value, e)

	// Special handling for evaluator-aware types
	if evalAware, ok := value.(core.EvaluatorAware); ok && e.evaluator != nil {
		fmt.Printf("DEBUG Define: Value is EvaluatorAware, setting evaluator for '%s'\n", symbol)
		evalAware.SetEvaluator(e.evaluator)
	}

	// Special handling for dictionaries to ensure they get the evaluator
	if dict, ok := value.(*core.PythonicDict); ok && e.evaluator != nil {
		fmt.Printf("DEBUG Define: Dictionary detected for symbol '%s', ensuring evaluator is set\n", symbol)
		dict.SetEvaluator(e.evaluator)
	}

	e.vars[symbol] = value

	// Verification
	if storedValue, exists := e.vars[symbol]; exists {
		fmt.Printf("DEBUG Define: Verified symbol '%s' exists after defining, value: %v\n", symbol, storedValue)
	} else {
		fmt.Printf("DEBUG Define: CRITICAL - Symbol '%s' not found after defining\n", symbol)
	}
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

	// Register type constants in the environment
	// We need to keep track of any type constants that shadow builtins
	// and store them in a different name
	for name, value := range core.TypeConstants {
		// Skip 'list' as it conflicts with the built-in list function
		if name == "list" {
			// Register as 'type_list' instead
			e.Set(core.LispSymbol("type_list"), value)
		} else {
			e.Set(core.LispSymbol(name), value)
		}
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

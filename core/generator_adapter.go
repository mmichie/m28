package core

// GeneratorAdapter implements the ObjProtocol for Generator types
// This makes generators usable with the unified object protocol
type GeneratorAdapter struct {
	Gen *Generator
}

// NewGeneratorAdapter creates a new adapter for a Generator
func NewGeneratorAdapter(gen *Generator) *GeneratorAdapter {
	return &GeneratorAdapter{Gen: gen}
}

// GetProp implements the ObjProtocol.GetProp method
func (ga *GeneratorAdapter) GetProp(name string) (LispValue, bool) {
	// Support properties like current_value, is_done, etc.
	switch name {
	case "current_value":
		return ga.Gen.CurrentValue, true
	case "is_done":
		return ga.Gen.IsDone, true
	case "is_started":
		return ga.Gen.IsStarted, true
	case "next":
		// Return a function that can be called to advance the generator
		return BuiltinFunc(func(args []LispValue, env Environment) (LispValue, error) {
			// We need an evaluator to advance the generator
			if evalValue, exists := env.Get("EVALUATOR"); exists {
				if eval, ok := evalValue.(Evaluator); ok {
					return ga.Gen.NextWithEval(eval)
				}
			}
			return ga.Gen.Next() // Will return an error about needing evaluator
		}), true
	}
	return nil, false
}

// SetProp implements the ObjProtocol.SetProp method
func (ga *GeneratorAdapter) SetProp(name string, value LispValue) error {
	// Generators don't support setting properties
	return ErrDotNoPropertyf(name)
}

// HasMethodP implements the ObjProtocol.HasMethodP method
func (ga *GeneratorAdapter) HasMethodP(name string) bool {
	// Only support the next method for now
	return name == "next" || name == "__iter__"
}

// CallMethodP implements the ObjProtocol.CallMethodP method
func (ga *GeneratorAdapter) CallMethodP(name string, args []LispValue, eval Evaluator, env Environment) (LispValue, error) {
	switch name {
	case "next":
		return ga.Gen.NextWithEval(eval)
	case "__iter__":
		return ga.Gen, nil // Return self for iteration
	}
	return nil, ErrDotNoMethodf(name)
}

// Ensure Generator type implements AdaptableLispValue by extending it
func (g *Generator) AsObject() ObjProtocol {
	return NewGeneratorAdapter(g)
}

// Make sure Generator implements AdaptableLispValue interface
var _ AdaptableLispValue = (*Generator)(nil)
package core

import (
	"fmt"
)

// Generator represents a generator object
type Generator struct {
	BaseObject
	name     string           // Generator function name
	state    GeneratorState   // Current state
	context  *Context         // Execution context
	code     Value            // Generator body
	yielded  Value            // Last yielded value
	locals   map[string]Value // Local variables
	position int              // Current position in execution
	registry *MethodRegistry  // Method registry
}

// GeneratorState represents the state of a generator
type GeneratorState int

const (
	GeneratorCreated GeneratorState = iota
	GeneratorSuspended
	GeneratorRunning
	GeneratorCompleted
)

// NewGenerator creates a new generator
func NewGenerator(name string, code Value, ctx *Context) *Generator {
	g := &Generator{
		BaseObject: *NewBaseObject(Type("generator")),
		name:       name,
		state:      GeneratorCreated,
		context:    NewContext(ctx),
		code:       code,
		locals:     make(map[string]Value),
		position:   0,
	}

	// Initialize the method registry
	g.registry = g.createRegistry()

	return g
}

// Type returns the generator type
func (g *Generator) Type() Type {
	return Type("generator")
}

// String returns the string representation
func (g *Generator) String() string {
	stateStr := "?"
	switch g.state {
	case GeneratorCreated:
		stateStr = "created"
	case GeneratorSuspended:
		stateStr = "suspended"
	case GeneratorRunning:
		stateStr = "running"
	case GeneratorCompleted:
		stateStr = "completed"
	}

	if g.name != "" {
		return fmt.Sprintf("<generator object %s at %p, state=%s>", g.name, g, stateStr)
	}
	return fmt.Sprintf("<generator object at %p, state=%s>", g, stateStr)
}

// createRegistry sets up all methods for generator
func (g *Generator) createRegistry() *MethodRegistry {
	registry := NewMethodRegistry()

	// Register methods
	registry.RegisterMethods(
		// __next__ / next method
		MakeMethod("__next__", 0, "Retrieve the next value from the generator", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			gen, err := TypedReceiver[*Generator](receiver, "__next__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("__next__", args, 0); err != nil {
				return nil, err
			}
			return gen.Next()
		}),

		// Also register as "next" for compatibility
		MakeMethod("next", 0, "Retrieve the next value from the generator", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			gen, err := TypedReceiver[*Generator](receiver, "next")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("next", args, 0); err != nil {
				return nil, err
			}
			return gen.Next()
		}),

		// send method
		MakeMethod("send", 1, "Send a value into the generator", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			gen, err := TypedReceiver[*Generator](receiver, "send")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("send", args, 1); err != nil {
				return nil, err
			}
			return gen.Send(args[0])
		}),

		// close method
		MakeMethod("close", 0, "Close the generator", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			gen, err := TypedReceiver[*Generator](receiver, "close")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("close", args, 0); err != nil {
				return nil, err
			}
			return gen.Close()
		}),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (g *Generator) GetRegistry() *MethodRegistry {
	return g.registry
}

// GetBaseObject implements AttributeProvider
func (g *Generator) GetBaseObject() *BaseObject {
	return &g.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (g *Generator) GetAttr(name string) (Value, bool) {
	return GetAttrWithRegistry(g, name)
}

// Next advances the generator and returns the next value
func (g *Generator) Next() (Value, error) {
	if g.state == GeneratorCompleted {
		return nil, &StopIteration{}
	}

	if g.state == GeneratorCreated {
		g.state = GeneratorSuspended
	}

	// This would be implemented by the evaluator
	// For now, return a placeholder
	return nil, &StopIteration{Message: "generator implementation pending"}
}

// Send sends a value into the generator
func (g *Generator) Send(value Value) (Value, error) {
	if g.state == GeneratorCreated && value != Nil {
		return nil, fmt.Errorf("can't send non-None value to a just-started generator")
	}

	// Store the sent value for yield expressions
	g.yielded = value

	return g.Next()
}

// Close closes the generator
func (g *Generator) Close() (Value, error) {
	g.state = GeneratorCompleted
	return Nil, nil
}

// Iterator returns an iterator for the generator
func (g *Generator) Iterator() Iterator {
	return &GeneratorIterator{generator: g}
}

// GetState returns the generator's current state
func (g *Generator) GetState() GeneratorState {
	return g.state
}

// SetState updates the generator's state
func (g *Generator) SetState(state GeneratorState) {
	g.state = state
}

// GetCode returns the generator's code
func (g *Generator) GetCode() Value {
	return g.code
}

// YieldValue represents a yielded value during generator execution
type YieldValue struct {
	Value Value
}

// Type implements Value.Type
func (y *YieldValue) Type() Type {
	return Type("yield")
}

// String implements Value.String
func (y *YieldValue) String() string {
	return fmt.Sprintf("<yield %v>", y.Value)
}

// IsYield checks if a value is a yield marker
func IsYield(v Value) (*YieldValue, bool) {
	y, ok := v.(*YieldValue)
	return y, ok
}

// StopIteration is the error raised when a generator is exhausted
type StopIteration struct {
	Message string
	Value   Value
}

func (e *StopIteration) Error() string {
	if e.Message != "" {
		return e.Message
	}
	return "StopIteration"
}

// GeneratorIterator implements Iterator for generators
type GeneratorIterator struct {
	generator *Generator
}

// Next advances the iterator and returns the next value
func (gi *GeneratorIterator) Next() (Value, bool) {
	val, err := gi.generator.Next()
	if err != nil {
		// Check if it's StopIteration
		if _, ok := err.(*StopIteration); ok {
			return nil, false
		}
		// For other errors, we could log or handle differently
		return nil, false
	}
	return val, true
}

// Reset resets the iterator to the beginning
func (gi *GeneratorIterator) Reset() {
	// Generators typically can't be reset in Python
	// We'll leave this as a no-op
}

// GeneratorFunction wraps a function that contains yield statements
type GeneratorFunction struct {
	BaseObject
	Function Value  // The underlying function
	Name     string // Function name
}

// NewGeneratorFunction creates a new generator function
func NewGeneratorFunction(function Value, name string) *GeneratorFunction {
	return &GeneratorFunction{
		BaseObject: *NewBaseObject(FunctionType),
		Function:   function,
		Name:       name,
	}
}

// Type returns the function type
func (gf *GeneratorFunction) Type() Type {
	return FunctionType
}

// String returns the string representation
func (gf *GeneratorFunction) String() string {
	if gf.Name != "" {
		return fmt.Sprintf("<generator function %s>", gf.Name)
	}
	return "<generator function>"
}

// Call creates and returns a new generator
func (gf *GeneratorFunction) Call(args []Value, ctx *Context) (Value, error) {
	// Create a new generator with the function's code
	gen := NewGenerator(gf.Name, gf.Function, ctx)

	// Store the arguments and the function for later execution
	gen.SetAttr("__args__", ListValue(args))
	gen.SetAttr("__function__", gf.Function)

	return gen, nil
}

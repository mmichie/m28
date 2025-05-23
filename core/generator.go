package core

import (
	"fmt"
)

// Generator represents a generator object
type Generator struct {
	BaseObject
	name       string             // Generator function name
	state      GeneratorState     // Current state
	context    *Context           // Execution context
	code       Value              // Generator body
	yielded    Value              // Last yielded value
	locals     map[string]Value   // Local variables
	position   int                // Current position in execution
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
	return &Generator{
		BaseObject: *NewBaseObject(Type("generator")),
		name:       name,
		state:      GeneratorCreated,
		context:    NewContext(ctx),
		code:       code,
		locals:     make(map[string]Value),
		position:   0,
	}
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

// GetAttr implements Object interface
func (g *Generator) GetAttr(name string) (Value, bool) {
	switch name {
	case "__next__", "next":
		// Return bound next method
		return &BoundMethod{
			Receiver: g,
			Method: &MethodDescriptor{
				Name:    "next",
				Arity:   0,
				Doc:     "Retrieve the next value from the generator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					gen := receiver.(*Generator)
					return gen.Next()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(g),
		}, true
		
	case "send":
		// Return bound send method
		return &BoundMethod{
			Receiver: g,
			Method: &MethodDescriptor{
				Name:    "send",
				Arity:   1,
				Doc:     "Send a value into the generator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("send() takes exactly one argument")
					}
					gen := receiver.(*Generator)
					return gen.Send(args[0])
				},
			},
			TypeDesc: GetTypeDescriptorForValue(g),
		}, true
		
	case "close":
		// Return bound close method
		return &BoundMethod{
			Receiver: g,
			Method: &MethodDescriptor{
				Name:    "close",
				Arity:   0,
				Doc:     "Close the generator",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					gen := receiver.(*Generator)
					return gen.Close()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(g),
		}, true
	}
	
	return g.BaseObject.GetAttr(name)
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
	
	// Store the arguments in the generator's context
	// This would be handled by the evaluator
	
	return gen, nil
}
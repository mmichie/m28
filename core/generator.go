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
	code     Value            // Generator body (for generator functions)
	yielded  Value            // Last yielded value
	locals   map[string]Value // Local variables
	position int              // Current position in execution
	registry *MethodRegistry  // Method registry

	// Fields for generator expressions
	expr         Value                                // Expression to evaluate
	varName      string                               // Loop variable name
	iterable     Value                                // Original iterable
	condition    Value                                // Optional condition (nil if none)
	items        []Value                              // Converted items from iterable
	currentIndex int                                  // Current position in items
	evalFunc     func(Value, *Context) (Value, error) // Evaluator function
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

// NewGeneratorExpression creates a new generator for generator expressions
// This converts the iterable to items immediately, but evaluates the expression lazily
func NewGeneratorExpression(name string, expr Value, varName string, iterable Value, condition Value, ctx *Context, evalFunc func(Value, *Context) (Value, error)) (*Generator, error) {
	g := &Generator{
		BaseObject:   *NewBaseObject(Type("generator")),
		name:         name,
		state:        GeneratorCreated,
		context:      NewContext(ctx),
		expr:         expr,
		varName:      varName,
		iterable:     iterable,
		condition:    condition,
		currentIndex: 0,
		locals:       make(map[string]Value),
		evalFunc:     evalFunc,
	}

	// Convert iterable to a sequence of items
	var items []Value
	switch v := iterable.(type) {
	case *ListValue:
		items = v.Items()
	case TupleValue:
		items = v
	case StringValue:
		// Convert string to list of characters
		for _, ch := range string(v) {
			items = append(items, StringValue(string(ch)))
		}
	default:
		// Try using Iterator interface
		if iterableObj, ok := v.(Iterable); ok {
			iter := iterableObj.Iterator()
			for {
				val, hasNext := iter.Next()
				if !hasNext {
					break
				}
				items = append(items, val)
			}
		} else {
			return nil, fmt.Errorf("generator expression iterable must be a sequence, got %s", v.Type())
		}
	}

	g.items = items

	// Initialize the method registry
	g.registry = g.createRegistry()

	return g, nil
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
		// __iter__ method - per Python iterator protocol, returns self
		MakeMethod("__iter__", 0, "Return the iterator object itself", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			gen, err := TypedReceiver[*Generator](receiver, "__iter__")
			if err != nil {
				return nil, err
			}
			if err := ValidateArity("__iter__", args, 0); err != nil {
				return nil, err
			}
			return gen, nil // Return self
		}),

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

	// Handle generator expressions
	if g.expr != nil && g.evalFunc != nil {
		// Loop through items until we find one that satisfies the condition
		for g.currentIndex < len(g.items) {
			item := g.items[g.currentIndex]
			g.currentIndex++

			// Define the loop variable in the generator's context
			g.context.Define(g.varName, item)

			// Check condition if present
			if g.condition != nil {
				condResult, err := g.evalFunc(g.condition, g.context)
				if err != nil {
					return nil, fmt.Errorf("error evaluating condition: %v", err)
				}

				// Skip if condition is falsy
				if !IsTruthy(condResult) {
					continue
				}
			}

			// Evaluate the expression
			exprResult, err := g.evalFunc(g.expr, g.context)
			if err != nil {
				return nil, fmt.Errorf("error evaluating expression: %v", err)
			}

			return exprResult, nil
		}

		// No more items - mark as completed and raise StopIteration
		g.state = GeneratorCompleted
		return nil, &StopIteration{}
	}

	// Generator functions with yield are not yet implemented
	return nil, &StopIteration{Message: "generator functions with yield not yet implemented"}
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

// YieldFromValue represents a yield from expression during generator execution
type YieldFromValue struct {
	Iterable Value
}

// Type implements Value.Type
func (y *YieldFromValue) Type() Type {
	return Type("yield-from")
}

// String implements Value.String
func (y *YieldFromValue) String() string {
	return fmt.Sprintf("<yield from %v>", y.Iterable)
}

// IsYieldFrom checks if a value is a yield-from marker
func IsYieldFrom(v Value) (*YieldFromValue, bool) {
	y, ok := v.(*YieldFromValue)
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
	gen.SetAttr("__args__", NewList(args...))
	gen.SetAttr("__function__", gf.Function)

	return gen, nil
}

// GetAttr implements attribute access for generator functions
// Provides default values for standard function attributes
func (gf *GeneratorFunction) GetAttr(name string) (Value, bool) {
	// First check explicitly set attributes
	if val, ok := gf.BaseObject.GetAttr(name); ok {
		return val, true
	}

	// Try to delegate to the underlying function if it has GetAttr
	if funcWithAttrs, ok := gf.Function.(interface {
		GetAttr(string) (Value, bool)
	}); ok {
		if val, ok := funcWithAttrs.GetAttr(name); ok {
			return val, true
		}
	}

	// Provide defaults for standard function attributes
	switch name {
	case "__name__":
		if gf.Name != "" {
			return StringValue(gf.Name), true
		}
		return StringValue("<generator>"), true
	case "__qualname__":
		if gf.Name != "" {
			return StringValue(gf.Name), true
		}
		return StringValue("<generator>"), true
	case "__module__":
		return StringValue("__main__"), true
	case "__doc__":
		return None, true
	case "__annotations__":
		return NewDict(), true
	case "__type_params__":
		return TupleValue{}, true
	case "__dict__":
		// Return the attrs map as a dict
		return NewDict(), true
	}

	return nil, false
}

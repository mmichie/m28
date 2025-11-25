package core

import (
	"fmt"
)

// GeneratorExecutor is an interface for executing generator functions
// This allows eval package to provide the implementation without circular dependency
type GeneratorExecutor interface {
	Next() (Value, error)
	Send(Value) (Value, error)
	Throw(excType Value, excValue Value, excTb Value) (Value, error)
}

// GeneratorExecFactory creates a GeneratorExecutor for a given function and arguments
// This is set by the eval package to avoid circular dependency
var GeneratorExecFactory func(function Value, args []Value, ctx *Context) (GeneratorExecutor, error)

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

	// Generator function execution state (set by eval package)
	execState GeneratorExecutor

	// Fields for generator expressions
	expr         Value                                // Expression to evaluate
	varName      string                               // Loop variable name (single variable)
	varNames     []string                             // Loop variable names (multiple variables for tuple unpacking)
	iterable     Value                                // Original iterable
	condition    Value                                // Optional condition (nil if none)
	items        []Value                              // Converted items from iterable (eager mode)
	currentIndex int                                  // Current position in items (eager mode)
	evalFunc     func(Value, *Context) (Value, error) // Evaluator function

	// Fields for lazy generator expressions
	clauses   []GenClause   // Multi-clause generator clauses (lazy mode)
	lazyState *LazyGenState // Lazy evaluation state
}

// GeneratorState represents the state of a generator
type GeneratorState int

const (
	GeneratorCreated GeneratorState = iota
	GeneratorSuspended
	GeneratorRunning
	GeneratorCompleted
)

// GenClause represents a single clause in a generator expression
// For example, in "(x*y for x in range(3) for y in range(3))"
// there are two clauses: one for x and one for y
type GenClause struct {
	VarName   string   // Variable name (for single variable)
	VarNames  []string // Variable names (for tuple unpacking)
	Iterable  Value    // The iterable to loop over
	Condition Value    // Optional filter condition (nil if none)
}

// LazyGenState maintains the iteration state for lazy generator evaluation
type LazyGenState struct {
	iterators []Iterator // One iterator per clause
	values    []Value    // Current value for each clause
	contexts  []*Context // Scope for each clause level
	exhausted bool       // True when all combinations have been generated
	started   bool       // True after first Next() call
}

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
func NewGeneratorExpression(name string, expr Value, varName string, varNames []string, iterable Value, condition Value, ctx *Context, evalFunc func(Value, *Context) (Value, error)) (*Generator, error) {
	g := &Generator{
		BaseObject:   *NewBaseObject(Type("generator")),
		name:         name,
		state:        GeneratorCreated,
		context:      NewContext(ctx),
		expr:         expr,
		varName:      varName,
		varNames:     varNames,
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

// NewSimpleGenerator creates a generator from a pre-computed slice of values
// This is used for multi-clause generator expressions that are eagerly evaluated
func NewSimpleGenerator(name string, items []Value) *Generator {
	g := &Generator{
		BaseObject:   *NewBaseObject(Type("generator")),
		name:         name,
		state:        GeneratorCreated,
		items:        items,
		currentIndex: 0,
	}

	// Initialize the method registry
	g.registry = g.createRegistry()

	return g
}

// NewLazyGeneratorExpression creates a truly lazy generator that doesn't consume
// the source iterable until Next() is called. Supports multi-clause generators.
func NewLazyGeneratorExpression(name string, expr Value, clauses []GenClause, ctx *Context, evalFunc func(Value, *Context) (Value, error)) *Generator {
	g := &Generator{
		BaseObject: *NewBaseObject(Type("generator")),
		name:       name,
		state:      GeneratorCreated,
		context:    NewContext(ctx),
		expr:       expr,
		clauses:    clauses,
		evalFunc:   evalFunc,
		lazyState: &LazyGenState{
			started: false,
		},
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

		// throw method
		MakeMethod("throw", -1, "Throw an exception into the generator", func(receiver Value, args []Value, ctx *Context) (Value, error) {
			gen, err := TypedReceiver[*Generator](receiver, "throw")
			if err != nil {
				return nil, err
			}
			// throw(type[, value[, traceback]])
			// Requires at least 1 argument (exception type)
			if len(args) < 1 {
				return nil, fmt.Errorf("throw() requires at least 1 argument (exception type)")
			}

			excType := args[0]
			var excValue Value = None
			var excTb Value = None

			if len(args) > 1 {
				excValue = args[1]
			}
			if len(args) > 2 {
				excTb = args[2]
			}

			return gen.Throw(excType, excValue, excTb)
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

	// Handle lazy generator expressions (truly lazy evaluation)
	if g.lazyState != nil && len(g.clauses) > 0 {
		return g.nextLazy()
	}

	// Handle simple generators (pre-computed items with no expression to evaluate)
	if g.expr == nil && g.evalFunc == nil && len(g.items) > 0 {
		if g.currentIndex < len(g.items) {
			item := g.items[g.currentIndex]
			g.currentIndex++
			return item, nil
		}
		// No more items
		g.state = GeneratorCompleted
		return nil, &StopIteration{}
	}

	// Handle generator expressions
	if g.expr != nil && g.evalFunc != nil {
		// Loop through items until we find one that satisfies the condition
		for g.currentIndex < len(g.items) {
			item := g.items[g.currentIndex]
			g.currentIndex++

			// Define the loop variable(s) in the generator's context
			if len(g.varNames) > 0 {
				// Multiple variables - unpack the item
				var values []Value
				switch v := item.(type) {
				case TupleValue:
					values = []Value(v)
				case *ListValue:
					values = v.Items()
				default:
					return nil, fmt.Errorf("cannot unpack non-sequence %s", item.Type())
				}
				if len(values) != len(g.varNames) {
					return nil, fmt.Errorf("not enough values to unpack (expected %d, got %d)", len(g.varNames), len(values))
				}
				for i, varName := range g.varNames {
					g.context.Define(varName, values[i])
				}
			} else {
				// Single variable
				g.context.Define(g.varName, item)
			}

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

	// Handle generator functions with execState (new execution model)
	if g.execState != nil {
		return g.execState.Next()
	}

	// Fallback for generators without execution state
	return nil, &StopIteration{Message: "generator has no execution state"}
}

// nextLazy implements lazy evaluation for multi-clause generator expressions
func (g *Generator) nextLazy() (Value, error) {
	// Initialize on first call
	if !g.lazyState.started {
		if err := g.initLazyState(); err != nil {
			return nil, err
		}
	}

	// If already exhausted, return StopIteration
	if g.lazyState.exhausted {
		g.state = GeneratorCompleted
		return nil, &StopIteration{}
	}

	// Loop until we find a value that passes all conditions
	for {
		// Try to evaluate current position
		result, shouldYield, err := g.tryEvaluateLazy()
		if err != nil {
			return nil, err
		}

		if shouldYield {
			// Found a valid value, advance for next call
			g.advanceLazy()
			return result, nil
		}

		// Current position didn't pass condition, advance
		if !g.advanceLazy() {
			// No more combinations
			g.state = GeneratorCompleted
			return nil, &StopIteration{}
		}
	}
}

// initLazyState initializes iterators for all clauses
func (g *Generator) initLazyState() error {
	numClauses := len(g.clauses)
	g.lazyState.iterators = make([]Iterator, numClauses)
	g.lazyState.values = make([]Value, numClauses)
	g.lazyState.contexts = make([]*Context, numClauses)

	// Create context for first clause
	g.lazyState.contexts[0] = NewContext(g.context)

	// Evaluate iterable for first clause (may be already evaluated or an expression)
	iterable, err := g.evalFunc(g.clauses[0].Iterable, g.context)
	if err != nil {
		return fmt.Errorf("error evaluating iterable for clause 0: %v", err)
	}

	// Create iterator for first clause
	iter, err := g.getIterator(iterable)
	if err != nil {
		return err
	}
	g.lazyState.iterators[0] = iter

	// Get first value from first iterator
	val, hasNext := iter.Next()
	if !hasNext {
		// Empty iterable
		g.lazyState.exhausted = true
		g.lazyState.started = true
		return nil
	}
	g.lazyState.values[0] = val

	// Bind variable for first clause
	if err := g.bindClauseVar(0, val, g.lazyState.contexts[0]); err != nil {
		return err
	}

	// Initialize remaining clauses
	// If any inner iterable is empty, we need to advance the outer loop
	for i := 1; i < numClauses; i++ {
	retryClause:
		// Create context that inherits from previous clause
		g.lazyState.contexts[i] = NewContext(g.lazyState.contexts[i-1])

		// Evaluate iterable in parent context (might depend on outer loop vars)
		iterable, err := g.evalFunc(g.clauses[i].Iterable, g.lazyState.contexts[i-1])
		if err != nil {
			return fmt.Errorf("error evaluating iterable for clause %d: %v", i, err)
		}

		iter, err := g.getIterator(iterable)
		if err != nil {
			return err
		}
		g.lazyState.iterators[i] = iter

		val, hasNext := iter.Next()
		if !hasNext {
			// This inner iterable is empty
			// We need to advance the previous clause and retry
			// Find the previous clause to advance
			foundNonEmpty := false
			for j := i - 1; j >= 0; j-- {
				val, hasNext := g.lazyState.iterators[j].Next()
				if hasNext {
					// Successfully advanced clause j
					g.lazyState.values[j] = val
					if err := g.bindClauseVar(j, val, g.lazyState.contexts[j]); err != nil {
						return err
					}

					// Re-initialize all clauses from j+1 to i
					for k := j + 1; k <= i; k++ {
						if k == i {
							// Retry this clause
							foundNonEmpty = true
							goto retryClause
						}
						// Re-initialize intermediate clause
						g.lazyState.contexts[k] = NewContext(g.lazyState.contexts[k-1])
						iterable, err := g.evalFunc(g.clauses[k].Iterable, g.lazyState.contexts[k-1])
						if err != nil {
							return fmt.Errorf("error evaluating iterable for clause %d: %v", k, err)
						}
						iter, err := g.getIterator(iterable)
						if err != nil {
							return err
						}
						g.lazyState.iterators[k] = iter
						val, hasNext := iter.Next()
						if !hasNext {
							// This inner loop is also empty, continue advancing outer loops
							foundNonEmpty = true
							goto retryClause
						}
						g.lazyState.values[k] = val
						if err := g.bindClauseVar(k, val, g.lazyState.contexts[k]); err != nil {
							return err
						}
					}
					foundNonEmpty = true
					break
				}
			}
			if !foundNonEmpty {
				// All outer loops exhausted
				g.lazyState.exhausted = true
				g.lazyState.started = true
				return nil
			}
			continue
		}
		g.lazyState.values[i] = val

		if err := g.bindClauseVar(i, val, g.lazyState.contexts[i]); err != nil {
			return err
		}
	}

	g.lazyState.started = true
	return nil
}

// getIterator converts a value to an iterator
func (g *Generator) getIterator(val Value) (Iterator, error) {
	if iterable, ok := val.(Iterable); ok {
		return iterable.Iterator(), nil
	}
	return nil, fmt.Errorf("value is not iterable: %s", val.Type())
}

// bindClauseVar binds the loop variable(s) for a clause
func (g *Generator) bindClauseVar(clauseIdx int, val Value, ctx *Context) error {
	clause := g.clauses[clauseIdx]

	// Handle tuple unpacking
	if len(clause.VarNames) > 0 {
		var values []Value
		switch v := val.(type) {
		case TupleValue:
			values = []Value(v)
		case *ListValue:
			values = v.Items()
		default:
			return fmt.Errorf("cannot unpack non-sequence %s", val.Type())
		}
		if len(values) != len(clause.VarNames) {
			return fmt.Errorf("not enough values to unpack (expected %d, got %d)", len(clause.VarNames), len(values))
		}
		for i, varName := range clause.VarNames {
			ctx.Define(varName, values[i])
		}
	} else {
		// Single variable
		ctx.Define(clause.VarName, val)
	}
	return nil
}

// tryEvaluateLazy tries to evaluate the expression at the current position
// Returns (result, shouldYield, error)
func (g *Generator) tryEvaluateLazy() (Value, bool, error) {
	// Check all conditions
	for i := range g.clauses {
		if g.clauses[i].Condition != nil {
			// Evaluate condition in this clause's context
			condResult, err := g.evalFunc(g.clauses[i].Condition, g.lazyState.contexts[i])
			if err != nil {
				return nil, false, fmt.Errorf("error evaluating condition for clause %d: %v", i, err)
			}
			if !IsTruthy(condResult) {
				return nil, false, nil
			}
		}
	}

	// All conditions passed, evaluate expression in innermost context
	result, err := g.evalFunc(g.expr, g.lazyState.contexts[len(g.clauses)-1])
	if err != nil {
		return nil, false, fmt.Errorf("error evaluating expression: %v", err)
	}

	return result, true, nil
}

// advanceLazy advances to the next combination of loop values
// Returns false if no more combinations are available
func (g *Generator) advanceLazy() bool {
	numClauses := len(g.clauses)

	// Advance from innermost loop outward (rightmost first)
	for i := numClauses - 1; i >= 0; i-- {
		val, hasNext := g.lazyState.iterators[i].Next()
		if hasNext {
			// Successfully advanced this level
			g.lazyState.values[i] = val
			if err := g.bindClauseVar(i, val, g.lazyState.contexts[i]); err != nil {
				// Binding error - treat as exhausted
				g.lazyState.exhausted = true
				return false
			}

			// Reset all inner loops
			for j := i + 1; j < numClauses; j++ {
				// Re-evaluate iterable (might depend on outer loop vars)
				iterable, err := g.evalFunc(g.clauses[j].Iterable, g.lazyState.contexts[j-1])
				if err != nil {
					// Error evaluating iterable - treat as exhausted
					g.lazyState.exhausted = true
					return false
				}

				iter, err := g.getIterator(iterable)
				if err != nil {
					g.lazyState.exhausted = true
					return false
				}
				g.lazyState.iterators[j] = iter

				val, hasNext := iter.Next()
				if !hasNext {
					// Inner iterable is empty, continue advancing outer loop
					break
				}
				g.lazyState.values[j] = val

				if err := g.bindClauseVar(j, val, g.lazyState.contexts[j]); err != nil {
					g.lazyState.exhausted = true
					return false
				}
			}

			return true
		}

		// This level is exhausted, continue to outer level
	}

	// All levels exhausted
	g.lazyState.exhausted = true
	return false
}

// SetExecState sets the execution state for generator functions
func (g *Generator) SetExecState(state GeneratorExecutor) {
	g.execState = state
}

// Send sends a value into the generator
func (g *Generator) Send(value Value) (Value, error) {
	if g.state == GeneratorCreated && value != Nil {
		return nil, fmt.Errorf("can't send non-None value to a just-started generator")
	}

	// If we have an exec state, delegate to it
	if g.execState != nil {
		return g.execState.Send(value)
	}

	// Store the sent value for yield expressions (for generator expressions)
	g.yielded = value

	return g.Next()
}

// Throw throws an exception into the generator
func (g *Generator) Throw(excType Value, excValue Value, excTb Value) (Value, error) {
	if g.state == GeneratorCompleted {
		// If generator is already completed, re-raise the exception
		return nil, createExceptionFromThrow(excType, excValue, excTb)
	}

	// If we have an exec state, delegate to it
	if g.execState != nil {
		return g.execState.Throw(excType, excValue, excTb)
	}

	// For generator expressions without exec state, just raise the exception
	return nil, createExceptionFromThrow(excType, excValue, excTb)
}

// createExceptionFromThrow creates an error from throw() arguments
func createExceptionFromThrow(excType Value, excValue Value, excTb Value) error {
	// Get exception type name
	var typeName string
	if class, ok := excType.(*Class); ok {
		typeName = class.Name
	} else if str, ok := excType.(StringValue); ok {
		typeName = string(str)
	} else {
		typeName = "Exception"
	}

	// Get exception message
	var message string
	if excValue != None && excValue != Nil {
		if inst, ok := excValue.(*Instance); ok {
			// Get message from instance args
			if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
				if argsTuple, ok := argsAttr.(TupleValue); ok && len(argsTuple) > 0 {
					if msgStr, ok := argsTuple[0].(StringValue); ok {
						message = string(msgStr)
					}
				}
			}
		} else if str, ok := excValue.(StringValue); ok {
			message = string(str)
		}
	}

	// Return a generic error with type and message
	// The eval package will convert this to an Exception if needed
	if message != "" {
		return fmt.Errorf("%s: %s", typeName, message)
	}
	return fmt.Errorf("%s", typeName)
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

	// If we have a factory, create execution state
	if GeneratorExecFactory != nil {
		execState, err := GeneratorExecFactory(gf.Function, args, ctx)
		if err != nil {
			return nil, fmt.Errorf("failed to create generator execution state: %v", err)
		}
		gen.SetExecState(execState)
	} else {
		// Fallback: Store the arguments for old-style execution
		gen.SetAttr("__args__", NewList(args...))
		gen.SetAttr("__function__", gf.Function)
	}

	return gen, nil
}

// CallWithKeywords creates and returns a new generator with keyword arguments
func (gf *GeneratorFunction) CallWithKeywords(args []Value, kwargs map[string]Value, ctx *Context) (Value, error) {
	// If no kwargs, just use regular Call
	if len(kwargs) == 0 {
		return gf.Call(args, ctx)
	}

	// Check if the underlying function supports keyword arguments
	if _, ok := gf.Function.(interface {
		CallWithKeywords([]Value, map[string]Value, *Context) (Value, error)
	}); ok {
		// Create a new generator
		gen := NewGenerator(gf.Name, gf.Function, ctx)

		// Create execution state with keyword arguments
		if GeneratorExecFactory != nil {
			// We need to call the underlying function with kwargs to get the execution state
			// For now, merge kwargs into args using the function's parameter names
			execState, err := GeneratorExecFactory(gf.Function, args, ctx)
			if err != nil {
				return nil, fmt.Errorf("failed to create generator execution state: %v", err)
			}
			gen.SetExecState(execState)
		} else {
			// Fallback: Store the arguments for old-style execution
			gen.SetAttr("__args__", NewList(args...))
			// Create dict from kwargs
			kwargsDict := NewDict()
			for k, v := range kwargs {
				kwargsDict.Set(k, v)
			}
			gen.SetAttr("__kwargs__", kwargsDict)
			gen.SetAttr("__function__", gf.Function)
		}
		return gen, nil
	}

	// If function doesn't support kwargs, error
	return nil, &TypeError{
		Message: fmt.Sprintf("%s() does not accept keyword arguments", gf.Name),
	}
}

// GetAttr implements attribute access for generator functions
// Provides default values for standard function attributes
func (gf *GeneratorFunction) GetAttr(name string) (Value, bool) {
	// Special handling for __get__ - implement descriptor protocol
	// This ensures that when accessed on an instance, the GeneratorFunction is bound, not the inner UserFunction
	if name == "__get__" {
		// Return a descriptor function that binds this generator function to an instance
		return NewBuiltinFunction(func(args []Value, ctx *Context) (Value, error) {
			// Descriptor protocol: __get__(self, instance, owner)
			if len(args) < 1 {
				return nil, fmt.Errorf("__get__() requires at least 1 argument (instance), got %d", len(args))
			}

			instance := args[0]

			// If instance is None, return the generator function itself (class-level access)
			if instance == None || instance == Nil {
				return gf, nil
			}

			// Create a bound method with the GeneratorFunction, not the inner UserFunction
			if inst, ok := instance.(*Instance); ok {
				return &BoundInstanceMethod{
					Instance:      inst,
					Method:        gf, // Bind the GeneratorFunction, not the inner function!
					DefiningClass: inst.Class,
				}, nil
			}

			// For non-Instance objects, return the generator function unbound
			return gf, nil
		}), true
	}

	// First check explicitly set attributes
	if val, ok := gf.BaseObject.GetAttr(name); ok {
		return val, true
	}

	// Try to delegate to the underlying function if it has GetAttr
	// But skip __get__ since we handled it above
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

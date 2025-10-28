package eval

import (
	"fmt"

	"github.com/mmichie/m28/core"
)

func init() {
	// Register the generator execution factory
	core.GeneratorExecFactory = createGeneratorExecState
}

// protocolsIteratorAdapter adapts a protocols.Iterator to core.Iterator
type protocolsIteratorAdapter struct {
	iter interface {
		Next() (core.Value, error)
		HasNext() bool
	}
}

func (a *protocolsIteratorAdapter) Next() (core.Value, bool) {
	if !a.iter.HasNext() {
		return nil, false
	}
	val, err := a.iter.Next()
	if err != nil {
		// If there's an error, treat it as end of iteration
		return nil, false
	}
	return val, true
}

func (a *protocolsIteratorAdapter) Reset() {
	// protocols.Iterator doesn't have Reset, so this is a no-op
}

// createGeneratorExecState is the factory function for creating generator execution state
func createGeneratorExecState(function core.Value, args []core.Value, ctx *core.Context) (core.GeneratorExecutor, error) {
	// The function should be a UserFunction
	userFunc, ok := function.(*UserFunction)
	if !ok {
		return nil, fmt.Errorf("generator function must be a UserFunction, got %T", function)
	}

	return NewGeneratorExecState(userFunc, args, ctx)
}

// StepKind represents the type of execution step
type StepKind int

const (
	StepStatement StepKind = iota // Regular statement to evaluate
	StepYield                     // Yield expression
	StepReturn                    // Return statement
	StepLoopInit                  // Initialize loop
	StepLoopNext                  // Get next loop iteration
	StepLoopEnd                   // End of loop
	StepIfStart                   // Start of if/conditional
	StepIfEnd                     // End of if block
)

// ExecutionStep represents one step in generator execution
type ExecutionStep struct {
	Kind  StepKind
	Node  core.Value // AST node to evaluate
	Label string     // For jumps (loops, breaks, etc.)
	Arg   int        // For jumps: target step index
}

// GeneratorExecState holds the execution state of a generator
type GeneratorExecState struct {
	Steps       []ExecutionStep // Flattened execution steps
	CurrentStep int             // Current position
	Locals      *core.Context   // Local variables context
	LoopStates  []LoopState     // Stack of active loop states
	completed   bool            // Whether generator has finished
	sentValue   core.Value      // Value sent via send()
	started     bool            // Whether generator has started
}

// LoopState tracks the state of an active loop
type LoopState struct {
	Iterator core.Iterator // Iterator for the loop
	VarName  string        // Loop variable name
	EndStep  int           // Step to jump to when loop completes
}

// NewGeneratorExecState creates a new execution state for a generator
func NewGeneratorExecState(function *UserFunction, args []core.Value, ctx *core.Context) (*GeneratorExecState, error) {
	// Create local context with function's environment as parent
	locals := core.NewContext(function.env)

	// Bind arguments to parameters
	if function.signature != nil {
		// Use signature-based binding
		err := function.signature.BindArguments(args, nil, function.env, locals)
		if err != nil {
			return nil, err
		}
	} else {
		// Legacy parameter binding
		if len(args) != len(function.params) {
			return nil, fmt.Errorf("generator function expected %d arguments, got %d", len(function.params), len(args))
		}
		for i, param := range function.params {
			locals.Define(string(param), args[i])
		}
	}

	// Transform function body into execution steps
	steps, err := transformToSteps(function.body)
	if err != nil {
		return nil, err
	}

	return &GeneratorExecState{
		Steps:       steps,
		CurrentStep: 0,
		Locals:      locals,
		LoopStates:  nil,
		completed:   false,
		sentValue:   core.Nil,
		started:     false,
	}, nil
}

// Next executes the generator until the next yield or completion
func (state *GeneratorExecState) Next() (core.Value, error) {
	if state.completed {
		return nil, &core.StopIteration{}
	}

	state.started = true

	// Execute steps until we hit a yield or complete
	for state.CurrentStep < len(state.Steps) {
		step := state.Steps[state.CurrentStep]

		switch step.Kind {
		case StepStatement:
			// Evaluate the statement
			result, err := Eval(step.Node, state.Locals)
			if err != nil {
				return nil, err
			}

			// Check if it's actually a return value (shouldn't happen in steps)
			if ret, ok := result.(*ReturnValue); ok {
				state.completed = true
				return nil, &core.StopIteration{Value: ret.Value}
			}

			state.CurrentStep++

		case StepYield:
			// Evaluate the yield expression
			var yieldValue core.Value = core.Nil
			if step.Node != nil {
				val, err := Eval(step.Node, state.Locals)
				if err != nil {
					return nil, err
				}
				yieldValue = val
			}

			// Advance to next step for when we resume
			state.CurrentStep++

			// Return the yielded value
			return yieldValue, nil

		case StepReturn:
			// Evaluate return value
			var returnValue core.Value = core.Nil
			if step.Node != nil {
				val, err := Eval(step.Node, state.Locals)
				if err != nil {
					return nil, err
				}
				returnValue = val
			}

			state.completed = true
			return nil, &core.StopIteration{Value: returnValue}

		case StepLoopInit:
			// Initialize a for loop
			// step.Node should be a list: (for var iterable ...)
			if listNode, ok := step.Node.(*core.ListValue); ok && listNode.Len() >= 3 {
				// Get variable name
				varSym, ok := listNode.Items()[1].(core.SymbolValue)
				if !ok {
					return nil, fmt.Errorf("for loop variable must be a symbol")
				}

				// Evaluate iterable
				iterableVal, err := Eval(listNode.Items()[2], state.Locals)
				if err != nil {
					return nil, err
				}

				// Get iterator - check if value is already an iterator or has Iterator() method
				var iterator core.Iterator
				if iter, ok := iterableVal.(core.Iterator); ok {
					// Value is already a core.Iterator
					iterator = iter
				} else if piter, ok := iterableVal.(interface {
					Next() (core.Value, error)
					HasNext() bool
				}); ok {
					// Value is a protocols.Iterator - wrap it
					iterator = &protocolsIteratorAdapter{iter: piter}
				} else if iterable, ok := iterableVal.(interface{ Iterator() core.Iterator }); ok {
					// Value is an iterable with Iterator() method
					iterator = iterable.Iterator()
				} else {
					return nil, fmt.Errorf("for loop iterable must be iterable, got %T", iterableVal)
				}

				// Push loop state
				state.LoopStates = append(state.LoopStates, LoopState{
					Iterator: iterator,
					VarName:  string(varSym),
					EndStep:  step.Arg, // Target step when loop ends
				})
			}
			state.CurrentStep++

		case StepLoopNext:
			// Try to get next item from loop
			if len(state.LoopStates) == 0 {
				return nil, fmt.Errorf("loop next without active loop")
			}

			loopState := &state.LoopStates[len(state.LoopStates)-1]
			nextVal, hasNext := loopState.Iterator.Next()

			if !hasNext {
				// Loop exhausted - jump to end
				state.CurrentStep = loopState.EndStep
				// Pop loop state
				state.LoopStates = state.LoopStates[:len(state.LoopStates)-1]
			} else {
				// Bind loop variable and continue
				state.Locals.Define(loopState.VarName, nextVal)
				state.CurrentStep++
			}

		case StepLoopEnd:
			// Jump back to loop next
			state.CurrentStep = step.Arg

		default:
			return nil, fmt.Errorf("unknown step kind: %d", step.Kind)
		}
	}

	// Completed all steps
	state.completed = true
	return nil, &core.StopIteration{}
}

// transformToSteps converts an AST node into a flat list of execution steps
func transformToSteps(node core.Value) ([]ExecutionStep, error) {
	var steps []ExecutionStep

	// Handle different node types
	switch n := node.(type) {
	case *core.ListValue:
		if n.Len() == 0 {
			return steps, nil
		}

		// Check for special forms
		if sym, ok := n.Items()[0].(core.SymbolValue); ok {
			switch string(sym) {
			case "do", "begin":
				// Flatten do block into sequential steps
				for i := 1; i < n.Len(); i++ {
					substeps, err := transformToSteps(n.Items()[i])
					if err != nil {
						return nil, err
					}

					// Adjust jump targets (Arg values) to account for offset
					offset := len(steps)
					for j := range substeps {
						// Adjust Arg if it's a valid jump target
						if substeps[j].Arg > 0 {
							substeps[j].Arg += offset
						} else if substeps[j].Arg == -1 {
							// Keep -1 as a sentinel value (will be filled in later)
						}
					}

					steps = append(steps, substeps...)
				}
				return steps, nil

			case "yield":
				// Yield statement
				var yieldNode core.Value
				if n.Len() > 1 {
					yieldNode = n.Items()[1]
				}
				steps = append(steps, ExecutionStep{
					Kind: StepYield,
					Node: yieldNode,
				})
				return steps, nil

			case "return":
				// Return statement
				var returnNode core.Value
				if n.Len() > 1 {
					returnNode = n.Items()[1]
				}
				steps = append(steps, ExecutionStep{
					Kind: StepReturn,
					Node: returnNode,
				})
				return steps, nil

			case "for":
				// For loop: (for var iterable body...)
				if n.Len() < 4 {
					return nil, fmt.Errorf("for loop requires variable, iterable, and body")
				}

				// Create steps for loop
				loopStart := len(steps)

				// Step 1: Initialize loop
				steps = append(steps, ExecutionStep{
					Kind: StepLoopInit,
					Node: n,
					Arg:  -1, // Will be filled in later with end step
				})

				// Step 2: Try to get next item
				loopNextStep := len(steps)
				steps = append(steps, ExecutionStep{
					Kind: StepLoopNext,
				})

				// Steps 3+: Loop body
				for i := 3; i < n.Len(); i++ {
					substeps, err := transformToSteps(n.Items()[i])
					if err != nil {
						return nil, err
					}
					steps = append(steps, substeps...)
				}

				// Final step: Jump back to loop next
				loopEndStep := len(steps)
				steps = append(steps, ExecutionStep{
					Kind: StepLoopEnd,
					Arg:  loopNextStep,
				})

				// Fill in the end step for loop init
				steps[loopStart].Arg = loopEndStep + 1

				return steps, nil

			case "if":
				// For now, treat if as a regular statement
				// TODO: Proper conditional handling with step skipping
				steps = append(steps, ExecutionStep{
					Kind: StepStatement,
					Node: node,
				})
				return steps, nil

			default:
				// Regular function call or expression
				steps = append(steps, ExecutionStep{
					Kind: StepStatement,
					Node: node,
				})
				return steps, nil
			}
		}

		// Not a special form, treat as statement
		steps = append(steps, ExecutionStep{
			Kind: StepStatement,
			Node: node,
		})
		return steps, nil

	default:
		// Literal value or other - treat as statement
		steps = append(steps, ExecutionStep{
			Kind: StepStatement,
			Node: node,
		})
		return steps, nil
	}
}

// Send sends a value into the generator (for yield expressions)
func (state *GeneratorExecState) Send(value core.Value) (core.Value, error) {
	// Python semantics: can't send non-None to just-started generator
	if !state.started && value != core.Nil {
		return nil, fmt.Errorf("can't send non-None value to a just-started generator")
	}

	// Store the value to be used by yield expression
	// TODO: Implement yield expressions (x = yield val)
	state.sentValue = value

	// Continue execution
	return state.Next()
}

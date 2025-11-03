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
	StepStatement    StepKind = iota // Regular statement to evaluate
	StepYield                        // Yield expression
	StepReturn                       // Return statement
	StepLoopInit                     // Initialize loop
	StepLoopNext                     // Get next loop iteration
	StepLoopEnd                      // End of loop
	StepIfStart                      // Start of if/conditional
	StepIfEnd                        // End of if block
	StepTryStart                     // Start of try block
	StepTryEnd                       // End of try block (jump to finally)
	StepFinallyStart                 // Start of finally block
	StepFinallyEnd                   // End of finally block
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
	Steps        []ExecutionStep // Flattened execution steps
	CurrentStep  int             // Current position
	Locals       *core.Context   // Local variables context
	LoopStates   []LoopState     // Stack of active loop states
	TryStates    []TryState      // Stack of active try/finally blocks
	completed    bool            // Whether generator has finished
	sentValue    core.Value      // Value sent via send()
	started      bool            // Whether generator has started
	pendingError error           // Error to be raised after finally blocks
}

// TryState tracks the state of an active try/except/finally block
type TryState struct {
	ExceptSteps []int // Steps to execute for each except handler
	FinallyStep int   // Step to execute for finally block (0 if no finally)
	EndStep     int   // Step after the entire try/except/finally
	InFinally   bool  // Whether we're currently in the finally block
	InExcept    bool  // Whether we're currently in an except handler
}

// LoopState tracks the state of an active loop
type LoopState struct {
	Iterator core.Iterator // Iterator for the loop
	VarName  string        // Loop variable name (empty if VarNames is used)
	VarNames []string      // Multiple loop variable names for tuple unpacking
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
		// Check if there's a pending error from throw()
		// If there is, just propagate it and let normal exception handling deal with it
		if state.pendingError != nil {
			err := state.pendingError
			state.pendingError = nil
			// Treat this like any other error - it will be caught by try/except if present
			return nil, err
		}

		step := state.Steps[state.CurrentStep]

		switch step.Kind {
		case StepStatement:
			// Evaluate the statement
			result, err := Eval(step.Node, state.Locals)
			if err == nil {
				// Check if it's actually a return value
				if ret, ok := result.(*ReturnValue); ok {
					state.completed = true
					return nil, &core.StopIteration{Value: ret.Value}
				}
			} else {
				// If we're in a try block, jump to except handler (or finally)
				if len(state.TryStates) > 0 {
					tryState := &state.TryStates[len(state.TryStates)-1]
					if !tryState.InExcept && !tryState.InFinally {
						// Jump to first except handler and save error
						if len(tryState.ExceptSteps) > 0 && tryState.ExceptSteps[0] > 0 {
							state.pendingError = err
							tryState.InExcept = true
							state.CurrentStep = tryState.ExceptSteps[0]
							continue
						}
					}
				}
				// Not in a try block or already in except/finally - propagate error
				return nil, err
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
				// Get variable pattern - can be a single symbol or tuple of symbols
				varPattern := listNode.Items()[1]
				fmt.Printf("[DEBUG LoopInit] varPattern = %v (%T)\n", varPattern, varPattern)

				var varName string
				var varNames []string

				// Check if it's a single symbol or a tuple pattern
				if varSym, ok := varPattern.(core.SymbolValue); ok {
					// Single variable
					varName = string(varSym)
					fmt.Printf("[DEBUG LoopInit] Single var: %s\n", varName)
				} else if tuplePattern, ok := varPattern.(core.TupleValue); ok {
					fmt.Printf("[DEBUG LoopInit] Tuple pattern with %d elements\n", len(tuplePattern))
					// Tuple unpacking: (k, v)
					varNames = make([]string, len(tuplePattern))
					for i, elem := range tuplePattern {
						if sym, ok := elem.(core.SymbolValue); ok {
							varNames[i] = string(sym)
						} else {
							return nil, fmt.Errorf("tuple unpacking pattern must contain symbols")
						}
					}
				} else if listPattern, ok := varPattern.(*core.ListValue); ok {
					// List unpacking: [k, v]
					fmt.Printf("[DEBUG LoopInit] List pattern with %d elements\n", listPattern.Len())
					varNames = make([]string, listPattern.Len())
					for i := 0; i < listPattern.Len(); i++ {
						if sym, ok := listPattern.Items()[i].(core.SymbolValue); ok {
							varNames[i] = string(sym)
							fmt.Printf("[DEBUG LoopInit]   varNames[%d] = %s\n", i, varNames[i])
						} else {
							return nil, fmt.Errorf("tuple unpacking pattern must contain symbols")
						}
					}
				} else {
					return nil, fmt.Errorf("for loop variable must be a symbol or tuple pattern, got %T", varPattern)
				}
				fmt.Printf("[DEBUG LoopInit] Final: varName=%s, varNames=%v\n", varName, varNames)

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
					VarName:  varName,
					VarNames: varNames,
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
				// Bind loop variable(s) and continue
				if len(loopState.VarNames) > 0 {
					// Tuple unpacking: unpack nextVal into multiple variables
					fmt.Printf("[DEBUG LoopNext] Unpacking with VarNames=%v, nextVal=%v (%T)\n", loopState.VarNames, nextVal, nextVal)
					var values []core.Value
					switch v := nextVal.(type) {
					case core.TupleValue:
						values = []core.Value(v)
					case *core.ListValue:
						values = v.Items()
					default:
						return nil, fmt.Errorf("cannot unpack non-sequence type %s in for loop", nextVal.Type())
					}

					if len(values) != len(loopState.VarNames) {
						return nil, fmt.Errorf("cannot unpack %d values into %d variables", len(values), len(loopState.VarNames))
					}

					// Bind each variable
					for i, varName := range loopState.VarNames {
						fmt.Printf("[DEBUG LoopNext] Binding %s = %v\n", varName, values[i])
						state.Locals.Define(varName, values[i])
					}
				} else {
					// Single variable: bind directly
					fmt.Printf("[DEBUG LoopNext] Single var: %s = %v\n", loopState.VarName, nextVal)
					state.Locals.Define(loopState.VarName, nextVal)
				}
				state.CurrentStep++
			}

		case StepLoopEnd:
			// Jump back to loop next
			state.CurrentStep = step.Arg

		case StepTryStart:
			// Push try state
			// step.Arg points to either except handler or finally block
			state.TryStates = append(state.TryStates, TryState{
				ExceptSteps: []int{step.Arg}, // First except or finally
				FinallyStep: 0,               // Will be set when we see StepFinallyStart
				EndStep:     0,               // Will be set at StepTryEnd
				InFinally:   false,
				InExcept:    false,
			})
			state.CurrentStep++

		case StepTryEnd:
			// Normal exit from try block - skip except handlers, go to finally or end
			// step.Arg points to finally block or after try
			state.CurrentStep = step.Arg

		case StepFinallyStart:
			// Just a marker, continue to next step
			state.CurrentStep++

		case StepFinallyEnd:
			// Pop try state
			if len(state.TryStates) > 0 {
				state.TryStates = state.TryStates[:len(state.TryStates)-1]
			}
			// Check if there's a pending error to re-raise
			if state.pendingError != nil {
				err := state.pendingError
				state.pendingError = nil
				return nil, err
			}
			// Jump to step after finally
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

			case "yield-from":
				// Yield from statement: (yield-from iterable)
				// Transform into: for __yield_from_item in iterable: yield __yield_from_item
				if n.Len() < 2 {
					return nil, fmt.Errorf("yield-from requires an iterable argument")
				}

				// Create a unique variable name for the loop
				loopVar := core.SymbolValue("__yield_from_item")
				iterableExpr := n.Items()[1]

				// Build the equivalent for loop structure
				// (for __yield_from_item iterable (yield __yield_from_item))
				forLoop := core.NewList(
					core.SymbolValue("for"),
					loopVar,
					iterableExpr,
					core.NewList(core.SymbolValue("yield"), loopVar),
				)

				// Transform the for loop into steps
				return transformToSteps(forLoop)

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

			case "try":
				// Try/except/finally block: (try body... (except handler...) (finally cleanup...))
				// Parse the try form
				var tryBody []core.Value
				var exceptClauses []*core.ListValue
				var finallyBody []core.Value

				for i := 1; i < n.Len(); i++ {
					item := n.Items()[i]
					if list, ok := item.(*core.ListValue); ok && list.Len() > 0 {
						if sym, ok := list.Items()[0].(core.SymbolValue); ok {
							switch string(sym) {
							case "except":
								exceptClauses = append(exceptClauses, list)
								continue
							case "finally":
								finallyBody = list.Items()[1:]
								continue
							}
						}
					}
					// If we haven't seen except or finally yet, it's part of try body
					if len(exceptClauses) == 0 && len(finallyBody) == 0 {
						tryBody = append(tryBody, item)
					}
				}

				// Build the step sequence
				tryStartIdx := len(steps)

				// Mark start of try block - store index of first except handler (or finally if no except)
				steps = append(steps, ExecutionStep{
					Kind: StepTryStart,
					Arg:  -1, // Will be filled in later
				})

				// Transform try body
				for _, stmt := range tryBody {
					substeps, err := transformToSteps(stmt)
					if err != nil {
						return nil, err
					}
					steps = append(steps, substeps...)
				}

				// Mark end of try block - jump past except handlers
				tryEndIdx := len(steps)
				steps = append(steps, ExecutionStep{
					Kind: StepTryEnd,
					Arg:  -1, // Will jump to finally or after
				})

				// Transform except handlers (for now, treat as opaque statements)
				// In the future, we can add proper exception matching
				exceptStartIdx := len(steps)
				for _, exceptClause := range exceptClauses {
					// For now, treat each except as a single statement
					// This means if an error occurs, we jump here and evaluate it
					steps = append(steps, ExecutionStep{
						Kind: StepStatement,
						Node: exceptClause,
					})
				}

				// Transform finally block if present
				var finallyStartIdx int
				if len(finallyBody) > 0 {
					finallyStartIdx = len(steps)
					steps = append(steps, ExecutionStep{
						Kind: StepFinallyStart,
					})

					for _, stmt := range finallyBody {
						substeps, err := transformToSteps(stmt)
						if err != nil {
							return nil, err
						}
						steps = append(steps, substeps...)
					}

					steps = append(steps, ExecutionStep{
						Kind: StepFinallyEnd,
						Arg:  len(steps) + 1, // Jump past finally
					})
				}

				afterTryIdx := len(steps)

				// Fill in jump targets
				if len(exceptClauses) > 0 {
					steps[tryStartIdx].Arg = exceptStartIdx // On error, jump to except
				} else if len(finallyBody) > 0 {
					steps[tryStartIdx].Arg = finallyStartIdx // On error, jump to finally
				} else {
					steps[tryStartIdx].Arg = afterTryIdx // No except or finally
				}

				// Try end jumps to finally (if exists) or past everything
				if len(finallyBody) > 0 {
					steps[tryEndIdx].Arg = finallyStartIdx
				} else {
					steps[tryEndIdx].Arg = afterTryIdx
				}

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

// Throw throws an exception into the generator
func (state *GeneratorExecState) Throw(excType core.Value, excValue core.Value, excTb core.Value) (core.Value, error) {
	// If generator hasn't started, just raise the exception
	if !state.started {
		return nil, createExceptionFromThrowArgs(excType, excValue, excTb)
	}

	// If generator is completed, re-raise the exception
	if state.completed {
		return nil, createExceptionFromThrowArgs(excType, excValue, excTb)
	}

	// Create the exception error
	err := createExceptionFromThrowArgs(excType, excValue, excTb)

	// Store as pending error - it will be raised when we resume execution
	// This simulates Python's behavior where the exception is raised at the yield point
	state.pendingError = err

	// Continue execution - the exception will be raised at the current yield point
	return state.Next()
}

// createExceptionFromThrowArgs creates an exception error from throw() arguments
func createExceptionFromThrowArgs(excType core.Value, excValue core.Value, excTb core.Value) error {
	// Get exception type name
	var typeName string
	if class, ok := excType.(*core.Class); ok {
		typeName = class.Name
	} else if str, ok := excType.(core.StringValue); ok {
		typeName = string(str)
	} else {
		typeName = "Exception"
	}

	// Get exception message
	var message string
	if excValue != core.None && excValue != core.Nil {
		if inst, ok := excValue.(*core.Instance); ok {
			// Get message from instance args
			if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
				if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
					if msgStr, ok := argsTuple[0].(core.StringValue); ok {
						message = string(msgStr)
					}
				}
			}
		} else if str, ok := excValue.(core.StringValue); ok {
			message = string(str)
		}
	}

	// Return an Exception error
	return &Exception{Type: typeName, Message: message}
}

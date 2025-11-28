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
	StepStatement      StepKind = iota // Regular statement to evaluate
	StepYield                          // Yield expression
	StepReturn                         // Return statement
	StepLoopInit                       // Initialize loop
	StepLoopNext                       // Get next loop iteration
	StepLoopEnd                        // End of loop
	StepWhileCondition                 // While loop condition check
	StepIfStart                        // Start of if/conditional
	StepIfEnd                          // End of if block
	StepTryStart                       // Start of try block
	StepTryEnd                         // End of try block (jump to finally)
	StepFinallyStart                   // Start of finally block
	StepFinallyEnd                     // End of finally block
	StepWithEnter                      // Enter context manager
	StepWithExit                       // Exit context manager
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
	WithStates   []WithState     // Stack of active with statements
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

// WithState tracks the state of an active with statement
type WithState struct {
	ContextManager core.ContextManager // The context manager
	EnterValue     core.Value          // Value returned by __enter__
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

				// Handle quoted patterns: (quote [a, b]) -> [a, b]
				// This happens when parsing Python syntax: for a, b in ...
				if quotedList, ok := varPattern.(*core.ListValue); ok && quotedList.Len() >= 2 {
					if quoteSymbol, ok := quotedList.Items()[0].(core.SymbolValue); ok && string(quoteSymbol) == "quote" {
						varPattern = quotedList.Items()[1]
					}
				}

				var varName string
				var varNames []string

				// Check if it's a single symbol or a tuple pattern
				if varSym, ok := varPattern.(core.SymbolValue); ok {
					// Single variable
					varName = string(varSym)
				} else if tuplePattern, ok := varPattern.(core.TupleValue); ok {
					// Tuple unpacking: (k, v)
					varNames = make([]string, len(tuplePattern))
					for i, elem := range tuplePattern {
						if sym, ok := elem.(core.SymbolValue); ok {
							varNames[i] = string(sym)
						} else {
							return nil, fmt.Errorf("tuple unpacking pattern must contain symbols, got %T at index %d", elem, i)
						}
					}
				} else if listPattern, ok := varPattern.(*core.ListValue); ok {
					// List unpacking: [k, v]
					varNames = make([]string, listPattern.Len())
					for i := 0; i < listPattern.Len(); i++ {
						elem := listPattern.Items()[i]
						if sym, ok := elem.(core.SymbolValue); ok {
							varNames[i] = string(sym)
						} else {
							return nil, fmt.Errorf("tuple unpacking pattern must contain symbols, got %T at index %d", elem, i)
						}
					}
				} else {
					return nil, fmt.Errorf("for loop variable must be a symbol or tuple pattern, got %T", varPattern)
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
						state.Locals.Define(varName, values[i])
					}
				} else {
					// Single variable: bind directly
					state.Locals.Define(loopState.VarName, nextVal)
				}
				state.CurrentStep++
			}

		case StepLoopEnd:
			// Jump back to loop next
			state.CurrentStep = step.Arg

		case StepWhileCondition:
			// Evaluate while loop condition
			condValue, err := Eval(step.Node, state.Locals)
			if err != nil {
				return nil, err
			}

			// Check if condition is truthy
			if !core.IsTruthy(condValue) {
				// Condition is false - jump to end (stored in step.Arg)
				state.CurrentStep = step.Arg
			} else {
				// Condition is true - continue to next step (loop body)
				state.CurrentStep++
			}

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

		case StepWithEnter:
			// Enter a context manager
			// step.Node is the context expression (or a list of [context-expr, pattern] for tuple unpacking)
			// step.Label is the variable name (empty if no binding or tuple pattern)
			var contextExpr core.Value
			var varPattern core.Value

			if listNode, ok := step.Node.(*core.ListValue); ok && listNode.Len() == 2 {
				// Has tuple pattern: [context-expr, pattern]
				contextExpr = listNode.Items()[0]
				varPattern = listNode.Items()[1]
			} else {
				// Simple case: just the context expression
				contextExpr = step.Node
			}

			// Evaluate the context manager expression
			mgrValue, err := Eval(contextExpr, state.Locals)
			if err != nil {
				return nil, err
			}

			// Check if it's a context manager
			cm, ok := core.IsContextManager(mgrValue)
			if !ok {
				return nil, fmt.Errorf("'%s' object does not support the context manager protocol", mgrValue.Type())
			}

			// Call __enter__
			enterValue, err := cm.Enter()
			if err != nil {
				return nil, err
			}

			// Push with state
			state.WithStates = append(state.WithStates, WithState{
				ContextManager: cm,
				EnterValue:     enterValue,
			})

			// Bind the value if there's an 'as' clause
			if step.Label != "" {
				// Simple variable binding
				state.Locals.Define(step.Label, enterValue)
			} else if varPattern != nil {
				// Tuple unpacking
				if err := UnpackPattern(varPattern, enterValue, state.Locals); err != nil {
					// Call __exit__ before returning error
					cm.Exit(core.Nil, core.Nil, core.Nil)
					state.WithStates = state.WithStates[:len(state.WithStates)-1]
					return nil, err
				}
			}

			state.CurrentStep++

		case StepWithExit:
			// Exit a context manager
			if len(state.WithStates) == 0 {
				return nil, fmt.Errorf("with exit without active with statement")
			}

			// Pop the with state
			withState := state.WithStates[len(state.WithStates)-1]
			state.WithStates = state.WithStates[:len(state.WithStates)-1]

			// Call __exit__ with no exception
			_, exitErr := withState.ContextManager.Exit(core.Nil, core.Nil, core.Nil)
			if exitErr != nil {
				return nil, exitErr
			}

			state.CurrentStep++

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

	// Unwrap LocatedValue if present
	if located, ok := node.(core.LocatedValue); ok {
		node = located.Unwrap()
	}

	// Handle different node types
	switch n := node.(type) {
	case *core.ListValue:
		if n.Len() == 0 {
			return steps, nil
		}

		// Check for special forms
		// Unwrap LocatedValue to get the actual symbol
		firstItem := n.Items()[0]
		if located, ok := firstItem.(core.LocatedValue); ok {
			firstItem = located.Unwrap()
		}

		if sym, ok := firstItem.(core.SymbolValue); ok {
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
						// Adjust Arg if it's a valid jump target (>= 0, but not -1 sentinel)
						if substeps[j].Arg >= 0 {
							substeps[j].Arg += offset
						}
						// -1 is a sentinel value (will be filled in later), don't adjust
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

			case "while":
				// While loop: (while condition body...)
				if n.Len() < 2 {
					return nil, fmt.Errorf("while loop requires condition and body")
				}

				// Extract condition and body
				condition := n.Items()[1]
				body := n.Items()[2:]

				// Create steps for while loop
				// Step 1: Check condition (and jump to end if false)
				conditionStep := len(steps)
				steps = append(steps, ExecutionStep{
					Kind: StepWhileCondition,
					Node: condition,
					Arg:  -1, // Will be filled in later with end step
				})

				// Steps 2+: Loop body
				for _, bodyExpr := range body {
					substeps, err := transformToSteps(bodyExpr)
					if err != nil {
						return nil, err
					}
					steps = append(steps, substeps...)
				}

				// Final step: Jump back to condition check
				loopEndStep := len(steps)
				steps = append(steps, ExecutionStep{
					Kind: StepLoopEnd,
					Arg:  conditionStep,
				})

				// Fill in the end step for condition (where to jump if condition is false)
				steps[conditionStep].Arg = loopEndStep + 1

				return steps, nil

			case "if":
				// For now, treat if as a regular statement
				// TODO(M28-7095): Proper conditional handling with step skipping
				steps = append(steps, ExecutionStep{
					Kind: StepStatement,
					Node: node,
				})
				return steps, nil

			case "with":
				// With statement: (with context-expr var body...)
				// or (with context-expr None body...)
				// Need to properly handle yields inside the with body
				if n.Len() < 3 {
					// Malformed with, treat as statement
					steps = append(steps, ExecutionStep{
						Kind: StepStatement,
						Node: node,
					})
					return steps, nil
				}

				// Parse the with statement structure
				// Format: (with context-expr var-or-None body...)
				contextExpr := n.Items()[1]
				varOrNone := n.Items()[2]
				bodyStart := 3

				// Check if varOrNone is actually the body start (old format without explicit var)
				// In old format: (with context-expr body...)
				// In new format: (with context-expr var body...) or (with context-expr None body...)
				var varName string
				var varPattern core.Value

				if sym, ok := varOrNone.(core.SymbolValue); ok {
					symStr := string(sym)
					if symStr == "None" {
						// Explicit None - no variable binding
						varName = ""
					} else {
						// It's a variable name
						varName = symStr
					}
				} else if list, ok := varOrNone.(*core.ListValue); ok {
					// Could be tuple pattern for unpacking, or could be body
					// Check if it looks like an expression to evaluate
					if list.Len() > 0 {
						if firstSym, ok := list.Items()[0].(core.SymbolValue); ok {
							symStr := string(firstSym)
							// If it starts with a known form, it's body
							if symStr == "do" || symStr == "begin" || symStr == "yield" ||
								symStr == "return" || symStr == "if" || symStr == "for" ||
								symStr == "while" || symStr == "try" || symStr == "with" ||
								symStr == "=" || symStr == "print" {
								// This is body, not a variable pattern
								bodyStart = 2
							} else {
								// It's a variable pattern
								varPattern = varOrNone
							}
						} else {
							// Assume it's a tuple pattern
							varPattern = varOrNone
						}
					} else {
						// Empty list - probably body start
						bodyStart = 2
					}
				} else if varOrNone == core.None || varOrNone == core.Nil {
					// Explicit None
					varName = ""
				} else {
					// Unknown type - assume it's body
					bodyStart = 2
				}

				body := n.Items()[bodyStart:]

				// Check if body contains any yield statements
				hasYield := containsYieldInBody(body)

				if !hasYield {
					// No yields in body - can treat as atomic statement
					steps = append(steps, ExecutionStep{
						Kind: StepStatement,
						Node: node,
					})
					return steps, nil
				}

				// Body contains yield - need to break it into steps
				// Step 1: WithEnter - evaluate context expr and call __enter__
				steps = append(steps, ExecutionStep{
					Kind:  StepWithEnter,
					Node:  contextExpr,
					Label: varName,
					// Store variable pattern as a separate step if needed
				})

				// Store variable pattern info if using tuple unpacking
				if varPattern != nil {
					// We'll handle this in StepWithEnter by storing the pattern
					steps[len(steps)-1].Node = core.NewList(contextExpr, varPattern)
				}

				// Transform body into steps
				for _, stmt := range body {
					substeps, err := transformToSteps(stmt)
					if err != nil {
						return nil, err
					}
					steps = append(steps, substeps...)
				}

				// Step N: WithExit - call __exit__
				steps = append(steps, ExecutionStep{
					Kind: StepWithExit,
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
	// TODO(M28-2260): Implement yield expressions (x = yield val)
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
	// Handle None/Nil excType - this indicates generator cleanup or improper throw() call
	if excType == nil || excType == core.Nil || excType == core.None {
		// Default to GeneratorExit for generator cleanup scenarios
		return &Exception{Type: "GeneratorExit", Message: ""}
	}

	// Get exception type name - handle both exception classes and structured errors
	var typeName string
	var message string

	// Check if excType is already a Go error (structured exception type)
	if err, ok := excType.(error); ok {
		// It's a structured error like *core.TypeError, *core.AttributeError, etc.
		// Just return it directly
		return err
	}

	// Check if it's an exception class
	if class, ok := excType.(*core.Class); ok {
		typeName = class.Name
	} else if str, ok := excType.(core.StringValue); ok {
		typeName = string(str)
	} else if inst, ok := excType.(*core.Instance); ok {
		// It's an exception instance - get the class name and message
		typeName = inst.Class.Name
		if argsAttr, hasArgs := inst.GetAttr("args"); hasArgs {
			if argsTuple, ok := argsAttr.(core.TupleValue); ok && len(argsTuple) > 0 {
				if msgStr, ok := argsTuple[0].(core.StringValue); ok {
					message = string(msgStr)
				}
			}
		}
		// Return early since we already have everything from the instance
		return &Exception{Type: typeName, Message: message}
	} else {
		// Unknown type - create a descriptive error
		return &core.TypeError{
			Message: fmt.Sprintf("throw() argument must be an exception class or instance, not %s", excType.Type()),
		}
	}

	// Get exception message from excValue
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

// containsYieldInBody checks if a list of AST nodes contains any yield statements
func containsYieldInBody(nodes []core.Value) bool {
	for _, node := range nodes {
		if containsYield(node) {
			return true
		}
	}
	return false
}

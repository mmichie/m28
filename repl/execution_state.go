package repl

import (
	"fmt"
	"sync"

	"github.com/mmichie/m28/core"
)

// ExecutionState tracks the state of REPL execution including counters and output history
type ExecutionState struct {
	mu              sync.Mutex
	executionCount  int
	outputHistory   map[string]core.Value // _1, _2, etc.
	lastOutput      core.Value            // _ variable
	lastPrintOutput string                 // Track last print output to avoid duplication
	ctx             *core.Context
}

// NewExecutionState creates a new execution state manager
func NewExecutionState(ctx *core.Context) *ExecutionState {
	return &ExecutionState{
		executionCount: 0,
		outputHistory:  make(map[string]core.Value),
		ctx:            ctx,
	}
}

// NextExecutionNumber increments and returns the next execution number
func (es *ExecutionState) NextExecutionNumber() int {
	es.mu.Lock()
	defer es.mu.Unlock()
	es.executionCount++
	return es.executionCount
}

// GetExecutionNumber returns the current execution number
func (es *ExecutionState) GetExecutionNumber() int {
	es.mu.Lock()
	defer es.mu.Unlock()
	return es.executionCount
}

// FormatInputPrompt formats the input prompt with execution number
func (es *ExecutionState) FormatInputPrompt() string {
	return fmt.Sprintf("In[%d]: ", es.GetExecutionNumber()+1)
}

// FormatContinuationPrompt formats the continuation prompt with proper indentation
func (es *ExecutionState) FormatContinuationPrompt() string {
	// Match the length of the input prompt for alignment
	spaces := len(fmt.Sprintf("In[%d]: ", es.GetExecutionNumber()+1))
	return fmt.Sprintf("%*s", spaces-4, "...") + " "
}

// FormatOutputPrompt formats the output prompt with execution number
func (es *ExecutionState) FormatOutputPrompt(num int) string {
	return fmt.Sprintf("Out[%d]: ", num)
}

// StoreOutput stores the output value in history and updates context variables
func (es *ExecutionState) StoreOutput(num int, value core.Value) error {
	es.mu.Lock()
	defer es.mu.Unlock()

	// Don't store nil values
	if value == core.Nil {
		return nil
	}

	// Store in output history
	varName := fmt.Sprintf("_%d", num)
	es.outputHistory[varName] = value
	es.lastOutput = value

	// Update context variables
	if err := es.ctx.Set(varName, value); err != nil {
		return err
	}

	// Update the _ variable (last output)
	if err := es.ctx.Set("_", value); err != nil {
		return err
	}

	// Also set __builtin_ellipsis__ for ... support
	if num == 1 {
		if err := es.ctx.Set("__builtin_ellipsis__", value); err != nil {
			return err
		}
	}

	return nil
}

// GetLastOutput returns the last output value
func (es *ExecutionState) GetLastOutput() core.Value {
	es.mu.Lock()
	defer es.mu.Unlock()
	return es.lastOutput
}

// SetLastPrintOutput sets the last print output to track duplicates
func (es *ExecutionState) SetLastPrintOutput(output string) {
	es.mu.Lock()
	defer es.mu.Unlock()
	es.lastPrintOutput = output
}

// GetLastPrintOutput returns the last print output
func (es *ExecutionState) GetLastPrintOutput() string {
	es.mu.Lock()
	defer es.mu.Unlock()
	return es.lastPrintOutput
}

// ClearLastPrintOutput clears the last print output
func (es *ExecutionState) ClearLastPrintOutput() {
	es.mu.Lock()
	defer es.mu.Unlock()
	es.lastPrintOutput = ""
}

// IsOutputDuplicate checks if the output is a duplicate of the last print
func (es *ExecutionState) IsOutputDuplicate(output string) bool {
	es.mu.Lock()
	defer es.mu.Unlock()
	return es.lastPrintOutput != "" && es.lastPrintOutput == output
}
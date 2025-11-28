// Package core provides traceback support for Python-style error reporting.
package core

import (
	"fmt"
	"strings"
	"sync"
)

// CallFrame represents a single frame in the call stack
type CallFrame struct {
	FunctionName string           // Name of the function (or "<module>" for top-level)
	Location     *SourceLocation  // Source location of the call
	LocalVars    map[string]Value // Local variables (for debugging, can be nil)
}

// String returns a Python-style frame representation
func (f *CallFrame) String() string {
	return fmt.Sprintf("  File \"%s\", line %d, in %s",
		f.getFile(), f.getLine(), f.FunctionName)
}

func (f *CallFrame) getFile() string {
	if f.Location != nil && f.Location.File != "" {
		return f.Location.File
	}
	return "<unknown>"
}

func (f *CallFrame) getLine() int {
	if f.Location != nil {
		return f.Location.Line
	}
	return 0
}

// CallStack tracks the call stack for traceback generation
type CallStack struct {
	mu     sync.RWMutex
	frames []*CallFrame
}

// NewCallStack creates a new call stack
func NewCallStack() *CallStack {
	return &CallStack{
		frames: make([]*CallFrame, 0, 16),
	}
}

// Push adds a new frame to the call stack
func (cs *CallStack) Push(funcName string, loc *SourceLocation) {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	cs.frames = append(cs.frames, &CallFrame{
		FunctionName: funcName,
		Location:     loc,
	})
}

// Pop removes the top frame from the call stack
func (cs *CallStack) Pop() {
	cs.mu.Lock()
	defer cs.mu.Unlock()
	if len(cs.frames) > 0 {
		cs.frames = cs.frames[:len(cs.frames)-1]
	}
}

// Depth returns the current call stack depth
func (cs *CallStack) Depth() int {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	return len(cs.frames)
}

// Frames returns a copy of the current call frames (oldest first)
func (cs *CallStack) Frames() []*CallFrame {
	cs.mu.RLock()
	defer cs.mu.RUnlock()
	result := make([]*CallFrame, len(cs.frames))
	copy(result, cs.frames)
	return result
}

// Snapshot creates a copy of the current stack for embedding in errors
func (cs *CallStack) Snapshot() []*CallFrame {
	return cs.Frames()
}

// TracebackError wraps an error with traceback information
type TracebackError struct {
	Err      error           // The underlying error
	Frames   []*CallFrame    // Call stack at time of error
	Location *SourceLocation // Where the error occurred
}

// Error implements the error interface with Python-style formatting
func (te *TracebackError) Error() string {
	return te.FormatTraceback()
}

// Unwrap returns the underlying error for errors.Is/As support
func (te *TracebackError) Unwrap() error {
	return te.Err
}

// FormatTraceback returns a Python-style traceback string
func (te *TracebackError) FormatTraceback() string {
	var sb strings.Builder

	sb.WriteString("Traceback (most recent call last):\n")

	// Print frames from oldest to newest
	for _, frame := range te.Frames {
		sb.WriteString(frame.String())
		sb.WriteString("\n")
	}

	// Add location where error occurred if we have it
	if te.Location != nil {
		sb.WriteString(fmt.Sprintf("  File \"%s\", line %d\n",
			te.Location.File, te.Location.Line))
	}

	// Add the error message
	sb.WriteString(formatErrorMessage(te.Err))

	return sb.String()
}

// formatErrorMessage formats an error in Python style
func formatErrorMessage(err error) string {
	if err == nil {
		return "Error: <nil>"
	}

	// Check for specific M28 error types
	switch e := err.(type) {
	case *TypeError:
		return fmt.Sprintf("TypeError: %s", e.Message)
	case *ValueError:
		return fmt.Sprintf("ValueError: %s", e.Message)
	case *AttributeError:
		return fmt.Sprintf("AttributeError: %s", e.Message)
	case *KeyError:
		return fmt.Sprintf("KeyError: %s", e.Message)
	case *IndexError:
		return fmt.Sprintf("IndexError: %s", e.Message)
	case *NameError:
		return fmt.Sprintf("NameError: name '%s' is not defined", e.Name)
	case *ImportError:
		return fmt.Sprintf("ImportError: %s", e.Message)
	case *StopIteration:
		if e.Message != "" {
			return fmt.Sprintf("StopIteration: %s", e.Message)
		}
		return "StopIteration"
	case *ExceptionValue:
		return e.String()
	default:
		// Try to extract a type name from the error
		errStr := err.Error()
		// If it already looks like "TypeName: message", keep it
		if strings.Contains(errStr, ": ") {
			return errStr
		}
		return fmt.Sprintf("Exception: %s", errStr)
	}
}

// WrapWithTraceback wraps an error with traceback information
func WrapWithTraceback(err error, stack *CallStack, loc *SourceLocation) error {
	if err == nil {
		return nil
	}

	// Don't double-wrap
	if _, ok := err.(*TracebackError); ok {
		return err
	}

	var frames []*CallFrame
	if stack != nil {
		frames = stack.Snapshot()
	}

	return &TracebackError{
		Err:      err,
		Frames:   frames,
		Location: loc,
	}
}

// Global call stack for the current goroutine
// In a multi-threaded scenario, this would need to be goroutine-local
var globalCallStack = NewCallStack()

// GetCallStack returns the global call stack
func GetCallStack() *CallStack {
	return globalCallStack
}

// PushFrame is a convenience function to push a frame onto the global stack
func PushFrame(funcName string, loc *SourceLocation) {
	globalCallStack.Push(funcName, loc)
}

// PopFrame is a convenience function to pop a frame from the global stack
func PopFrame() {
	globalCallStack.Pop()
}

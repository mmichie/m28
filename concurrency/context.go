// Package concurrency provides concurrency primitives for M28 programs.
package concurrency

import (
	"fmt"
	"sync"
	"time"

	"github.com/mmichie/m28/core"
)

// ContextType represents the M28 context object type
type ContextType string

const (
	// TypeContext is the type identifier for context objects
	TypeContext ContextType = "context"
)

// M28Context represents a context with cancellation
type M28Context struct {
	parent    *M28Context
	done      chan struct{}
	err       error
	mu        sync.Mutex
	deadline  time.Time
	hasDeadl  bool
	children  []*M28Context
	canceled  bool
	evaluator core.Evaluator
}

// NewBackgroundContext creates a new background context that is never canceled
func NewBackgroundContext() *M28Context {
	return &M28Context{
		done: make(chan struct{}),
	}
}

// WithCancel creates a new cancellable child context
func (c *M28Context) WithCancel() *M28Context {
	child := &M28Context{
		parent: c,
		done:   make(chan struct{}),
	}

	// Inherit deadline from parent if it exists
	if c.hasDeadl {
		child.deadline = c.deadline
		child.hasDeadl = true
	}

	// Add to parent's children list
	c.mu.Lock()
	c.children = append(c.children, child)
	c.mu.Unlock()

	// Set up cancellation propagation from parent
	if c.done != nil {
		go func() {
			select {
			case <-c.done:
				// Parent was canceled, cancel this context too
				child.cancel(fmt.Errorf("parent context canceled"))
			case <-child.done:
				// This context was explicitly canceled
			}
		}()
	}

	return child
}

// WithDeadline creates a new context with a deadline
func (c *M28Context) WithDeadline(deadline time.Time) *M28Context {
	child := c.WithCancel()
	child.deadline = deadline
	child.hasDeadl = true

	// Set up automatic cancellation at deadline
	if !deadline.IsZero() {
		remaining := time.Until(deadline)
		if remaining <= 0 {
			// Deadline already passed, cancel immediately
			child.cancel(fmt.Errorf("deadline already passed"))
			return child
		}

		go func() {
			select {
			case <-time.After(remaining):
				child.cancel(fmt.Errorf("deadline exceeded"))
			case <-child.done:
				// Context was canceled before deadline
			}
		}()
	}

	return child
}

// WithTimeout creates a new context with a timeout
func (c *M28Context) WithTimeout(timeout time.Duration) *M28Context {
	return c.WithDeadline(time.Now().Add(timeout))
}

// Done returns a channel that's closed when this context is canceled
func (c *M28Context) Done() <-chan struct{} {
	return c.done
}

// Canceled returns true if this context has been canceled
func (c *M28Context) Canceled() bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.canceled
}

// Err returns the error describing why this context was canceled
func (c *M28Context) Err() error {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.err
}

// cancel cancels this context and all its children
func (c *M28Context) cancel(err error) {
	c.mu.Lock()

	if c.canceled {
		c.mu.Unlock()
		return // Already canceled
	}

	c.canceled = true
	c.err = err

	// Close the done channel
	close(c.done)

	// Cancel all children
	for _, child := range c.children {
		child.cancel(fmt.Errorf("parent context canceled: %v", err))
	}

	// Clear children slice to allow GC
	c.children = nil

	c.mu.Unlock()
}

// Deadline returns the deadline time and whether a deadline is set
func (c *M28Context) Deadline() (time.Time, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.deadline, c.hasDeadl
}

// SetEvaluator sets the evaluator for the context
func (c *M28Context) SetEvaluator(e core.Evaluator) {
	c.evaluator = e
}

// GetEvaluator gets the evaluator for the context
func (c *M28Context) GetEvaluator() core.Evaluator {
	return c.evaluator
}

// String implements the LispValue interface
func (c *M28Context) String() string {
	state := "active"
	if c.Canceled() {
		state = "canceled"
	} else if c.hasDeadl {
		if time.Now().After(c.deadline) {
			state = "expired"
		} else {
			remaining := time.Until(c.deadline).Round(time.Millisecond)
			state = fmt.Sprintf("deadline in %v", remaining)
		}
	}

	return fmt.Sprintf("<context: %s>", state)
}

// Equal implements the LispValue interface
func (c *M28Context) Equal(other core.LispValue) bool {
	if otherCtx, ok := other.(*M28Context); ok {
		return c == otherCtx
	}
	return false
}

// ContextBackgroundFunc creates a new background context
func ContextBackgroundFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("context-background takes no arguments")
	}

	return NewBackgroundContext(), nil
}

// ContextWithCancelFunc creates a new cancelable context
func ContextWithCancelFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("context-with-cancel takes exactly one context argument")
	}

	// Get the parent context
	parentCtx, ok := args[0].(*M28Context)
	if !ok {
		return nil, fmt.Errorf("context-with-cancel expected context, got %T", args[0])
	}

	// Create the new context
	ctx := parentCtx.WithCancel()

	// Return the context and its cancel function
	cancelFunc := core.NewBuiltinFunction("context-cancel", func(args ...core.LispValue) (core.LispValue, error) {
		if len(args) > 1 {
			return nil, fmt.Errorf("cancel function takes at most one argument")
		}

		var err error
		if len(args) == 1 {
			// Optional error message
			if errMsg, isStr := args[0].(string); isStr {
				err = fmt.Errorf(errMsg)
			} else {
				err = fmt.Errorf("context canceled: %v", args[0])
			}
		} else {
			err = fmt.Errorf("context canceled")
		}

		ctx.cancel(err)
		return core.PythonicNone{}, nil
	})

	return core.LispList{ctx, cancelFunc}, nil
}

// ContextWithTimeoutFunc creates a new context with a timeout
func ContextWithTimeoutFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("context-with-timeout takes exactly two arguments: context and milliseconds")
	}

	// Get the parent context
	parentCtx, ok := args[0].(*M28Context)
	if !ok {
		return nil, fmt.Errorf("context-with-timeout expected context, got %T", args[0])
	}

	// Get the timeout in milliseconds
	timeoutMs, ok := args[1].(float64)
	if !ok {
		return nil, fmt.Errorf("context-with-timeout expected timeout in milliseconds (number), got %T", args[1])
	}

	// Create the new context
	ctx := parentCtx.WithTimeout(time.Duration(timeoutMs) * time.Millisecond)

	// Return the context and its cancel function
	cancelFunc := core.NewBuiltinFunction("context-cancel", func(args ...core.LispValue) (core.LispValue, error) {
		if len(args) > 1 {
			return nil, fmt.Errorf("cancel function takes at most one argument")
		}

		var err error
		if len(args) == 1 {
			// Optional error message
			if errMsg, isStr := args[0].(string); isStr {
				err = fmt.Errorf(errMsg)
			} else {
				err = fmt.Errorf("context canceled: %v", args[0])
			}
		} else {
			err = fmt.Errorf("context canceled")
		}

		ctx.cancel(err)
		return core.PythonicNone{}, nil
	})

	return core.LispList{ctx, cancelFunc}, nil
}

// ContextDoneFunc returns a channel that's closed when the context is done
func ContextDoneFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("context-done takes exactly one context argument")
	}

	// Get the context
	ctx, ok := args[0].(*M28Context)
	if !ok {
		return nil, fmt.Errorf("context-done expected context, got %T", args[0])
	}

	return NewChannelFromGoChannel(ctx.done), nil
}

// ContextCanceledFunc returns whether the context has been canceled
func ContextCanceledFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("context-canceled? takes exactly one context argument")
	}

	// Get the context
	ctx, ok := args[0].(*M28Context)
	if !ok {
		return nil, fmt.Errorf("context-canceled? expected context, got %T", args[0])
	}

	return core.PythonicBool(ctx.Canceled()), nil
}

// ContextErrorFunc returns the error describing why the context was canceled
func ContextErrorFunc(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("context-error takes exactly one context argument")
	}

	// Get the context
	ctx, ok := args[0].(*M28Context)
	if !ok {
		return nil, fmt.Errorf("context-error expected context, got %T", args[0])
	}

	err := ctx.Err()
	if err == nil {
		return core.PythonicNone{}, nil
	}

	return err.Error(), nil
}

// RegisterContextFunctions registers context-related functions
func RegisterContextFunctions() {
	core.RegisterBuiltin("context-background", ContextBackgroundFunc)
	core.RegisterBuiltin("context-with-cancel", ContextWithCancelFunc)
	core.RegisterBuiltin("context-with-timeout", ContextWithTimeoutFunc)
	core.RegisterBuiltin("context-done", ContextDoneFunc)
	core.RegisterBuiltin("context-canceled?", ContextCanceledFunc)
	core.RegisterBuiltin("context-error", ContextErrorFunc)
}

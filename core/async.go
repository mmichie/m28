package core

import (
	"fmt"
	"sync"
	"time"
)

// Task represents an async task (goroutine)
type Task struct {
	BaseObject
	Name     string
	Function Value
	Args     []Value
	Result   Value
	Err      error
	Done     chan bool
	Started  bool
	Finished bool
	Mu       sync.Mutex
	registry *MethodRegistry
}

// NewTask creates a new async task
func NewTask(name string, function Value, args []Value) *Task {
	t := &Task{
		BaseObject: *NewBaseObject(Type("task")),
		Name:       name,
		Function:   function,
		Args:       args,
		Done:       make(chan bool, 1),
	}

	// Initialize the method registry
	t.registry = t.createRegistry()

	return t
}

// Type returns the task type
func (t *Task) Type() Type {
	return Type("task")
}

// String returns the string representation
func (t *Task) String() string {
	t.Mu.Lock()
	defer t.Mu.Unlock()

	status := "pending"
	if t.Finished {
		status = "finished"
	} else if t.Started {
		status = "running"
	}

	if t.Name != "" {
		return fmt.Sprintf("<Task '%s' %s>", t.Name, status)
	}
	return fmt.Sprintf("<Task %s>", status)
}

// Start begins execution of the task
func (t *Task) Start(ctx *Context) {
	t.Mu.Lock()
	if t.Started {
		t.Mu.Unlock()
		return
	}
	t.Started = true
	t.Mu.Unlock()

	// Run in goroutine
	go func() {
		defer func() {
			t.Mu.Lock()
			t.Finished = true
			t.Mu.Unlock()
			t.Done <- true
		}()

		// Call the function
		if callable, ok := t.Function.(interface {
			Call([]Value, *Context) (Value, error)
		}); ok {
			t.Result, t.Err = callable.Call(t.Args, ctx)
		} else {
			t.Err = fmt.Errorf("task function is not callable")
		}
	}()
}

// Wait waits for the task to complete and returns its result
func (t *Task) Wait() (Value, error) {
	<-t.Done
	return t.Result, t.Err
}

// IsFinished checks if the task has completed
func (t *Task) IsFinished() bool {
	t.Mu.Lock()
	defer t.Mu.Unlock()
	return t.Finished
}

// createRegistry sets up all methods for task
func (t *Task) createRegistry() *MethodRegistry {
	registry := NewMethodRegistry()

	// Register methods
	registry.RegisterMethods(
		// result method
		MakeMethod("result", 0, "Get the result of the task (blocking)",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				task, err := TypedReceiver[*Task](receiver, "result")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("result", args, 0); err != nil {
					return nil, err
				}
				return task.Wait()
			}),

		// done method
		MakeMethod("done", 0, "Check if the task is finished",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				task, err := TypedReceiver[*Task](receiver, "done")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("done", args, 0); err != nil {
					return nil, err
				}
				return BoolValue(task.IsFinished()), nil
			}),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (t *Task) GetRegistry() *MethodRegistry {
	return t.registry
}

// GetBaseObject implements AttributeProvider
func (t *Task) GetBaseObject() *BaseObject {
	return &t.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (t *Task) GetAttr(name string) (Value, bool) {
	return GetAttrWithRegistry(t, name)
}

// Channel represents a Go channel with Pythonic interface
type Channel struct {
	BaseObject
	ch       chan Value
	capacity int
	closed   bool
	mu       sync.Mutex
	registry *MethodRegistry
}

// NewChannel creates a new channel
func NewChannel(capacity int) *Channel {
	c := &Channel{
		BaseObject: *NewBaseObject(Type("channel")),
		ch:         make(chan Value, capacity),
		capacity:   capacity,
	}

	// Initialize the method registry
	c.registry = c.createRegistry()

	return c
}

// Type returns the channel type
func (c *Channel) Type() Type {
	return Type("channel")
}

// String returns the string representation
func (c *Channel) String() string {
	c.mu.Lock()
	defer c.mu.Unlock()

	if c.closed {
		return fmt.Sprintf("<channel(capacity=%d) closed>", c.capacity)
	}
	return fmt.Sprintf("<channel(capacity=%d)>", c.capacity)
}

// Send sends a value to the channel
func (c *Channel) Send(value Value) error {
	c.mu.Lock()
	if c.closed {
		c.mu.Unlock()
		return fmt.Errorf("send on closed channel")
	}
	c.mu.Unlock()

	c.ch <- value
	return nil
}

// Receive receives a value from the channel
func (c *Channel) Receive() (Value, error) {
	value, ok := <-c.ch
	if !ok {
		return nil, fmt.Errorf("receive from closed channel")
	}
	return value, nil
}

// TryReceive attempts to receive without blocking
func (c *Channel) TryReceive() (Value, bool) {
	select {
	case value, ok := <-c.ch:
		return value, ok
	default:
		return nil, false
	}
}

// Close closes the channel
func (c *Channel) Close() error {
	c.mu.Lock()
	defer c.mu.Unlock()

	if c.closed {
		return fmt.Errorf("close of closed channel")
	}

	c.closed = true
	close(c.ch)
	return nil
}

// GetChan returns the internal Go channel
func (c *Channel) GetChan() chan Value {
	return c.ch
}

// createRegistry sets up all methods for channel
func (c *Channel) createRegistry() *MethodRegistry {
	registry := NewMethodRegistry()

	// Register methods
	registry.RegisterMethods(
		// send method (also aliased as "put")
		MakeMethod("send", 1, "Send a value to the channel",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				ch, err := TypedReceiver[*Channel](receiver, "send")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("send", args, 1); err != nil {
					return nil, err
				}

				err = ch.Send(args[0])
				if err != nil {
					return nil, err
				}
				return Nil, nil
			}),

		// put is an alias for send
		MakeMethod("put", 1, "Send a value to the channel",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				ch, err := TypedReceiver[*Channel](receiver, "put")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("put", args, 1); err != nil {
					return nil, err
				}

				err = ch.Send(args[0])
				if err != nil {
					return nil, err
				}
				return Nil, nil
			}),

		// receive method (also aliased as "recv" and "get")
		MakeMethod("receive", 0, "Receive a value from the channel",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				ch, err := TypedReceiver[*Channel](receiver, "receive")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("receive", args, 0); err != nil {
					return nil, err
				}
				return ch.Receive()
			}),

		// recv is an alias for receive
		MakeMethod("recv", 0, "Receive a value from the channel",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				ch, err := TypedReceiver[*Channel](receiver, "recv")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("recv", args, 0); err != nil {
					return nil, err
				}
				return ch.Receive()
			}),

		// get is an alias for receive
		MakeMethod("get", 0, "Receive a value from the channel",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				ch, err := TypedReceiver[*Channel](receiver, "get")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("get", args, 0); err != nil {
					return nil, err
				}
				return ch.Receive()
			}),

		// close method
		MakeMethod("close", 0, "Close the channel",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				ch, err := TypedReceiver[*Channel](receiver, "close")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("close", args, 0); err != nil {
					return nil, err
				}

				err = ch.Close()
				if err != nil {
					return nil, err
				}
				return Nil, nil
			}),

		// __len__ method
		MakeMethod("__len__", 0, "Get the number of values in the channel buffer",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				ch, err := TypedReceiver[*Channel](receiver, "__len__")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("__len__", args, 0); err != nil {
					return nil, err
				}
				return NumberValue(len(ch.ch)), nil
			}),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (c *Channel) GetRegistry() *MethodRegistry {
	return c.registry
}

// GetBaseObject implements AttributeProvider
func (c *Channel) GetBaseObject() *BaseObject {
	return &c.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (c *Channel) GetAttr(name string) (Value, bool) {
	return GetAttrWithRegistry(c, name)
}

// SelectCase represents a case in a select statement
type SelectCase struct {
	Channel *Channel
	Value   Value                      // Value to send (for send cases)
	IsSend  bool                       // true for send, false for receive
	Body    func(Value) (Value, error) // Function to execute
}

// Select performs a select operation on multiple channels
func Select(cases []SelectCase, defaultCase func() (Value, error)) (Value, error) {
	// Build reflect cases for Go's select
	goCases := make([]interface{}, 0, len(cases))

	for _, c := range cases {
		if c.IsSend {
			goCases = append(goCases, sendCase{ch: c.Channel.ch, val: c.Value})
		} else {
			goCases = append(goCases, recvCase{ch: c.Channel.ch})
		}
	}

	// Simple implementation - try each case
	for i, c := range cases {
		if c.IsSend {
			select {
			case c.Channel.ch <- c.Value:
				if c.Body != nil {
					return c.Body(Nil)
				}
				return NumberValue(i), nil
			default:
				continue
			}
		} else {
			select {
			case val, ok := <-c.Channel.ch:
				if !ok {
					continue
				}
				if c.Body != nil {
					return c.Body(val)
				}
				return val, nil
			default:
				continue
			}
		}
	}

	// No case ready, use default
	if defaultCase != nil {
		return defaultCase()
	}

	// Block on all cases
	// This is a simplified implementation
	// A full implementation would use reflect.Select
	return nil, fmt.Errorf("select: no cases ready and no default")
}

type sendCase struct {
	ch  chan Value
	val Value
}

type recvCase struct {
	ch chan Value
}

// Coroutine represents a Python coroutine object (unevaluated async function call)
type Coroutine struct {
	BaseObject
	Function Value
	Args     []Value
	Name     string
	closed   bool
	registry *MethodRegistry
}

// NewCoroutine creates a new coroutine
func NewCoroutine(function Value, args []Value, name string) *Coroutine {
	c := &Coroutine{
		BaseObject: *NewBaseObject(Type("coroutine")),
		Function:   function,
		Args:       args,
		Name:       name,
	}
	c.registry = c.createRegistry()
	return c
}

// Type returns the coroutine type
func (c *Coroutine) Type() Type {
	return Type("coroutine")
}

// String returns the string representation
func (c *Coroutine) String() string {
	if c.Name != "" {
		return fmt.Sprintf("<coroutine object %s>", c.Name)
	}
	return "<coroutine object>"
}

// Close closes the coroutine (prevents it from being executed)
func (c *Coroutine) Close() error {
	c.closed = true
	return nil
}

// createRegistry sets up all methods for coroutine
func (c *Coroutine) createRegistry() *MethodRegistry {
	registry := NewMethodRegistry()

	registry.RegisterMethods(
		MakeMethod("close", 0, "Close the coroutine",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				coro, err := TypedReceiver[*Coroutine](receiver, "close")
				if err != nil {
					return nil, err
				}
				if err := ValidateArity("close", args, 0); err != nil {
					return nil, err
				}
				return Nil, coro.Close()
			}),

		MakeMethod("send", 1, "Send a value into the coroutine",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				// Simplified: just return None for now
				// Full implementation would resume execution
				return Nil, nil
			}),

		MakeMethod("throw", 1, "Throw an exception into the coroutine",
			func(receiver Value, args []Value, ctx *Context) (Value, error) {
				// Simplified: just return None for now
				return Nil, nil
			}),
	)

	return registry
}

// GetRegistry implements AttributeProvider
func (c *Coroutine) GetRegistry() *MethodRegistry {
	return c.registry
}

// GetBaseObject implements AttributeProvider
func (c *Coroutine) GetBaseObject() *BaseObject {
	return &c.BaseObject
}

// GetAttr implements the new simplified GetAttr pattern
func (c *Coroutine) GetAttr(name string) (Value, bool) {
	return GetAttrWithRegistry(c, name)
}

// AsyncFunction wraps a function to run asynchronously
type AsyncFunction struct {
	BaseObject
	Function Value
	Name     string
}

// NewAsyncFunction creates a new async function
func NewAsyncFunction(function Value, name string) *AsyncFunction {
	return &AsyncFunction{
		BaseObject: *NewBaseObject(FunctionType),
		Function:   function,
		Name:       name,
	}
}

// Type returns the function type
func (af *AsyncFunction) Type() Type {
	return FunctionType
}

// String returns the string representation
func (af *AsyncFunction) String() string {
	if af.Name != "" {
		return fmt.Sprintf("<async function %s>", af.Name)
	}
	return "<async function>"
}

// Call creates a coroutine object (does NOT execute the function)
// This matches Python behavior where calling an async function returns a coroutine
func (af *AsyncFunction) Call(args []Value, ctx *Context) (Value, error) {
	// Return a coroutine object, not a running task
	// The coroutine can be awaited later or closed
	return NewCoroutine(af.Function, args, af.Name), nil
}

// Sleep pauses execution for the specified duration
func Sleep(seconds float64) {
	time.Sleep(time.Duration(seconds * float64(time.Second)))
}

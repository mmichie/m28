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
}

// NewTask creates a new async task
func NewTask(name string, function Value, args []Value) *Task {
	return &Task{
		BaseObject: *NewBaseObject(Type("task")),
		Name:       name,
		Function:   function,
		Args:       args,
		Done:       make(chan bool, 1),
	}
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

// GetAttr implements Object interface
func (t *Task) GetAttr(name string) (Value, bool) {
	switch name {
	case "result":
		return &BoundMethod{
			Receiver: t,
			Method: &MethodDescriptor{
				Name:    "result",
				Arity:   0,
				Doc:     "Get the result of the task (blocking)",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					task := receiver.(*Task)
					return task.Wait()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(t),
		}, true
		
	case "done":
		return &BoundMethod{
			Receiver: t,
			Method: &MethodDescriptor{
				Name:    "done",
				Arity:   0,
				Doc:     "Check if the task is finished",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					task := receiver.(*Task)
					return BoolValue(task.IsFinished()), nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(t),
		}, true
	}
	
	return t.BaseObject.GetAttr(name)
}

// Channel represents a Go channel with Pythonic interface
type Channel struct {
	BaseObject
	ch       chan Value
	capacity int
	closed   bool
	mu       sync.Mutex
}

// NewChannel creates a new channel
func NewChannel(capacity int) *Channel {
	return &Channel{
		BaseObject: *NewBaseObject(Type("channel")),
		ch:         make(chan Value, capacity),
		capacity:   capacity,
	}
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

// GetAttr implements Object interface
func (c *Channel) GetAttr(name string) (Value, bool) {
	switch name {
	case "send", "put":
		return &BoundMethod{
			Receiver: c,
			Method: &MethodDescriptor{
				Name:    "send",
				Arity:   1,
				Doc:     "Send a value to the channel",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					if len(args) != 1 {
						return nil, fmt.Errorf("send() takes exactly one argument")
					}
					ch := receiver.(*Channel)
					err := ch.Send(args[0])
					if err != nil {
						return nil, err
					}
					return Nil, nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(c),
		}, true
		
	case "receive", "recv", "get":
		return &BoundMethod{
			Receiver: c,
			Method: &MethodDescriptor{
				Name:    "receive",
				Arity:   0,
				Doc:     "Receive a value from the channel",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					ch := receiver.(*Channel)
					return ch.Receive()
				},
			},
			TypeDesc: GetTypeDescriptorForValue(c),
		}, true
		
	case "close":
		return &BoundMethod{
			Receiver: c,
			Method: &MethodDescriptor{
				Name:    "close",
				Arity:   0,
				Doc:     "Close the channel",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					ch := receiver.(*Channel)
					err := ch.Close()
					if err != nil {
						return nil, err
					}
					return Nil, nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(c),
		}, true
		
	case "__len__":
		return &BoundMethod{
			Receiver: c,
			Method: &MethodDescriptor{
				Name:    "__len__",
				Arity:   0,
				Doc:     "Get the number of values in the channel buffer",
				Builtin: true,
				Handler: func(receiver Value, args []Value, ctx *Context) (Value, error) {
					ch := receiver.(*Channel)
					return NumberValue(len(ch.ch)), nil
				},
			},
			TypeDesc: GetTypeDescriptorForValue(c),
		}, true
	}
	
	return c.BaseObject.GetAttr(name)
}

// SelectCase represents a case in a select statement
type SelectCase struct {
	Channel *Channel
	Value   Value      // Value to send (for send cases)
	IsSend  bool       // true for send, false for receive
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

// Call creates and starts a new task
func (af *AsyncFunction) Call(args []Value, ctx *Context) (Value, error) {
	task := NewTask(af.Name, af.Function, args)
	task.Start(ctx)
	return task, nil
}

// Sleep pauses execution for the specified duration
func Sleep(seconds float64) {
	time.Sleep(time.Duration(seconds * float64(time.Second)))
}
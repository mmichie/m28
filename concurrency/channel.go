package concurrency

import (
	"fmt"
	"sync"

	"github.com/mmichie/m28/core"
)

// PythonicChannel represents a Go-like channel for M28
type PythonicChannel struct {
	ch     chan core.LispValue // The actual Go channel
	closed bool                // Whether the channel is closed
	mu     sync.RWMutex        // For thread safety around the closed flag
	buffer int                 // Buffer size (0 for unbuffered)
}

// NewPythonicChannel creates a new channel with the given buffer size
func NewPythonicChannel(buffer int) *PythonicChannel {
	return &PythonicChannel{
		ch:     make(chan core.LispValue, buffer),
		closed: false,
		buffer: buffer,
	}
}

// DummyChannelType represents the type for dummy channels
type DummyChannelType string

const (
	// TypeDummyChannel is the type identifier for dummy channels
	TypeDummyChannel DummyChannelType = "channel"
)

// DummyChannel is a simple struct that implements the necessary interfaces
// to function as a channel placeholder until the real channel implementation is added
type DummyChannel struct {
	closed bool
	name   string
}

// String returns a string representation of the dummy channel
func (c *DummyChannel) String() string {
	return fmt.Sprintf("<dummy channel: %s>", c.name)
}

// NewChannelFromGoChannel creates a dummy channel that represents a Go channel
// This is a temporary implementation until the full channel system is implemented
func NewChannelFromGoChannel(ch chan struct{}) *DummyChannel {
	return &DummyChannel{
		closed: false,
		name:   "context.done",
	}
}

// Send sends a value to the channel
func (c *PythonicChannel) Send(value core.LispValue) error {
	c.mu.RLock()
	closed := c.closed
	c.mu.RUnlock()

	if closed {
		return fmt.Errorf("send on closed channel")
	}

	c.ch <- value
	return nil
}

// TrySend attempts to send a value to the channel without blocking
// Returns true if the value was sent successfully, false otherwise
func (c *PythonicChannel) TrySend(value core.LispValue) (bool, error) {
	c.mu.RLock()
	closed := c.closed
	c.mu.RUnlock()

	if closed {
		return false, fmt.Errorf("send on closed channel")
	}

	select {
	case c.ch <- value:
		return true, nil
	default:
		return false, nil
	}
}

// Receive receives a value from the channel
// Returns the value and a boolean indicating whether the channel is still open
func (c *PythonicChannel) Receive() (core.LispValue, bool, error) {
	value, ok := <-c.ch
	return value, ok, nil
}

// TryReceive attempts to receive a value from the channel without blocking
// Returns the value, a boolean indicating success, and a boolean indicating whether the channel is still open
func (c *PythonicChannel) TryReceive() (core.LispValue, bool, bool, error) {
	select {
	case value, ok := <-c.ch:
		return value, true, ok, nil
	default:
		return nil, false, true, nil
	}
}

// Close closes the channel
func (c *PythonicChannel) Close() {
	c.mu.Lock()
	defer c.mu.Unlock()

	if !c.closed {
		close(c.ch)
		c.closed = true
	}
}

// IsClosed returns whether the channel is closed
func (c *PythonicChannel) IsClosed() bool {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return c.closed
}

// String returns a string representation of the channel
func (c *PythonicChannel) String() string {
	c.mu.RLock()
	defer c.mu.RUnlock()

	if c.buffer > 0 {
		return fmt.Sprintf("<channel buffer=%d closed=%t>", c.buffer, c.closed)
	}
	return fmt.Sprintf("<channel unbuffered closed=%t>", c.closed)
}

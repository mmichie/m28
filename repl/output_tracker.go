package repl

import (
	"bytes"
	"io"
	"strings"
	"sync"
)

// OutputTracker wraps an io.Writer and tracks what was printed
type OutputTracker struct {
	writer       io.Writer
	mu           sync.Mutex
	lastOutput   string
	buffer       bytes.Buffer
	trackingMode bool
}

// NewOutputTracker creates a new output tracker
func NewOutputTracker(writer io.Writer) *OutputTracker {
	return &OutputTracker{
		writer:       writer,
		trackingMode: false,
	}
}

// Write implements io.Writer
func (ot *OutputTracker) Write(p []byte) (n int, err error) {
	ot.mu.Lock()
	defer ot.mu.Unlock()

	// Always write to the underlying writer
	n, err = ot.writer.Write(p)

	// Track output if in tracking mode
	if ot.trackingMode {
		ot.buffer.Write(p)
	}

	return n, err
}

// StartTracking starts tracking output
func (ot *OutputTracker) StartTracking() {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	ot.trackingMode = true
	ot.buffer.Reset()
}

// StopTracking stops tracking and returns the tracked output
func (ot *OutputTracker) StopTracking() string {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	ot.trackingMode = false
	output := strings.TrimRight(ot.buffer.String(), "\n")
	ot.lastOutput = output
	ot.buffer.Reset()
	return output
}

// GetLastOutput returns the last tracked output
func (ot *OutputTracker) GetLastOutput() string {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	return ot.lastOutput
}

// ClearLastOutput clears the last tracked output
func (ot *OutputTracker) ClearLastOutput() {
	ot.mu.Lock()
	defer ot.mu.Unlock()
	ot.lastOutput = ""
}

package builtin

import (
	"fmt"

	"github.com/mmichie/m28/concurrency"
	"github.com/mmichie/m28/core"
)

func init() {
	// Channel creation
	core.RegisterBuiltin("chan", MakeChan)
	core.RegisterBuiltin("make-chan", MakeChan) // Alternative name

	// Channel operations
	core.RegisterBuiltin("send", ChanSend)
	core.RegisterBuiltin("try-send", ChanTrySend)
	core.RegisterBuiltin("recv", ChanRecv)
	core.RegisterBuiltin("try-recv", ChanTryRecv)
	core.RegisterBuiltin("close-chan", ChanClose)
	core.RegisterBuiltin("chan-closed?", ChanIsClosed)

	// Mutex creation
	core.RegisterBuiltin("mutex", concurrency.MakeMutex)
	core.RegisterBuiltin("make-mutex", concurrency.MakeMutex)
	core.RegisterBuiltin("rwmutex", concurrency.MakeRWMutex)
	core.RegisterBuiltin("make-rwmutex", concurrency.MakeRWMutex)

	// Mutex operations
	core.RegisterBuiltin("mutex-lock", concurrency.MutexLock)
	core.RegisterBuiltin("mutex-unlock", concurrency.MutexUnlock)
	core.RegisterBuiltin("mutex-try-lock", concurrency.MutexTryLock)
	core.RegisterBuiltin("rwmutex-rlock", concurrency.RWMutexRLock)
	core.RegisterBuiltin("rwmutex-runlock", concurrency.RWMutexRUnlock)
	core.RegisterBuiltin("rwmutex-try-rlock", concurrency.RWMutexTryRLock)

	// WaitGroup creation and operations
	core.RegisterBuiltin("waitgroup", concurrency.MakeWaitGroup)
	core.RegisterBuiltin("make-waitgroup", concurrency.MakeWaitGroup)
	core.RegisterBuiltin("waitgroup-add", concurrency.WaitGroupAdd)
	core.RegisterBuiltin("waitgroup-done", concurrency.WaitGroupDone)
	core.RegisterBuiltin("waitgroup-wait", concurrency.WaitGroupWait)
}

// MakeChan creates a new channel with an optional buffer size
func MakeChan(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) > 1 {
		return nil, fmt.Errorf("make-chan takes 0 or 1 arguments")
	}

	// Default buffer size is 0 (unbuffered)
	bufferSize := 0

	// If a buffer size is provided, use it
	if len(args) == 1 {
		if n, ok := args[0].(float64); ok {
			if n < 0 {
				return nil, fmt.Errorf("buffer size cannot be negative")
			}
			bufferSize = int(n)
		} else {
			return nil, fmt.Errorf("buffer size must be a non-negative integer")
		}
	}

	// Create the channel
	return concurrency.NewPythonicChannel(bufferSize), nil
}

// ChanSend sends a value to a channel (blocking)
func ChanSend(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("send requires 2 arguments: channel and value")
	}

	// Extract the channel
	ch, ok := args[0].(*concurrency.PythonicChannel)
	if !ok {
		return nil, fmt.Errorf("first argument must be a channel")
	}

	// Send the value
	err := ch.Send(args[1])
	if err != nil {
		return nil, err
	}

	// Return None on success
	return core.PythonicNone{}, nil
}

// ChanTrySend attempts to send a value to a channel without blocking
// Returns a boolean indicating success
func ChanTrySend(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("try-send requires 2 arguments: channel and value")
	}

	// Extract the channel
	ch, ok := args[0].(*concurrency.PythonicChannel)
	if !ok {
		return nil, fmt.Errorf("first argument must be a channel")
	}

	// Try to send the value
	success, err := ch.TrySend(args[1])
	if err != nil {
		return nil, err
	}

	// Return success boolean
	return core.PythonicBool(success), nil
}

// ChanRecv receives a value from a channel (blocking)
// Returns the value and a boolean indicating whether the channel is open
func ChanRecv(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("recv requires 1 argument: channel")
	}

	// Extract the channel
	ch, ok := args[0].(*concurrency.PythonicChannel)
	if !ok {
		return nil, fmt.Errorf("argument must be a channel")
	}

	// Receive value from the channel
	value, ok, err := ch.Receive()
	if err != nil {
		return nil, err
	}

	// Return a tuple of (value, ok)
	return core.LispTuple{value, core.PythonicBool(ok)}, nil
}

// ChanTryRecv attempts to receive a value from a channel without blocking
// Returns a tuple of (value, success, ok)
// - value: the received value or None if no value was available
// - success: true if a value was received, false if the channel was empty
// - ok: true if the channel is still open, false if closed
func ChanTryRecv(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("try-recv requires 1 argument: channel")
	}

	// Extract the channel
	ch, ok := args[0].(*concurrency.PythonicChannel)
	if !ok {
		return nil, fmt.Errorf("argument must be a channel")
	}

	// Try to receive value from the channel
	value, success, ok, err := ch.TryReceive()
	if err != nil {
		return nil, err
	}

	// If no value was received, return None for the value
	if !success {
		value = core.PythonicNone{}
	}

	// Return a tuple of (value, success, ok)
	return core.LispTuple{value, core.PythonicBool(success), core.PythonicBool(ok)}, nil
}

// ChanClose closes a channel
func ChanClose(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("close-chan requires 1 argument: channel")
	}

	// Extract the channel
	ch, ok := args[0].(*concurrency.PythonicChannel)
	if !ok {
		return nil, fmt.Errorf("argument must be a channel")
	}

	// Close the channel
	ch.Close()

	// Return None on success
	return core.PythonicNone{}, nil
}

// ChanIsClosed checks if a channel is closed
func ChanIsClosed(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("chan-closed? requires 1 argument: channel")
	}

	// Extract the channel
	ch, ok := args[0].(*concurrency.PythonicChannel)
	if !ok {
		return nil, fmt.Errorf("argument must be a channel")
	}

	// Return whether the channel is closed
	return core.PythonicBool(ch.IsClosed()), nil
}

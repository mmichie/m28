package concurrency

import (
	"fmt"
	"sync"

	"github.com/mmichie/m28/core"
)

// PythonicMutex represents a mutex for M28
type PythonicMutex struct {
	mu sync.Mutex // The actual Go mutex
}

// NewPythonicMutex creates a new mutex
func NewPythonicMutex() *PythonicMutex {
	return &PythonicMutex{}
}

// Lock locks the mutex
func (m *PythonicMutex) Lock() {
	m.mu.Lock()
}

// Unlock unlocks the mutex
func (m *PythonicMutex) Unlock() {
	m.mu.Unlock()
}

// TryLock tries to lock the mutex without blocking
// Returns true if the lock was acquired, false otherwise
func (m *PythonicMutex) TryLock() bool {
	return m.mu.TryLock()
}

// String returns a string representation of the mutex
func (m *PythonicMutex) String() string {
	return "<mutex>"
}

// PythonicRWMutex represents a read-write mutex for M28
type PythonicRWMutex struct {
	mu sync.RWMutex // The actual Go RWMutex
}

// NewPythonicRWMutex creates a new read-write mutex
func NewPythonicRWMutex() *PythonicRWMutex {
	return &PythonicRWMutex{}
}

// Lock locks the mutex for writing
func (m *PythonicRWMutex) Lock() {
	m.mu.Lock()
}

// Unlock unlocks the mutex for writing
func (m *PythonicRWMutex) Unlock() {
	m.mu.Unlock()
}

// RLock locks the mutex for reading
func (m *PythonicRWMutex) RLock() {
	m.mu.RLock()
}

// RUnlock unlocks the mutex for reading
func (m *PythonicRWMutex) RUnlock() {
	m.mu.RUnlock()
}

// TryLock tries to lock the mutex for writing without blocking
func (m *PythonicRWMutex) TryLock() bool {
	return m.mu.TryRLock()
}

// TryRLock tries to lock the mutex for reading without blocking
func (m *PythonicRWMutex) TryRLock() bool {
	return m.mu.TryRLock()
}

// String returns a string representation of the RWMutex
func (m *PythonicRWMutex) String() string {
	return "<rwmutex>"
}

// PythonicWaitGroup represents a wait group for M28
type PythonicWaitGroup struct {
	wg sync.WaitGroup // The actual Go WaitGroup
}

// NewPythonicWaitGroup creates a new wait group
func NewPythonicWaitGroup() *PythonicWaitGroup {
	return &PythonicWaitGroup{}
}

// Add adds delta to the wait group counter
func (wg *PythonicWaitGroup) Add(delta int) {
	wg.wg.Add(delta)
}

// Done decrements the wait group counter
func (wg *PythonicWaitGroup) Done() {
	wg.wg.Done()
}

// Wait waits for the wait group counter to reach zero
func (wg *PythonicWaitGroup) Wait() {
	wg.wg.Wait()
}

// String returns a string representation of the wait group
func (wg *PythonicWaitGroup) String() string {
	return "<waitgroup>"
}

// EvalWithMutex implements the with-mutex special form
// (with-mutex mutex
//
//	body...
//
// )
func EvalWithMutex(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("with-mutex requires at least a mutex and one body expression")
	}

	// Evaluate the mutex expression
	mutexVal, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Check if it's a mutex
	var mutex interface {
		Lock()
		Unlock()
	}

	switch m := mutexVal.(type) {
	case *PythonicMutex:
		mutex = m
	case *PythonicRWMutex:
		mutex = m
	default:
		return nil, fmt.Errorf("with-mutex requires a mutex or rwmutex as first argument")
	}

	// Lock the mutex
	mutex.Lock()

	// Make sure we unlock even if there's an error
	defer mutex.Unlock()

	// Evaluate body expressions
	var result core.LispValue = core.PythonicNone{}
	for _, expr := range args[1:] {
		result, err = e.Eval(expr, env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

// EvalWithRLock implements the with-rlock special form
// (with-rlock rwmutex
//
//	body...
//
// )
func EvalWithRLock(e core.Evaluator, args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) < 2 {
		return nil, fmt.Errorf("with-rlock requires at least a rwmutex and one body expression")
	}

	// Evaluate the mutex expression
	mutexVal, err := e.Eval(args[0], env)
	if err != nil {
		return nil, err
	}

	// Check if it's an RWMutex
	rwmutex, ok := mutexVal.(*PythonicRWMutex)
	if !ok {
		return nil, fmt.Errorf("with-rlock requires a rwmutex as first argument")
	}

	// Lock for reading
	rwmutex.RLock()

	// Make sure we unlock even if there's an error
	defer rwmutex.RUnlock()

	// Evaluate body expressions
	var result core.LispValue = core.PythonicNone{}
	for _, expr := range args[1:] {
		result, err = e.Eval(expr, env)
		if err != nil {
			return nil, err
		}
	}

	return result, nil
}

// Built-in functions for mutexes and wait groups

// MakeMutex creates a new mutex
func MakeMutex(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("make-mutex takes no arguments")
	}
	return NewPythonicMutex(), nil
}

// MakeRWMutex creates a new read-write mutex
func MakeRWMutex(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("make-rwmutex takes no arguments")
	}
	return NewPythonicRWMutex(), nil
}

// MakeWaitGroup creates a new wait group
func MakeWaitGroup(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 0 {
		return nil, fmt.Errorf("make-waitgroup takes no arguments")
	}
	return NewPythonicWaitGroup(), nil
}

// MutexLock locks a mutex
func MutexLock(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("mutex-lock requires 1 argument: mutex")
	}

	switch m := args[0].(type) {
	case *PythonicMutex:
		m.Lock()
	case *PythonicRWMutex:
		m.Lock()
	default:
		return nil, fmt.Errorf("mutex-lock requires a mutex or rwmutex")
	}

	return core.PythonicNone{}, nil
}

// MutexUnlock unlocks a mutex
func MutexUnlock(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("mutex-unlock requires 1 argument: mutex")
	}

	switch m := args[0].(type) {
	case *PythonicMutex:
		m.Unlock()
	case *PythonicRWMutex:
		m.Unlock()
	default:
		return nil, fmt.Errorf("mutex-unlock requires a mutex or rwmutex")
	}

	return core.PythonicNone{}, nil
}

// MutexTryLock tries to lock a mutex without blocking
func MutexTryLock(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("mutex-try-lock requires 1 argument: mutex")
	}

	var success bool
	switch m := args[0].(type) {
	case *PythonicMutex:
		success = m.TryLock()
	case *PythonicRWMutex:
		success = m.TryLock()
	default:
		return nil, fmt.Errorf("mutex-try-lock requires a mutex or rwmutex")
	}

	return core.PythonicBool(success), nil
}

// RWMutexRLock locks a read-write mutex for reading
func RWMutexRLock(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("rwmutex-rlock requires 1 argument: rwmutex")
	}

	m, ok := args[0].(*PythonicRWMutex)
	if !ok {
		return nil, fmt.Errorf("rwmutex-rlock requires a rwmutex")
	}

	m.RLock()
	return core.PythonicNone{}, nil
}

// RWMutexRUnlock unlocks a read-write mutex for reading
func RWMutexRUnlock(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("rwmutex-runlock requires 1 argument: rwmutex")
	}

	m, ok := args[0].(*PythonicRWMutex)
	if !ok {
		return nil, fmt.Errorf("rwmutex-runlock requires a rwmutex")
	}

	m.RUnlock()
	return core.PythonicNone{}, nil
}

// RWMutexTryRLock tries to lock a read-write mutex for reading without blocking
func RWMutexTryRLock(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("rwmutex-try-rlock requires 1 argument: rwmutex")
	}

	m, ok := args[0].(*PythonicRWMutex)
	if !ok {
		return nil, fmt.Errorf("rwmutex-try-rlock requires a rwmutex")
	}

	success := m.TryRLock()
	return core.PythonicBool(success), nil
}

// WaitGroupAdd adds delta to a wait group counter
func WaitGroupAdd(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("waitgroup-add requires 2 arguments: waitgroup and delta")
	}

	wg, ok := args[0].(*PythonicWaitGroup)
	if !ok {
		return nil, fmt.Errorf("waitgroup-add requires a waitgroup as first argument")
	}

	delta, ok := args[1].(float64)
	if !ok {
		return nil, fmt.Errorf("waitgroup-add requires a number as second argument")
	}

	wg.Add(int(delta))
	return core.PythonicNone{}, nil
}

// WaitGroupDone decrements a wait group counter
func WaitGroupDone(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("waitgroup-done requires 1 argument: waitgroup")
	}

	wg, ok := args[0].(*PythonicWaitGroup)
	if !ok {
		return nil, fmt.Errorf("waitgroup-done requires a waitgroup")
	}

	wg.Done()
	return core.PythonicNone{}, nil
}

// WaitGroupWait waits for a wait group counter to reach zero
func WaitGroupWait(args []core.LispValue, env core.Environment) (core.LispValue, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("waitgroup-wait requires 1 argument: waitgroup")
	}

	wg, ok := args[0].(*PythonicWaitGroup)
	if !ok {
		return nil, fmt.Errorf("waitgroup-wait requires a waitgroup")
	}

	wg.Wait()
	return core.PythonicNone{}, nil
}

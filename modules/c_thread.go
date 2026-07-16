package modules

import (
	"fmt"
	"sync"
	"sync/atomic"
	"time"

	"github.com/mmichie/m28/common/validation"
	"github.com/mmichie/m28/core"
)

// Thread ID counter
var threadIDCounter int64

// InitThreadModule initializes the _thread C extension module
// This provides threading support for Python stdlib modules
func InitThreadModule() *core.DictValue {
	threadModule := core.NewDict()

	// start_new_thread(function, args, kwargs=None)
	// Starts a new thread and returns its identifier
	threadModule.SetStr("start_new_thread", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("start_new_thread", args)
		if err := v.Range(2, 3); err != nil {
			return nil, err
		}

		fn, err := v.GetCallable(0)
		if err != nil {
			return nil, err
		}

		threadArgs, err := v.GetList(1)
		if err != nil {
			return nil, err
		}

		// Generate thread ID
		threadID := atomic.AddInt64(&threadIDCounter, 1)

		// Start goroutine to run the function
		go func() {
			// Convert list to slice for function call
			argList := threadArgs.Items()
			_, _ = fn.Call(argList, ctx)
		}()

		return core.NumberValue(float64(threadID)), nil
	}))

	// start_joinable_thread(function, *, handle=None, daemon=True) -> handle
	// Python 3.13 joinable-thread API used by threading.py. Runs function in a
	// goroutine and marks the handle done when it returns.
	threadModule.SetStr("start_joinable_thread", &core.BuiltinFunctionWithKwargs{
		BaseObject: *core.NewBaseObject(core.FunctionType),
		Name:       "start_joinable_thread",
		Fn: func(args []core.Value, kwargs *core.Kwargs, ctx *core.Context) (core.Value, error) {
			if len(args) < 1 {
				return nil, &core.TypeError{Message: "start_joinable_thread() missing required argument: 'function'"}
			}
			fn, ok := args[0].(interface {
				Call([]core.Value, *core.Context) (core.Value, error)
			})
			if !ok {
				return nil, &core.TypeError{Message: "start_joinable_thread() argument must be callable"}
			}
			var handle *ThreadHandle
			if h, ok := kwargs.Get("handle"); ok {
				handle, _ = h.(*ThreadHandle)
			}
			if handle == nil {
				handle = newThreadHandle(0)
			}
			id := atomic.AddInt64(&threadIDCounter, 1)
			handle.mu.Lock()
			handle.ident = id
			handle.mu.Unlock()
			go func() {
				defer handle.setDone()
				defer func() { _ = recover() }() // a thread panic must not crash the process
				_, _ = fn.Call(nil, ctx)
			}()
			return handle, nil
		},
	})

	// _make_thread_handle(ident) -> handle for an already-running thread
	// (main/dummy threads); not marked done.
	threadModule.SetStr("_make_thread_handle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		var id int64
		if len(args) >= 1 {
			if n, ok := args[0].(core.NumberValue); ok {
				id = int64(n)
			}
		}
		return newThreadHandle(id), nil
	}))

	// _ThreadHandle() -> a fresh, unstarted thread handle
	threadModule.SetStr("_ThreadHandle", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return newThreadHandle(0), nil
	}))

	// _get_main_thread_ident() -> int
	threadModule.SetStr("_get_main_thread_ident", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.NumberValue(1), nil
	}))

	// _is_main_interpreter() -> bool. M28 runs a single interpreter.
	threadModule.SetStr("_is_main_interpreter", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.BoolValue(true), nil
	}))

	// _shutdown() — called at interpreter exit; no-op for M28's goroutine threads.
	threadModule.SetStr("_shutdown", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return core.Nil, nil
	}))

	// allocate_lock() -> Lock
	// Creates a new lock object
	threadModule.SetStr("allocate_lock", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("allocate_lock", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return newThreadLock(), nil
	}))

	// get_ident() -> int
	// Returns the thread identifier of the current thread
	threadModule.SetStr("get_ident", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get_ident", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		// For now, return a fixed ID for the main thread
		// In a full implementation, this would return different IDs per goroutine
		return core.NumberValue(1), nil
	}))

	// get_native_id() -> int
	// Returns the native thread ID
	threadModule.SetStr("get_native_id", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("get_native_id", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return core.NumberValue(1), nil
	}))

	// RLock - reentrant lock (stub - returns regular lock for now)
	threadModule.SetStr("RLock", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("RLock", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}

		return newThreadLock(), nil
	}))

	// TIMEOUT_MAX constant
	threadModule.SetStr("TIMEOUT_MAX", core.NumberValue(float64(1<<31-1))) // Max 32-bit int

	// daemon_threads_allowed - controls whether daemon threads are allowed (Python 3.12+)
	threadModule.SetStr("daemon_threads_allowed", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("daemon_threads_allowed", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}
		// Always return True for now
		return core.BoolValue(true), nil
	}))

	// _set_sentinel() -> lock
	// Returns a lock that will be released when the thread exits
	threadModule.SetStr("_set_sentinel", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("_set_sentinel", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		// Return a lock that's already acquired
		lock := newThreadLock()
		lock.locked = true
		return lock, nil
	}))

	// _excepthook - hook for uncaught exceptions in threads
	threadModule.SetStr("_excepthook", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: just return nil
		return core.Nil, nil
	}))

	// _is_stopped - check if a thread is stopped
	threadModule.SetStr("_is_stopped", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: always return False
		return core.BoolValue(false), nil
	}))

	// _stop - stop a thread
	threadModule.SetStr("_stop", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.Nil, nil
	}))

	// ident - alias for get_ident
	threadModule.SetStr("ident", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("ident", args)
		if err := v.Exact(0); err != nil {
			return nil, err
		}
		return core.NumberValue(1), nil
	}))

	// stack_size([size]) - get/set thread stack size
	threadModule.SetStr("stack_size", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("stack_size", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}
		// If called with no args, return current stack size (stub: 0)
		// If called with arg, set stack size and return previous (stub: ignore)
		return core.NumberValue(0), nil
	}))

	// _HAVE_THREAD_NATIVE_ID - indicates if get_native_id() is supported
	// Set to False for now to avoid issues with _set_native_id
	threadModule.SetStr("_HAVE_THREAD_NATIVE_ID", core.BoolValue(false))

	// HAVE_THREAD_NATIVE_ID - public version (alias)
	threadModule.SetStr("HAVE_THREAD_NATIVE_ID", core.BoolValue(false))

	// error - exception type for thread errors
	errorClass := core.NewClassWithParents("error", []*core.Class{})
	threadModule.SetStr("error", errorClass)

	// LockType - the lock constructor. threading.py does `Lock = _thread.LockType`
	// and then `Lock()`, so calling it must return a working lock instance.
	threadModule.SetStr("LockType", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		return newThreadLock(), nil
	}))

	// _local - thread-local storage class
	localClass := core.NewClass("_local", nil)
	localClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Thread-local storage: just return the instance as-is
		// In a real implementation, each thread would see different attributes
		return core.None, nil
	}))
	threadModule.SetStr("_local", localClass)

	// _ExceptHookArgs - namedtuple for exception hook arguments
	exceptHookArgsClass := core.NewClass("_ExceptHookArgs", nil)
	exceptHookArgsClass.SetMethod("__init__", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		if len(args) < 1 {
			return nil, fmt.Errorf("_ExceptHookArgs requires self")
		}
		self, ok := args[0].(*core.Instance)
		if !ok {
			return core.None, nil
		}
		// Set default attributes
		self.Attributes["exc_type"] = core.None
		self.Attributes["exc_value"] = core.None
		self.Attributes["exc_traceback"] = core.None
		self.Attributes["thread"] = core.None
		return core.None, nil
	}))
	threadModule.SetStr("_ExceptHookArgs", exceptHookArgsClass)

	// interrupt_main() - raises KeyboardInterrupt in the main thread
	threadModule.SetStr("interrupt_main", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		v := validation.NewArgs("interrupt_main", args)
		if err := v.Range(0, 1); err != nil {
			return nil, err
		}
		// Stub: raise KeyboardInterrupt
		return nil, fmt.Errorf("KeyboardInterrupt")
	}))

	// exit() - exit the current thread
	threadModule.SetStr("exit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Raise SystemExit to exit the thread
		return nil, fmt.Errorf("SystemExit")
	}))

	// _count() - return the number of active threads
	threadModule.SetStr("_count", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: return 1 (main thread only)
		return core.NumberValue(1), nil
	}))

	// _register_atexit - register function to call at thread exit
	threadModule.SetStr("_register_atexit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.None, nil
	}))

	// _unregister_atexit - unregister atexit function
	threadModule.SetStr("_unregister_atexit", core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
		// Stub: do nothing
		return core.None, nil
	}))

	return threadModule
}

// ThreadLock represents a lock object
type ThreadLock struct {
	mu     sync.Mutex
	locked bool
}

func newThreadLock() *ThreadLock {
	return &ThreadLock{locked: false}
}

func (l *ThreadLock) Type() core.Type {
	return core.Type("lock")
}

func (l *ThreadLock) String() string {
	return "<lock object>"
}

func (l *ThreadLock) Repr() string {
	return "<lock object>"
}

func (l *ThreadLock) Equals(other core.Value) bool {
	otherLock, ok := other.(*ThreadLock)
	if !ok {
		return false
	}
	return l == otherLock
}

// Implement methods for the lock
func (l *ThreadLock) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "acquire":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// acquire(blocking=True, timeout=-1)
			v := validation.NewArgs("acquire", args)
			if err := v.Range(0, 2); err != nil {
				return nil, err
			}

			l.mu.Lock()
			if l.locked {
				l.mu.Unlock()
				return core.BoolValue(false), nil
			}
			l.locked = true
			l.mu.Unlock()
			return core.BoolValue(true), nil
		}), true

	case "release":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			v := validation.NewArgs("release", args)
			if err := v.Exact(0); err != nil {
				return nil, err
			}

			l.mu.Lock()
			if !l.locked {
				l.mu.Unlock()
				return nil, fmt.Errorf("release unlocked lock")
			}
			l.locked = false
			l.mu.Unlock()
			return core.Nil, nil
		}), true

	case "locked":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			v := validation.NewArgs("locked", args)
			if err := v.Exact(0); err != nil {
				return nil, err
			}

			l.mu.Lock()
			result := l.locked
			l.mu.Unlock()
			return core.BoolValue(result), nil
		}), true

	case "__enter__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			l.mu.Lock()
			l.locked = true
			l.mu.Unlock()
			return core.Value(l), nil
		}), true

	case "__exit__":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			l.mu.Lock()
			l.locked = false
			l.mu.Unlock()
			return core.Nil, nil
		}), true
	}

	return nil, false
}

func (l *ThreadLock) SetAttr(name string, value core.Value) error {
	return fmt.Errorf("cannot set attribute '%s' on lock object", name)
}

// ThreadHandle implements _thread._ThreadHandle: a joinable handle for a thread
// started via start_joinable_thread (Python 3.13 threading API). The wrapped
// goroutine closes doneCh when it finishes.
type ThreadHandle struct {
	core.BaseObject
	mu     sync.Mutex
	ident  int64
	done   bool
	doneCh chan struct{}
}

func newThreadHandle(ident int64) *ThreadHandle {
	return &ThreadHandle{
		BaseObject: *core.NewBaseObject(core.Type("_thread._ThreadHandle")),
		ident:      ident,
		doneCh:     make(chan struct{}),
	}
}

func (h *ThreadHandle) setDone() {
	h.mu.Lock()
	if !h.done {
		h.done = true
		close(h.doneCh)
	}
	h.mu.Unlock()
}

func (h *ThreadHandle) isDone() bool {
	h.mu.Lock()
	d := h.done
	h.mu.Unlock()
	return d
}

func (h *ThreadHandle) Type() core.Type { return core.Type("_thread._ThreadHandle") }

func (h *ThreadHandle) String() string {
	h.mu.Lock()
	defer h.mu.Unlock()
	return fmt.Sprintf("<_thread._ThreadHandle ident=%d done=%v>", h.ident, h.done)
}

// GetAttr exposes the handle's ident attribute and is_done/join/_set_done methods.
func (h *ThreadHandle) GetAttr(name string) (core.Value, bool) {
	switch name {
	case "ident":
		h.mu.Lock()
		id := h.ident
		h.mu.Unlock()
		if id == 0 {
			return core.None, true
		}
		return core.NumberValue(float64(id)), true
	case "is_done":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			return core.BoolValue(h.isDone()), nil
		}), true
	case "_set_done":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			h.setDone()
			return core.Nil, nil
		}), true
	case "join":
		return core.NewBuiltinFunction(func(args []core.Value, ctx *core.Context) (core.Value, error) {
			// join(timeout=None): wait until the thread finishes, up to timeout seconds.
			if len(args) >= 1 {
				if n, ok := args[0].(core.NumberValue); ok && float64(n) >= 0 {
					select {
					case <-h.doneCh:
					case <-time.After(time.Duration(float64(n) * float64(time.Second))):
					}
					return core.None, nil
				}
			}
			<-h.doneCh
			return core.None, nil
		}), true
	}
	return h.BaseObject.GetAttr(name)
}

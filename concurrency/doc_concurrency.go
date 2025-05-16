package concurrency

import (
	"github.com/mmichie/m28/core"
)

// init registers documentation for concurrency-related forms and functions
func init() {
	// Documentation for context-related functions
	core.RegisterDoc(core.DocEntry{
		Name:        "context-background",
		Type:        "builtin-function",
		Brief:       "Create a new background context",
		Description: "Creates a new background context that is never canceled. This is typically used as the root context for a request or operation.",
		Params:      []core.ParamDoc{},
		Returns:     "A new background context",
		Examples:    []string{`(def ctx (context-background))`},
		Related:     []string{"context-with-cancel", "context-with-timeout", "context-done", "context-canceled?", "context-error"},
		Module:      "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "context-with-cancel",
		Type:        "builtin-function",
		Brief:       "Create a new cancellable context",
		Description: "Creates a new context that can be canceled. Returns a list containing the context and a cancel function.",
		Params: []core.ParamDoc{
			{
				Name:        "parent",
				Description: "The parent context",
			},
		},
		Returns: "A list [ctx cancel-fn] where ctx is the new context and cancel-fn is a function to cancel it",
		Examples: []string{
			`(def result (context-with-cancel ctx))
(def new-ctx (nth result 0))
(def cancel-fn (nth result 1))
# Later, to cancel:
(cancel-fn)`,
		},
		Related: []string{"context-background", "context-with-timeout", "context-done", "context-canceled?"},
		Module:  "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "context-with-timeout",
		Type:        "builtin-function",
		Brief:       "Create a new context with a timeout",
		Description: "Creates a new context that will be automatically canceled after the specified timeout in milliseconds.",
		Params: []core.ParamDoc{
			{
				Name:        "parent",
				Description: "The parent context",
			},
			{
				Name:        "timeout-ms",
				Description: "The timeout in milliseconds after which the context will be canceled",
			},
		},
		Returns: "A list [ctx cancel-fn] where ctx is the new context and cancel-fn is a function to cancel it",
		Examples: []string{
			`(def result (context-with-timeout ctx 5000))  # 5 seconds
(def timeout-ctx (nth result 0))
(def cancel-fn (nth result 1))`,
		},
		Related: []string{"context-background", "context-with-cancel", "context-done"},
		Module:  "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "context-done",
		Type:        "builtin-function",
		Brief:       "Get a channel that's closed when the context is done",
		Description: "Returns a channel that's closed when the context is canceled or times out. This can be used with select to respond to context cancellation.",
		Params: []core.ParamDoc{
			{
				Name:        "context",
				Description: "The context to get the done channel from",
			},
		},
		Returns: "A channel that's closed when the context is canceled or times out",
		Examples: []string{
			`(def done-ch (context-done ctx))
(select
  [(case :recv done-ch)
    (println "Context was canceled")]
  [(case :recv work-ch)
    (println "Received work")])`,
		},
		Related: []string{"context-canceled?", "context-error", "select"},
		Module:  "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "context-canceled?",
		Type:        "builtin-function",
		Brief:       "Check if a context is canceled",
		Description: "Returns true if the context is canceled or timed out, false otherwise.",
		Params: []core.ParamDoc{
			{
				Name:        "context",
				Description: "The context to check",
			},
		},
		Returns: "A boolean indicating if the context is canceled",
		Examples: []string{
			`(if (context-canceled? ctx)
  (println "Context is canceled")
  (println "Context is still active"))`,
		},
		Related: []string{"context-error", "context-done"},
		Module:  "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "context-error",
		Type:        "builtin-function",
		Brief:       "Get the error message for a canceled context",
		Description: "Returns the error message describing why the context was canceled, or None if the context is still active.",
		Params: []core.ParamDoc{
			{
				Name:        "context",
				Description: "The context to get the error from",
			},
		},
		Returns: "A string with the error message, or None if the context is not canceled",
		Examples: []string{
			`(let [err (context-error ctx)]
  (if (eq? err None)
    (println "Context is still active")
    (println "Context canceled because:" err)))`,
		},
		Related: []string{"context-canceled?", "context-done"},
		Module:  "concurrency",
	})
	// Documentation for the go special form
	core.RegisterDoc(core.DocEntry{
		Name:        "go",
		Type:        "special-form",
		Brief:       "Evaluate an expression in a new goroutine",
		Description: "Spawns a new goroutine and evaluates the given expression in that goroutine. Returns None immediately without waiting for the goroutine to complete.",
		Params: []core.ParamDoc{
			{
				Name:        "expr",
				Description: "The expression to evaluate in the new goroutine",
			},
		},
		Returns:  "None",
		Examples: []string{`(go (println "Hello from goroutine"))`, `(go (sleep 1) (println "Delayed message"))`},
		Related:  []string{"chan", "send", "recv", "try-send", "try-recv", "close-chan"},
		Module:   "concurrency",
	})

	// Documentation for the chan function
	core.RegisterDoc(core.DocEntry{
		Name:        "chan",
		Type:        "builtin-function",
		Brief:       "Create a new channel",
		Description: "Creates a new channel for communication between goroutines. If size is provided, creates a buffered channel with the given capacity.",
		Params: []core.ParamDoc{
			{
				Name:        "size",
				Description: "The buffer size for the channel (0 for unbuffered)",
				Optional:    true,
				Default:     "0",
			},
		},
		Returns:  "A new channel",
		Examples: []string{`(def ch (chan))`, `(def buffered-ch (chan 10))`},
		Related:  []string{"send", "recv", "try-send", "try-recv", "close-chan"},
		Module:   "concurrency",
	})

	// Documentation for send
	core.RegisterDoc(core.DocEntry{
		Name:        "send",
		Type:        "builtin-function",
		Brief:       "Send a value to a channel",
		Description: "Sends a value to the given channel. If the channel is unbuffered or full, blocks until a receiver is available.",
		Params: []core.ParamDoc{
			{
				Name:        "channel",
				Description: "The channel to send to",
			},
			{
				Name:        "value",
				Description: "The value to send",
			},
		},
		Returns:  "None",
		Examples: []string{`(send ch 42)`, `(send ch "hello")`},
		Related:  []string{"chan", "recv", "try-send"},
		Module:   "concurrency",
	})

	// Documentation for try-send
	core.RegisterDoc(core.DocEntry{
		Name:        "try-send",
		Type:        "builtin-function",
		Brief:       "Try to send a value to a channel without blocking",
		Description: "Attempts to send a value to the given channel without blocking. Returns true if the send was successful, false if the channel is full.",
		Params: []core.ParamDoc{
			{
				Name:        "channel",
				Description: "The channel to send to",
			},
			{
				Name:        "value",
				Description: "The value to send",
			},
		},
		Returns:  "Boolean indicating success (true if sent, false if channel full)",
		Examples: []string{`(if (try-send ch 42) (println "Sent") (println "Channel full"))`},
		Related:  []string{"chan", "send", "try-recv"},
		Module:   "concurrency",
	})

	// Documentation for recv
	core.RegisterDoc(core.DocEntry{
		Name:        "recv",
		Type:        "builtin-function",
		Brief:       "Receive a value from a channel",
		Description: "Receives a value from the given channel. Blocks until a value is available or the channel is closed.",
		Params: []core.ParamDoc{
			{
				Name:        "channel",
				Description: "The channel to receive from",
			},
		},
		Returns:  "A tuple (value, ok) where value is the received value and ok is true if the channel is still open, false if closed",
		Examples: []string{`(def result (recv ch))`, `(if (nth result 1) (println (nth result 0)) (println "Channel closed"))`},
		Related:  []string{"chan", "send", "try-recv"},
		Module:   "concurrency",
	})

	// Documentation for try-recv
	core.RegisterDoc(core.DocEntry{
		Name:        "try-recv",
		Type:        "builtin-function",
		Brief:       "Try to receive a value from a channel without blocking",
		Description: "Attempts to receive a value from the given channel without blocking. Returns a tuple of (value, success, ok) where success indicates if a value was received.",
		Params: []core.ParamDoc{
			{
				Name:        "channel",
				Description: "The channel to receive from",
			},
		},
		Returns:  "A tuple (value, success, ok) where value is the received value (or None if no value), success is true if a value was received, and ok is true if the channel is still open",
		Examples: []string{`(def result (try-recv ch))`, `(if (nth result 1) (println "Received:" (nth result 0)) (println "No value available"))`},
		Related:  []string{"chan", "recv", "try-send"},
		Module:   "concurrency",
	})

	// Documentation for close-chan
	core.RegisterDoc(core.DocEntry{
		Name:        "close-chan",
		Type:        "builtin-function",
		Brief:       "Close a channel",
		Description: "Closes the given channel. After closing, no more values can be sent to the channel, but values can still be received until the channel is empty.",
		Params: []core.ParamDoc{
			{
				Name:        "channel",
				Description: "The channel to close",
			},
		},
		Returns:  "None",
		Examples: []string{`(close-chan ch)`},
		Related:  []string{"chan", "chan-closed?"},
		Module:   "concurrency",
	})

	// Documentation for chan-closed?
	core.RegisterDoc(core.DocEntry{
		Name:        "chan-closed?",
		Type:        "builtin-function",
		Brief:       "Check if a channel is closed",
		Description: "Checks if the given channel is closed. Returns true if the channel is closed, false otherwise.",
		Params: []core.ParamDoc{
			{
				Name:        "channel",
				Description: "The channel to check",
			},
		},
		Returns:  "Boolean indicating if the channel is closed",
		Examples: []string{`(if (chan-closed? ch) (println "Channel closed") (println "Channel open"))`},
		Related:  []string{"chan", "close-chan"},
		Module:   "concurrency",
	})

	// Documentation for the select special form
	core.RegisterDoc(core.DocEntry{
		Name:        "select",
		Type:        "special-form",
		Brief:       "Multiplex on multiple channel operations",
		Description: "Performs a select operation across multiple channel operations. Chooses one case that is ready to proceed and executes its body. If multiple cases are ready, one is chosen randomly.",
		Params: []core.ParamDoc{
			{
				Name:        "cases",
				Description: "One or more case clauses for channel operations",
			},
		},
		Returns: "The result of executing the body of the selected case",
		Examples: []string{
			`(select
  [(case :recv ch1) 
    (print "Received from ch1:" select-value)
    select-value]
  [(case :send [ch2 42])
    (print "Sent 42 to ch2")]
  [(default) 
    (print "No channel ready")])`,
		},
		Related: []string{"select-timeout", "go", "chan"},
		Module:  "concurrency",
	})

	// Documentation for the select-timeout special form
	core.RegisterDoc(core.DocEntry{
		Name:        "select-timeout",
		Type:        "special-form",
		Brief:       "Multiplex on multiple channel operations with a timeout",
		Description: "Like select, but with a timeout in milliseconds. If no channel operation is ready within the timeout, the timeout case is executed.",
		Params: []core.ParamDoc{
			{
				Name:        "timeout-ms",
				Description: "Timeout in milliseconds",
			},
			{
				Name:        "cases",
				Description: "One or more case clauses for channel operations or timeout",
			},
		},
		Returns: "The result of executing the body of the selected case",
		Examples: []string{
			`(select-timeout 1000
  [(case :recv ch1) 
    (print "Received from ch1:" select-value)
    select-value]
  [(case :send [ch2 42])
    (print "Sent 42 to ch2")]
  [(timeout) 
    (print "Timed out after 1000ms")])`,
		},
		Related: []string{"select", "go", "chan"},
		Module:  "concurrency",
	})

	// Documentation for mutex creation
	core.RegisterDoc(core.DocEntry{
		Name:        "mutex",
		Type:        "builtin-function",
		Brief:       "Create a new mutex",
		Description: "Creates a new mutex for synchronizing access to shared resources.",
		Params:      []core.ParamDoc{},
		Returns:     "A new mutex",
		Examples:    []string{`(= m (mutex))`},
		Related:     []string{"mutex-lock", "mutex-unlock", "with-mutex"},
		Module:      "concurrency",
	})

	// Documentation for rwmutex creation
	core.RegisterDoc(core.DocEntry{
		Name:        "rwmutex",
		Type:        "builtin-function",
		Brief:       "Create a new read-write mutex",
		Description: "Creates a new read-write mutex that allows multiple readers or a single writer.",
		Params:      []core.ParamDoc{},
		Returns:     "A new read-write mutex",
		Examples:    []string{`(= m (rwmutex))`},
		Related:     []string{"rwmutex-rlock", "rwmutex-runlock", "with-rlock"},
		Module:      "concurrency",
	})

	// Documentation for waitgroup creation
	core.RegisterDoc(core.DocEntry{
		Name:        "waitgroup",
		Type:        "builtin-function",
		Brief:       "Create a new wait group",
		Description: "Creates a new wait group for waiting for a collection of goroutines to finish.",
		Params:      []core.ParamDoc{},
		Returns:     "A new wait group",
		Examples:    []string{`(= wg (waitgroup))`},
		Related:     []string{"waitgroup-add", "waitgroup-done", "waitgroup-wait"},
		Module:      "concurrency",
	})

	// Documentation for mutex operations
	core.RegisterDoc(core.DocEntry{
		Name:        "mutex-lock",
		Type:        "builtin-function",
		Brief:       "Lock a mutex",
		Description: "Locks a mutex. Blocks until the mutex is available.",
		Params: []core.ParamDoc{
			{
				Name:        "mutex",
				Description: "The mutex to lock",
			},
		},
		Returns:  "None",
		Examples: []string{`(mutex-lock m)`},
		Related:  []string{"mutex-unlock", "with-mutex"},
		Module:   "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "mutex-unlock",
		Type:        "builtin-function",
		Brief:       "Unlock a mutex",
		Description: "Unlocks a mutex.",
		Params: []core.ParamDoc{
			{
				Name:        "mutex",
				Description: "The mutex to unlock",
			},
		},
		Returns:  "None",
		Examples: []string{`(mutex-unlock m)`},
		Related:  []string{"mutex-lock", "with-mutex"},
		Module:   "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "mutex-try-lock",
		Type:        "builtin-function",
		Brief:       "Try to lock a mutex without blocking",
		Description: "Tries to lock a mutex without blocking. Returns true if the lock was acquired, false otherwise.",
		Params: []core.ParamDoc{
			{
				Name:        "mutex",
				Description: "The mutex to try to lock",
			},
		},
		Returns:  "Boolean indicating success (true if locked, false if mutex was already locked)",
		Examples: []string{`(if (mutex-try-lock m) (print "Locked") (print "Already locked"))`},
		Related:  []string{"mutex-lock", "mutex-unlock"},
		Module:   "concurrency",
	})

	// Documentation for rwmutex operations
	core.RegisterDoc(core.DocEntry{
		Name:        "rwmutex-rlock",
		Type:        "builtin-function",
		Brief:       "Lock a read-write mutex for reading",
		Description: "Locks a read-write mutex for reading. Multiple readers can hold the lock simultaneously.",
		Params: []core.ParamDoc{
			{
				Name:        "rwmutex",
				Description: "The read-write mutex to lock for reading",
			},
		},
		Returns:  "None",
		Examples: []string{`(rwmutex-rlock m)`},
		Related:  []string{"rwmutex-runlock", "with-rlock"},
		Module:   "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "rwmutex-runlock",
		Type:        "builtin-function",
		Brief:       "Unlock a read-write mutex for reading",
		Description: "Unlocks a read-write mutex previously locked for reading.",
		Params: []core.ParamDoc{
			{
				Name:        "rwmutex",
				Description: "The read-write mutex to unlock",
			},
		},
		Returns:  "None",
		Examples: []string{`(rwmutex-runlock m)`},
		Related:  []string{"rwmutex-rlock", "with-rlock"},
		Module:   "concurrency",
	})

	// Documentation for waitgroup operations
	core.RegisterDoc(core.DocEntry{
		Name:        "waitgroup-add",
		Type:        "builtin-function",
		Brief:       "Add to a wait group counter",
		Description: "Adds a delta to a wait group counter.",
		Params: []core.ParamDoc{
			{
				Name:        "waitgroup",
				Description: "The wait group to modify",
			},
			{
				Name:        "delta",
				Description: "The delta to add to the counter",
			},
		},
		Returns:  "None",
		Examples: []string{`(waitgroup-add wg 1)`},
		Related:  []string{"waitgroup-done", "waitgroup-wait"},
		Module:   "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "waitgroup-done",
		Type:        "builtin-function",
		Brief:       "Decrement a wait group counter",
		Description: "Decrements a wait group counter by one.",
		Params: []core.ParamDoc{
			{
				Name:        "waitgroup",
				Description: "The wait group to decrement",
			},
		},
		Returns:  "None",
		Examples: []string{`(waitgroup-done wg)`},
		Related:  []string{"waitgroup-add", "waitgroup-wait"},
		Module:   "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "waitgroup-wait",
		Type:        "builtin-function",
		Brief:       "Wait for a wait group counter to reach zero",
		Description: "Blocks until the wait group counter reaches zero.",
		Params: []core.ParamDoc{
			{
				Name:        "waitgroup",
				Description: "The wait group to wait for",
			},
		},
		Returns:  "None",
		Examples: []string{`(waitgroup-wait wg)`},
		Related:  []string{"waitgroup-add", "waitgroup-done"},
		Module:   "concurrency",
	})

	// Documentation for special forms
	core.RegisterDoc(core.DocEntry{
		Name:        "with-mutex",
		Type:        "special-form",
		Brief:       "Evaluate expressions with a mutex locked",
		Description: "Locks a mutex, evaluates the body expressions, and unlocks the mutex when done. The unlock happens even if an error occurs.",
		Params: []core.ParamDoc{
			{
				Name:        "mutex",
				Description: "The mutex or rwmutex to lock",
			},
			{
				Name:        "body",
				Description: "One or more expressions to evaluate with the mutex locked",
			},
		},
		Returns: "The result of the last body expression",
		Examples: []string{
			`(with-mutex m
  (print "Critical section")
  (update-shared-resource))`,
		},
		Related: []string{"mutex", "mutex-lock", "mutex-unlock"},
		Module:  "concurrency",
	})

	core.RegisterDoc(core.DocEntry{
		Name:        "with-rlock",
		Type:        "special-form",
		Brief:       "Evaluate expressions with a read lock",
		Description: "Locks a read-write mutex for reading, evaluates the body expressions, and unlocks the mutex when done.",
		Params: []core.ParamDoc{
			{
				Name:        "rwmutex",
				Description: "The read-write mutex to lock for reading",
			},
			{
				Name:        "body",
				Description: "One or more expressions to evaluate with the read lock",
			},
		},
		Returns: "The result of the last body expression",
		Examples: []string{
			`(with-rlock m
  (print "Read-only section")
  (read-shared-resource))`,
		},
		Related: []string{"rwmutex", "rwmutex-rlock", "rwmutex-runlock"},
		Module:  "concurrency",
	})
}

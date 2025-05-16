# Concurrency in M28

M28 includes a concurrency model based on Go's goroutines and channels, with a Pythonic interface. This model provides lightweight, efficient concurrency with a communication-based approach rather than shared memory and locks.

## Goroutines

Goroutines are lightweight threads managed by the M28 runtime. You can create a new goroutine with the `go` special form:

```lisp
(go expr)
```

The `go` form evaluates `expr` in a new goroutine and returns `None` immediately without waiting for the goroutine to complete. The goroutine inherits the lexical environment from where it was spawned.

Example:

```lisp
(go (println "Hello from goroutine"))

# Goroutine that sleeps before printing
(go
  (sleep 1)
  (println "This prints after a delay"))
```

## Channels

Channels are typed conduits through which you can send and receive values. They provide a way for goroutines to communicate and synchronize.

### Creating Channels

Create a channel with the `chan` function:

```lisp
# Unbuffered channel
(def ch (chan))

# Buffered channel with capacity 10
(def buffered-ch (chan 10))
```

An unbuffered channel will block the sender until a receiver is ready, while a buffered channel will only block when the buffer is full.

### Sending Values

Send values to a channel with the `send` function:

```lisp
(send ch 42)
```

This will block if the channel is unbuffered or its buffer is full.

For non-blocking sends, use `try-send`:

```lisp
# Returns true if sent, false if channel is full
(def success (try-send ch "hello"))
```

### Receiving Values

Receive values from a channel with the `recv` function:

```lisp
# Returns a tuple: (value, ok)
# ok is false if the channel is closed
(def result (recv ch))
(def value (nth result 0))
(def channel-open (nth result 1))
```

For non-blocking receives, use `try-recv`:

```lisp
# Returns a tuple: (value, success, ok)
# success is false if no value was available
# ok is false if the channel is closed
(def result (try-recv ch))
(def value (nth result 0))      # The received value (None if no value)
(def received (nth result 1))   # Whether a value was received
(def channel-open (nth result 2))  # Whether the channel is still open
```

### Closing Channels

Close a channel with `close-chan`:

```lisp
(close-chan ch)
```

After closing a channel:
- You cannot send more values to it
- You can still receive values until the channel is empty
- Receivers will get `(value, false)` when the channel is closed and empty

Check if a channel is closed with `chan-closed?`:

```lisp
(if (chan-closed? ch)
  (println "Channel is closed")
  (println "Channel is open"))
```

## Examples

### Simple Producer-Consumer Pattern

```lisp
(def jobs (chan 10))
(def results (chan 10))

# Producer
(go
  (for i (range 5)
    (println "Producing job" i)
    (send jobs i)
    (sleep 0.1))
  (close-chan jobs))

# Consumer
(go
  (while True
    (def result (recv jobs))
    (def job (nth result 0))
    (def ok (nth result 1))
    (if (not ok)
      (break))
    (println "Processing job" job)
    (send results (* job 2))))
    
# Collect results
(for i (range 5)
  (def result (recv results))
  (println "Result:" (nth result 0)))
```

### Fan-out Pattern

```lisp
(def ch (chan))
(def results (chan))

# Spawn 3 worker goroutines
(for i (range 3)
  (go
    (def worker-id i)
    (while True
      (def result (recv ch))
      (def value (nth result 0))
      (def ok (nth result 1))
      (if (not ok)
        (break))
      (println "Worker" worker-id "processing" value)
      (send results (* value 2)))))

# Send some work
(for i (range 10)
  (send ch i))

# Close channel to signal workers to exit
(close-chan ch)

# Collect results
(for i (range 10)
  (println "Result:" (nth (recv results) 0)))
```

## Best Practices

1. **Don't communicate by sharing memory; share memory by communicating**
   - Use channels to communicate between goroutines
   - Avoid shared mutable state

2. **Prefer unbuffered channels for synchronization**
   - Unbuffered channels ensure the sender and receiver are synchronized
   - Use buffered channels when you need to decouple producers and consumers

3. **Always handle channel closure**
   - Check the `ok` value when receiving from channels
   - Use `try-send` if you want to avoid blocking on sends

4. **Close channels from the sender side**
   - The sender should close the channel when done sending
   - Multiple receivers can safely receive from the same channel

5. **Use select for multiplexing**
   - The `select` statement allows you to wait on multiple channel operations

## Select Statement

The `select` statement enables multiplexing on multiple channel operations. It's similar to Go's select statement.

### Basic Select

```lisp
(select
  [(case :recv ch1) 
    (print "Received from ch1:" select-value)
    select-value]
  [(case :send [ch2 42])
    (print "Sent 42 to ch2")]
  [(default) 
    (print "No channel ready")])
```

This will:
- Try to receive from `ch1` and send `42` to `ch2`
- If multiple operations are ready, one will be chosen randomly
- If no operations are ready, the default case will be executed
- For receive operations, the received value is bound to `select-value` and the channel status to `select-ok`

### Select with Timeout

```lisp
(select-timeout 1000
  [(case :recv ch1) 
    (print "Received from ch1:" select-value)
    select-value]
  [(case :send [ch2 42])
    (print "Sent 42 to ch2")]
  [(timeout) 
    (print "Timed out after 1000ms")])
```

This functions like the regular select, but with a timeout (in milliseconds):
- If no channel operation is ready within the timeout, the timeout case is executed
- The first argument is the timeout in milliseconds

## Synchronization Primitives

M28 provides synchronization primitives for coordinating goroutines and protecting shared resources.

### Mutexes

Mutexes provide exclusive access to shared resources:

```lisp
# Create a mutex
(= m (mutex))

# Using mutex explicitly
(mutex-lock m)
(print "Critical section")
(mutex-unlock m)

# Using with-mutex (automatically unlocks, even on errors)
(with-mutex m
  (print "Critical section")
  (update-shared-resource))

# Try to lock without blocking
(if (mutex-try-lock m)
  (try
    (print "Got lock")
    (finally (mutex-unlock m))))
```

### Read-Write Mutexes

Read-write mutexes allow multiple readers or a single writer:

```lisp
# Create a read-write mutex
(= m (rwmutex))

# Read lock (multiple readers allowed)
(with-rlock m
  (print "Reading shared data")
  (read-shared-resource))

# Write lock (exclusive access)
(with-mutex m
  (print "Modifying shared data")
  (write-shared-resource))
```

### WaitGroups

WaitGroups allow waiting for a collection of goroutines to finish:

```lisp
# Create a wait group
(= wg (waitgroup))

# Launch multiple goroutines
(for i (range 5)
  # Increment counter before starting goroutine
  (waitgroup-add wg 1)
  (go
    (try
      (print "Worker" i "starting")
      (sleep (/ i 10)) # Simulate work
      (print "Worker" i "done")
      (finally
        # Decrement counter when done
        (waitgroup-done wg)))))

# Wait for all goroutines to finish
(print "Waiting for workers to finish...")
(waitgroup-wait wg)
(print "All workers done")
```

## Context System

The context system provides a way to carry deadlines, cancellation signals, and other request-scoped values across API boundaries and between goroutines.

### Creating Contexts

```lisp
# Create a background context (never canceled)
(= bg (context-background))

# Create a cancellable context
(= result (context-with-cancel bg))
(= ctx (nth result 0))
(= cancel-fn (nth result 1))

# Create a context with a timeout (in milliseconds)
(= result (context-with-timeout bg 5000))  # 5 second timeout
(= timeout-ctx (nth result 0))
(= cancel-timeout (nth result 1))
```

### Cancellation

```lisp
# Cancel a context directly
(cancel-fn)

# Cancel with a specific reason
(cancel-fn "operation aborted by user")
```

### Using Context for Cancellation

```lisp
# Get a channel that's closed when the context is done
(= done-ch (context-done ctx))

# Check if a context is canceled
(if (context-canceled? ctx)
  (println "Context is canceled")
  (println "Context is still active"))

# Get the error describing why a context was canceled
(= err (context-error ctx))
```

### Example: Worker with Cancellation

```lisp
(= bg (context-background))
(= result (context-with-timeout bg 5000)) # 5 second timeout
(= ctx (nth result 0))
(= cancel (nth result 1))
(= done-ch (context-done ctx))
(= work-ch (chan))

# Start a worker
(go
  (println "Worker: starting...")
  (select
    # Wait for work
    [(case :recv work-ch)
      (println "Worker: processing" select-value)]
    # Or context cancellation
    [(case :recv done-ch)
      (println "Worker: stopping due to context cancellation")])
  (println "Worker: done"))

# Cancel the context
(cancel "work no longer needed")
```

### Context Best Practices

1. **Pass contexts explicitly**: Always pass contexts as the first parameter to functions that need them
2. **Don't store contexts in structs**: Pass them through the call chain instead
3. **Use context values sparingly**: Prefer explicit parameters for data that functions need
4. **Cancel contexts when appropriate**: To free resources and stop ongoing operations
5. **Create context chains**: Derive contexts from existing ones to form a tree of contexts

## Future Enhancements

1. **Advanced Context Values**: Support for request-scoped values in contexts
2. **Context Deadline Functions**: Add functions to check context deadlines and remaining time
3. **Context Value Accessors**: Methods to store and retrieve values from contexts
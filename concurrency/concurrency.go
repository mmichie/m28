package concurrency

// Documentation strings for concurrency forms
var ConcurrencyDocs = map[string]string{
	"go": `(go expr)
	
Evaluate expr in a new goroutine. Returns None immediately.

Example:
(go (println "Hello from goroutine"))`,

	"chan": `(chan [size])
	
Create a new channel with optional buffer size.
If size is not provided, an unbuffered channel is created.

Example:
(def unbuffered-channel (chan))
(def buffered-channel (chan 10))`,

	"send": `(send channel value)
	
Send a value to a channel. Blocks until the value is received if the channel is unbuffered
or the buffer is full.

Example:
(send my-channel 42)`,

	"try-send": `(try-send channel value)
	
Attempt to send a value to a channel without blocking.
Returns true if the value was sent, false if the channel buffer is full.

Example:
(if (try-send my-channel 42)
  (println "Sent successfully")
  (println "Channel buffer full"))`,

	"recv": `(recv channel)
	
Receive a value from a channel. Blocks until a value is available.
Returns a tuple of (value, ok) where:
- value is the received value
- ok is true if the channel is still open, false if closed

Example:
(let [result (recv my-channel)]
  (if (nth result 1)
    (println "Received:" (nth result 0))
    (println "Channel closed")))`,

	"try-recv": `(try-recv channel)
	
Attempt to receive a value from a channel without blocking.
Returns a tuple of (value, success, ok) where:
- value is the received value (or None if no value was available)
- success is true if a value was received, false if the channel was empty
- ok is true if the channel is still open, false if closed

Example:
(let [result (try-recv my-channel)]
  (if (nth result 1)
    (println "Received:" (nth result 0))
    (println "No value available")))`,

	"close-chan": `(close-chan channel)
	
Close a channel. After closing, no more values can be sent,
but values can still be received until the channel is empty.

Example:
(close-chan my-channel)`,

	"chan-closed?": `(chan-closed? channel)
	
Check if a channel is closed. Returns true if the channel is closed, false otherwise.

Example:
(if (chan-closed? my-channel)
  (println "Channel is closed")
  (println "Channel is open"))`,
}

// RegisterConcurrencyDocs registers documentation for concurrency forms and functions
func RegisterConcurrencyDocs(docRegistry map[string]string) {
	for name, doc := range ConcurrencyDocs {
		docRegistry[name] = doc
	}
}

package concurrency

import (
	"github.com/mmichie/m28/core"
)

// init registers documentation for concurrency-related forms and functions
func init() {
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
}

package core

import (
	"fmt"
)

// TraceEntry represents a single entry in a stack trace
type TraceEntry struct {
	Function string
	File     string
	Line     int
	Column   int
}

// Context holds the execution context for evaluation
type Context struct {
	// Variables in the current scope
	Vars map[string]Value

	// Parent scope
	Outer *Context

	// Global scope for module-level variables
	Global *Context

	// Call stack for debugging and error reporting
	CallStack []TraceEntry

	// Current function name for error reporting
	CurrentFunction string
}

// NewContext creates a new evaluation context
func NewContext(outer *Context) *Context {
	ctx := &Context{
		Vars:      make(map[string]Value),
		Outer:     outer,
		CallStack: make([]TraceEntry, 0),
	}

	// If this is the global context, set Global to self
	if outer == nil {
		ctx.Global = ctx
	} else {
		// Otherwise, inherit global from parent
		ctx.Global = outer.Global
	}

	return ctx
}

// Define defines a new variable in the current scope
func (c *Context) Define(name string, value Value) {
	c.Vars[name] = value
}

// DefineBuiltin defines a builtin function with optional duplicate checking
// This should be used instead of Define when registering builtins to catch duplicates
func (c *Context) DefineBuiltin(name string, value Value) error {
	// Only check for duplicates in the global context
	if c.Outer == nil {
		if _, exists := c.Vars[name]; exists {
			// Return error indicating duplicate
			return fmt.Errorf("builtin '%s' already registered", name)
		}
	}
	c.Vars[name] = value
	return nil
}

// Set updates an existing variable in the nearest scope where it's defined
func (c *Context) Set(name string, value Value) error {
	// Check current scope
	if _, ok := c.Vars[name]; ok {
		c.Vars[name] = value
		return nil
	}

	// Check outer scopes
	if c.Outer != nil {
		return c.Outer.Set(name, value)
	}

	return fmt.Errorf("variable not defined: %s", name)
}

// Lookup finds a variable in the current or outer scopes
func (c *Context) Lookup(name string) (Value, error) {
	// Check current scope
	if val, ok := c.Vars[name]; ok {
		return val, nil
	}

	// Check outer scopes
	if c.Outer != nil {
		return c.Outer.Lookup(name)
	}

	return nil, &NameError{Name: name}
}

// PushStack adds a new entry to the call stack
func (c *Context) PushStack(function, file string, line, column int) {
	c.CallStack = append(c.CallStack, TraceEntry{
		Function: function,
		File:     file,
		Line:     line,
		Column:   column,
	})
	c.CurrentFunction = function
}

// PopStack removes the most recent entry from the call stack
func (c *Context) PopStack() {
	if len(c.CallStack) > 0 {
		c.CallStack = c.CallStack[:len(c.CallStack)-1]
		if len(c.CallStack) > 0 {
			c.CurrentFunction = c.CallStack[len(c.CallStack)-1].Function
		} else {
			c.CurrentFunction = ""
		}
	}
}

// FormatStackTrace returns a formatted stack trace for error reporting
func (c *Context) FormatStackTrace() string {
	trace := "Traceback (most recent call last):\n"
	for i := len(c.CallStack) - 1; i >= 0; i-- {
		entry := c.CallStack[i]
		trace += fmt.Sprintf("  File \"%s\", line %d, in %s\n",
			entry.File, entry.Line, entry.Function)
	}
	return trace
}

// GetAllSymbols returns all defined symbols in this context and its parents
func (c *Context) GetAllSymbols() []string {
	symbols := make(map[string]bool)

	// Collect symbols from this context and all parents
	ctx := c
	for ctx != nil {
		for k := range ctx.Vars {
			symbols[k] = true
		}
		ctx = ctx.Outer
	}

	// Convert to slice
	result := make([]string, 0, len(symbols))
	for sym := range symbols {
		result = append(result, sym)
	}

	return result
}

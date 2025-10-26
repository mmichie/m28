package core

import (
	"fmt"
	"log"
	"os"
)

var debugLookups = os.Getenv("M28_DEBUG_LOOKUPS") != ""

// builtinRegistry tracks all builtin registrations
var builtinRegistry = NewRegistry("builtin")

// BuiltinLogging controls whether to log duplicate builtin registrations
var BuiltinLogging = false

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

	// Metadata table for tracking source locations, types, comments
	// This is shared across all contexts in a program
	Metadata *IRMetadata

	// Variables declared as global in this scope (Python global statement)
	GlobalVars map[string]bool

	// Variables declared as nonlocal in this scope (Python nonlocal statement)
	NonlocalVars map[string]bool

	// Optional module dict to sync definitions to (for circular import support)
	// When set, Define() will also update this dict in real-time
	ModuleDict *DictValue
}

// NewContext creates a new evaluation context
func NewContext(outer *Context) *Context {
	ctx := &Context{
		Vars:         make(map[string]Value),
		Outer:        outer,
		CallStack:    make([]TraceEntry, 0),
		GlobalVars:   make(map[string]bool),
		NonlocalVars: make(map[string]bool),
	}

	// If this is the global context, set Global to self and create new metadata
	if outer == nil {
		ctx.Global = ctx
		ctx.Metadata = NewIRMetadata()
	} else {
		// Otherwise, inherit global and metadata from parent
		ctx.Global = outer.Global
		ctx.Metadata = outer.Metadata
	}

	return ctx
}

// WithMetadata creates a new context with a specific metadata table
// Useful for module imports or testing
func (c *Context) WithMetadata(metadata *IRMetadata) *Context {
	newCtx := &Context{
		Vars:            c.Vars,
		Outer:           c.Outer,
		Global:          c.Global,
		CallStack:       c.CallStack,
		CurrentFunction: c.CurrentFunction,
		Metadata:        metadata,
		GlobalVars:      c.GlobalVars,
		NonlocalVars:    c.NonlocalVars,
	}
	return newCtx
}

// Define defines a new variable in the current scope
func (c *Context) Define(name string, value Value) {
	// If this is the global context and the value is a builtin function,
	// register it in the builtin registry for tracking
	if c.Outer == nil {
		if _, isBuiltin := value.(*BuiltinFunction); isBuiltin {
			// Try to register in the builtin registry
			// Use depth 3: Define -> RegisterWithDepth -> getCallerLocation
			if err := builtinRegistry.RegisterWithDepth(name, value, 3); err != nil {
				// Log the duplicate if logging is enabled
				if BuiltinLogging {
					log.Printf("WARNING: %v", err)
				}
			}
		}
	}
	c.Vars[name] = value

	// If this context has a ModuleDict, sync the definition to it in real-time
	// This enables circular imports to see partially-populated modules
	if c.ModuleDict != nil {
		// Skip dunder variables (__name__, __file__, etc.) - they're internal
		if len(name) >= 2 && name[:2] == "__" && name[len(name)-2:] == "__" {
			return
		}
		// Skip private vars (starting with _) for module exports
		if len(name) > 0 && name[0] == '_' {
			return
		}
		// Add to module dict with proper key formatting
		key := ValueToKey(StringValue(name))
		c.ModuleDict.SetWithKey(key, StringValue(name), value)
	}
}

// DefineBuiltin defines a builtin function with duplicate tracking
// This should be used instead of Define when registering builtins
func (c *Context) DefineBuiltin(name string, value Value) error {
	// Only track builtins in the global context
	if c.Outer == nil {
		// Try to register in the global builtin registry
		// Use depth 3: DefineBuiltin -> RegisterWithDepth -> getCallerLocation
		if err := builtinRegistry.RegisterWithDepth(name, value, 3); err != nil {
			// Log the duplicate if logging is enabled
			if BuiltinLogging {
				log.Printf("WARNING: %v", err)
			}
			// Return error but caller can choose to ignore it
			return err
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

// Delete removes a variable from the current scope
// Returns error if variable doesn't exist in current scope
func (c *Context) Delete(name string) error {
	// Check if variable exists in current scope
	if _, ok := c.Vars[name]; ok {
		delete(c.Vars, name)
		return nil
	}

	// Variable not found in current scope
	return &NameError{Name: name}
}

// Lookup finds a variable in the current or outer scopes
func (c *Context) Lookup(name string) (Value, error) {
	return c.lookupWithDepth(name, 0)
}

func (c *Context) lookupWithDepth(name string, depth int) (Value, error) {
	// Prevent infinite loops in context chain
	if depth > 100 {
		log.Printf("ERROR: Lookup depth exceeded for '%s' - possible circular context chain", name)
		return nil, &NameError{Name: name}
	}

	// DEBUG: Log lookups that exceed a certain depth
	if depth > 50 {
		log.Printf("WARN: Deep lookup for '%s' at depth %d", name, depth)
	}

	// DEBUG: Detailed lookup logging if enabled
	if debugLookups && depth < 5 {
		log.Printf("LOOKUP[%d]: '%s' in context %p (vars=%d, outer=%p, global=%p)",
			depth, name, c, len(c.Vars), c.Outer, c.Global)
	}

	// Check current scope
	if val, ok := c.Vars[name]; ok {
		if debugLookups && depth < 5 {
			log.Printf("  FOUND in local scope")
		}
		return val, nil
	}

	// Also check ModuleDict if it exists on current context
	// This allows globals().update({...}) to work - names added to the module dict
	// via .update() will be accessible even if they weren't added via Define()
	if c.ModuleDict != nil {
		// Convert name to the internal key representation used by dict
		key := ValueToKey(StringValue(name))
		if val, ok := c.ModuleDict.Get(key); ok {
			return val, nil
		}
	}

	// Check outer scopes
	if c.Outer != nil {
		return c.Outer.lookupWithDepth(name, depth+1)
	}

	// If we reached the end of the scope chain and have a Global context,
	// check its ModuleDict as a last resort. This allows functions to access
	// module-level variables that were defined after the function was created.
	if c.Global != nil && c.Global.ModuleDict != nil && c.Global != c {
		key := ValueToKey(StringValue(name))
		if val, ok := c.Global.ModuleDict.Get(key); ok {
			return val, nil
		}
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

// GetBuiltinRegistry returns the global builtin registry
func GetBuiltinRegistry() *Registry {
	return builtinRegistry
}

// GetBuiltinRegistrationInfo returns information about builtin registrations
func GetBuiltinRegistrationInfo() map[string]interface{} {
	return builtinRegistry.GetAll()
}

// GetDuplicateBuiltins returns a list of builtin names that were attempted to be registered multiple times
func GetDuplicateBuiltins() []string {
	duplicates := builtinRegistry.GetDuplicates()
	names := make([]string, 0, len(duplicates))
	for name := range duplicates {
		names = append(names, name)
	}
	return names
}

// GetDuplicateBuiltinDetails returns detailed information about duplicate builtin registrations
func GetDuplicateBuiltinDetails() map[string][]string {
	return builtinRegistry.GetDuplicates()
}

// DeclareGlobal marks a variable as global in the current scope
// This means all assignments to this variable will modify the global scope
func (c *Context) DeclareGlobal(name string) {
	if c.GlobalVars == nil {
		c.GlobalVars = make(map[string]bool)
	}
	c.GlobalVars[name] = true
}

// DeclareNonlocal marks a variable as nonlocal in the current scope
// This means all assignments to this variable will modify the nearest enclosing scope
func (c *Context) DeclareNonlocal(name string) error {
	if c.NonlocalVars == nil {
		c.NonlocalVars = make(map[string]bool)
	}

	// Check that the variable exists in an enclosing scope
	if c.Outer == nil {
		return fmt.Errorf("no binding for nonlocal '%s' found", name)
	}

	// Check outer scopes for the variable
	if _, err := c.Outer.Lookup(name); err != nil {
		return fmt.Errorf("no binding for nonlocal '%s' found", name)
	}

	c.NonlocalVars[name] = true
	return nil
}

// IsGlobal checks if a variable is declared as global in this scope
func (c *Context) IsGlobal(name string) bool {
	return c.GlobalVars != nil && c.GlobalVars[name]
}

// IsNonlocal checks if a variable is declared as nonlocal in this scope
func (c *Context) IsNonlocal(name string) bool {
	return c.NonlocalVars != nil && c.NonlocalVars[name]
}

package core

import (
	"fmt"
	"os"

	"github.com/mmichie/m28/common/suggestions"
)

var debugLookups = os.Getenv("M28_DEBUG_LOOKUPS") != ""

// GetOperatorFunc is a hook to the operator registry's GetOperator function
// This avoids circular imports while allowing fast operator lookup
var GetOperatorFunc func(string) (Value, bool)

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

	// Evaluation counter for debugging and progress tracking
	EvalCount uint64

	// Current exception info for sys.exc_info()
	// Set when entering an except handler, cleared when exiting
	ExcType  Value
	ExcValue Value
	ExcTb    Value

	// Location stack for tracking source locations during evaluation
	// Eliminates need for LocatedValue unwrapping in evaluator internals
	LocationStack []*SourceLocation
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
					Log.Warn(SubsystemBuiltin, "Duplicate builtin registration", "error", err)
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
				Log.Warn(SubsystemBuiltin, "Duplicate builtin registration", "error", err)
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
	// Check if variable exists in ModuleDict first (since Lookup checks it first)
	if c.ModuleDict != nil {
		key := ValueToKey(StringValue(name))
		if _, ok := c.ModuleDict.Get(key); ok {
			c.ModuleDict.Delete(key)
			// Also delete from Vars if present
			delete(c.Vars, name)
			return nil
		}
	}

	// Check if variable exists in current scope Vars
	if _, ok := c.Vars[name]; ok {
		delete(c.Vars, name)
		return nil
	}

	// Variable not found in current scope
	// Generate suggestion based on similar names in scope
	availableNames := c.GetAllAvailableNames()
	suggestion := generateNameSuggestion(name, availableNames)
	return &NameError{
		Name:       name,
		Suggestion: suggestion,
	}
}

// Lookup finds a variable in the current or outer scopes
func (c *Context) Lookup(name string) (Value, error) {
	// Special case: Always return the singleton for None, True, False
	// This ensures identity checks (is None) work correctly across all contexts
	// These values are singletons and should have consistent identity
	switch name {
	case "None":
		return None, nil
	case "True":
		return BoolValue(true), nil
	case "False":
		return BoolValue(false), nil
	}

	// Fast path: Check global operator registry first (no context traversal)
	// This is critical for performance when operators are used in deeply nested contexts
	// Import is done via GetOperatorFunc to avoid circular dependency
	if GetOperatorFunc != nil {
		if op, isOperator := GetOperatorFunc(name); isOperator {
			return op, nil
		}
	}
	return c.lookupWithDepth(name, 0)
}

func (c *Context) lookupWithDepth(name string, depth int) (Value, error) {
	// Prevent infinite loops in context chain
	if depth > 100 {
		Log.Error(SubsystemScope, "Lookup depth exceeded - possible circular context chain", "name", name)
		availableNames := c.GetAllAvailableNames()
		suggestion := generateNameSuggestion(name, availableNames)
		return nil, &NameError{
			Name:       name,
			Suggestion: suggestion,
		}
	}

	// DEBUG: Log lookups that exceed a certain depth
	if depth > 50 {
		Log.Warn(SubsystemScope, "Deep lookup", "name", name, "depth", depth)
	}

	// DEBUG: Detailed lookup logging if enabled
	if debugLookups && depth < 5 {
		Log.Debug(SubsystemScope, "Variable lookup",
			"depth", depth, "name", name, "context_ptr", fmt.Sprintf("%p", c),
			"vars_count", len(c.Vars), "outer_ptr", fmt.Sprintf("%p", c.Outer), "global_ptr", fmt.Sprintf("%p", c.Global))
	}

	// For module contexts (those with ModuleDict), check ModuleDict FIRST
	// This ensures dynamic updates to module.__dict__ are visible to functions
	// This is critical for Python's importlib._setup() which dynamically injects globals
	if c.ModuleDict != nil {
		key := ValueToKey(StringValue(name))
		if val, ok := c.ModuleDict.Get(key); ok {
			if debugLookups && depth < 5 {
				Log.Debug(SubsystemScope, "Variable found in ModuleDict", "name", name)
			}
			return val, nil
		}
	}

	// Then check current scope Vars (for true local variables)
	if val, ok := c.Vars[name]; ok {
		if debugLookups && depth < 5 {
			Log.Debug(SubsystemScope, "Variable found in local scope", "name", name)
		}
		return val, nil
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

	// Variable not found - generate suggestion
	availableNames := c.GetAllAvailableNames()
	suggestion := generateNameSuggestion(name, availableNames)
	return nil, &NameError{
		Name:       name,
		Suggestion: suggestion,
	}
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

// PushLocation adds a source location to the location stack
// This is called when beginning evaluation of an expression with known location
func (c *Context) PushLocation(loc *SourceLocation) {
	if c.LocationStack == nil {
		c.LocationStack = make([]*SourceLocation, 0, 8)
	}
	c.LocationStack = append(c.LocationStack, loc)
}

// PopLocation removes the most recent location from the stack
// This should be called (typically via defer) when finishing evaluation of an expression
func (c *Context) PopLocation() {
	if len(c.LocationStack) > 0 {
		c.LocationStack = c.LocationStack[:len(c.LocationStack)-1]
	}
}

// CurrentLocation returns the current source location (top of stack)
// Returns nil if location stack is empty
func (c *Context) CurrentLocation() *SourceLocation {
	if len(c.LocationStack) > 0 {
		return c.LocationStack[len(c.LocationStack)-1]
	}
	return nil
}

// GetAllAvailableNames returns all available names in this context and its parents
// This is used for generating "did you mean" suggestions
func (c *Context) GetAllAvailableNames() []string {
	names := make(map[string]bool)

	// Collect from this context and all parents
	ctx := c
	for ctx != nil {
		// Add from Vars
		for k := range ctx.Vars {
			names[k] = true
		}
		// Add from ModuleDict if present
		if ctx.ModuleDict != nil {
			for _, keyStr := range ctx.ModuleDict.Keys() {
				names[keyStr] = true
			}
		}
		ctx = ctx.Outer
	}

	// Check global context ModuleDict as well
	if c.Global != nil && c.Global.ModuleDict != nil && c.Global != c {
		for _, keyStr := range c.Global.ModuleDict.Keys() {
			names[keyStr] = true
		}
	}

	// Convert to slice
	result := make([]string, 0, len(names))
	for name := range names {
		result = append(result, name)
	}

	return result
}

// generateNameSuggestion generates a "did you mean" suggestion for an undefined name
func generateNameSuggestion(target string, availableNames []string) string {
	// Find similar names within edit distance 2, suggest up to 3
	similar := suggestions.FindSimilarNames(target, availableNames, 2, 3)
	return suggestions.FormatSuggestion(similar)
}

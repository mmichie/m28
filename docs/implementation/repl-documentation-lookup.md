# REPL Documentation Lookup Design

This document outlines the design for adding documentation lookup functionality to the M28 REPL.

## Command Structure

The documentation lookup will be implemented with the following REPL commands:

1. `:doc <symbol>` - Look up documentation for a specific symbol
2. `:doc-builtins` - List all built-in functions with brief descriptions
3. `:doc-special-forms` - List all special forms with brief descriptions
4. `:doc-modules` - List all available modules with brief descriptions

The `:help` command will be updated to include these new commands.

## Documentation Storage System

Documentation will be stored in a central registry to make it easily accessible from the REPL and potentially from other parts of the system.

### Documentation Structure

Each documentation entry will contain:

```go
// DocEntry stores documentation for a symbol
type DocEntry struct {
    // Name of the symbol
    Name string
    
    // Type of the symbol (e.g. "function", "special-form", "class", "method")
    Type string
    
    // Brief one-line description
    Brief string
    
    // Detailed description (can include multiline text)
    Description string
    
    // Parameters documentation (only for callables)
    Params []ParamDoc
    
    // Return value documentation (only for callables)
    Returns string
    
    // Example usage
    Examples []string
    
    // Related topics
    Related []string
    
    // Module where this is defined (if applicable)
    Module string
}

// ParamDoc documents a parameter
type ParamDoc struct {
    // Parameter name
    Name string
    
    // Parameter type (if known)
    Type string
    
    // Description of the parameter
    Description string
    
    // Whether the parameter is optional
    Optional bool
    
    // Default value (if optional)
    Default string
}
```

### Documentation Registry

The documentation registry will be a singleton object that can be accessed from anywhere in the codebase:

```go
// DocRegistry is the global documentation registry
type DocRegistry struct {
    // Map from symbol name to documentation
    entries map[string]DocEntry
    
    // Map from module name to list of symbols in that module
    moduleSymbols map[string][]string
    
    // Map from documentation type to list of symbols
    typeSymbols map[string][]string
}

// Global instance of the documentation registry
var DocRegistryInstance = NewDocRegistry()

// RegisterDoc registers documentation for a symbol
func RegisterDoc(entry DocEntry) {
    // Code to add entry to the registry
}

// GetDoc retrieves documentation for a symbol
func GetDoc(name string) (DocEntry, bool) {
    // Code to retrieve entry from the registry
}

// ListByType retrieves all symbols of a given type
func ListByType(typeName string) []DocEntry {
    // Code to retrieve entries by type
}

// ListByModule retrieves all symbols from a given module
func ListByModule(moduleName string) []DocEntry {
    // Code to retrieve entries by module
}
```

## Implementation Details

### Documentation File Format

Documentation will be stored in Go code files to make it easy to maintain alongside the code it documents. A helper function will be used to register documentation for each item.

Example for registering documentation for a built-in function:

```go
// Documentation for the print function
func init() {
    core.RegisterDoc(core.DocEntry{
        Name:        "print",
        Type:        "builtin-function",
        Brief:       "Print objects to the console",
        Description: "Prints the given objects to the console, separated by spaces and followed by a newline.",
        Params: []core.ParamDoc{
            {
                Name:        "objects",
                Description: "One or more objects to print",
                Optional:    true,
            },
        },
        Returns:   "None",
        Examples:  []string{`(print "Hello, world!")`, `(print "Value:", 42)`},
        Related:   []string{"str", "input"},
        Module:    "builtin",
    })
}
```

### REPL Command Handlers

The REPL will be extended with command handlers for the new documentation commands:

```go
// handleDocCommand handles the :doc command
func (r *REPL) handleDocCommand(cmd string) bool {
    // Parse the command to extract the symbol name
    // Look up the documentation for the symbol
    // Format and display the documentation
    return true
}

// handleDocBuiltinsCommand handles the :doc-builtins command
func (r *REPL) handleDocBuiltinsCommand() bool {
    // Get all built-in function documentation entries
    // Format and display them in a table
    return true
}

// handleDocSpecialFormsCommand handles the :doc-special-forms command
func (r *REPL) handleDocSpecialFormsCommand() bool {
    // Get all special form documentation entries
    // Format and display them in a table
    return true
}

// handleDocModulesCommand handles the :doc-modules command
func (r *REPL) handleDocModulesCommand() bool {
    // Get all module documentation entries
    // Format and display them in a table
    return true
}
```

### Documentation Display

Documentation will be displayed in a formatted manner in the REPL:

```
m28> :doc print
┌───────────────────────────────────────────────────────────────────┐
│ print                                                   [function] │
├───────────────────────────────────────────────────────────────────┤
│ Print objects to the console                                       │
│                                                                    │
│ Description:                                                       │
│   Prints the given objects to the console, separated by spaces and │
│   followed by a newline.                                           │
│                                                                    │
│ Parameters:                                                        │
│   objects (optional) - One or more objects to print                │
│                                                                    │
│ Returns:                                                           │
│   None                                                             │
│                                                                    │
│ Examples:                                                          │
│   (print "Hello, world!")                                          │
│   (print "Value:", 42)                                             │
│                                                                    │
│ Related:                                                           │
│   str, input                                                       │
│                                                                    │
│ Module:                                                            │
│   builtin                                                          │
└───────────────────────────────────────────────────────────────────┘
```

## Documentation Coverage

The initial implementation will focus on documenting:

1. All built-in functions (core functions available by default)
2. All special forms (syntax elements like `if`, `for`, `def`, etc.)
3. Common modules and their functions

Later phases may include:

1. Standard library functions
2. Class and method documentation
3. Language syntax guide
4. Interactive tutorials

## Implementation Plan

1. Create the documentation registry in `core/doc_registry.go`
2. Add REPL command handlers for documentation lookup
3. Document all built-in functions
4. Document all special forms
5. Update help command to include new documentation commands
6. Add formatting utilities for pretty-printing documentation
7. Add module documentation

## Benefits

- Better developer experience for M28 users
- Inline documentation for functions and special forms
- Discoverability of language features
- Reduced need to consult external documentation
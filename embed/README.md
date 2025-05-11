# M28 Embedding Library

This package provides tools for embedding the M28 language interpreter in other Go applications, particularly for use as a shell scripting language.

## Features

- Clean, simple API for evaluating M28 code
- Built-in shell integration functions
- Customizable command execution
- Environment variable access
- Ability to define custom functions in Go that can be called from M28

## Usage

### Basic Usage

```go
import "github.com/mmichie/m28/embed"

// Create a new M28 engine
engine := embed.NewM28Engine()

// Evaluate code
result, err := engine.Evaluate("(+ 1 2)")
if err != nil {
    // Handle error
}
fmt.Println(result) // Output: 3

// Execute a file
err = engine.ExecuteFile("script.m28")
if err != nil {
    // Handle error
}
```

### Defining Custom Functions

```go
// Define a custom function
engine.DefineFunction("hello", func(args ...core.LispValue) (core.LispValue, error) {
    if len(args) != 1 {
        return nil, fmt.Errorf("hello: expected 1 argument, got %d", len(args))
    }
    
    name, ok := args[0].(core.LispString)
    if !ok {
        return nil, fmt.Errorf("hello: expected string argument, got %T", args[0])
    }
    
    return core.LispString(fmt.Sprintf("Hello, %s!", name)), nil
})

// Now you can call it from M28
result, _ := engine.Evaluate(`(hello "World")`)
fmt.Println(result) // Output: Hello, World!
```

### Using as a Shell

M28 can be used as a shell scripting language. The embedding library provides several shell-related functions:

- `(shell "command")` - Execute a shell command and return its output
- `(getenv "VAR")` - Get environment variable
- `(setenv "VAR" "value")` - Set environment variable
- `(pwd)` - Get current working directory
- `(cd "dir")` - Change directory
- `(exit [code])` - Exit with status code (default 0)

See the `m28shell` example for a complete implementation of a shell using M28.

## Shell Example

A complete example shell is provided in the `m28shell` directory. To use it:

1. Build the shell: `go build -o m28sh github.com/mmichie/m28/embed/m28shell`
2. Run it: `./m28sh`

### Shell Features

- Interactive command line with history
- Support for executing M28 code directly
- Shorthand for shell commands with `!` prefix (e.g., `!ls -la`)
- RC file loading (`.m28rc` in home directory or current directory)

### RC File Example

Create a `.m28rc` file in your home directory:

```lisp
;; Define a welcome message
(define welcome-message "Welcome to M28 Shell!")
(println welcome-message)

;; Define useful shell functions
(define (ls) (shell "ls -la"))
(define (grep pattern file) (shell (+ "grep " pattern " " file)))

;; Set up prompt
(define (prompt)
  (+ (getenv "USER") "@" (last (split "/" (pwd))) "> "))
```

## Custom Shell Integration

You can customize how shell commands are executed by setting the `ShellExecutor` function:

```go
engine.ShellExecutor = func(command string) (string, error) {
    // Custom implementation
    // ...
}
```

This allows for special handling of certain commands or integration with other systems.
# M28 Embedding Examples

This directory contains examples showing how to embed M28 in Go applications.

## Quick Start

The simplest way to embed M28:

```go
package main

import (
    "fmt"
    "log"
    "github.com/mmichie/m28/embed"
)

func main() {
    // Create the M28 engine
    engine := embed.NewM28Engine()
    
    // Evaluate arithmetic
    result, err := engine.Evaluate("(+ 1 2 3)")
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(result) // Output: 6
    
    // Work with lists
    result, err = engine.Evaluate("[1, 2, 3, 4, 5]")
    fmt.Println(result) // Output: [1, 2, 3, 4, 5]
    
    // Use functional programming
    result, err = engine.Evaluate("(map (lambda (x) (* x 2)) [1, 2, 3])")
    fmt.Println(result) // Output: [2, 4, 6]
}
```

## Examples

### 1. Simple Embedding (`simple/`)
Basic examples showing:
- Arithmetic evaluation
- Variable definition
- Custom Go functions
- Using built-in functions
- Shell command execution

### 2. Calculator (`calculator/`)
An interactive calculator demonstrating:
- REPL loop implementation
- Mathematical constants
- Custom math functions
- User interaction

### 3. Configuration Processor (`config-processor/`)
Using M28 as a configuration language:
- Dynamic configuration with code
- Environment variable integration
- Computed values
- Configuration validation

## Key Features

### Easy Integration
- Single import: `github.com/mmichie/m28/embed`
- Simple API: `NewM28Engine()` and `Evaluate()`
- No complex setup required

### Extend with Go Functions
```go
engine.DefineFunction("myFunc", func(args []core.Value, ctx *core.Context) (core.Value, error) {
    // Your Go code here
    return result, nil
})
```

### Share Data
```go
// From Go to M28
engine.DefineValue("myVar", core.StringValue("Hello"))

// Evaluate M28 code that uses the variable
result, _ := engine.Evaluate(`(str myVar " World")`)
```

### Built-in Shell Integration
```go
// Execute shell commands from M28
result, _ := engine.Evaluate(`(shell "ls -la")`)

// Access environment variables
result, _ := engine.Evaluate(`(getenv "HOME")`)
```

## Common Use Cases

1. **Scripting Engine**: Add scripting capabilities to your application
2. **Configuration Language**: More powerful than JSON/YAML
3. **Expression Evaluator**: Complex calculations and data transformations
4. **Plugin System**: Let users extend your app with M28 scripts
5. **Testing DSL**: Create domain-specific testing languages

## Running the Examples

```bash
# Run simple example
go run examples/embedding/simple/main.go

# Run calculator
go run examples/embedding/calculator/main.go

# Run config processor
go run examples/embedding/config-processor/main.go
```

## Advanced Usage

### Custom Shell Executor
```go
engine.ShellExecutor = func(cmd string) (string, error) {
    // Custom command execution logic
    return output, nil
}
```

### Execute Files
```go
err := engine.ExecuteFile("script.m28")
```

### Error Handling
```go
result, err := engine.Evaluate(code)
if err != nil {
    // Parse errors, runtime errors, etc.
    log.Printf("Evaluation failed: %v", err)
}
```

## Performance Considerations

- The engine is lightweight and fast
- Each evaluation creates a new child context (variables are isolated)
- Built-in functions are registered once at engine creation
- No compilation step - direct interpretation

## Security Notes

- Shell commands execute with current process permissions
- No sandboxing by default
- Consider implementing command whitelisting for untrusted input
- File operations use process working directory

## Next Steps

1. Check the [embed package documentation](../embed/README.md)
2. Look at the [M28 language guide](../../README.md)
3. Explore the [standard library](../../builtin/)
4. Build your own embedded application!
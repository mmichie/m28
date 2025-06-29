# M28 Integration Notes

This document captures lessons learned from integrating M28 with other applications, particularly shell environments like gosh.

## Key Integration Considerations

### 1. Expression Parsing
When embedding M28, be aware that:
- Single expressions like `(+ 1 2 3 4)` should be evaluated directly
- The parser returns complete S-expressions, not lists to iterate
- Only use iteration for multiple top-level expressions or "do" forms

### 2. Module Dependencies
When your application depends on M28:
- Go's module cache can cause issues after M28 updates
- Use `go clean -modcache` before rebuilding to ensure latest changes
- Consider using `replace` directives during development

### 3. Context Management
- The engine maintains a global context with all builtins
- Each evaluation creates a child context for isolation
- ExecuteFile now properly inherits builtins from the engine context

### 4. Shell Integration Patterns
For shell-like environments:
```go
// Pattern 1: Direct expression evaluation
if IsLispExpression(command) {
    result, err := m28Engine.Evaluate(command)
    // Handle result
}

// Pattern 2: Embedded expressions in shell commands
processedCmd, err := evaluateM28InCommand(shellCommand)
// This handles patterns like: echo "Result: (+ 1 2)"
```

### 5. Error Handling
Current limitations:
- Error messages don't include line/column information
- Shell command errors lose exit codes
- No distinction between parse and runtime errors

Workarounds:
- Wrap errors with context information
- Consider implementing custom error types
- Log additional debugging information

## Common Pitfalls and Solutions

### Pitfall 1: Losing Builtins
**Problem**: ExecuteFile created a new empty context
**Solution**: Fixed to use `core.NewContext(m.ctx)` to inherit builtins

### Pitfall 2: Expression Iteration
**Problem**: Code tried to iterate through `(+ 1 2 3 4)` as separate expressions
**Solution**: Remove iteration logic, evaluate expressions directly

### Pitfall 3: Type Mismatches
**Problem**: Documentation showed outdated types (LispValue vs Value)
**Solution**: Updated all examples to use current `core.Value` types

## Future Improvements

1. **Better Error Types**: Implement specific error types for parse/runtime/shell errors
2. **Stateful Evaluation**: Add options to preserve state between evaluations
3. **Shell Features**: Add timeout support, better process management
4. **Debugging Support**: Add trace/debug modes for troubleshooting

## Example: Minimal Shell Integration

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    
    "github.com/mmichie/m28/embed"
)

func main() {
    engine := embed.NewM28Engine()
    scanner := bufio.NewScanner(os.Stdin)
    
    for {
        fmt.Print("> ")
        if !scanner.Scan() {
            break
        }
        
        input := scanner.Text()
        
        // Check if it's a complete M28 expression
        if strings.HasPrefix(input, "(") && strings.HasSuffix(input, ")") {
            result, err := engine.Evaluate(input)
            if err != nil {
                fmt.Printf("Error: %v\n", err)
            } else {
                fmt.Println(result)
            }
        } else {
            // Handle as shell command
            fmt.Println("Shell commands not implemented")
        }
    }
}
```

This example shows the basic pattern for integrating M28 expressions into a shell-like environment.
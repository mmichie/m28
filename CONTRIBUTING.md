# Contributing to M28

Thank you for your interest in contributing to M28! This guide will help you understand the codebase and follow our development practices.

## Table of Contents

- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Code Style Guidelines](#code-style-guidelines)
- [LocatedValue Unwrapping Pattern](#locatedvalue-unwrapping-pattern)
- [Testing](#testing)
- [Submitting Changes](#submitting-changes)

## Getting Started

### Prerequisites

- Go 1.21 or later
- Make
- Git

### Building M28

```bash
make build
```

### Running Tests

```bash
make test
```

### Running the REPL

```bash
make run
```

## Development Workflow

1. Create a feature branch from `main`
2. Make your changes following our code style guidelines
3. Write tests for your changes
4. Run all tests to ensure nothing is broken
5. Commit your changes with conventional commit messages
6. Submit a pull request

## Code Style Guidelines

### Go Code

M28 is written in Go. Follow standard Go conventions:

- **Naming**:
  - Packages: lowercase (e.g., `builtin`, `core`, `eval`)
  - Functions: camelCase (e.g., `evalExpression`, `unwrapLocated`)
  - Types: PascalCase (e.g., `LocatedValue`, `SymbolValue`)
  - Constants: PascalCase or UPPER_CASE depending on visibility

- **Error Handling**:
  - Return errors as the second return value
  - Provide descriptive error messages
  - Include type information in type errors
  - Use source location when available for better error messages

- **Imports**:
  - Group imports: stdlib first, then third-party, then internal
  - Use goimports to format automatically

- **Comments**:
  - Document all exported functions, types, and methods
  - Explain complex logic or non-obvious behaviors
  - Reference relevant issues or design docs when applicable

### M28 Code

When writing M28 code (tests, examples):

- **Comments**: ALWAYS use `#` (NEVER use `;`)
- **Variables**: ALWAYS use `=` (NEVER use `def` for variables)
- **Functions**: Use `def` for function definitions only
- Follow Python naming conventions: `snake_case` for functions and variables
- Use descriptive names

### Example M28 Code

```lisp
# This is a comment

# Variable assignment
x = 10

# Function definition
def add(a, b):
  (+ a b)

# Function call - both styles work
(print "S-expression style")
print("Pythonic style")
```

## LocatedValue Unwrapping Pattern

**CRITICAL**: One of the most common bugs in M28 development is forgetting to unwrap `LocatedValue` before type assertions.

### The Rule

**Always unwrap before type assertions or type switches.**

### Why This Matters

M28 wraps values in `LocatedValue` to preserve source location information for better error messages. However, type assertions will fail if you don't unwrap first:

```go
// WRONG - Will panic if value is wrapped in LocatedValue
sym := value.(core.SymbolValue)

// CORRECT - Unwrap first, then assert safely
unwrapped := unwrapLocated(value)
sym, ok := unwrapped.(core.SymbolValue)
if !ok {
    return fmt.Errorf("expected symbol, got %v", unwrapped.Type())
}
```

### Two Approaches

#### 1. Manual Unwrapping (for special forms and general code)

```go
import "github.com/mmichie/m28/eval"

func mySpecialForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
    // Unwrap before type checking
    unwrapped := unwrapLocated(args.Items()[0])

    // Now safe to type assert with comma-ok idiom
    if sym, ok := unwrapped.(core.SymbolValue); ok {
        // Use sym
    } else {
        return nil, fmt.Errorf("expected symbol, got %v", unwrapped.Type())
    }
}
```

The `unwrapLocated()` function is in `eval/util.go`:

```go
func unwrapLocated(v core.Value) core.Value {
    for {
        if lv, ok := v.(core.LocatedValue); ok {
            v = lv.Value
        } else {
            return v
        }
    }
}
```

#### 2. Smart Accessors (for ListValue items)

```go
func mySpecialForm(args *core.ListValue, ctx *core.Context) (core.Value, error) {
    // GetItemAsSymbol automatically unwraps
    sym, ok := args.GetItemAsSymbol(0)
    if !ok {
        unwrapped, _ := args.GetItemUnwrapped(0)
        return nil, fmt.Errorf("expected symbol, got %v", unwrapped.Type())
    }

    // Use sym
}
```

Available smart accessors on `ListValue`:
- `GetItemUnwrapped(index) (Value, error)` - Returns unwrapped value
- `GetItemAsSymbol(index) (SymbolValue, bool)` - Returns symbol or false
- `GetItemAsString(index) (StringValue, bool)` - Returns string or false
- `GetItemAsList(index) (*ListValue, bool)` - Returns list or false

### When to Unwrap

You MUST unwrap in these situations:

1. **Before type assertions**: `value.(SomeType)`
2. **Before type switches**: `switch t := value.(type)`
3. **Before equality comparisons with specific types**
4. **When extracting values from lists for type-specific processing**

### Checklist for New Code

When writing code that handles `core.Value`:

- [ ] Did I unwrap before type assertions?
- [ ] Am I using comma-ok idiom (`value, ok :=`) instead of bare assertions?
- [ ] Are my error messages using the unwrapped value's type?
- [ ] Could I use a smart accessor instead of manual unwrapping?
- [ ] Did I test with Python syntax (which always wraps values)?

### Common Mistakes

```go
// MISTAKE 1: Bare type assertion
target := args.Items()[0].(core.SymbolValue)  // PANICS on LocatedValue

// FIX:
unwrapped := unwrapLocated(args.Items()[0])
target, ok := unwrapped.(core.SymbolValue)
if !ok {
    return nil, fmt.Errorf("expected symbol, got %v", unwrapped.Type())
}

// MISTAKE 2: Type switch without unwrapping
switch t := value.(type) {  // Won't match if value is LocatedValue
case core.SymbolValue:
    // ...
}

// FIX:
unwrapped := unwrapLocated(value)
switch t := unwrapped.(type) {
case core.SymbolValue:
    // ...
}

// MISTAKE 3: Forgetting to use smart accessors
target := args.Items()[1].(*core.ListValue)  // PANICS on LocatedValue

// FIX:
target, ok := args.GetItemAsList(1)
if !ok {
    unwrapped, _ := args.GetItemUnwrapped(1)
    return nil, fmt.Errorf("expected list, got %v", unwrapped.Type())
}
```

### Further Reading

For a comprehensive guide with examples and debugging tips, see:
- [docs/development/error-handling.md](docs/development/error-handling.md)
- [core/located_value.go](core/located_value.go) - Type definition with inline docs
- [core/containers.go](core/containers.go) - Smart accessor implementations

## Testing

### Test Organization

- Unit tests: `*_test.go` files alongside source code
- Integration tests: `tests/*.m28` files
- CPython compatibility tests: `tests/cpython/test_*.py` files

### Writing Tests

#### Go Tests

```go
func TestMyFeature(t *testing.T) {
    ctx := core.NewContext()

    // Test code here

    if result != expected {
        t.Errorf("expected %v, got %v", expected, result)
    }
}
```

#### M28 Tests

```lisp
# tests/test_my_feature.m28

# Test variable assignment
x = 10
(assert (== x 10) "x should be 10")

# Test function definition and call
def double(n):
  (* n 2)

(assert (== (double 5) 10) "double(5) should be 10")
```

### Running Tests

```bash
# All tests
make test

# Single M28 test
./bin/m28 tests/test_my_feature.m28

# Single CPython compatibility test
./bin/m28 tests/cpython/test_bool.py

# Go unit tests
go test ./...

# Specific package
go test ./eval
```

### Test Coverage

When adding new features:

1. Write tests for the happy path
2. Write tests for error cases
3. Test edge cases (empty inputs, nil values, etc.)
4. Test interaction with existing features
5. Add CPython compatibility tests if implementing Python features

## Submitting Changes

### Commit Messages

Use conventional commit format:

```
type(scope): brief description

Longer explanation if needed.

Closes ISSUE-ID
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `refactor`: Code refactoring
- `test`: Test additions or changes
- `chore`: Maintenance tasks

Examples:
```
feat(eval): implement async/await syntax

fix: unwrap LocatedValue in star unpacking

docs: add LocatedValue unwrapping guide

Closes M28-2f47
```

### Pull Request Process

1. Ensure all tests pass
2. Update documentation if needed
3. Add tests for new features or bug fixes
4. Follow the code style guidelines
5. Include a clear description of changes
6. Reference related issues

### Code Review

- Be responsive to feedback
- Explain your design decisions
- Be open to suggestions
- Keep discussions focused and respectful

## Getting Help

- Check existing documentation in `docs/`
- Look at similar code in the codebase
- Review recent commits for examples
- Ask questions in pull request discussions

## Project Structure

```
m28/
├── builtin/          # Built-in functions and types
├── core/             # Core types and interfaces
│   ├── value.go      # Value interface and basic types
│   ├── types.go      # Type system
│   ├── containers.go # List, dict, set implementations
│   └── located_value.go  # Source location tracking
├── eval/             # Expression evaluator
│   ├── evaluator.go  # Main evaluation logic
│   ├── util.go       # Helper functions (unwrapLocated, etc.)
│   └── *_forms.go    # Special form handlers
├── parser/           # S-expression and Python parsers
├── modules/          # Standard library modules
├── tests/            # M28 and Python test files
└── docs/             # Documentation
    ├── development/  # Developer guides
    └── design/       # Design documents
```

## Key Files to Know

- `core/value.go` - Value interface and type definitions
- `core/located_value.go` - Source location tracking
- `eval/evaluator.go` - Main evaluation loop
- `eval/util.go` - Helper functions including `unwrapLocated()`
- `core/containers.go` - Smart accessors for safe value access

Thank you for contributing to M28!

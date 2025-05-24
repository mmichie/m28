# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## M28 Language Syntax Rules

**CRITICAL**: M28 is a Lispy-Pythonic language with specific syntax rules:
1. **Comments**: ALWAYS use `#` for comments (NEVER use `;`)
2. **Variables**: ALWAYS use `=` for variable assignment (NEVER use `def` for variables)
3. **Functions**: ONLY use `def` for function definitions
4. **All operations use prefix notation**: `(+ 1 2)` not `1 + 2`

### Example
```lisp
# This is a comment (always use #)
(= x 10)              # Variable assignment (always use =)
(def add (a b)        # Function definition (def only for functions)
  (+ a b))
```

## Build/Test Commands
- Build: `make build`
- Run all tests: `make test`
- Run REPL: `make run`
- Run single test: `./bin/m28 tests/[test-file.m28]`
- Clean: `make clean`
- Install deps: `make deps`

## Code Style Guidelines
- **Go Style**: Follow standard Go conventions
  - Use camelCase for function names
  - Group imports properly (stdlib first, then third-party)
  - Return errors as second return value
  - Descriptive error messages
- **Types**: Use types defined in core/value.go and core/types.go
- **Validation**: Validate arguments before processing
- **Naming**:
  - Packages: lowercase (builtin, core, special_forms)
  - Functions: camelCase
- **Error Handling**: Return errors rather than panicking when possible
- **Comments**: Document complex logic or non-obvious behaviors

## M28 Language Features
- **S-expressions**: Everything is an expression in prefix notation
- **Python semantics**: Python keywords (if, elif, else, for, in, while, try, except, etc.)
- **Python data structures**: `[1, 2, 3]` for lists, `{"key": "value"}` for dicts, `{1, 2, 3}` for sets
- **Dot notation**: `obj.method()` and `dict.key` access
- **File I/O**: Python-style with context managers `(with (open "file.txt") as f ...)`

## When Writing M28 Code
1. Always use `#` for comments
2. Always use `=` for variables
3. Only use `def` for functions
4. Use Python idioms and naming conventions
5. Prefer Python built-in names: `len`, `range`, `map`, `filter`, etc.

## Testing
When writing tests:
- Use `.m28` extension
- Start with `#` comments explaining the test
- Use `(assert condition "message")` for test assertions
- Group related tests logically
- Test both success and error cases

When modifying the codebase, maintain consistency with existing patterns and style. Look for opportunities to consolidate similar code or refactor for clarity. Always run tests after making changes to ensure nothing is broken.
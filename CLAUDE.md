# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## M28 Language Syntax Rules

**CRITICAL**: M28 is a Lispy-Pythonic language with specific syntax rules:
1. **Comments**: ALWAYS use `#` for comments (NEVER use `;`)
2. **Variables**: ALWAYS use `=` for variable assignment (NEVER use `def` for variables)
3. **Functions**: ONLY use `def` for function definitions
4. **S-expressions AND Pythonic calls**: Both styles work!
   - S-expression: `(print "hello")`
   - Pythonic: `print("hello")`
5. **Operations**: Infix operators work inside expressions: `x * 2`, `i + 1`

### Syntax Examples
```lisp
# Comments (always use #)

# Variable assignment - BOTH styles work!
(= x 10)              # S-expression style
x = 10                # Pythonic style (NEW!)

# Function definition - BOTH styles work!
(def add (a b)          # S-expression style
  (+ a b))

def subtract(a, b): (- a b)  # Pythonic style (NEW!)

# Function calls - BOTH styles work!
(print "hello")        # S-expression style
print("hello")         # Pythonic style
print(len([1, 2, 3]))  # Perfect for shell one-liners!

# Mix and match
print((+ 1 2 3))       # Pythonic call with S-expression arg
(print (len [1, 2, 3])) # S-expression call with list

# Real-world example
data = [1, 2, 3, 4, 5]
total = sum(data)
print(f"Total: {total}")
```

### Pythonic Syntax Sugar

**Function Calls**: `name(args)` desugars to `(name args)`
- `print("hello")` → `(print "hello")`
- `len([1, 2, 3])` → `(len [1, 2, 3])`
- No whitespace rule: `name(args)` is sugar, `name (args)` is S-expression

**Assignment**: `x = value` desugars to `(= x value)`
- `x = 10` → `(= x 10)`
- `data = [1, 2, 3]` → `(= data [1, 2, 3])`
- Works anywhere: top-level, inside functions, in expressions

**Function Definition**: `def name(params): expr` desugars to `(def name (params) expr)`
- `def double(x): (* x 2)` → `(def double (x) (* x 2))`
- `def greet(name): f"Hello, {name}!"` → `(def greet (name) f"Hello, {name}!")`
- No whitespace between name and `(` distinguishes from S-expression style

**Shell scripting examples**:
```bash
m28 -e 'x = 10 print(x)'
m28 -e 'data = [1,2,3] print(sum(data))'
m28 -e 'print(sum([x*x for x in range(10)]))'
m28 -e 'def double(x): x*2 print(double(5))'
```

**Mix styles freely**: Use whichever makes your code clearer!

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
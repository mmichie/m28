# M28 Macro System

## Overview

M28 implements a Pythonic macro system using the `@macro` decorator syntax. This provides metaprogramming capabilities while maintaining Python's philosophy of "Explicit is better than implicit."

## Basic Usage

### Defining a Macro

```lisp
(@macro
 (def my_macro (arg1 arg2)
   # Macro body - receives unevaluated arguments
   arg1))
```

### Key Differences from Functions

| Aspect | Functions | Macros |
|--------|-----------|--------|
| Arguments | Evaluated before call | Unevaluated (raw AST) |
| Execution | Runtime | Compile-time expansion |
| Return value | Data value | Code (AST) that gets evaluated |
| Marking | No special marker | `__macro__` attribute = `true` |

## How It Works

### 1. Macro Definition

When you write:

```lisp
(@macro
 (def identity (x)
   x))
```

The decorator:
1. Evaluates the `def` form, creating a function
2. Sets the `__macro__` attribute to `true` on that function
3. Returns the modified function
4. The function is defined in the current scope

### 2. Macro Expansion

When you call a macro:

```lisp
(= result (identity (+ 1 2)))
```

The evaluator:
1. Checks if `identity` has `__macro__ = true`
2. Calls the macro with **unevaluated** arguments: `[(+ 1 2)]`
3. The macro returns an AST (in this case, `(+ 1 2)`)
4. The returned AST is evaluated: `(+ 1 2)` → `3`
5. The final result (`3`) is assigned to `result`

## Implementation Details

### Files Modified

- **`eval/decorator.go`** - Decorator form handler and macro expansion logic
  - `decoratorForm()` - Applies decorators to definitions
  - `isDecoratorForm()` - Checks if a form starts with `@`
  - `isMacroCall()` - Checks if a function has `__macro__` attribute
  - `evalMacroCall()` - Expands and evaluates macros

- **`eval/evaluator.go`** - Added decorator and macro checks to main evaluation loop
  - Decorators checked before special forms
  - Macros checked after special forms, before function calls

- **`builtin/decorators.go`** - Built-in `macro` decorator
  - Sets `__macro__` attribute on functions

- **`builtin/registry.go`** - Registers decorator built-ins

### Evaluation Order

The evaluator checks forms in this order:

1. Empty list → returns empty list
2. Decorator form (`@decorator ...`) → apply decorator
3. Special form (`if`, `def`, etc.) → special form handler
4. Macro call (function with `__macro__` attribute) → macro expansion
5. Regular function call → evaluate and call

## Current Capabilities

✅ **Working:**
- Decorator syntax `(@macro (def name (params) body))`
- Unevaluated argument passing to macros
- Two-phase evaluation (macro expansion → evaluation)
- Multiple macro definitions
- `__macro__` attribute checking
- **Quasiquote/unquote** - Easy code generation with substitution
- **Built-in macros**: `unless`, `when`, `->`, `->>`

## Example: Identity Macro

```lisp
# Define macro
(@macro
 (def identity (x)
   x))

# Call macro
(= result (identity (+ 1 2)))

# Flow:
# 1. identity is called with unevaluated (+ 1 2)
# 2. Macro returns (+ 1 2)
# 3. (+ 1 2) is evaluated to 3
# 4. result = 3
```

## Example: Constant Macro

```lisp
# Macro that always returns 42
(@macro
 (def const_42 (_)
   42))

(= result (const_42 anything))
# result = 42
```

## Quasiquote System

M28 now has full quasiquote support for easy code generation in macros.

### Syntax

- `(quasiquote expr)` - Like quote, but allows selective evaluation
- `(unquote expr)` - Evaluate `expr` within quasiquote
- `(unquote-splicing expr)` - Evaluate and splice list elements

### Example: unless Macro with Quasiquote

```lisp
(@macro
 (def unless (condition body)
   (quasiquote (if (not (unquote condition)) (unquote body) nil))))

(unless (> x 5) (print "x is small"))
```

This expands to: `(if (not (> x 5)) (print "x is small") nil)`

### Unquote-Splicing Example

```lisp
(= items [1 2 3])
(= expr (quasiquote (list a (unquote-splicing items) b)))
# Result: [list a 1 2 3 b]
```

## Built-in Macros

M28 includes several useful built-in macros:

### `unless` - Conditional Execution

Execute body if condition is false:

```lisp
(unless (> x 5)
  (print "x is not greater than 5"))
```

### `when` - Conditional Execution

Execute body if condition is true:

```lisp
(when (> x 5)
  (print "x is greater than 5"))
```

### `->` (thread-first) - Data Pipeline

Thread value through forms, inserting as first argument:

```lisp
(-> 5
    (+ 10)     # => (+ 5 10) = 15
    (* 2)      # => (* 15 2) = 30
    (- 5))     # => (- 30 5) = 25
```

### `->>` (thread-last) - Data Pipeline

Thread value through forms, inserting as last argument:

```lisp
(->> [1 2 3 4 5]
     (map (lambda (x) (* x 2)))           # => [2 4 6 8 10]
     (filter (lambda (x) (> x 4)))        # => [6 8 10]
     list)
```

## Limitations and Future Work

### Current Limitations

1. **S-String Integration**: While both s-strings and quasiquote work, quasiquote is recommended for macros
   - Quasiquote is specifically designed for code generation
   - S-strings are better for template-based string generation

2. **No Hygiene**: Macros can capture variables from calling context (not hygienic)

### Planned Enhancements

- [x] Quasiquote and unquote operators for easy AST construction
- [x] Unquote-splicing for list splicing in quasiquote
- [x] Built-in utility macros (`unless`, `when`, `->`, `->>`)
- [ ] Reader macro syntax: `` ` `` for quasiquote, `,` for unquote, `,@` for unquote-splicing
- [ ] Hygienic macro support (automatic variable capture prevention)
- [ ] Macro debugging tools (macro expansion inspection)
- [ ] Better error messages for macro expansion errors

## Comparison with Other Languages

### Lisp/Scheme
```lisp
; Lisp
(defmacro unless (condition body)
  `(if (not ,condition) ,body nil))
```

### M28
```lisp
# M28 (with quasiquote)
(@macro
 (def unless (condition body)
   (quasiquote (if (not (unquote condition)) (unquote body) nil))))

# M28 (future - with reader macro syntax)
(@macro
 (def unless (condition body)
   `(if (not ,condition) ,body nil)))
```

### Python
```python
# Python doesn't have macros - uses decorators for different purpose
@decorator
def function():
    pass
```

## Best Practices

1. **Use Macros Sparingly**: Only when you need compile-time code generation
2. **Prefer Functions**: If runtime execution works, use functions instead
3. **Document Macro Behavior**: Macros can be confusing - add clear documentation
4. **Test Thoroughly**: Macro bugs can be subtle - test edge cases

## Technical Notes

### Why Macros Receive Unevaluated Arguments

This is the key feature of macros. It allows macros to:
- Inspect the structure of their arguments
- Choose whether/when to evaluate arguments
- Transform code before execution
- Implement new control flow constructs

Example: A `when` macro can choose not to evaluate its body if the condition is false:

```lisp
(@macro
 (def when (condition body)
   # If condition is false, we never evaluate body
   [if condition body nil]))
```

If `when` were a function, `body` would always be evaluated before the function is called, defeating the purpose.

## See Also

- [S-Strings Documentation](SSTRINGS.md) - Template-based code generation
- [ROADMAP.md](../ROADMAP.md) - Future macro system enhancements
- [Parser Documentation](PARSER.md) - How `@` symbols are parsed

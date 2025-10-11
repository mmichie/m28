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

## Limitations and Future Work

### Current Limitations

1. **No Quasiquote/Unquote**: Cannot easily construct AST with substitutions
   - Cannot write: `(@macro (def unless (cond body) \`(if (not ,cond) ,body nil)))`
   - Need proper quasiquote mechanism

2. **S-String Integration**: While s-strings work for code generation, using them in macros requires careful handling
   - Value interpolation converts AST to strings, which may not always work as expected
   - Need better integration between macros and s-strings

3. **No Hygiene**: Macros can capture variables from calling context (not hygienic)

### Planned Enhancements

- [ ] Quasiquote (`` ` ``) and unquote (`,`) operators for easy AST construction
- [ ] Unquote-splicing (`,@`) for list splicing in quasiquote
- [ ] Hygienic macro support (automatic variable capture prevention)
- [ ] Built-in utility macros (`unless`, `when`, `->`, `->>`)
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
# M28 (current - manual AST construction)
(@macro
 (def unless (condition body)
   [if [not condition] body nil]))

# M28 (future - with quasiquote)
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

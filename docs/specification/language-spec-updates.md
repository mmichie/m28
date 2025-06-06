# Language Specification Updates for v0.1.0

## Summary
The language specification is largely accurate but needs minor updates to reflect the current implementation.

## Corrections Needed

### 1. Function Definition Syntax (Line 53)
Current spec shows:
```lisp
(def (function-name param1 param2 ...) body...)
```

Actual syntax:
```lisp
(def function-name (param1 param2 ...) body...)
```

### 2. List Comprehension Support
The specification mentions list comprehensions are planned but they are actually implemented:
```lisp
(= squares [(* x x) for x in (range 10)])
```

### 3. Import Syntax (Section 4.2.2)
Current spec shows both quoted and unquoted imports. The actual syntax requires quotes:
```lisp
(import "module_name")
(import "module_name" as alias)
```

### 4. Class Definition Syntax
Classes require an empty parent list if no inheritance:
```lisp
(class ClassName ()  # Empty parent list required
  ...)
```

### 5. List Methods Return New Lists
The spec should clarify that list methods like append, sort, etc. return new lists rather than modifying in place.

### 6. Features Actually Implemented
- For loops: ✅ Working
- List comprehensions: ✅ Working (with prefix notation inside)
- Specific exception catching: ✅ Working
- Generators: ✅ Working
- Async/await: ✅ Working
- Context managers: ✅ Working

### 7. Module Exports
Use `__exports__` not `__all__`:
```lisp
(= __exports__ (list "public_function" "public_var"))
```

## Notes
- The specification is very comprehensive and well-structured
- Most content is accurate for v0.1.0
- The main issues are minor syntax differences
- Examples should be tested against actual implementation
# Tuple Implementation for M28

This document describes the implementation of Python-style tuples in the M28 language.

## Overview

Tuples have been implemented as immutable sequences similar to Python's tuples, with the following features:
- Python-style syntax compatibility: `(1, 2, 3)`
- Single-element tuples with trailing comma: `(1,)`
- Empty tuples: `()`
- Tuple operations (comparison, equality, length, access)
- Nesting with other data types

## Implementation Details

### Type Definition

Tuples are defined as a slice of `LispValue` in `core/types.go`:

```go
// LispTuple represents a Python-style tuple (immutable sequence)
type LispTuple []LispValue
```

### Syntax Parsing

The parser identifies tuples by looking for commas within parentheses. This allows tuples to coexist with the Lisp-style function call syntax:

- `(func arg1 arg2)` - Function call (no commas)
- `(1, 2, 3)` - Tuple (contains commas)

Single-element tuples require a trailing comma to distinguish them from parenthesized expressions:
- `(1)` - Parenthesized expression, not a tuple
- `(1,)` - Single-element tuple

### String Representation

Tuples are represented as comma-separated values within parentheses, similar to Python:
- `(1, 2, 3)`
- Single-element tuples include a trailing comma: `(1,)`

### Supported Operations

The following operations are supported for tuples:

1. **Creation**:
   ```
   (= my_tuple (1, 2, 3))
   (= nested_tuple (1, (2, 3), [4, 5]))
   ```

2. **Element Access**:
   ```
   (nth my_tuple 0)  ; Returns 1
   ```

3. **Length**:
   ```
   (len my_tuple)  ; Returns 3
   ```

4. **Equality**:
   ```
   (eq? tuple1 tuple2)  ; Checks if tuples are equal
   ```

5. **Comparison**:
   ```
   (< tuple1 tuple2)  ; Lexicographical comparison
   ```

## Examples

The `examples/tuple_operations.m28` file demonstrates all supported tuple operations.

## Compatibility

This implementation maintains compatibility with existing Lisp syntax by using commas as a distinguishing feature for tuples, allowing both function calls and tuples to use parentheses.
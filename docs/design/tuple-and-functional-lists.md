# Tuples and Functional Lists Design Document

This document outlines the design for enhancing M28's tuple implementation and implementing functional list operations with improved type inference.

## 1. Background

M28 combines Lisp-style syntax with Python-like semantics, making it a unique language in the landscape. It currently supports tuples as immutable sequences, but there are opportunities to enhance the implementation and ensure clearer distinctions between tuples and lists.

## 2. Tuple Design Enhancements

### 2.1 Syntax Clarification

**Current Syntax:**
- Syntax for empty tuples: `()`
- Syntax for single-element tuples: `(1,)`
- Syntax for multi-element tuples: `(1, 2, 3)`

**Implementation Changes:**
1. Parser now properly recognizes empty parentheses `()` as tuples rather than empty lists
2. Ensure comma is used to distinguish tuples from function calls or grouped expressions
3. Maintain visual syntax compatibility with Python for easier adoption

### 2.2 Type Inference Improvements

**Current Implementation:**
- Type checking functions like `type()` and `isinstance()` correctly identify tuples
- Equality and comparison operations work correctly for tuples

**Implementation Enhancements:**
1. Add type annotations support for tuples in function signatures
2. Improve error messages involving tuples to be more descriptive
3. Add static type checking capabilities (optional)

### 2.3 Tuple Operations

**Core Tuple Features:**
1. **Immutability**: Tuples remain immutable once created
2. **Indexing**: Access elements by position with `(get tuple index)` or `(nth tuple index)`
3. **Length**: Get tuple size with `(len tuple)`
4. **Type checking**: Use `(type tuple)` or `(isinstance tuple "tuple")`

**Extended Operations:**
1. **Tuple concatenation**: Create a new tuple by joining two tuples
2. **Tuple repetition**: Create a new tuple by repeating elements
3. **Tuple unpacking**: Destructure tuples into individual variables
4. **Conversion**: Support conversion between tuples and other sequence types

## 3. Functional List Operations

### 3.1 Functional Operation Principles

**Core Principles:**
1. **Immutable operations**: List operations return new lists without modifying originals
2. **Chaining**: Operations can be chained for complex transformations
3. **Lazy evaluation**: Consider adding lazy evaluation for efficiency

### 3.2 Implementation Approach

**New List Operations:**
1. **`list.map(func)`**: Returns a new list with function applied to each element
2. **`list.filter(pred)`**: Returns a new list with elements matching predicate
3. **`list.reduce(func [initial])`**: Reduce list to single value by applying function
4. **`list.take(n)`**: Returns a new list with first n elements
5. **`list.drop(n)`**: Returns a new list with elements after first n
6. **`list.concat(other)`**: Returns a new list combining elements

**Examples:**
```lisp
; Immutable map (returns new list)
(= numbers (list 1 2 3 4))
(= doubled (numbers.map (lambda (x) (* x 2))))  ; doubled = (2 4 6 8), numbers = (1 2 3 4)

; Chaining operations
(= result (numbers.filter (lambda (x) (> x 2))
                 .map (lambda (x) (* x 3))))  ; result = (9 12)
```

### 3.3 Type Inference for Functions

**Type Annotation Syntax:**
```lisp
; Function with type annotations
(def (add [a int] [b int]) -> int
  (+ a b))

; Inferred return types
(def (process items) ; items: list[int] -> list[int]
  (items.map (lambda (x) (+ x 1))))
```

**Type Inference Rules:**
1. Infer parameter types based on usage
2. Infer return types based on function body
3. Propagate types through function chains
4. Provide feedback on type mismatches

## 4. Implementation Plan

### 4.1 Changes to Core Files

1. **Tuple Type Enhancement**
   - ✅ Fix parser.go to recognize empty `()` as tuples
   - Add concatenation methods in tuple_adapter.go
   - Enhance operator overloading for tuples

2. **Functional List Operations**
   - Modify list_adapter.go to return new lists for operations
   - Add new functional methods (map, filter, reduce, etc.)
   - Ensure original lists remain unchanged

3. **Type Inference System**
   - Create type annotation syntax in parser
   - Implement type checking in evaluator
   - Add optional static analysis capabilities

### 4.2 Backwards Compatibility

**Compatibility Considerations:**
1. Existing code using `()` as empty lists may break - provide migration guide
2. Ensure existing list methods continue to work with new implementation
3. Make type annotations optional to avoid breaking existing code

### 4.3 Performance Considerations

**Optimization Opportunities:**
1. Use structural sharing for efficient immutable operations
2. Consider lazy evaluation for chained operations
3. Optimize memory usage for large collections
4. Cache results of expensive operations where appropriate

## 5. Testing Strategy

**Test Coverage:**
1. **Unit tests** for tuple operations
2. **Integration tests** for complex functional chains
3. **Property-based tests** for functional correctness
4. **Performance benchmarks** for efficiency comparison

## 6. Documentation

**Documentation Updates:**
1. Update language guide with tuple enhancements
2. Add section on functional list operations
3. Provide examples of type annotations
4. Create migration guide for any breaking changes

## 7. Future Considerations

**Potential Enhancements:**
1. **Lazy sequences**: Add support for infinite or lazily evaluated sequences
2. **Pattern matching**: Enhance tuple support in pattern matching
3. **Parallel operations**: Add support for parallelized functional operations
4. **More collection types**: Add immutable vectors, maps, etc.

## Conclusion

These enhancements will strengthen M28's tuple implementation and add powerful functional list operations with improved type inference. The changes maintain compatibility with the language's Lisp syntax while bringing more Python-like semantics and modern functional programming features.
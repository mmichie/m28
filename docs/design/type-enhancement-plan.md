# Type Enhancement and Functional List Operations Plan

This document outlines a plan for enhancing the type system and implementing functional list operations in M28. 

## 1. Goals

1. Better distinguish between lists and tuples
2. Implement functional-style list operations
3. Enhance type inference and checking
4. Maintain compatibility with existing code

## 2. Tuple Enhancements

### Parser Changes
- Fix empty tuple identification
- Properly handle tuple literals with commas
- Ensure consistent type identification

### Core Implementation
- Add tuple operations:
  - Concatenation 
  - Slicing
  - Functional operations
- Ensure immutability is enforced

## 3. Functional List Operations

### Core Design
- List operations should return new lists instead of modifying originals
- Add higher-order operations (map, filter, reduce)
- Support method chaining for operations

### Key Operations
- `append` - Add item to end, return new list
- `extend` - Combine with other sequence, return new list
- `map` - Apply function to each element
- `filter` - Keep elements matching predicate
- `reduce` - Fold operation with accumulator
- `take`/`drop` - Take first N / drop first N elements
- `slice` - Extract a portion of the list
- `concat` - Join lists together
- `reverse` - Reverse element order

## 4. Type Inference Improvements

### Type System Design
- Add optional type annotations
- Infer parameter and return types
- Build type constraint system
- Provide descriptive type errors

### Core Typing Rules
- Types flow through expressions
- Higher-order functions maintain type signatures
- Collections constrain their element types
- Chained operations preserve type information

## 5. Implementation Strategy

Phase 1: Fix tuple identification in parser
- Modify parser.go to properly identify empty tuples
- Fix type function to correctly report tuple types

Phase 2: Functional list operations
- Update list_adapter.go
- Make operations return new lists
- Add higher-order functions

Phase 3: Type system enhancements
- Add type annotation syntax
- Implement type inference
- Add type checking

## 6. Testing Strategy

- Unit tests for operations
- Integration tests for chained operations
- Type annotation validity tests
- Backward compatibility tests

This implementation will enhance M28 with more Pythonic tuple behavior and functional programming capabilities while laying the groundwork for improved type safety.
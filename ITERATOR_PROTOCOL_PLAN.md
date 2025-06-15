# Iterator Protocol Implementation Plan

## Overview
Implement Python-style iterator protocol (`__iter__` and `__next__`) for M28, enabling custom iteration support for user-defined types and better integration with for loops.

## Current State
- Basic Iterator and Iterable interfaces exist in core/value.go
- ForForm in eval/loop.go handles iteration for built-in types (List, Tuple, String)
- ForForm checks for core.Iterable interface but doesn't use dunder methods
- No StopIteration exception exists
- No built-in iter() or next() functions

## Implementation Tasks

### 1. Core Iterator Support
- [ ] Create StopIteration exception type
- [ ] Add iterator protocol to core/protocols package
- [ ] Create protocol adapters for built-in types (List, Dict, Tuple, String, Range)
- [ ] Implement GetIterableOps similar to GetIndexableOps

### 2. Dunder Method Support
- [ ] Add CallIter and CallNext to common/types/dunder.go
- [ ] Update ForForm to check __iter__ before using core.Iterable
- [ ] Support __next__ method with StopIteration handling

### 3. Built-in Functions
- [ ] Implement iter() builtin function
- [ ] Implement next() builtin function with optional default
- [ ] Update existing iterables (range, generator) to support protocol

### 4. Iterator Adapters
- [ ] ListIterator - iterates over list elements
- [ ] DictIterator - iterates over dict keys (Python behavior)
- [ ] TupleIterator - iterates over tuple elements
- [ ] StringIterator - iterates over string characters
- [ ] RangeIterator - iterates over range values

### 5. Custom Iterator Support
- [ ] Allow classes to define __iter__ and __next__
- [ ] Support both iterator-returning __iter__ and self-returning __iter__
- [ ] Handle StopIteration in for loops

### 6. Testing
- [ ] Test built-in type iteration
- [ ] Test custom iterator classes
- [ ] Test StopIteration handling
- [ ] Test iter() and next() builtins
- [ ] Test edge cases (empty iterators, infinite iterators)

## Example Usage

```python
# Custom iterator class
(class Counter
  (def __init__ (self max)
    (= self.max max)
    (= self.count 0)
    self)
  
  (def __iter__ (self)
    self)  # Return self as iterator
  
  (def __next__ (self)
    (if (< self.count self.max)
      (do
        (= self.count (+ self.count 1))
        self.count)
      (raise StopIteration))))

# Usage
(for n (Counter 5)
  (print n))  # Prints 1, 2, 3, 4, 5

# With iter() and next()
(= it (iter [1, 2, 3]))
(print (next it))      # 1
(print (next it))      # 2
(print (next it))      # 3
(print (next it))      # StopIteration
(print (next it 42))   # 42 (default)
```

## Benefits
- Enables custom iteration patterns
- Memory-efficient iteration (generators, large datasets)
- Consistent with Python's iteration model
- Foundation for generator expressions and comprehensions
- Better integration with functional programming patterns
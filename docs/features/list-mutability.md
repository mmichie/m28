# List Mutability in M28

M28 takes a hybrid approach to list mutability, offering both mutable and immutable operations. This gives developers flexibility to choose the style that best fits their use case.

## Overview

Lists in M28 support two types of operations:

1. **Mutable operations** - Modify the list in-place
2. **Immutable operations** - Return a new list, leaving the original unchanged

## Mutable Operations

### Index Assignment
Direct modification of list elements using index assignment:

```m28
(= lst [1, 2, 3])
(= lst[0] 10)        # lst is now [10, 2, 3]
(= lst[1] 20)        # lst is now [10, 20, 3]

# Negative indices work too
(= lst[-1] 30)       # lst is now [10, 20, 30]
```

### Slice Assignment
Modify multiple elements at once:

```m28
(= lst [1, 2, 3, 4, 5])
(= lst[1:3] [20, 30])  # lst is now [1, 20, 30, 4, 5]
```

## Immutable Operations

Most list methods return a new list without modifying the original:

### Adding Elements
```m28
(= lst [1, 2, 3])

# append - returns new list with element added
(= lst2 (lst.append 4))      # lst: [1, 2, 3], lst2: [1, 2, 3, 4]

# extend - returns new list with elements from another list
(= lst3 (lst.extend [4, 5])) # lst: [1, 2, 3], lst3: [1, 2, 3, 4, 5]

# insert - returns new list with element inserted at index
(= lst4 (lst.insert 1 "x"))  # lst: [1, 2, 3], lst4: [1, "x", 2, 3]
```

### Removing Elements
```m28
(= lst [1, 2, 3, 2, 4])

# remove - returns new list with first occurrence removed
(= lst2 (lst.remove 2))      # lst: [1, 2, 3, 2, 4], lst2: [1, 3, 2, 4]

# pop - returns the removed element (not a new list!)
(= val (lst.pop))            # val: 4, lst unchanged
(= val (lst.pop 0))          # val: 1, lst unchanged

# clear - returns empty list
(= lst3 (lst.clear))         # lst: [1, 2, 3, 2, 4], lst3: []
```

### Transforming Lists
```m28
(= lst [3, 1, 4, 1, 5])

# sort - returns sorted list
(= sorted (lst.sort))        # lst: [3, 1, 4, 1, 5], sorted: [1, 1, 3, 4, 5]

# reverse - returns reversed list
(= rev (lst.reverse))        # lst: [3, 1, 4, 1, 5], rev: [5, 1, 4, 1, 3]

# map - returns new list with function applied
(= doubled (lst.map (lambda (x) (* x 2))))  # [6, 2, 8, 2, 10]

# filter - returns new list with filtered elements
(= evens (lst.filter (lambda (x) (== (% x 2) 0))))  # [4]
```

## Common Patterns

### Pattern 1: Update and Reassign
When you want to "modify" a list using immutable operations:

```m28
(= lst [1, 2, 3])
(= lst (lst.append 4))       # Reassign to same variable
(= lst (lst.append 5))       # lst is now [1, 2, 3, 4, 5]
```

### Pattern 2: Building Lists
When constructing a list step by step:

```m28
(def build-list (n)
  (= result [])
  (for i (range n)
    (= result (result.append (* i i))))
  result)

(= squares (build-list 5))   # [0, 1, 4, 9, 16]
```

### Pattern 3: Functional Pipeline
Chain operations for functional programming style:

```m28
(= numbers [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

(= result 
  (numbers
    .filter (lambda (x) (> x 3))
    .map (lambda (x) (* x 2))
    .filter (lambda (x) (< x 15))))
# result: [8, 10, 12, 14]
```

### Pattern 4: When to Use Mutation
Direct mutation is appropriate for:

```m28
# 1. Algorithms that naturally use mutation
(def bubble-sort (lst)
  (= n (len lst))
  (for i (range n)
    (for j (range (- n i 1))
      (if (> lst[j] lst[(+ j 1)])
        (begin
          (= temp lst[j])
          (= lst[j] lst[(+ j 1)])
          (= lst[(+ j 1)] temp)))))
  lst)

# 2. Performance-critical code
(def process-large-data (data)
  # Modify in-place to avoid copying
  (for i (range (len data))
    (= data[i] (transform data[i])))
  data)
```

## Best Practices

1. **Be Consistent**: Choose one style (mutable or immutable) and stick with it within a module or project.

2. **Default to Immutable**: Unless you have a specific need for mutation, prefer immutable operations:
   - Easier to reason about
   - No aliasing bugs
   - Thread-safe
   - More functional

3. **Document Mutation**: When a function mutates its arguments, document this clearly:
   ```m28
   (def sort-in-place! (lst)
     "Sorts the list in-place, modifying the original list."
     # ... implementation ...
   )
   ```

4. **Use Descriptive Names**: Consider naming conventions to indicate mutation:
   ```m28
   (def append (lst item)       # Returns new list
     (lst.append item))
   
   (def append! (lst item)      # Modifies in-place (by convention)
     (= lst[(len lst)] item)
     lst)
   ```

## Comparison with Python

Python lists are always mutable:
```python
lst = [1, 2, 3]
lst.append(4)    # Modifies lst to [1, 2, 3, 4]
```

M28's approach:
```m28
(= lst [1, 2, 3])
(lst.append 4)         # Returns [1, 2, 3, 4], lst unchanged
(= lst (lst.append 4)) # Reassign to update lst
```

## Performance Considerations

- **Mutable operations**: O(1) for index assignment
- **Immutable operations**: O(n) for most operations due to copying
- **Future optimization**: Structural sharing will improve immutable performance

## Summary

M28's hybrid approach provides flexibility:
- Use **index assignment** when you need mutation
- Use **list methods** for functional, immutable style
- Choose based on your problem domain and coding style

This design supports both imperative and functional programming paradigms, making M28 versatile for different types of applications.
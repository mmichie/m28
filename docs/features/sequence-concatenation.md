# Sequence Concatenation in M28

The M28 language supports concatenation of sequence types through the `+` operator, similar to Python.
This document outlines how concatenation works with different sequence types.

## Supported Sequence Types

The `+` operator supports concatenation of:

1. **Lists** - Created with the syntax `[1 2 3]`
2. **Tuples** - Created with the syntax `(1, 2, 3)`
3. **Mixed sequences** - Combining lists and tuples

## Behavior

### List Concatenation

When both operands are lists, the `+` operator creates a new list containing all elements:

```lisp
(= list1 [1 2 3])
(= list2 [4 5 6])
(= combined (+ list1 list2))  # Results in [1, 2, 3, 4, 5, 6]
```

### Tuple Concatenation

When both operands are tuples, the `+` operator creates a new tuple containing all elements:

```lisp
(= tuple1 (1, 2, 3))
(= tuple2 (4, 5, 6))
(= combined (+ tuple1 tuple2))  # Results in (1, 2, 3, 4, 5, 6)
```

### Mixed Sequences

When concatenating lists and tuples, the type of the first operand determines the result type:

```lisp
(= list1 [1 2 3])
(= tuple1 (4, 5, 6))

# List + Tuple = List
(= list_result (+ list1 tuple1))  # Results in [1, 2, 3, 4, 5, 6]

# Tuple + List = Tuple
(= tuple_result (+ tuple1 list1))  # Results in (1, 2, 3, 4, 5, 6)
```

### Multiple Concatenation

The `+` operator can combine more than two sequences:

```lisp
(= result (+ [1 2] [3 4] [5 6]))  # Results in [1, 2, 3, 4, 5, 6]
(= result (+ (1, 2) (3, 4) (5, 6)))  # Results in (1, 2, 3, 4, 5, 6)
```

## Key Differences from List/Tuple Methods

1. **New Objects**: The `+` operator always creates a new sequence rather than modifying existing ones.
2. **Immutability**: Even for mutable lists, the `+` operator provides a functional approach by creating new objects.
3. **Type Preservation**: The result type matches the type of the first operand in the expression.

## Error Handling

The `+` operator will raise errors when:

1. Attempting to concatenate sequences with non-sequences
2. Trying to use incompatible types

For example:

```lisp
(+ [1 2 3] 4)  # Error: cannot mix lists and non-lists
(+ (1, 2, 3) "string")  # Error: cannot mix tuples and non-sequences
```

## Example Usage

```lisp
# Append elements to a list
(= my_list [1 2 3])
(= updated_list (+ my_list [4]))  # [1, 2, 3, 4]

# Combine multiple lists
(= combined (+ [0] my_list [4 5]))  # [0, 1, 2, 3, 4, 5]

# Append element to a tuple
(= my_tuple (1, 2, 3))
(= updated_tuple (+ my_tuple (4,)))  # (1, 2, 3, 4)
```
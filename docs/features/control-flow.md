# Control Flow in M28

This document covers the control flow constructs in M28, including conditionals and loops.

## Contents
1. [Conditionals](#conditionals)
   - [If Expressions](#if-expressions)
   - [Nested Conditionals](#nested-conditionals)
   - [Boolean Operations](#boolean-operations)
2. [Loops](#loops)
   - [For Loops](#for-loops)
   - [While Loops](#while-loops)
   - [Loop Control Statements](#loop-control-statements)
3. [Exception Control Flow](#exception-control-flow)
4. [Examples](#examples)

## Conditionals

### If Expressions

The basic conditional structure in M28 is the `if` expression:

```lisp
(if condition
    then-expression
    else-expression)
```

Examples:

```lisp
# Simple if expression
(if (> x 0)
    (print "Positive")
    (print "Non-positive"))

# If expression returning a value
(= abs-x (if (< x 0) (- 0 x) x))
```

If the condition evaluates to any truthy value (anything except `False`, `None`, `0`, empty lists, and empty strings), the `then-expression` is evaluated; otherwise, the `else-expression` is evaluated.

### Nested Conditionals

For if-elif-else patterns, use nested if expressions:

```lisp
(if condition1
    result1
    (if condition2
        result2
        (if condition3
            result3
            default-result)))
```

Example:

```lisp
# Grading system
(if (>= score 90)
    "A"
    (if (>= score 80)
        "B"
        (if (>= score 70)
            "C"
            (if (>= score 60)
                "D"
                "F"))))
```

### Boolean Operations

Boolean operations can be combined with conditionals:

```lisp
# Logical AND
(if (and condition1 condition2)
    (print "Both conditions are true"))

# Logical OR
(if (or condition1 condition2)
    (print "At least one condition is true"))

# Logical NOT
(if (not condition)
    (print "Condition is false"))
```

## Loops

### For Loops

The `for` loop iterates over each item in a collection:

```lisp
(for item collection
    body-expression1
    body-expression2
    ...)
```

Examples:

```lisp
# Loop through a list
(for x (list 1 2 3 4 5)
    (print x))

# Use range to generate a sequence
(for i (range 5)
    (print (* i i)))
```

### While Loops

The `while` loop repeats while a condition is true:

```lisp
(while condition
    body-expression1
    body-expression2
    ...)
```

Examples:

```lisp
# Count from 1 to 5
(= i 1)
(while (<= i 5)
    (print i)
    (= i (+ i 1)))

# Find the first power of 2 greater than 1000
(= n 1)
(while (< n 1000)
    (= n (* n 2)))
(print n)  # Outputs: 1024
```

### Loop Control Statements

M28 supports `break` and `continue` for loop control (if implemented):

```lisp
# Skip even numbers and stop at 8
(for i (range 10)
    (if (== (% i 2) 0)
        (continue))  # Skip to next iteration
    (if (== i 8)
        (break))     # Exit the loop
    (print i))
```

## Exception Control Flow

Exception handling can also affect control flow:

```lisp
(try
    (risky-operation)
    (except Exception
        (print "An error occurred"))
    (finally
        (cleanup-operation)))
```

## Examples

### Example 1: Finding Prime Numbers

```lisp
(def (is-prime n)
  (if (<= n 1)
      False
      (begin
        (= i 2)
        (= is-prime True)
        (while (and is-prime (<= (* i i) n))
          (if (== (% n i) 0)
              (= is-prime False)
              (= i (+ i 1))))
        is-prime)))

# Print prime numbers up to 20
(for n (range 2 21)
  (if (is-prime n)
      (print n)))
```

### Example 2: FizzBuzz

```lisp
# FizzBuzz implementation
(for i (range 1 101)
  (if (== (% i 15) 0)
      (print "FizzBuzz")
      (if (== (% i 3) 0)
          (print "Fizz")
          (if (== (% i 5) 0)
              (print "Buzz")
              (print i)))))
```

### Example 3: Processing with Early Exit

```lisp
(def (find-first-match items predicate)
  (for item items
    (if (predicate item)
        (return item)))
  None)  # Return None if no match found

# Usage
(= numbers (list 1 3 5 7 9 10 11))
(= result (find-first-match numbers (lambda (x) (== (% x 2) 0))))
(print result)  # Outputs: 10
```

### Example 4: Nested Loop with Break

```lisp
# Find the first pair of numbers that sum to target
(def (find-pair numbers target)
  (for i (range (len numbers))
    (= a (get numbers i))
    (for j (range (+ i 1) (len numbers))
      (= b (get numbers j))
      (if (== (+ a b) target)
          (return (list a b))))
  None)  # Return None if no pair found

(= nums (list 1 3 5 7 9))
(= pair (find-pair nums 12))
(print pair)  # Outputs: (3 9)
```

## Implementation Notes

M28's control flow follows these principles:

1. **Expressions vs. Statements**: Most control structures are expressions that return values, not just statements
2. **Short-Circuit Evaluation**: Logical operators (`and`, `or`) use short-circuit evaluation
3. **Implicit Sequencing**: Multiple expressions in a body are implicitly wrapped in a `begin` block
4. **Scope**: Variables defined within a block are visible only within that block and nested blocks
5. **Truthiness**: M28 follows Python's rules for truthiness, where `False`, `None`, `0`, empty collections, and empty strings are considered falsy
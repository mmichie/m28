# Control Flow in M28

This document covers the control flow constructs in M28, including conditionals and loops.

## Contents
1. [Conditionals](#conditionals)
   - [If Expressions](#if-expressions)
   - [Nested Conditionals](#nested-conditionals)
   - [Variable Scoping](#variable-scoping)
   - [Boolean Operations](#boolean-operations)
2. [Loops](#loops)
   - [For Loops](#for-loops)
   - [While Loops](#while-loops)
   - [Loop Control Statements](#loop-control-statements)
3. [Exception Control Flow](#exception-control-flow)
4. [Early Returns](#early-returns)
5. [Examples](#examples)

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

### Variable Scoping

In M28, each if/else branch has its own lexical scope. Variables defined within a branch are only visible within that branch:

```lisp
(if (> x 0)
    (begin
      (= local_var "positive")  # local_var only exists in this branch
      (print local_var))
    (begin
      (= local_var "negative")  # different local_var in else branch
      (print local_var)))

# local_var is not accessible here
```

To make variables available outside conditional blocks, define them in the parent scope:

```lisp
(= result "unknown")
(if (> x 0)
    (= result "positive")  # updates existing variable
    (= result "negative"))
# result is now updated based on the condition
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

## Early Returns

M28 supports early returns from functions, which can be especially useful in nested conditionals:

```lisp
(def (check-number x)
  (if (< x 0)
      (return "negative"))  # Function returns immediately if x is negative
  
  (if (== x 0)
      (return "zero"))      # Function returns immediately if x is zero
  
  (if (< x 10)
      (return "small positive")
      (return "large positive")))
```

Early returns work correctly even in deeply nested contexts, making complex conditional logic easier to express and understand.

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
4. **Lexical Scoping**: Each control flow construct creates its own environment, ensuring proper variable scoping
5. **Propagating Control Signals**: Break, continue, and return signals properly propagate through nested contexts
6. **Truthiness**: M28 follows Python's rules for truthiness, where `False`, `None`, `0`, empty collections, and empty strings are considered falsy
7. **Environment Isolation**: Variables defined within conditional blocks are properly isolated to maintain clean scoping

M28 implements these control flow constructs as special forms in `special_forms/control_flow.go`, with careful attention to environment handling and control flow signal propagation.
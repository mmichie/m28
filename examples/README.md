# M28 Examples

This directory contains example programs that demonstrate the features and capabilities of the M28 programming language, a Python-inspired Lisp dialect.

## Getting Started

To run any example, use the M28 interpreter:

```bash
./bin/m28 examples/hello_world.m28
```

Or you can start the REPL and load an example:

```bash
make run
```

## Categories of Examples

### Basic Language Features

- **hello_world.m28**: Basic greeting function
- **simple_add.m28**: Simple arithmetic operations
- **factorial.m28**: Classic recursive function implementation
- **fibonacci.m28**: Recursive Fibonacci sequence implementation

### Functions and Lambdas

- **simple_lambda.m28**: Basic lambda function usage
- **factorial_lambda.m28**: Factorial using lambda functions
- **ycombinator.m28**: Y-combinator implementation for recursion

### Data Structures

- **simple_list.m28**: Basic list operations
- **list_comprehension.m28**: List comprehension examples
- **dict_operations_simple.m28**: Dictionary operations
- **set_operations_simple.m28**: Set operations
- **tuple_operations.m28**: Tuple operations

### State Management

- **counter_example.m28**: Simple counter implementation
- **working_closures.m28**: Stateful closures

### Object-Oriented Programming

- **simple_objects.m28**: Simple object system
- **working_objects.m28**: More complex object implementation
- **person_test.m28**: Person class example

### Algorithms

- **bubblesort.m28**: Bubble sort implementation
- **tree_traversal.m28**: Tree traversal algorithms
- **memoization.m28**: Function memoization pattern

## Learning Path

If you're new to M28, we recommend exploring the examples in this order:

1. Start with **hello_world.m28** to understand basic syntax
2. Move to **simple_add.m28** and **simple_function.m28** for basic operations
3. Try **simple_list.m28** and **dict_operations_simple.m28** to learn about data structures
4. Explore **simple_lambda.m28** for functional programming concepts
5. Study **simple_objects.m28** and **working_closures.m28** for object-oriented patterns
6. Look at **tree_traversal.m28** and other algorithm implementations for advanced usage

## Syntax Comparison

M28 combines Python-inspired semantics with Lisp syntax:

```lisp
# Python-style function definition with Lisp syntax
(def (greet name)
    (print (+ "Hello, " name "!")))

# Lambda functions
(= add (lambda (a b) (+ a b)))

# Lists and iterations
(for (i [1, 2, 3, 4, 5])
    (print i))

# Dictionary operations
(= person (dict "name" "John" "age" 30))
(print (get person "name"))

# Tuples
(= point (1, 2))
```
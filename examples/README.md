# M28 Examples

This directory contains example programs that demonstrate the features and capabilities of the M28 programming language, a Python-inspired Lisp dialect. The examples are now organized by category for easier navigation and learning.

## Directory Structure

- **basics/** - Simple examples to get started with M28
  - Hello world, simple operations, printing

- **functions/** - Function definition and usage
  - Basic functions, lambda functions, advanced patterns
  - **recursion/** - Recursive function examples (factorial, fibonacci, Y-combinator)

- **data_structures/** - Working with various data structures
  - **lists/** - List operations and comprehensions
  - **dictionaries/** - Dictionary creation and manipulation
  - **sets/** - Set operations
  - **tuples/** - Tuple operations
  - **trees/** - Tree data structures and algorithms

- **algorithms/** - Implementation of algorithms
  - **sorting/** - Sorting algorithms like bubblesort and quicksort
  - Memoization and other optimization techniques

- **closures/** - Closure patterns for state management
  - Basic closures, counter implementations, advanced patterns

- **oop/** - Object-oriented programming patterns
  - Simple objects, method dispatch, class patterns, inheritance
  - **examples/** - Real-world object examples (counter, person, bank accounts)

- **modules/** - Module system and imports
  - **modules/** - Sample modules
  - Import patterns, enhanced modules, dot notation

- **advanced/** - Advanced language features
  - Exception handling, generators, context managers

- **tutorials/** - Step-by-step tutorials
  - Closures, objects, counter implementations

## Getting Started

To run any example, use the M28 interpreter:

```bash
./bin/m28 examples/basics/hello_world.m28
```

Or you can start the REPL and load an example:

```bash
make run
```

## Learning Path

If you're new to M28, we recommend exploring the examples in this order:

1. Start with **basics/hello_world.m28** to understand basic syntax
2. Move to **basics/simple_operations.m28** and **functions/01_basic_functions.m28**
3. Try **data_structures/lists/01_basic_lists.m28** and **data_structures/dictionaries/01_basic_dict.m28**
4. Explore **functions/02_lambda_functions.m28** for functional programming concepts
5. Study the tutorial files in **tutorials/** to understand state management
6. Progress to **oop/01_simple_objects.m28** and subsequent OOP examples
7. Examine the algorithm implementations in the **algorithms/** directory for advanced usage

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
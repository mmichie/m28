# M28 Examples Index

This directory contains a comprehensive collection of M28 examples, organized from basic concepts to complete projects. All examples follow the M28 Lispy-Pythonic syntax rules.

## Directory Structure

### 00_basics - Getting Started
Learn the fundamentals of M28 programming.

- **hello_world.m28** - The classic first program
- **variables.m28** - Variable declaration with `=` operator
- **data_types.m28** - Numbers, strings, booleans, and None
- **operators.m28** - Arithmetic, comparison, and logical operators

### 01_functions - Functions and Lambdas
Master function definition and functional programming basics.

- **basic_functions.m28** - Function definition with `def`
- **lambda_functions.m28** - Anonymous functions
- **higher_order.m28** - Functions that take/return functions

### 02_control_flow - Control Structures
Control program flow with conditions and loops.

- **conditionals.m28** - if/elif/else and cond statements
- **loops.m28** - for and while loops with break/continue
- **exceptions.m28** - try/except/finally error handling

### 03_data_structures - Built-in Data Types
Work with M28's powerful data structures.

- **lists.m28** - List creation, manipulation, and comprehensions
- **dictionaries.m28** - Dictionary operations and methods
- **sets_and_tuples.m28** - Set operations and immutable tuples

### 04_classes - Object-Oriented Programming
Create classes and objects with inheritance.

- **basic_classes.m28** - Class definition, constructors, and methods
- **inheritance.m28** - Single and multiple inheritance
- **class_features.m28** - Static methods, properties, and operator overloading

### 05_modules - Module System
Organize code with modules and packages.

- **basic_modules.m28** - Import statements and module usage
- **math_utils.m28** - Example module with utility functions
- **using_custom_modules.m28** - Importing and using custom modules
- **package_structure.m28** - Organizing modules into packages

### 06_file_io - File Operations
Read, write, and manipulate files.

- **reading_files.m28** - Various ways to read files
- **writing_files.m28** - Writing text and binary files
- **file_operations.m28** - Advanced file and directory operations

### 07_functional - Functional Programming
Advanced functional programming patterns.

- **functional_basics.m28** - First-class functions and composition
- **map_filter_reduce.m28** - Core functional operations
- **closures_decorators.m28** - Closures and function decorators

### 08_projects - Complete Applications
Full example projects demonstrating M28 capabilities.

- **todo_app.m28** - Command-line todo list manager
- **calculator.m28** - Scientific calculator with expression evaluation
- **text_adventure.m28** - Text-based adventure game framework
- **dice_game.m28** - Simple dice game with probability

### 09_algorithms - Classic Algorithms
Implementations of fundamental computer science algorithms.

- **sorting.m28** - Bubble sort, selection sort, insertion sort, quicksort, merge sort
- **fibonacci.m28** - Multiple Fibonacci implementations (recursive, iterative, memoized, generator, etc.)
- **searching.m28** - Linear search, binary search, jump search, interpolation search
- **graph_algorithms.m28** - BFS, DFS, shortest path, topological sort, cycle detection
- **dynamic_programming.m28** - LCS, knapsack, edit distance, coin change, and more
- **tree_algorithms.m28** - BST operations, tree traversals, LCA, tree properties
- **heap_algorithms.m28** - Heap sort, priority queue, find k largest elements

## Running Examples

To run any example:
```bash
./bin/m28 examples/00_basics/hello_world.m28
```

Or from the REPL:
```lisp
(load "examples/00_basics/hello_world.m28")
```

## Learning Path

1. Start with **00_basics** to understand M28 syntax
2. Move to **01_functions** to learn about functions
3. Study **02_control_flow** for program control
4. Explore **03_data_structures** for data manipulation
5. Learn OOP with **04_classes**
6. Understand code organization with **05_modules**
7. Master I/O operations in **06_file_io**
8. Dive into **07_functional** for advanced patterns
9. Study **08_projects** to see everything in action
10. Explore **09_algorithms** for classic algorithm implementations

## Key Syntax Rules

Remember these M28 syntax rules:
- Comments ALWAYS use `#` (never `;`)
- Variables ALWAYS use `=` (never `def`)
- Functions ONLY use `def`
- All operations use prefix notation
- Code is data (homoiconic)
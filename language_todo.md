# M28 Language Implementation TODO

Based on testing the examples, here are the language features that need to be implemented to match the Lispy-Pythonic design:

## Critical - Syntax Changes

### 1. Comment Syntax
- [ ] Implement `#` for comments (currently causes "name error")
- [ ] Disable `;` comments (currently still work)
- [ ] Support `#` at beginning of line and inline

### 2. String Formatting
- [ ] Implement f-strings: `f"Hello, {name}!"`
- [ ] Support expressions in f-string braces

### 3. Restrict `def` to Functions Only
- [ ] Make `def` work ONLY for function definitions
- [ ] Ensure `def` for variables throws an error
- [ ] Update error messages to guide users to use `=`

## Currently Working Features
- ✓ Basic print statements
- ✓ Variable assignment with `=` (but `def` also works for variables)
- ✓ Arithmetic operations (+, -, *, /)
- ✓ List literals `[1, 2, 3]`
- ✓ Dictionary literals `{"key": "value"}`
- ✓ Function definitions with `def`
- ✓ Lambda expressions
- ✓ If/else conditionals
- ✓ For loops (with parentheses: `(for (i seq) body)`)
- ✓ While loops
- ✓ Comparison operators (>, <, ==)
- ✓ Boolean operations (and, or, not)
- ✓ len function
- ✓ range function
- ✓ Dot notation for dict access: `(. dict "key")`
- ✓ Comments with `;` (but `#` doesn't work)

## Features That Need Implementation/Fixes

### 1. Indexing and Slicing
- [ ] List indexing: `(list 0)` currently fails with "expected callable, got list"
- [ ] Direct dictionary access: `(dict "key")` likely fails similarly
- [ ] Need an index/get function for lists: `(nth list 0)` or similar
- [ ] Slicing syntax: `list[1:3]` or `(slice list 1 3)`

### 2. Property Access
- [ ] Dot notation: `obj.property` (special form exists but needs testing)
- [ ] Method calls: `obj.method(args)`
- [ ] Setting properties: `(= obj.property value)`

### 3. Classes
- [ ] Class definition syntax - currently errors with "parent class 'def' not found"
- [ ] Method definitions inside classes
- [ ] Constructor `__init__`
- [ ] Inheritance syntax
- [ ] `super` calls

### 4. String Operations
- [ ] String methods: `.upper()`, `.lower()`, `.split()`, etc.
- [ ] String concatenation with `+`
- [ ] String repetition with `*`

### 5. List Operations  
- [ ] List methods: `.append()`, `.extend()`, `.pop()`, etc.
- [ ] List comprehensions: `[x*2 for x in range(10)]`
- [ ] `in` operator for membership testing (currently "name error")
- [ ] `getattr` doesn't work for list indexing

### 6. For Loop Variations
- [ ] Multiple variable unpacking: `(for ((k v) dict.items()) ...)`
- [ ] Enumerate: `(for ((i val) (enumerate list)) ...)`

## Implementation Priority

Based on testing, the most critical features to implement first are:

1. **Comment syntax** - Switch from `;` to `#`
2. **F-strings** - Essential for many examples
3. **List/dict indexing** - Need a way to access elements
4. **Classes** - Currently broken
5. **For loop syntax** - Consider supporting both `(for (i seq))` and `(for i seq)`
6. **in operator** - Common operation

## Notes on Syntax Differences

The examples assume Python-like syntax in some places that may need adjustment:
- For loops: Currently require `(for (var seq) body)` not `(for var seq body)`
- Property access: Using `(. obj prop)` instead of `obj.prop`
- No direct indexing: Can't do `(list 0)`, need alternative

## Testing Progress

### 00_basics
- [ ] hello_world.m28 - Fails due to # comments and f-strings
- [ ] variables.m28 - Should work except for # comments
- [ ] data_types.m28 - Need to test
- [ ] operators.m28 - Need to test

### 01_functions
- [ ] basic_functions.m28 - Need to test
- [ ] lambda_functions.m28 - Need to test
- [ ] higher_order.m28 - Need to test

### 02_control_flow
- [ ] conditionals.m28 - Need to test
- [ ] loops.m28 - Need to test
- [ ] exceptions.m28 - Need to test

### 03_data_structures
- [ ] lists.m28 - Need to test
- [ ] dictionaries.m28 - Need to test
- [ ] sets_and_tuples.m28 - Need to test

### 04_classes
- [ ] basic_classes.m28 - Need to test
- [ ] inheritance.m28 - Need to test
- [ ] class_features.m28 - Need to test

### 05_modules
- [ ] basic_modules.m28 - Need to test
- [ ] math_utils.m28 - Need to test
- [ ] using_custom_modules.m28 - Need to test
- [ ] package_structure.m28 - Need to test

### 06_file_io
- [ ] reading_files.m28 - Need to test
- [ ] writing_files.m28 - Need to test
- [ ] file_operations.m28 - Need to test

### 07_functional
- [ ] functional_basics.m28 - Need to test
- [ ] map_filter_reduce.m28 - Need to test
- [ ] closures_decorators.m28 - Need to test

### 08_projects
- [ ] todo_app.m28 - Need to test
- [ ] calculator.m28 - Need to test
- [ ] text_adventure.m28 - Need to test

## Additional Features to Test

- [ ] List comprehensions: `[x*2 for x in range(10)]`
- [ ] Dictionary comprehensions: `{k: v for k, v in items}`
- [ ] Set literals: `{1, 2, 3}`
- [ ] Tuple literals: `(1, 2, 3)`
- [ ] Multiple assignment: `(= x y z (values 1 2 3))`
- [ ] Augmented assignment: `(+= x 1)`
- [ ] Method calls: `(obj.method args)`
- [ ] Property access: `obj.property`
- [ ] Slicing: `list[1:3]`
- [ ] `in` operator: `(in x collection)`
- [ ] `is` operator: `(is x y)`
- [ ] String methods: `s.upper()`, `s.split()`
- [ ] List methods: `list.append()`, `list.extend()`
- [ ] Dict methods: `dict.get()`, `dict.items()`
- [ ] Class inheritance syntax
- [ ] `super` calls
- [ ] Exception handling with multiple except blocks
- [ ] `with` statement (context managers)
- [ ] Generator expressions
- [ ] `yield` keyword
- [ ] Decorators
- [ ] `*args` and `**kwargs`
- [ ] Module imports and aliasing
- [ ] `__name__ == "__main__"` idiom
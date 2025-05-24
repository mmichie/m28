# M28 Language Implementation TODO

Based on testing the examples, here are the language features that need to be implemented to match the Lispy-Pythonic design:

## Critical - Syntax Changes

### 1. Comment Syntax ✅ DONE
- [x] Implement `#` for comments
- [x] Disable `;` comments (note: `;` as bare symbol causes timeout - needs investigation)
- [x] Support `#` at beginning of line and inline

### 2. String Formatting ✅ DONE
- [x] Implement f-strings: `f"Hello, {name}!"`
- [x] Support expressions in f-string braces
- [x] Escape sequences work: `\{` and `\}`

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

## Additional Working Features Found
- ✓ String concatenation with `+`
- ✓ Comparison operators: `<=`, `>=`, `!=`
- ✓ Built-in functions: `min`, `max`, `sum`
- ✓ `return` statement
- ✓ `begin` blocks
- ✓ `with` statement (but no context managers)
- ✓ `mod` operator (but not `%`)

## Features That Need Implementation/Fixes

### 1. Indexing and Slicing ✅ PARTIALLY DONE
- [x] List indexing: Use `(nth list 0)` function
- [x] Dictionary access: Use `(get dict "key")` or `(. dict "key")`
- [ ] Direct indexing syntax: `(list 0)` still doesn't work
- [ ] Slicing syntax: `list[1:3]` or `(slice list 1 3)`

### 2. Property Access
- [ ] Dot notation: `obj.property` (special form exists but needs testing)
- [ ] Method calls: `obj.method(args)`
- [ ] Setting properties: `(= obj.property value)`

### 3. Classes ✅ DONE
- [x] Class definition syntax - fixed parser logic
- [x] Method definitions inside classes
- [x] Constructor `__init__`
- [x] Inheritance syntax: `(class Dog (Animal) ...)`
- [x] Method overriding works

### 4. String Operations
- [ ] String methods: `.upper()`, `.lower()`, `.split()`, etc.
- [ ] String concatenation with `+`
- [ ] String repetition with `*`

### 5. List Operations  
- [ ] List methods: `.append()`, `.extend()`, `.pop()`, etc.
- [ ] List comprehensions: `[x*2 for x in range(10)]`
- [x] `in` operator for membership testing - DONE
- [ ] `getattr` doesn't work for list indexing

### 6. For Loop Variations
- [ ] Multiple variable unpacking: `(for ((k v) dict.items()) ...)`
- [ ] Enumerate: `(for ((i val) (enumerate list)) ...)`

### 7. Exception Handling ✅ DONE
- [x] `try/except` blocks work with multiple syntaxes
- [x] Multiple except clauses: `(except ZeroDivisionError e ...)`
- [x] Finally clause works
- [x] Catch-all: `(except handler)` or `(except e handler)`
- [x] Type-specific: `(except Type handler)` or `(except Type e handler)`
- [ ] `raise` statement for re-raising exceptions

### 8. Advanced Operators ✅ PARTIALLY DONE
- [x] Power operator `**` - DONE (added as alias for pow)
- [x] Modulo operator `%` - DONE (was already implemented)
- [ ] Augmented assignment: `+=`, `-=`, etc. (gives "name error")

### 9. Data Structure Literals
- [ ] Set literals `{1, 2, 3}` - parser thinks it's a dict
- [ ] Tuple literals `(1, 2, 3)` - gives "name error"
- [ ] List comprehensions `[x*2 for x in seq]` - treated as literal list

### 10. Control Flow
- [ ] `cond` statement gives "name error"
- [ ] `elif` in if statements needs testing

### 11. Module System
- [ ] `import` gives "module not found" even for standard modules
- [ ] No standard library modules available

### 12. Function Features
- [ ] `*args` and `**kwargs` give "name error"
- [ ] Keyword arguments in function calls
- [ ] Default parameter values

### 13. Built-in Functions ✅ PARTIALLY DONE
- [x] `abs` - absolute value (in math.go)
- [x] `sorted` - sort a sequence (added to list.go)
- [x] `reversed` - reverse a sequence (added to list.go)
- [x] `enumerate` - get index/value pairs (in utilities.go)
- [x] `zip` - combine sequences (in utilities.go)
- [x] `map`, `filter`, `reduce` - functional operations (in list.go)
- [ ] `isinstance`, `issubclass` - type checking
- [ ] `getattr`, `setattr`, `hasattr` - attribute manipulation
- [ ] `all`, `any` - boolean operations on sequences
- [ ] `round`, `int`, `float`, `str` - type conversions

### 14. Object Protocol
- [ ] `__name__ == "__main__"` idiom
- [ ] Magic methods beyond `__init__`
- [ ] Property decorators
- [ ] Static methods, class methods

### 15. File I/O
- [ ] `open` function doesn't support context manager protocol
- [ ] No file object methods like `.read()`, `.write()`, etc.

## Implementation Priority

Critical features completed ✅:
1. ✅ **Comment syntax** - Switched from `;` to `#`
2. ✅ **F-strings** - Implemented with `{expression}` syntax
3. ✅ **List/dict indexing** - Using `nth` and `get` functions
4. ✅ **Classes** - Fixed parser logic
5. ✅ **Basic operators**: `**` for power, `%` for modulo
6. ✅ **in operator** - Works for lists, strings, dicts
7. ✅ **try/except/finally** - Exception handling with Python-style syntax
8. ✅ **Core built-ins** - `abs`, `sorted`, `reversed`, `enumerate`, `zip`

Next priorities:
1. **List comprehensions** - Currently parsed as literal lists
2. **Property/method access** - `obj.method()` syntax
3. **Augmented assignment** - `+=`, `-=`, etc.
4. **Set and tuple literals**
5. **String/list methods** - `.append()`, `.split()`, etc.
6. **Type checking** - `isinstance`, `type` improvements
7. **More built-ins** - `all`, `any`, `round`, type conversions

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
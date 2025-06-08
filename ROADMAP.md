# M28 Language Roadmap

This is the single source of truth for M28 development. All other roadmap/todo documents have been consolidated here.

## Current Implementation Status

### ✅ Completed Features

- [x] **Core Language**
  - [x] S-expression syntax with Python semantics
  - [x] Comments with `#` (`;` disabled)
  - [x] F-strings: `f"Hello, {name}!"` ✅ ENHANCED - Now supports nested quotes!
  - [x] All primitive types (numbers, strings, booleans, nil)
  - [x] Variable assignment with `=`
  - [x] Function definitions with `def` and lambda
  - [x] Arithmetic operators: `+`, `-`, `*`, `/`, `**` (power), `%` (modulo)
  - [x] Comparison operators: `==`, `!=`, `<`, `>`, `<=`, `>=`
  - [x] Logical operators: `and`, `or`, `not`
  - [x] `in` operator for membership testing

- [x] **Data Structures**
  - [x] Lists: `[1, 2, 3]`
  - [x] Dictionaries: `{"key": "value"}`
  - [x] Tuples: `(1, 2, 3)`
  - [x] Sets: `{1, 2, 3}`
  - [x] List comprehensions: `[x*2 for x in range(10) if x > 5]`

- [x] **Control Flow**
  - [x] `if`/`elif`/`else` conditionals
  - [x] `for` loops with iteration
  - [x] `while` loops
  - [x] `break` and `continue`
  - [x] `try`/`except`/`finally` exception handling

- [x] **Object System**
  - [x] Class definitions with `class`
  - [x] Method definitions
  - [x] Constructor `__init__`
  - [x] Inheritance: `(class Dog (Animal) ...)`
  - [x] Method overriding
  - [x] `super` for parent class access

- [x] **Built-in Functions**
  - [x] I/O: `print`, `input`
  - [x] Type functions: `type`, `len`
  - [x] Sequence functions: `range`, `enumerate`, `zip`
  - [x] Math functions: `abs`, `min`, `max`, `sum`, `pow`
  - [x] List functions: `sorted`, `reversed`, `map`, `filter`, `reduce`
  - [x] Dictionary functions: `dict`, `get`

- [x] **Advanced Features**
  - [x] Module system with `import`
  - [x] Context managers with `with`/`as`
  - [x] File context managers (`with open(...) as f`) ✅
  - [x] Generators with `yield`
  - [x] Tail call optimization
  - [x] REPL with history and completion

## 🚧 In Progress / Partially Working

- [ ] **Module System** - Basic imports work, but missing:
  - [ ] `from module import symbol`
  - [ ] `import module as alias`
  - [ ] Standard library modules (shutil, pathlib, tempfile, zipfile)
  
- [x] **Function and Type `__name__` Attribute** ✅ PARTIAL
  - [x] User-defined functions have `__name__`
  - [x] Classes have `__name__` method
  - [ ] Built-in functions show `<anonymous>` instead of actual names

## ❌ Not Implemented

### High Priority - Language Core

**Note**: Most core features are complete. Main gaps are:

- [ ] **Macro System** (Fundamental Lisp Feature)
  - [ ] `defmacro` for defining macros
  - [ ] `quasiquote` (backtick) for template expressions
  - [ ] `unquote` (comma) and `unquote-splice` (comma-at) for interpolation
  - [ ] Hygienic macro support to prevent variable capture
  - [ ] Macro expansion phase in evaluation

- [x] **Property/Method Access** ✅ COMPLETE
  - [x] Direct dot notation: `obj.method()` instead of `(. obj method)`
  - [x] Chained access: `obj.prop.method().value`
  - [x] Property assignment: `obj.prop = value` ✅ DONE

- [x] **Indexing and Slicing** ✅ COMPLETE
  - [x] Direct indexing: `list[0]`, `dict["key"]`
  - [x] Slice syntax: `list[1:3]` ✅ DONE (including negative indices and step)
  - [x] Index assignment: `list[0] = value`, `dict[key] = value` ✅ DONE (fixed dict assignment with variable keys)

- [x] **Operators**
  - [x] Augmented assignment: `+=`, `-=`, `*=`, `/=`, `%=`, `**=`
  - [x] String repetition: `"a" * 3` (already working)
  - [x] List repetition: `[1, 2] * 3` (already working)

- [x] **Function Features** ✅ COMPLETE
  - [x] Restrict `def` to functions only ✅ (now gives clear error for non-functions)
  - [x] Keyword arguments: `func(a=1, b=2)` ✅ DONE
  - [x] Default parameters: `(def func (a (b 10)) ...)` ✅ DONE
  - [x] `*args` and `**kwargs` ✅ DONE

- [ ] **Remaining Python Features**
  - [x] Tuple unpacking in loops: `for key, value in items:` ✅ DONE
  - [ ] Set literals: `{1, 2, 3}` (currently parsed as dict)
  - [ ] Multiple assignment: `a, b = 1, 2`
  - [ ] Exception variable binding: `except Error as e`
  - [ ] Argument unpacking: `func(*args, **kwargs)` syntax
  - [ ] `from module import name` syntax
  - [ ] `import module as alias` syntax
  - [ ] Local `.m28` module imports
  - [ ] Multiple inheritance (partial)
  - [ ] Proper `super()` calls

### Medium Priority - Built-in Methods

- [x] **String Methods** ✅ DONE
  - [x] Case methods: `.upper()`, `.lower()`, `.capitalize()`
  - [x] Search methods: `.find()`, `.startswith()`, `.endswith()`
  - [x] Transform methods: `.split()`, `.join()`, `.strip()`, `.replace()`
  - [x] Format methods: `.format()`

- [x] **List Methods** ✅ DONE
  - [x] Mutating methods: `.append()`, `.extend()`, `.insert()`, `.remove()`, `.pop()`
  - [x] Search methods: `.index()`, `.count()`
  - [x] In-place operations: `.sort()`, `.reverse()`
  - [x] Additional methods: `.clear()`, `.copy()`

- [x] **Dictionary Methods** ✅ DONE
  - [x] Access methods: `.keys()`, `.values()`, `.items()`
  - [x] Update methods: `.update()`, `.pop()`, `.setdefault()`
  - [x] Additional methods: `.get()`, `.clear()`, `.copy()`
  - [ ] View methods: proper dict views instead of lists (future enhancement)

- [x] **Set Methods** ✅ DONE
  - [x] Set operations: `.union()`, `.intersection()`, `.difference()`, `.symmetric_difference()`
  - [x] Mutating methods: `.add()`, `.remove()`, `.discard()`, `.update()`, `.clear()`, `.copy()`, `.pop()`
  - [x] Comparison methods: `.issubset()`, `.issuperset()`, `.isdisjoint()`

### Low Priority - Additional Features

- [x] **Type System** (Partially Complete)
  - [x] Fix `isinstance()` and `issubclass()` ✅ DONE
  - [x] Type conversion functions: `int()`, `float()`, `str()`, `bool()` ✅ DONE
  - [x] `hasattr()`, `getattr()`, `setattr()` ✅ DONE
  - [x] `callable()` function ✅ DONE

- [x] **More Built-ins** (Partially Complete)
  - [x] Boolean operations: `all()`, `any()` ✅ DONE
  - [x] Math functions: `round()`, `divmod()` ✅ DONE
  - [ ] Iterator functions: `iter()`, `next()` with default parameter
  - [ ] `eval()` and `exec()`
  - [ ] `slice()` object creator
  - [ ] `vars()`, `dir()` for object introspection
  - [ ] `globals()`, `locals()` for namespace access

- [ ] **Functional Programming Enhancements**
  - [ ] Key parameter for `sorted()`, `min()`, `max()`: `sorted(items, key=lambda x: x.age)`
  - [ ] `reversed()` as iterator (currently returns new list)
  - [ ] `sum()` with start parameter: `sum([1,2,3], start=10)`
  - [ ] `round()` with ndigits: `round(3.14159, 2)`
  - [ ] `isinstance()` with tuple of types: `isinstance(x, (int, float))`
  - [ ] `frozenset()` immutable set type
  - [ ] **Threading Macros** (Clojure/Hy-style)
    - [ ] `->` thread-first macro: `(-> x (foo) (bar 2) (baz))`
    - [ ] `->>` thread-last macro: `(->> data (map inc) (filter even?) (take 5))`

- [ ] **Itertools-like Functions**
  - [ ] `product()` - Cartesian product
  - [ ] `permutations()` - All permutations
  - [ ] `combinations()` - Combinations without replacement
  - [ ] `chain()` - Chain multiple iterables
  - [ ] `groupby()` - Group consecutive elements
  - [ ] `repeat()` - Repeat element n times
  - [ ] `cycle()` - Infinite cycle through iterable

- [ ] **Advanced Features**
  - [ ] Decorators with `@` syntax
  - [ ] Generator expressions: `(x*2 for x in range(10))`
  - [ ] Dictionary/set comprehensions
  - [ ] Multiple assignment: `a, b = 1, 2`
  - [ ] Async/await support

- [ ] **Type Inference (Low Priority)**
  - [ ] Local type inference within functions
  - [ ] Flow-sensitive type narrowing (isinstance checks)
  - [ ] Optional type annotations for function signatures
  - [ ] Basic type checking for common errors
  - [ ] Union types from control flow analysis

- [ ] **Literals**
  - [ ] Set literals that work (currently `{1,2,3}` parsed as dict)
  - [ ] Tuple literals without name error
  - [ ] Binary/octal/hex number literals
  - [ ] Raw strings: `r"raw\string"`

## Implementation Priorities

### ✅ Completed Major Features
- [x] Core syntax (dot notation, indexing, augmented assignment)
- [x] All string, list, dict, set methods
- [x] Type system improvements (isinstance, type conversions)
- [x] Essential built-ins (all, any, round, divmod)
- [x] Enhanced f-strings with nested quotes
- [x] Tuple unpacking in for loops
- [x] File context managers

### 🎯 High Priority - Next Steps
1. **Module System Improvements**
   - [ ] Local `.m28` module imports
   - [ ] `from module import name` syntax
   - [ ] `import module as alias` syntax

2. **Functional Programming**
   - [ ] Key parameter for sorted/min/max (high impact)
   - [ ] Default parameter for next()
   - [ ] slice() object creator

3. **Core Python Features**
   - [ ] Set literals `{1, 2, 3}`
   - [ ] Multiple assignment `a, b = 1, 2`
   - [ ] Exception variable binding `except Error as e`

### 📊 Medium Priority
- [ ] Itertools-like functions (product, combinations, etc.)
- [ ] Enhanced built-ins (reversed as iterator, sum with start)
- [ ] Object introspection (dir, vars)
- [ ] **REPL Enhancements**
  - [ ] Module reload without restart: `reload(module)`
  - [ ] Magic commands: `%time`, `%debug`, `%who`
  - [ ] Enhanced multi-line editing with syntax highlighting
  - [ ] Better autocomplete with type hints
  - [ ] Command history search (Ctrl+R style)

### 🔮 Future Enhancements
- [ ] Decorators with @ syntax
- [ ] Generator expressions
- [ ] Dictionary/set comprehensions
- [ ] Async/await support

## Remaining Implementation Gaps

### 1. Tuple Unpacking in For Loops ✅ COMPLETE
**Status**: Fully implemented with support for multiple variables
**Syntax supported**:
- `(for key value in (dict.items) ...)`
- `(for i item in (enumerate list) ...)`
- `(for a b c in nested-list ...)`
- Old syntax still works: `(for (k v (dict.items)) ...)`

### 2. Local Module Import Resolution  
**Issue**: Cannot import local `.m28` modules
**Examples failing**: `using_custom_modules.m28`
**Needed**: Update module resolution to check for `.m28` files in current directory

### 3. Missing Python Standard Library Modules
**Needed modules**:
- `shutil` - File operations (copy, move, rmtree)
- `pathlib` - Path manipulation  
- `tempfile` - Temporary file creation
- `zipfile` - Zip file handling

## Testing Checklist

### Examples to Verify
- [ ] `examples/00_basics/` - All files should run
- [ ] `examples/01_functions/` - All files should run
- [ ] `examples/02_control_flow/` - All files should run
- [ ] `examples/03_data_structures/` - All files should run
- [ ] `examples/04_classes/` - All files should run
- [ ] `examples/05_modules/` - All files should run
- [ ] `examples/06_file_io/` - All files should run
- [ ] `examples/07_functional/` - All files should run
- [ ] `examples/08_projects/` - All files should run

### Core Test Files
- [ ] All files in `tests/` directory pass
- [ ] REPL works with all features
- [ ] Performance benchmarks show reasonable speed

## Success Criteria

The language is considered "complete" when:
1. All Python-like syntax works naturally (no `(. obj method)` workarounds)
2. All example files run without modification
3. Common Python patterns are supported
4. The REPL feels natural to use
5. Error messages are clear and helpful

## Next Steps

1. Start with fixing dot notation - this unblocks the most functionality
2. Then add direct indexing - another major usability improvement
3. Focus on making existing features work properly before adding new ones

## Code Quality and Architecture Improvements

### 🐍 Python Protocol Implementation

#### High Priority - Essential Protocols
1. **Core Object Protocols**
   - [x] `__repr__` - Developer-friendly string representation ✅
   - [ ] `__hash__` - Enable custom objects as dict keys/set members
   - [x] `__bool__` - Custom truthiness for `if obj:` statements ✅
   - [x] `__len__` - Length protocol for all containers ✅
   - [x] `__contains__` - Custom `in` operator behavior ✅

2. **Complete Arithmetic Protocols**
   - [ ] Basic operators: `__sub__`, `__mul__`, `__truediv__`, `__floordiv__`, `__mod__`, `__pow__`
   - [ ] Reverse operators: `__radd__`, `__rsub__`, `__rmul__`, etc. (for `5 + custom_obj`)
   - [ ] In-place operators: `__iadd__`, `__isub__`, `__imul__`, etc.
   - [ ] Unary operators: `__neg__`, `__pos__`, `__abs__`

3. **Comparison Protocols**
   - [ ] Ordering operators: `__lt__`, `__le__`, `__gt__`, `__ge__`
   - [ ] Inequality: `__ne__`
   - [ ] Support for `sorted()` and `min()`/`max()` with custom objects

4. **Container Protocols**
   - [ ] `__getitem__` - Custom indexing `obj[key]`
   - [ ] `__setitem__` - Custom assignment `obj[key] = value`
   - [ ] `__delitem__` - Custom deletion `del obj[key]`
   - [ ] `__reversed__` - Custom reversed iteration

#### Medium Priority - Advanced Protocols
1. **Callable and Iteration**
   - [ ] `__call__` - Make any object callable like a function
   - [ ] `__iter__` - Make any object iterable with full protocol
   - [ ] `__getattr__` - Dynamic attribute access fallback
   - [ ] `__setattr__` - Custom attribute setting behavior

2. **Type Conversion and Formatting**
   - [ ] `__format__` - Custom f-string formatting
   - [ ] `__bytes__` - Bytes representation
   - [ ] `__index__` - Convert to integer for slicing

3. **Copying Support**
   - [ ] `__copy__` - Shallow copy protocol
   - [ ] `__deepcopy__` - Deep copy protocol

#### Low Priority - Specialized Protocols
1. **Bitwise Operators**
   - [ ] `__and__`, `__or__`, `__xor__` - Bitwise operations
   - [ ] `__lshift__`, `__rshift__` - Bit shifting
   - [ ] `__invert__` - Bitwise NOT

2. **Advanced Python Features**
   - [ ] Descriptor protocol: `__get__`, `__set__`, `__delete__`
   - [ ] Metaclass protocols: `__new__`, `__del__`
   - [ ] Async protocols: `__aenter__`, `__aexit__`, `__aiter__`, `__anext__`
   - [ ] Pickling: `__getstate__`, `__setstate__`, `__reduce__`

### 🏗️ Architectural Refactoring

#### High Priority - Code Quality
1. **Refactor Complex Functions**
   - [ ] Break down `eval/evaluator.go` tryForm (300+ lines) into smaller functions
   - [ ] Simplify `parser/fstring.go` parseFString with helper functions
   - [ ] Extract common patterns in builtin registration functions

2. **Reduce Code Duplication**
   - [ ] Create error handling utilities for consistent patterns across builtins
   - [ ] Extract common type checking logic into shared utilities
   - [ ] Implement generic iterator framework for container types

3. **Improve Error Handling**
   - [ ] Replace remaining panic() calls with proper error returns
   - [ ] Implement consistent error wrapping with context
   - [ ] Add error type hierarchy for better error discrimination
   - [ ] **Better Error Messages** (Critical for Developer Experience)
     - [ ] Add source location tracking (line and column numbers)
     - [ ] Include source context in error messages (show problematic line)
     - [ ] Improve stack traces with function names and locations
     - [ ] Add "Did you mean?" suggestions for common typos

#### Medium Priority - Type System Enhancements
1. **Define Missing Protocols**
   - [ ] Add numeric operation interfaces (Numeric protocol)
   - [ ] Define comparison protocol for ordering operations
   - [ ] Implement equality protocol for custom equality

2. **Testing Infrastructure**
   - [ ] Add Go unit tests for core functionality (currently only M28 tests)
   - [ ] Implement property-based testing for parser
   - [ ] Add performance benchmarks for critical paths

#### Low Priority - Long-term Improvements
1. **Concurrency Safety**
   - [ ] Add race detector to test suite
   - [ ] Document thread-safety guarantees for each component
   - [ ] Fix potential races in module loading
   - [ ] Use sync.Map where appropriate for concurrent access

2. **Code Organization**
   - [ ] Consolidate special_forms into eval package
   - [ ] Create separate packages for each builtin category
   - [ ] Move type descriptors closer to type definitions

3. **Documentation**
   - [ ] Add interface documentation with contracts
   - [ ] Document concurrency guarantees
   - [ ] Add architecture decision records (ADRs)

4. **Compiler Architecture Improvements**
   - [ ] Separate compilation phases (parse → expand → compile → execute)
   - [ ] Source map tracking through transformations
   - [ ] AST optimization passes before evaluation
   - [ ] Macro expansion as distinct phase

### 📊 Code Quality Metrics to Track
- Function complexity (cyclomatic complexity < 10)
- Test coverage (aim for >80% for core packages)
- Code duplication percentage (target <5%)
- Interface adherence and abstraction levels
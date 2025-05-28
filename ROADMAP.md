# M28 Language Roadmap

This is the single source of truth for M28 development. All other roadmap/todo documents have been consolidated here.

## Current Implementation Status

### ✅ Completed Features

- [x] **Core Language**
  - [x] S-expression syntax with Python semantics
  - [x] Comments with `#` (`;` disabled)
  - [x] F-strings: `f"Hello, {name}!"`
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
  - [x] Generators with `yield`
  - [x] Tail call optimization
  - [x] REPL with history and completion

## 🚧 In Progress / Partially Working

- [ ] **Dot Notation** - Works with special form `(. obj prop)` but not `obj.prop`
- [ ] **Direct Indexing** - Must use `(nth list 0)` instead of `list[0]`
- [ ] **Module System** - Basic imports work, but missing:
  - [ ] `from module import symbol`
  - [ ] `import module as alias`
  - [ ] Standard library modules

## ❌ Not Implemented / Broken

### High Priority - Language Core

- [x] **Property/Method Access**
  - [x] Direct dot notation: `obj.method()` instead of `(. obj method)`
  - [x] Chained access: `obj.prop.method().value`
  - [ ] Property assignment: `obj.prop = value`

- [x] **Indexing and Slicing**
  - [x] Direct indexing: `list[0]`, `dict["key"]`
  - [ ] Slice syntax: `list[1:3]` or `(slice list 1 3)`
  - [ ] Index assignment: `list[0] = value`

- [x] **Operators**
  - [x] Augmented assignment: `+=`, `-=`, `*=`, `/=`, `%=`, `**=`
  - [x] String repetition: `"a" * 3` (already working)
  - [x] List repetition: `[1, 2] * 3` (already working)

- [ ] **Function Features**
  - [ ] Restrict `def` to functions only (currently works for variables)
  - [ ] Keyword arguments: `func(a=1, b=2)`
  - [ ] Default parameters: `(def func (a b=10) ...)`
  - [ ] `*args` and `**kwargs`

### Medium Priority - Built-in Methods

- [ ] **String Methods**
  - [ ] Case methods: `.upper()`, `.lower()`, `.capitalize()`
  - [ ] Search methods: `.find()`, `.startswith()`, `.endswith()`
  - [ ] Transform methods: `.split()`, `.join()`, `.strip()`, `.replace()`
  - [ ] Format methods: `.format()`

- [ ] **List Methods**
  - [ ] Mutating methods: `.append()`, `.extend()`, `.insert()`, `.remove()`, `.pop()`
  - [ ] Search methods: `.index()`, `.count()`
  - [ ] In-place operations: `.sort()`, `.reverse()`

- [ ] **Dictionary Methods**
  - [ ] Access methods: `.keys()`, `.values()`, `.items()`
  - [ ] Update methods: `.update()`, `.pop()`, `.setdefault()`
  - [ ] View methods: proper dict views instead of lists

- [ ] **Set Methods**
  - [ ] Set operations: `.union()`, `.intersection()`, `.difference()`
  - [ ] Mutating methods: `.add()`, `.remove()`, `.discard()`
  - [ ] Comparison methods: `.issubset()`, `.issuperset()`

### Low Priority - Additional Features

- [ ] **Type System**
  - [ ] Fix `isinstance()` and `issubclass()`
  - [ ] Type conversion functions: `int()`, `float()`, `str()`, `bool()`
  - [ ] `hasattr()`, `getattr()`, `setattr()`
  - [ ] `callable()` function

- [ ] **More Built-ins**
  - [ ] Boolean operations: `all()`, `any()`
  - [ ] Math functions: `round()`, `divmod()`
  - [ ] Iterator functions: `next()`, `iter()`
  - [ ] `eval()` and `exec()`

- [ ] **Advanced Features**
  - [ ] Decorators with `@` syntax
  - [ ] Generator expressions: `(x*2 for x in range(10))`
  - [ ] Dictionary/set comprehensions
  - [ ] Multiple assignment: `a, b = 1, 2`
  - [ ] Async/await support

- [ ] **Literals**
  - [ ] Set literals that work (currently `{1,2,3}` parsed as dict)
  - [ ] Tuple literals without name error
  - [ ] Binary/octal/hex number literals
  - [ ] Raw strings: `r"raw\string"`

## Implementation Plan

### Week 1: Fix Core Syntax Issues
- [x] Day 1-2: Fix dot notation for method calls (`obj.method()`) ✅ DONE
- [x] Day 3: Add direct indexing (`list[0]`, `dict["key"]`) ✅ DONE
- [x] Day 4: Implement augmented assignment (`+=`, `-=`, etc.) ✅ DONE
- [ ] Day 5: Restrict `def` to functions only

### Week 2: Essential Built-in Methods
- [ ] Day 1-2: Implement string methods
- [ ] Day 3-4: Implement list methods
- [ ] Day 5: Implement dictionary methods

### Week 3: Type System and Built-ins
- [ ] Day 1-2: Fix `isinstance`, `issubclass`, add type conversions
- [ ] Day 3-4: Add remaining essential built-ins (`all`, `any`, `round`, etc.)
- [ ] Day 5: Test and fix issues

### Week 4: Polish and Testing
- [ ] Day 1-3: Run all example files and fix failures
- [ ] Day 4-5: Update documentation and clean up

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
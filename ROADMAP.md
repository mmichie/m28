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
  - [x] Generators with `yield`
  - [x] Tail call optimization
  - [x] REPL with history and completion

## 🚧 In Progress / Partially Working

- [ ] **Module System** - Basic imports work, but missing:
  - [ ] `from module import symbol`
  - [ ] `import module as alias`
  - [ ] Standard library modules

## ❌ Not Implemented / Broken

### High Priority - Language Core

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

- [ ] **Additional Python Features**
  - [ ] Argument unpacking: `func(*args, **kwargs)` syntax
  - [ ] Keyword argument support in built-in functions (sorted, min, max, etc.)
  - [ ] Set literals: `{1, 2, 3}` (currently must use `(set [1, 2, 3])`)
  - [ ] Exception variable binding: `except Error as e` syntax
  - [ ] Multiple inheritance (currently not fully functional)
  - [ ] super() calls (currently not working properly)

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
  - [ ] Iterator functions: `next()`, `iter()`
  - [ ] `eval()` and `exec()`

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

## Implementation Plan

### Week 1: Fix Core Syntax Issues
- [x] Day 1-2: Fix dot notation for method calls (`obj.method()`) ✅ DONE
- [x] Day 3: Add direct indexing (`list[0]`, `dict["key"]`) ✅ DONE
- [x] Day 4: Implement augmented assignment (`+=`, `-=`, etc.) ✅ DONE
- [x] Day 5: Restrict `def` to functions only ✅ DONE

### Week 2: Essential Built-in Methods
- [x] Day 1-2: Implement string methods ✅ DONE
- [x] Day 3-4: Implement list methods ✅ DONE
- [x] Day 5: Implement dictionary methods ✅ DONE

### Week 3: Type System and Built-ins
- [x] Day 1-2: Fix `isinstance`, `issubclass`, add type conversions ✅ DONE
- [x] Day 3-4: Add remaining essential built-ins (`all`, `any`, `round`, etc.) ✅ DONE
- [x] Day 5: Test and fix issues ✅ DONE

### Week 4: Polish and Testing
- [x] Day 1: Enhanced f-strings with nested quote support ✅ DONE
- [ ] Day 2-3: Run all example files and fix failures
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
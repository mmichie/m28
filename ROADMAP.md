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
  - [x] Full readline support (arrow keys, vi/emacs modes, Ctrl+R search) ✅ NEW

## 🚧 In Progress / Partially Working

- [x] **Module System** - Basic imports work with enhanced features:
  - [x] `(import "module" from [symbol1 symbol2])` - Import specific symbols
  - [x] `(import "module" from *)` - Wildcard imports
  - [x] `(import "module" as alias)` - Import with alias
  - [x] Support for .m28 extension in imports
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
  - [x] Set literals: `{1, 2, 3}` ✅ DONE
  - [x] Multiple assignment: `a, b = 1, 2` ✅ DONE
  - [x] Exception variable binding: `except Error as e` ✅ DONE
  - [ ] Argument unpacking: `func(*args, **kwargs)` syntax
  - [x] `from module import name` syntax ✅ DONE (using list syntax)
  - [x] `import module as alias` syntax ✅ DONE
  - [x] Local `.m28` module imports ✅ DONE
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
  - [ ] Iterator functions: `iter()` (next() default parameter already implemented)
  - [ ] `eval()` and `exec()`
  - [x] `slice()` object creator ✅ DONE
  - [ ] `vars()`, `dir()` for object introspection
  - [ ] `globals()`, `locals()` for namespace access

- [ ] **Functional Programming Enhancements**
  - [x] Key parameter for `sorted()`, `min()`, `max()`: `sorted(items, key=lambda x: x.age)` ✅ DONE
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
  - [ ] Async/await support

- [ ] **Type Inference (Low Priority)**
  - [ ] Local type inference within functions
  - [ ] Flow-sensitive type narrowing (isinstance checks)
  - [ ] Optional type annotations for function signatures
  - [ ] Basic type checking for common errors
  - [ ] Union types from control flow analysis

- [ ] **Literals**
  - [x] Set literals that work ✅ DONE
  - [ ] Tuple literals without name error
  - [ ] Binary/octal/hex number literals
  - [ ] Raw strings: `r"raw\string"`

- [ ] **Missing Python Builtin Data Structures**
  - [ ] **Binary Data Types**
    - [ ] `bytes` - Immutable sequences of bytes (b"hello")
    - [ ] `bytearray` - Mutable sequences of bytes
    - [ ] `memoryview` - Memory view objects for buffer protocol
  - [ ] **Numeric Types**
    - [ ] `complex` - Complex numbers with real/imaginary parts (3+4j)
    - [ ] `decimal.Decimal` - Fixed-point decimal numbers
    - [ ] `fractions.Fraction` - Rational numbers
  - [ ] **Collection Types**
    - [ ] `frozenset` - Immutable set type (hashable, can be dict keys)
    - [x] `range` object - Lazy range object ✅ DONE
    - [ ] `deque` - Double-ended queue (collections module)
    - [ ] `Counter` - Dict subclass for counting (collections module)
    - [ ] `defaultdict` - Dict with default factory (collections module)
    - [ ] `OrderedDict` - Order-preserving dict (collections module)
    - [ ] `ChainMap` - View of multiple dicts (collections module)
    - [ ] `namedtuple` - Tuple with named fields (collections module)
  - [ ] **Other Types**
    - [ ] `array.array` - Efficient arrays of numeric values
    - [ ] `enum.Enum` - Enumeration support
    - [ ] `datetime` types - date, time, datetime, timedelta

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

0. **P0: Fix list(range(n)) conversion** ✅ FIXED
   - [x] Previously `list(range(5))` returned `[range(5)]` instead of `[0, 1, 2, 3, 4]`
   - [x] Fixed list() function to properly iterate over Iterable types
   - [x] Added comprehensive tests in tests/test_list_range_conversion.m28
   - [x] Critical for Python compatibility - now working correctly!

1. **Record Stream and JSON Processing** (NEW)
   - [ ] Path-based access functions (`get-in`, `assoc-in`, `update-in`, `dissoc-in`)
   - [ ] JSONL streaming support (`recordstream` module)
   - [ ] Basic record operations (`select-keys`, `rename-keys`)
   - [ ] Dictionary enhancements (`deep-merge`, `map-keys`, `map-values`)

2. **Module System Improvements** ✅ COMPLETE
   - [x] Local `.m28` module imports
   - [x] List syntax `[name1 name2]` in from imports
   - [x] Wildcard imports with `*`
   - [x] `.m28` extension support with proper caching

3. **Functional Programming**
   - [x] Key parameter for sorted/min/max ✅ DONE (Python-style key=func syntax)
   - [x] Default parameter for next() ✅ DONE (already implemented)
   - [x] slice() object creator ✅ DONE

4. **Core Python Features**
   - [x] Set literals `{1, 2, 3}` ✅ DONE
   - [x] Multiple assignment `a, b = 1, 2` ✅ DONE
   - [x] Exception variable binding `except Error as e` ✅ DONE

5. **Essential Missing Data Structures**
   - [ ] `bytes` and `bytearray` - Binary data handling
   - [ ] `frozenset` - Immutable sets for use as dict keys
   - [x] `range` object - Memory-efficient lazy ranges ✅ DONE
   - [ ] `complex` - Complex number support

6. **F-String Enhancements** (Building on existing nested quotes support)
   - [ ] **Format Specifications** (High Priority)
     - [ ] Float precision: `f"{pi:.2f}"` → "3.14"
     - [ ] Integer padding: `f"{num:04d}"` → "0042"
     - [ ] Alignment: `f"{text:>10}"`, `f"{text:^20}"`, `f"{text:<15}"`
     - [ ] Sign handling: `f"{num:+}"` shows + for positive
     - [ ] Thousands separator: `f"{num:,}"` → "1,234"
     - [ ] Percentage: `f"{ratio:.1%}"` → "45.5%"
     - [ ] Binary/hex: `f"{num:b}"`, `f"{num:x}"`
   - [ ] **Conversion Flags**
     - [ ] `!r` for repr(): `f"{value!r}"`
     - [ ] `!s` for str(): `f"{value!s}"`
     - [ ] `!a` for ascii(): `f"{value!a}"`
   - [ ] **Self-Documenting Expressions**: `f"{x+y=}"` → "x+y=8"
   - [ ] **Complex Expressions in F-Strings**
     - [ ] Inline conditionals: `f"{x if x > 0 else 'negative'}"`
     - [ ] List comprehensions: `f"{[x*2 for x in range(3)]}"`
     - [ ] Nested f-strings: `f"{f'{x}'*3}"`

### 🚀 Major Feature: Record Stream and JSON Processing

**Status**: Proposed
**Priority**: High
**Impact**: Makes M28 a best-in-class language for record stream processing and JSON manipulation

#### Overview
Transform M28 into a powerful tool for processing record streams (line-delimited JSON) and JSON data, surpassing tools like jq while maintaining Pythonic simplicity.

#### Core Philosophy
- **Simpler than jq**: Use Python-like syntax instead of jq's domain-specific language
- **More powerful than jq**: Full programming language with classes, generators, etc.
- **Stream-oriented**: First-class support for record streams (line-delimited JSON)
- **Path-based access**: Simple syntax for nested data access
- **Type-aware**: Smart handling of JSON types with Python semantics

#### Phase 1: JSON Path and Query Syntax

##### 1.1 Dot Notation for Nested Access
```python
# Current (verbose)
user_name = data["users"][0]["name"]

# Proposed (elegant)
user_name = data.users[0].name

# With null safety
user_name = data?.users?[0]?.name  # Returns None if any part is missing
```

##### 1.2 Path-Based Operations
```python
# Get nested value with path
(get-in data ["users" 0 "profile" "email"])
(get-in data "users.0.profile.email")  # String path syntax

# Set nested value
(= data (assoc-in data ["users" 0 "active"] True))

# Update nested value
(= data (update-in data ["users" 0 "score"] + 10))

# Delete nested key
(= data (dissoc-in data ["users" 0 "temp_field"]))
```

##### 1.3 Query DSL
```python
# Simple query syntax inspired by MongoDB/JsonPath
(query data {
  "users": {
    "$filter": (lambda (u) u["age"] > 21),
    "$map": (lambda (u) {"name": u["name"], "email": u["email"]})
  }
})

# Or using a more Pythonic approach
(select data 
  .users
  [u for u in users if u.age > 21]
  [{name: u.name, email: u.email} for u in users])
```

#### Phase 2: Record Stream Support

##### 2.1 Line-Delimited JSON (JSONL)
```python
# Read record stream
(with-records "data.jsonl" as records
  (for r in records
    (process r)))

# Write record stream
(def write-records (filename records)
  (with (open filename "w") as f
    (for r in records
      (f.write (json.dumps r))
      (f.write "\n"))))

# Built-in streaming functions
(import recordstream as rs)

# Read records lazily
(= records (rs.read "huge-file.jsonl"))  # Returns generator

# Process in chunks
(for chunk in (rs.chunks records 1000)
  (process-batch chunk))
```

##### 2.2 Stream Processing Functions
```python
# Record-specific map that preserves record structure
(def rmap (fn records)
  (for r in records
    (yield {**r, **fn(r)})))

# Record filtering with multiple conditions
(def rfilter (conditions records)
  (for r in records
    (if (all [(cond r) for cond in conditions])
      (yield r))))

# Record transformation pipelines
(def pipeline (*transforms)
  (lambda (records)
    (reduce (lambda (rs t) (t rs)) transforms records)))

# Usage
(= process (pipeline
  (rfilter [(lambda (r) r["active"]),
            (lambda (r) r["age"] > 18)])
  (rmap (lambda (r) {"full_name": f"{r['first']} {r['last']}"}))
  (rs.sort-by "full_name")))
```

#### Phase 3: Data Manipulation Utilities

##### 3.1 Dictionary Enhancements
```python
# Select specific keys
(select-keys record ["id" "name" "email"])

# Rename keys
(rename-keys record {"old_name": "new_name", "legacy_id": "id"})

# Deep merge with custom resolution
(deep-merge record1 record2 
  on-conflict=(lambda (k v1 v2) v2))  # Take second value on conflict

# Transform keys
(map-keys str.lower record)  # Lowercase all keys
(map-values float prices)    # Convert all values

# Nested operations
(def deep-map (fn data)
  "Recursively apply function to all values"
  (cond
    (isinstance data dict) 
      {k: (deep-map fn v) for (k, v) in (items data)}
    (isinstance data list)
      [(deep-map fn x) for x in data]
    True
      (fn data)))
```

##### 3.2 Collection Operations
```python
# Group by with multiple keys
(group-by records ["category" "status"])

# Pivot operations
(pivot records 
  index="date"
  columns="category"
  values="amount"
  aggfunc=sum)

# Window functions
(def sliding-window (n records)
  (= window [])
  (for r in records
    (window.append r)
    (if (> (len window) n)
      (window.pop 0))
    (yield (list window))))

# Deduplicate by key
(unique-by records "email")
```

#### Phase 4: Type System and Validation

##### 4.1 Schema Definition
```python
# Define schemas using Python-like syntax
(defschema User {
  "id": int,
  "name": str,
  "email": str,
  "age": (int, (lambda (x) (>= x 0))),  # Type with validation
  "tags": [str],
  "profile": {
    "bio": (str, optional=True),
    "avatar": (str, optional=True)
  }
})

# Validate records
(validate User record)  # Raises on invalid
(valid? User record)    # Returns True/False

# Coerce types
(coerce User raw-data)  # Attempts type conversion
```

##### 4.2 Type Conversion Utilities
```python
# Smart type conversion
(def parse-value (s)
  "Parse string to appropriate type"
  (try
    (if (re.match r"^\d+$" s) (int s)
    (elif (re.match r"^\d+\.\d+$" s) (float s)
    (elif (s.lower in ["true" "false"]) (bool s)
    (else s)))
    (except ValueError
      s)))

# Batch conversion
(def parse-records (records)
  (for r in records
    (yield {k: (parse-value v) for (k, v) in (items r)})))
```

#### Implementation Priority

1. **High Priority** (Enables core record stream functionality):
   - Path-based access (`get-in`, `assoc-in`)
   - JSONL streaming support
   - Basic record operations (`select-keys`, `rename-keys`)

2. **Medium Priority** (Improves developer experience):
   - Dot notation for nested access
   - Schema validation
   - Type conversion utilities

3. **Low Priority** (Nice to have):
   - JSON diff/patch
   - Template system
   - Performance optimizations

#### Benefits Over jq

1. **Simpler Syntax**: 
   - jq: `.users[] | select(.age > 21) | {name, email}`
   - M28: `[{name: u.name, email: u.email} for u in data.users if u.age > 21]`

2. **Full Programming Language**:
   - Define functions, classes, use loops
   - Import libraries, handle errors properly
   - Reuse code across projects

3. **Better Debugging**:
   - Print statements, breakpoints
   - Step through transformations
   - Type checking and validation

4. **Performance**:
   - Lazy evaluation for large datasets
   - Parallel processing support
   - Compiled path expressions

5. **Extensibility**:
   - Easy to add custom functions
   - Integrate with Python libraries
   - Build domain-specific tools

### 🚀 Major Feature: Macro System with S-Strings

**Status**: Proposed
**Priority**: High
**Impact**: Enables metaprogramming and DSL creation

#### Overview
Introduce s-strings (S-expression strings) as a Pythonic approach to macros, combining Python's familiar f-string syntax with Lisp's powerful metaprogramming capabilities.

#### S-String Syntax Specification

##### Basic Syntax
```lisp
# S-strings use 's' prefix like f-strings use 'f'
s"(+ 1 2)"          # Simple s-string
s'(list {x})'       # Single quotes work too
s"""                # Triple quotes for multiline
(def {name} {value})
"""
```

##### Interpolation Rules
- `{expr}` - Interpolate a single value
- `{*expr}` - Splice a sequence (like Python's *args)
- `{**expr}` - Splice key-value pairs (like Python's **kwargs)
- `{{` and `}}` - Literal braces
- `{expr:format}` - Format specification (for future enhancement)

##### Examples
```lisp
# Variable interpolation
(= x 42)
s"(+ {x} 1)"  # Expands to: (+ 42 1)

# Splicing
(= args [1 2 3])
s"(list {*args})"  # Expands to: (list 1 2 3)

# Conditional interpolation
s"(if {test} {then_expr} {else_expr if has_else else 'nil'})"
```

#### Macro Definition Syntax

##### Basic Form
```lisp
(defmacro name (params...)
  body)
```

##### Parameter Types
- Regular parameters: `param`
- Rest parameters: `*rest`
- Keyword parameters: `**kwargs`
- Optional parameters with defaults: `(param default)`

##### Examples
```lisp
# Simple macro
(defmacro when (condition *body)
  s"(if {condition} (do {*body}))")

# Macro with optional parameter
(defmacro with-default (value (default 0))
  s"(if (nil? {value}) {default} {value})")

# Complex macro with pattern building
(defmacro -> (x *forms)
  (reduce (lambda (acc form)
            (if (list? form)
              s"({form[0]} {acc} {*form[1:]})"
              s"({form} {acc})"))
          forms
          x))
```

#### Implementation Plan

##### Phase 1: S-String Parser (2-3 weeks)
1. **Lexer modifications**:
   - Recognize s" s' s""" prefixes
   - Track interpolation boundaries with {}
   - Handle nested braces and escape sequences

2. **Parser modifications**:
   - Create new SStringValue type in core/value.go
   - Parse s-strings into AST with interpolation nodes
   - Support {expr}, {*expr}, {**expr} syntax
   - Handle multiline s-strings with proper indentation

3. **AST representation**:
   ```go
   type SStringValue struct {
       BaseObject
       Parts []SStringPart  // Mix of literal and interpolation parts
   }
   
   type SStringPart interface {
       IsSStringPart()
   }
   
   type LiteralPart struct {
       Text string
   }
   
   type InterpolationPart struct {
       Expr     Value
       Type     InterpolationType  // Normal, Splice, KwSplice
   }
   ```

##### Phase 2: Macro System (3-4 weeks)
1. **Macro definition**:
   - Add `defmacro` special form
   - Store macros in a separate namespace
   - Macro objects contain parameter info and body

2. **Macro expansion**:
   - Detect macro calls during evaluation
   - Bind arguments to macro parameters
   - Evaluate s-string body with bound values
   - Parse resulting string as S-expression
   - Return parsed AST for evaluation

3. **Evaluation order**:
   - Macros expand at parse time (before evaluation)
   - Nested macro calls expand inside-out
   - Macro expansion is recursive

##### Phase 3: Advanced Features (2-3 weeks)
1. **Hygiene**:
   - Automatic gensym for local bindings
   - Namespace isolation for macro-generated code
   - Optional unhygienic mode with special markers

2. **Built-in macros**:
   ```lisp
   # Threading macros
   (defmacro -> (x *forms) ...)
   (defmacro ->> (x *forms) ...)
   
   # Binding macros
   (defmacro let1 (var val *body) ...)
   (defmacro when-let (bindings *body) ...)
   
   # Control flow macros
   (defmacro cond (*clauses) ...)
   (defmacro case (expr *cases) ...)
   ```

3. **Debugging support**:
   - Macro expansion viewer: `(macroexpand '(when test body))`
   - Step-through macro expansion
   - Source location preservation

#### Integration Points

1. **With f-strings**:
   ```lisp
   (defmacro debug (expr)
     s'(let ((value {expr}))
         (print f"{expr} = {{value}}")
         value))
   ```

2. **With classes**:
   ```lisp
   (defmacro defprop (name)
     s"""
     (do
       (def get_{name} (self)
         (. self _{name}))
       (def set_{name} (self value)
         (setattr self '_{name} value)))
     """)
   ```

3. **With module system**:
   ```lisp
   (defmacro import-as (module name)
     s"(= {name} (import '{module}))")
   ```

#### Testing Strategy
1. **Parser tests**:
   - S-string tokenization
   - Interpolation parsing
   - Edge cases (nested braces, quotes)

2. **Macro tests**:
   - Basic macro definition and expansion
   - Parameter binding (positional, rest, keyword)
   - Recursive macro expansion
   - Hygiene tests

3. **Integration tests**:
   - Macros using s-strings with f-strings
   - Macros generating macros
   - Performance benchmarks

#### Success Criteria
- [ ] S-strings parse correctly with all interpolation types
- [ ] Macros can be defined and called
- [ ] Macro expansion happens before evaluation
- [ ] Built-in macros provide useful abstractions
- [ ] Performance overhead is minimal
- [ ] Error messages are clear and helpful
- [ ] Documentation and examples are comprehensive

#### Future Enhancements
- Pattern matching in macro parameters
- Compile-time computation in macros
- Macro modules and namespacing
- Reader macros for custom syntax
- Syntax-case style pattern matching

### 📊 Medium Priority
- [ ] Itertools-like functions (product, combinations, etc.)
- [ ] Enhanced built-ins (reversed as iterator, sum with start)
- [ ] Object introspection (dir, vars)
- [ ] **Documentation Testing Framework**
  - [ ] Doctest-style execution of code examples in documentation
  - [ ] Extract and validate code blocks from Markdown files
  - [ ] Support for expected output verification
  - [ ] Integration with test suite to ensure docs stay accurate
  - [ ] Command: `m28 --doctest README.md` or `make doctest`
  - [ ] Example format:
    ```
    # In documentation:
    >>> (+ 1 2)
    3
    >>> (= x [1, 2, 3])
    >>> (map (lambda (n) (* n 2)) x)
    [2, 4, 6]
    ```
- [x] **REPL Enhancements** ✅ DONE
  - [x] Full readline support with arrow key navigation
  - [x] Vi/Emacs keybinding modes (toggle with `:toggle-keybindings`)
  - [x] Tab completion for functions, variables, and keywords
  - [x] Command history search (Ctrl+R)
  - [x] Enhanced multi-line editing with smart indentation
  - [ ] Module reload without restart: `reload(module)`
  - [ ] Magic commands: `%time`, `%debug`, `%who`
  - [ ] Syntax highlighting (future enhancement)

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

## Known Issues

### 1. Continue Statement in While Loops
**Issue**: The `continue` statement can cause infinite loops when used in while loops if the loop counter is incremented after the continue statement
**Status**: Working as designed - matches Python behavior
**Note**: This is expected behavior. When `continue` is executed, it skips all remaining statements in the loop body and jumps to the loop condition. Ensure loop counters are updated before any `continue` statements to avoid infinite loops.

### 2. Generator Iteration
**Issue**: Generator functions are defined but iteration over generators is not fully implemented
**Example**: `(for val in generator)` may fail with "expected sequence, got number"
**Status**: Feature incomplete
**Workaround**: Collect generator values manually using next()

### 3. No raise/error Function
**Issue**: No built-in function to raise custom exceptions
**Status**: Missing feature
**Workaround**: Trigger errors through invalid operations (e.g., division by zero)

### 4. Performance Issues with Large Files
**Issue**: Some algorithm examples (fibonacci.m28, searching.m28) timeout on execution
**Status**: Performance optimization needed
**Notes**: Individual functions work but full file execution is slow

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
   - [x] Basic operators: `__add__` (already implemented)
   - [ ] Basic operators: `__sub__`, `__mul__`, `__truediv__`, `__floordiv__`, `__mod__`, `__pow__`
     - Note: Currently only `__add__` is registered for NumberType. Other operators need dunder methods added to enable operator overloading while maintaining fast paths for built-in types.
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
     - [ ] Implement proper source location tracking (see detailed design below)
     - [ ] Include source context in error messages (show problematic line)
     - [ ] Improve stack traces with function names and locations
     - [ ] Add "Did you mean?" suggestions for common typos

### 🎯 Better Error Messages - Detailed Implementation Plan

**Problem**: Current error messages lack source location information, making debugging difficult.

**Current Issues with LocatedValue Approach**:
1. Type system pollution - requires unwrapping everywhere
2. Breaks type assertions - `val.(core.NumberValue)` fails if wrapped
3. Value equality problems - wrapped and unwrapped values not equal
4. Performance overhead from constant unwrapping

**Recommended Approach**: Hybrid AST + Context-based tracking

#### Implementation Steps:

1. **Create AST Node Types** (parser/ast.go)
   ```go
   type ASTNode interface {
       GetLocation() *SourceLocation
       GetValue() core.Value
   }
   
   type ExprNode struct {
       Value    core.Value
       Location *SourceLocation
   }
   ```

2. **Modify Parser to Build AST**
   - Parser returns AST nodes instead of raw Values
   - Each node includes source location (file, line, column)
   - Preserve location through all parsing steps
   - Example: `parseNumber()` returns `&ExprNode{Value: NumberValue(n), Location: loc}`

3. **Update Evaluator for AST**
   - `Eval(node ASTNode, ctx *Context)` instead of `Eval(value Value, ctx *Context)`
   - Extract location from AST node at evaluation time
   - Store current location in Context during evaluation
   - Pass location to error creation functions

4. **Enhance Context with Location Tracking**
   ```go
   type Context struct {
       // ... existing fields
       CurrentLocation *SourceLocation // Current evaluation location
       SourceMap       map[string][]string // Cache of source files
   }
   ```

5. **Improve Error Types**
   ```go
   type EvalError struct {
       BaseError
       Location    *SourceLocation
       Expression  string // Source expression that caused error
       Context     *Context // For stack trace
   }
   ```

6. **Error Formatting with Context**
   ```
   Error at /path/to/file.m28:42:10
   
   40 | (def calculate (x y)
   41 |   (= result (+ x y))
   42 |   (/ result zero))  # <-- Error here
        |          ^^^^
   43 |   
   
   ZeroDivisionError: division by zero
   
   Stack trace:
     File "main.m28", line 10, in <module>
     File "main.m28", line 42, in calculate
   ```

7. **Implementation Priority**:
   - Phase 1: AST nodes with locations (parser changes)
   - Phase 2: Context location tracking (evaluator changes)
   - Phase 3: Enhanced error formatting (error reporter)
   - Phase 4: Source caching and display

**Benefits**:
- No runtime value wrapping
- Clean separation of parse-time and runtime
- Zero performance impact on successful execution
- Precise location tracking for all errors
- Extensible for future features (debugger, LSP)

**Files to Modify**:
- parser/parser.go - Return AST nodes
- parser/ast.go (new) - AST node definitions
- eval/evaluator.go - Accept AST nodes
- core/context.go - Add location tracking
- core/error.go - Enhanced error types
- repl/error_reporter.go - Format errors with context

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

### 🔧 Constructor Enhancements

**Issue**: The `dict`, `tuple`, and `set` constructors don't support iterables like the original list() bug.

#### Current Limitations
1. **dict constructor** - Only accepts key-value pairs as arguments
   - Cannot do: `(dict [["a" 1] ["b" 2]])`
   - Cannot do: `(dict (zip keys values))`

2. **tuple constructor** - Only handles specific types (list, tuple, string)
   - Missing generic iterable support
   - Cannot convert arbitrary iterables to tuples

3. **set constructor** - Implementation needs to be checked for iterable support
   - Should accept any iterable to create a set

#### Proposed Enhancement
Add support for creating these collections from iterables, matching Python's behavior:
- `dict(iterable)` - where iterable contains key-value pairs
- `tuple(iterable)` - convert any iterable to a tuple
- `set(iterable)` - create set from any iterable

This would prevent bugs similar to the one where `list()` didn't support the Iterable interface.
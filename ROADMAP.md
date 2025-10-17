# M28 Language Roadmap

## 🎯 Active Focus Areas

### Language Features

#### Macro System ✅ COMPLETE
- [x] S-string syntax: `s"(+ {x} 1)"` with interpolation
  - Parser recognizes s-strings and raw s-strings
  - Five interpolation types: value, code, splice, dict-splice, gensym
  - Evaluator generates AST from templates
  - Comprehensive test coverage
- [x] `@macro` decorator for defining macros (Pythonic approach)
  - Decorator syntax: `(@macro (def name (params) body))`
  - Macros receive unevaluated arguments
  - Macro expansion at call time
  - `__macro__` attribute marks functions as macros
- [x] Quasiquote/unquote for code generation in macros
  - `(quasiquote expr)`, `(unquote expr)`, `(unquote-splicing expr)`
  - Perfect for macro code generation
  - Seamless integration with macro system
- [x] Built-in utility macros
  - `unless` - execute if condition is false
  - `when` - execute if condition is true
  - `->` (thread-first) - data pipeline, first argument
  - `->>` (thread-last) - data pipeline, last argument
- [x] Reader macro syntax (syntactic sugar): `` ` `` for quasiquote, `,` for unquote, `,@` for unquote-splicing

#### Record Stream & JSON Processing 🟡 HIGH VALUE
- [x] Path-based access: Pythonic path operations
  - `get-path` - Get value at nested path with optional default
  - `set-path` - Immutably set value at path (returns new structure)
  - `update-path` - Apply function to value at path
  - `has-path?` - Check if path exists
  - String paths: `"user.address.city"` with automatic numeric index parsing
  - List paths: `["users", 0, "profile", "email"]` for complex keys
  - Works with dicts (key access), lists/tuples (index access), objects (attribute access)
  - Immutable updates preserve original structures
  - Comprehensive test coverage (tests/test-path-ops.m28)
- [x] Record operations: Pythonic dict operations
  - `select-keys` - Extract subset of keys: `(select-keys user ["name" "email"])`
  - `rename-keys` - Rename keys with mapping or function: `(rename-keys data {"old": "new"})`
  - `map-keys` - Transform all keys: `(map-keys dict upper)`
  - `map-values` - Transform all values: `(map-values dict (lambda (x) (* x 2)))`
  - `filter-keys` - Keep keys matching predicate: `(filter-keys dict (lambda (k) (k.startswith "user_")))`
  - `filter-values` - Keep values matching predicate: `(filter-values dict (lambda (v) (> v 10)))`
  - Immutable operations (original unchanged)
  - Collision detection for rename operations
  - Composable with threading macros (->)
  - Works with nested structures
  - Comprehensive test coverage (tests/test-dict-ops.m28)
- [x] Deep merge and transformations: Pythonic merge operations with predictable semantics
  - `merge` - Shallow merge multiple dicts, later values win: `(merge dict1 dict2 dict3)`
  - `deep-merge` - Recursive merge of nested structures: `(deep-merge config1 config2)`
  - `merge-with` - Custom conflict resolution with function: `(merge-with + dict1 dict2)`
  - Immutable operations (originals unchanged)
  - Variadic arguments (merge unlimited dicts)
  - Type mismatch handling (later replaces earlier, no errors)
  - Fully composable with path-ops and dict-ops
  - Comprehensive test coverage (tests/test-merge-ops.m28)
- [x] JSONL streaming support: Record-oriented JSON processing
  - `parse-jsonl-line` - Parse single JSONL line into object
  - `format-jsonl-line` - Format object as JSONL line (with newline)
  - `read-jsonl` - Read entire JSONL file (eager loading)
  - `write-jsonl` - Write list of objects to JSONL file (with append mode)
  - `append-jsonl-line` - Append single object to JSONL file
  - Type-safe JSON conversion between M28 and Go types
  - Empty line handling (automatically skipped)
  - Composable with path-ops, dict-ops, merge-ops, filter/map
  - Perfect for log processing, data pipelines, API streaming
  - Test coverage (tests/test-jsonl.m28)

#### Missing Python Features
- [ ] Multiple inheritance
- [x] Generator expressions
- [x] Dictionary/set comprehensions
- [x] Decorators with `@` syntax (implemented for macros)
- [ ] General decorator system for functions (not just macros)
- [ ] `eval()` and `exec()`

#### F-String Enhancements ✅
- [x] Format specifications: `f"{pi:.2f}"`
- [x] Alignment: `f"{text:>10}"`
- [x] Conversion flags: `f"{value!r}"`
- [x] Self-documenting: `f"{x+y=}"`

### Data Types
- [ ] `complex` numbers
- [x] `frozenset`
- [x] `decimal.Decimal`
- [ ] Collections module types (deque, Counter, defaultdict)

### Error Messages & Developer Experience
- [x] Parser errors with source locations (via token position tracking)
- [x] Context-aware error reporting (shows source code with line numbers and carets)
- [ ] Stack traces with function names (partially implemented via CallStack)
- [x] "Did you mean?" suggestions (Levenshtein-based similarity matching for undefined names)

## 🔧 Code Quality & Refactoring

### AST Layer for Multiple Frontend Support 🟡 HIGH PRIORITY

**Status:** Design complete (see [docs/design/ast-ir-multiple-frontends.md](docs/design/ast-ir-multiple-frontends.md))
**Timeline:** 5-6 weeks for critical path
**Impact:** Foundation for Python frontend, IDE tooling, type system, optimization passes

This is the foundational work that enables multiple frontend languages (Python, DSLs) to share M28's runtime.

**Current Problem:**
- Parser directly returns `core.Value` (IR) with no source location tracking
- Error messages can't show "error at line 5, column 12" in original syntax
- Lost original syntax (can't tell if user wrote `x = 10` or `(= x 10)`)
- No way to preserve Python type hints (`def f(x: int) -> int:`)
- Hard to add new frontends (would need to duplicate desugaring logic)

**Proposed Three-Layer Architecture:**
```
Frontend (Python/Lisp/DSL) → AST (with locations/types/comments) → IR (core.Value) → Evaluator
```

**Implementation Phases:**

#### Phase 1: Core AST Infrastructure (Week 1-2) ✅ COMPLETE
- [x] Create `core/ast/` package with ASTNode interface
- [x] Implement node types: Identifier, Literal, SExpr, DefForm, AssignForm, IfForm
- [x] Add IRMetadata table for tracking locations/types/comments
- [x] Update Context to carry metadata
- [x] Deliverable: AST types working, all tests pass (8 AST tests + 7 metadata tests)

#### Phase 2: Parser Refactoring (Week 3-4) ✅ COMPLETE
- [x] Refactor parser to build AST instead of returning IR directly
- [x] Add location tracking to every node creation
- [x] Populate metadata table during parsing
- [x] Parser returns `(ASTNode, *IRMetadata, error)` via ParseToAST()
- [x] Deliverable: AST-based parser with full location info (7 parser tests passing)

#### Phase 3: Enhanced Error Messages (Week 5)
- [ ] Update error types to include SourceLocation from metadata
- [ ] Show original syntax in errors (Python vs Lisp)
- [ ] Source snippets with caret indicators pointing to exact location
- [ ] Deliverable: Beautiful error messages with source context

#### Phase 4: Type Annotation Support (Week 6)
- [ ] Add TypeInfo structure for Python type hints
- [ ] Parse Python type annotations into AST
- [ ] Store type info in metadata table
- [ ] Deliverable: Type hints preserved, queryable by tooling

**Benefits:**
- ✅ **Multiple frontends** - Python, JSON DSLs share same runtime
- ✅ **Perfect errors** - Show exact location in original syntax
- ✅ **IDE support** - Foundation for LSP (go-to-def, hover, refactor)
- ✅ **Type system** - Preserve Python type hints for gradual typing
- ✅ **Optimizations** - AST transformation passes for performance

**Design Decisions:**
- Keep IR as S-expressions (current `core.Value`) - evaluator unchanged
- Use separate metadata table instead of wrapping values (no type pollution)
- AST implements Value interface for backwards compatibility during migration
- Three layers: AST (tooling) → IR (evaluation) → Runtime (execution)

**Migration Strategy:**
- Zero breaking changes - add AST layer alongside existing parser
- Evaluator continues using IR (completely unchanged)
- Gradual rollout over 6 weeks with parallel testing

**Success Metrics:**
- [x] Phase 1: AST infrastructure complete (8 tests passing)
- [x] Phase 1: Metadata table working (7 tests passing)
- [x] Phase 1: Zero regressions (54/54 M28 tests passing)
- [x] Phase 2: Parser builds AST with locations (7 parser tests passing)
- [x] Phase 2: ParseToAST() method returns AST + metadata
- [x] Phase 2: Zero regressions (54/54 M28 tests still passing)
- [ ] Phase 3: Error messages show source locations
- [ ] Phase 4: Type annotations preserved from Python
- [ ] No eval performance regression
- [ ] Documentation complete

See [docs/design/ast-ir-multiple-frontends.md](docs/design/ast-ir-multiple-frontends.md) for complete design.

### High-Impact Refactoring

#### Break down large functions (100+ lines) ✅
- [x] core/module_loader_enhanced.go: LoadModule() - refactored into 12 focused functions
- [x] core/doc_registry.go: FormatDocEntry() - refactored into 11 focused functions
- [x] core/repr.go: Repr() - refactored into 13 focused functions

#### Complete type system migration ✅
- [x] builtin/iteration.go - iterable type switches
- [x] builtin/attributes.go - object type checking
- [x] eval/evaluator.go - special forms validation (AST pattern matching - appropriate)
- [x] builtin/modules/math.go - numeric validation
- [x] builtin/string_search.go (~42 manual checks)
- [x] builtin/os.go (~35+ manual checks)
- [x] builtin/json.go (complex validation)
- [x] builtin/pathlib.go (~20+ type checks)

#### Protocol wrapper implementations ✅
- [x] DunderIterator wrapper (core/protocols/iterable.go) - Wraps objects with `__iter__`/`__next__`
- [x] DunderNumeric wrapper (core/protocols/numeric.go) - Wraps objects with arithmetic dunder methods
- [x] DunderIndexable wrapper (core/protocols/indexable.go) - Wraps objects with indexing dunder methods

#### Internal code refactoring to use protocols
Current: Some operators use protocols (multiply, comparisons, `in`), but most follow pattern: Dunder → Type-switch
Target: Consistent pattern: Dunder → Protocol → Type-switch

**High Priority - Arithmetic Operators (builtin/operators/operators.go):** ✅ COMPLETE
- [x] Refactor `addTwo()` (line 74) to use `GetNumericOps().Add()`
- [x] Refactor `subtractTwo()` (line 187) to use `GetNumericOps().Subtract()`
- [x] Refactor `divideValue()` (line 386) to use `GetNumericOps().Divide()`
- [ ] Refactor `floorDivideValue()` (line 437) to use protocol (needs FloorDivide method in Numeric protocol)
- [x] Refactor `moduloTwo()` (line 510) to use `GetNumericOps().Modulo()`
- [x] Refactor `powerTwo()` (line 572) to use `GetNumericOps().Power()`
- [x] Refactor `negateValue()` (line 167) to use `GetNumericOps().Negate()`

**Medium Priority - Iterator/Indexable Protocol Usage:**
- [ ] Update `IterBuilder()` in builtin/iteration.go to use `GetIterableOps()`
- [ ] Audit eval/indexing.go for `GetIndexableOps()` opportunities

**Benefits:** Consistent codebase, DunderNumeric/Iterator wrappers work everywhere, easier maintenance

#### Internal builtin functions need dunder method support
Many builtin functions bypass dunder methods and go straight to type switches. Need to add proper dunder method checks.

**High Priority - Type Conversion Functions:** ✅ COMPLETE
- [x] `int()` (builtin/types.go:114) - Add `__int__()` dunder method support
- [x] `str()` (builtin/types.go:64) - Add `__str__()` dunder method support
- [x] `repr()` (builtin/misc.go:21) - Add `__repr__()` dunder method support
- [x] `bool()` (builtin/types.go:96) - Add `__bool__()` dunder method support in constructor
- [x] `hash()` (builtin/misc.go:51) - Add `__hash__()` dunder method support

**Medium Priority - Attribute Functions:** ✅ COMPLETE
- [x] `getattr()` (builtin/essential_builtins.go:200) - Add `__getattr__()` and `__getattribute__()` support
- [x] `setattr()` (builtin/essential_builtins.go:277) - Add `__setattr__()` dunder method support
- [x] `delattr()` (builtin/attributes.go:82) - Implement `__delattr__()` dunder method
- [x] `dir()` (builtin/attributes.go:27) - Add `__dir__()` dunder method support

**Medium Priority - Numeric Functions:** ✅ COMPLETE
- [x] `abs()` - Verified uses `GetNumericOps().Absolute()` properly with __abs__() dunder support
- [x] `round()` - Added `__round__()` dunder method support
- [x] `divmod()` - Added `__divmod__()` and `__rdivmod__()` dunder method support

**Medium Priority - Missing Builtins:** ✅ COMPLETE
- [x] `format()` - Implemented Python-style format(value, format_spec='') with `__format__()` dunder method (builtin/types.go:397)
  - Replaced old sprintf-style format() which didn't work with {} placeholders
  - Calls __format__(format_spec) on objects when available
  - Falls back to str() conversion for built-in types
- [x] `reversed()` - Implemented with `__reversed__()` dunder method support (builtin/iteration.go)
- [x] `bytes()` (core/type_registry_primitives.go:307) - Added `__bytes__()` dunder method support

**Status:**
- ✅ `float()` - Already has `__float__()` support (builtin/types.go:199)
- ✅ `len()` - Already has `__len__()` support (builtin/collections.go:297)
- ✅ Operators - All arithmetic operators now use protocol layer

#### Python Dunder Methods - Missing Implementations

##### Currently Implemented ✅
Arithmetic: `__add__`, `__sub__`, `__mul__`, `__truediv__`, `__floordiv__`, `__mod__`, `__pow__`, `__neg__`, `__abs__`, `__pos__`
Reflected: `__radd__`, `__rsub__`, `__rmul__`, `__rtruediv__`, `__rfloordiv__`, `__rmod__`, `__rpow__`
Comparison: `__eq__`, `__ne__`, `__lt__`, `__le__`, `__gt__`, `__ge__`
Container: `__len__`, `__getitem__`, `__setitem__`, `__delitem__`, `__contains__`, `__iter__`, `__next__`
Bitwise: `__and__`, `__or__`, `__xor__`, `__invert__`, `__lshift__`, `__rshift__`, `__hash__`
Reflected Bitwise: `__rand__`, `__ror__`, `__rxor__`, `__rlshift__`, `__rrshift__`
Augmented Assignment: `__iadd__`, `__isub__`, `__imul__`, `__itruediv__`, `__ifloordiv__`, `__imod__`, `__ipow__`, `__iand__`, `__ior__`, `__ixor__`, `__ilshift__`, `__irshift__`
Context Manager: `__enter__`, `__exit__`
Conversion: `__int__`, `__float__`, `__str__`, `__repr__`, `__bool__`, `__bytes__`, `__format__`, `__index__`
Lifecycle: `__init__`, `__call__`
Metaclass: `__instancecheck__`, `__subclasscheck__`
Iteration: `__iter__`, `__next__`, `__reversed__`
Numeric Extended: `__round__`, `__divmod__`, `__rdivmod__`

##### High Priority - Core Operations ✅ COMPLETE

**Attribute Access:**
- [x] `__getattr__` - Fallback for undefined attributes (builtin/essential_builtins.go:242-253)
- [x] `__getattribute__` - Intercept all attribute access (builtin/essential_builtins.go:200-215)
- [x] `__setattr__` - Attribute assignment (builtin/essential_builtins.go:277-288)
- [x] `__delattr__` - Attribute deletion (builtin/attributes.go:106-117)
- [x] `__dir__` - Directory listing for dir() (builtin/attributes.go:27-42)

**Type Conversion:**
- [x] `__bytes__` - Convert to bytes (core/type_registry_primitives.go:329)
- [ ] `__complex__` - Convert to complex number (blocked: requires complex number type implementation)
- [x] `__index__` - Convert to integer index (for slicing) - Enables custom classes as indices
- [x] `__format__` - Custom string formatting (builtin/types.go:397)

##### Medium Priority - Extended Functionality 🟡

**Object Lifecycle:**
- [ ] `__new__` - Constructor (before __init__)
- [ ] `__del__` - Finalizer/destructor

**Numeric Protocol:**
- [ ] `__divmod__` - divmod() function support
- [ ] `__matmul__` - Matrix multiplication (@)
- [ ] `__rmatmul__` - Reflected matrix multiplication
- [ ] `__imatmul__` - In-place matrix multiplication (@=)
- [ ] `__round__` - Rounding behavior
- [ ] `__trunc__` - Truncation to integer
- [ ] `__floor__` - Floor value
- [ ] `__ceil__` - Ceiling value

**Sequence Protocol:**
- [ ] `__reversed__` - Reverse iteration
- [ ] `__missing__` - For dict subclasses when key not found
- [ ] `__length_hint__` - Length hint for optimization

**Descriptors:**
- [ ] `__get__` - Descriptor getter
- [ ] `__set__` - Descriptor setter
- [ ] `__delete__` - Descriptor deleter
- [ ] `__set_name__` - Descriptor name binding

**Copy Protocol:**
- [ ] `__copy__` - Shallow copy support
- [ ] `__deepcopy__` - Deep copy support

##### Low Priority - Advanced Features 🟢

**Async Protocol (Future):**
- [ ] `__await__` - Make object awaitable
- [ ] `__aiter__` - Async iterator protocol
- [ ] `__anext__` - Async next()
- [ ] `__aenter__` - Async context manager enter
- [ ] `__aexit__` - Async context manager exit

**Pickle Protocol:**
- [ ] `__reduce__` - Pickle support
- [ ] `__reduce_ex__` - Extended pickle support
- [ ] `__getnewargs__` - Pickle arguments for __new__
- [ ] `__getnewargs_ex__` - Extended pickle arguments
- [ ] `__getstate__` - Get object state for pickling
- [ ] `__setstate__` - Restore object state from pickle

**Metaclass Protocol:**
- [ ] `__prepare__` - Metaclass namespace preparation
- [ ] `__init_subclass__` - Subclass initialization hook
- [ ] `__mro_entries__` - MRO computation hook
- [ ] `__class_getitem__` - Generic class indexing (List[int])

**Buffer Protocol (Low-level):**
- [ ] `__buffer__` - Buffer protocol support
- [ ] `__release_buffer__` - Buffer release

### Architecture Improvements
- [ ] Package reorganization (core package doing too much)
- [ ] Visitor pattern for AST
- [ ] Data-driven approach for type registry
- [ ] Consider code generation for repetitive patterns
- [ ] Standardize error handling patterns
- [ ] Define clear error types in core/error.go
- [ ] Use consistent error wrapping throughout
- [ ] Fix import organization (follow Go conventions)
- [x] Apply validation framework everywhere
- [ ] Consider using generics (Go 1.18+) for common patterns

### Missing Core Functionality
- [ ] Sentinel iterator in iter() (builtin/iteration.go:65)
- [ ] Dict initialization from pairs (core/type_registry.go:925)
- [ ] Container protocol __len__ and __contains__ checks (core/protocols/container.go:176)

### Container Protocols Implementation
- [ ] Create Indexable protocol adapters for List, Dict, Tuple, String, Range
- [ ] Update indexing operations in eval/indexing.go
- [ ] Add DelItemForm for del operations
- [ ] Integrate with three-tier dispatch: dunder → protocol → type-specific
- [ ] Add comprehensive tests for protocol-based indexing

### Testing Gaps
- [ ] Add Go unit tests (currently only M28 tests)
- [ ] Property-based testing for parser
- [ ] Performance benchmarks
- [ ] Documentation testing framework

## 🏗️ Long-Term Architecture Vision

### AST & Parser Abstraction (Foundation)
**See detailed design:** [docs/design/ast-ir-multiple-frontends.md](docs/design/ast-ir-multiple-frontends.md)
**Active work:** See "AST Layer for Multiple Frontend Support" under Code Quality & Refactoring

This section tracks the long-term vision after the core AST layer is complete.

#### Core AST Layer (5-6 weeks) - See roadmap item above for details
- [ ] Define language-agnostic AST nodes in `core/ast/`
- [ ] Include source location info in every node for error reporting
- [ ] Design AST to support type annotations
- [ ] Implement IRMetadata table for tracking locations/types/comments
- [ ] Refactor parser to build AST instead of returning IR directly

#### Multiple Parser Support (Future)
- [ ] Create `Parser` interface with multiple implementations
- [ ] Move current M28 parser to `parser/m28/`
- [ ] Add parser registry for file extension mapping (`.m28`, `.py`, etc.)
- [ ] Support for parsing strings, files, and streams
- [ ] Create AST visitor pattern for transformations

#### IR Optimization Layers (Future)
- [ ] Keep core.Value as evaluation IR (lightweight S-expressions)
- [ ] Consider HIR (High-level IR) for optimization passes
- [ ] Consider MIR (Mid-level IR) for type-directed optimizations
- [ ] Consider SSA form for advanced optimizations
- [ ] Support for type information flowing through IR layers

### Error Handling Enhancement
- [x] Add line/column info to all tokens (via tokenizer)
- [x] Implement source span tracking (StartPos/EndPos in tokens)
- [x] Keep original source text for error display (stored in ParseError)
- [x] Rich error types with structured information (ParseError, NameError, etc.)
- [x] Colorized error output with source highlights (via ErrorReporter)
- [ ] Add line/column info to all AST nodes (Phase 2 of AST layer - in progress)
- [ ] Use metadata table to show original syntax in errors (Phase 3 of AST layer)
- [ ] Support for multi-file error traces
- [ ] Error recovery in parser (continue after errors)
- [ ] Error explanation system (like Rust's `--explain`)

### Type System Foundation
**Prerequisite:** AST layer with type annotation support (Phase 4)

- [ ] Type AST nodes (for type annotations) - Phase 4 of AST layer
- [ ] TypeInfo structure for Python type hints - Phase 4 of AST layer
- [ ] Type inference infrastructure
- [ ] Gradual typing support (typed + untyped code)
- [ ] Generic/parametric types foundation (List[int], Dict[str, int])
- [ ] Optional type checking phase (can run without types)
- [ ] Type error reporting using diagnostic framework
- [ ] Type-directed optimizations (specialize on known types)
- [ ] Runtime type checking for gradual typing

### Multiple Frontend Support
**Prerequisite:** AST layer complete (Phases 1-4)
**See design:** [docs/design/ast-ir-multiple-frontends.md](docs/design/ast-ir-multiple-frontends.md)

Once AST layer is in place, adding new frontends becomes straightforward:
```
Python Parser → AST (same structure) → IR (core.Value) → Evaluator (shared)
```

#### Python Frontend (Next Major Feature)
- [ ] Python 3.x tokenizer (indentation-aware)
- [ ] Python 3.x grammar implementation (recursive descent or parser combinator)
- [ ] Python AST → M28 AST translation layer
- [ ] Handle Python-specific features:
  - [ ] Indentation-based blocks
  - [ ] Multiple assignment (`x, y = 1, 2`)
  - [ ] Unpacking (`*args, **kwargs`)
  - [ ] Decorators (already supported for macros)
  - [ ] Context managers (`with` statement - already supported)
  - [ ] Async/await (already supported in IR)
- [ ] Python REPL with Python syntax
- [ ] Error messages in Python syntax (using metadata table)
- [ ] Test suite: Run Python code and M28 code side-by-side

#### Other Frontend Options (Future)
- [ ] JSON configuration DSL
- [ ] Typed M28 variant (stricter semantics, immutability by default)
- [ ] SQL-like query language for data processing

### Optimization & Performance
**Prerequisite:** AST layer enables optimization passes

With AST in place, we can implement optimization passes that transform code before evaluation.

#### AST-Based Optimizations (Enabled by AST layer)
- [ ] Constant folding (evaluate constant expressions at compile time)
- [ ] Dead code elimination (remove unused definitions)
- [ ] Function inlining (inline small functions)
- [ ] Tail call optimization (detect and optimize tail recursion)
- [ ] Escape analysis (determine if values escape scope)

#### IR-Level Optimizations (Future)
- [ ] Design M28 HIR (High-level IR) with dynamic operation support
- [ ] Include type guards and profiling points
- [ ] Support for gradual lowering to different backends
- [ ] Deoptimization points for returning to interpreter

#### Bytecode VM (Future - Major undertaking)
- [ ] Bytecode VM with register-based design
- [ ] Inline caching for method lookups
- [ ] Quickening (bytecode specialization)
- [ ] Computed goto dispatch (if supported)
- [ ] Hot path detection
- [ ] On-stack replacement (OSR) for hot loops

#### JIT Compilation (Far Future)
- [ ] Type annotation validator
- [ ] LLVM IR generation for typed functions
- [ ] FFI boundary between compiled and interpreted code
- [ ] Benchmark common numeric kernels
- [ ] Runtime type profiling infrastructure
- [ ] Speculative optimization with guards

### Developer Experience & Tooling
**Prerequisite:** AST layer provides foundation for all tooling

The AST layer with source locations enables all modern IDE features:

#### LSP (Language Server Protocol)
- [ ] Basic LSP implementation (foundation with AST)
- [ ] Go-to-definition (using AST locations)
- [ ] Find references (AST traversal)
- [ ] Hover documentation (from docstrings in AST)
- [ ] Auto-completion (using context + type info from AST)
- [ ] Rename symbol (AST-based refactoring)
- [ ] Inline error display (from AST metadata)
- [ ] Document symbols (outline view from AST)

#### Development Tools
- [ ] Code formatter (like gofmt/black) - operates on AST
- [ ] Linter with configurable rules - analyzes AST
- [ ] Documentation generator - extracts from AST comments
- [ ] Syntax highlighting - AST-aware semantic highlighting
- [ ] Debug adapter protocol (DAP) - uses AST for breakpoints

#### Package Management
- [ ] Package manager (dependency resolution)
- [ ] Module registry
- [ ] Semantic versioning support

### Community & Ecosystem
- [ ] Plugin system for extensions
- [ ] FFI for C extensions
- [ ] Web playground
- [ ] Comprehensive documentation
- [ ] Standard library expansion

## ✅ Recently Completed (2024-2025)

### January 2025
- **`__format__` Dunder Method**: Python-style format() builtin with custom formatting support
  - **Python-style format()**: Implemented format(value, format_spec='') builtin function
  - **Dunder method support**: Calls __format__(format_spec) on objects when available
  - **Built-in type fallback**: Falls back to str() conversion for types without __format__
  - **String handling**: Properly returns strings without repr quotes
  - **Replaced old format()**: Old sprintf-style format() didn't work with {} placeholders anyway
  - **Implementation**: FormatBuilder() in builtin/types.go:397-445
  - **Test integration**: test-missing-builtins.m28 added to test.sh
  - **Example usage**:
    ```python
    (class FormattableNumber ()
      (def __init__ (self value) (= self.value value))
      (def __format__ (self format_spec)
        (+ "NUM:" (str self.value))))
    (= num (FormattableNumber 42))
    (format num ".2f")  # Returns "NUM:42"
    (format 123)        # Returns "123"
    (format "hello")    # Returns "hello"
    ```
  - All 50 tests passing (was 49, added 1 new test)
  - Completes __format__ dunder method from roadmap
  - Note: __bytes__ already implemented, __complex__ blocked on complex number type
- **`__index__` Dunder Method**: Python-compatible integer index conversion protocol
  - **CallIndex() helper**: Calls `__index__` dunder method on objects (common/types/dunder.go)
  - **ToIndex() converter**: Standard way to convert values to indices with __index__ support
  - **NumberValue.__index__**: Integer validation for built-in numbers (must be whole numbers)
  - **List indexing integration**: listMethodGetItem() uses toIndex() helper
  - **Slice operations support**: SliceForm() and handleSliceObject() use types.ToIndex()
  - **Custom class support**: User-defined classes can implement __index__ for indexing
  - **Proper validation**: Returns error for non-integer values or missing __index__
  - **Example usage**:
    ```python
    (class MyIndex
      (def __init__ (self value) (= self.value value))
      (def __index__ (self) self.value))
    (= idx (MyIndex 2))
    lst[idx]        # Works! Returns lst[2]
    lst[idx:4]      # Works! Returns lst[2:4]
    ```
  - Comprehensive test suite: tests/test-index-dunder.m28 with 10 test scenarios
  - All 37 existing tests passing + new __index__ tests
  - Completes high-priority Type Conversion dunder method from roadmap
- **Enhanced Error Messages**: Token-based error reporting with source context and suggestions
  - **ParseError type** with precise source location tracking (line, column, lexeme, filename)
  - **Source context display**: Shows 2-3 lines before/after error with line numbers
  - **Caret indicators**: Points exactly to error location with `^` or `^^^` for multi-char tokens
  - **Unclosed delimiter tracking**: "expected ']' to match '[' at line 3, column 1"
  - **Better runtime errors**: NameError shows variable name, preserves details through wrapping
  - **"Did you mean?" suggestions**: Levenshtein-based similarity matching for typos
  - **Example improvements**:
    - Before: `Error: parse error: unclosed vector`
    - After: Shows exact location with source snippet and helpful message
    - Before: `EvalError: name error`
    - After: `NameError: name 'prin' is not defined\n\nDid you mean: in, print?`
  - Integrated with ErrorReporter for consistent formatting
  - All error types use structured formatting with position info
  - No test regressions (100% pass rate maintained)
- **Complete Tokenizer Integration**: Token-based parsing with 100% test pass rate
  - **Token infrastructure**: TOKEN_NUMBER, TOKEN_STRING, TOKEN_IDENTIFIER, etc. with position tracking
  - **Token navigation**: currentToken(), advanceToken(), expectToken(), matchToken()
  - **Position-aware parsing**: Every token has Line, Col, StartPos, EndPos fields
  - **Infix operator detection**: Uses token position (no whitespace = infix)
  - **Parser methods migrated**: parseAtomFromToken(), parseListFromToken(), parsePostfixFromToken()
  - **Special handling**:
    - Negative literals: `-1` as single token (MINUS + NUMBER with no whitespace)
    - F-strings: Check for `f"..."` before general identifier scanning
    - Kebab-case: Identifiers support dashes (get-item, set-item)
    - Varargs: Transform `(* args)` into `*args` symbol during parameter parsing
  - **Error improvements**: Token positions enable precise error locations
  - **Whitespace significance**: Method calls vs property access detected via token spacing
  - All 37 integration tests passing
  - Foundation for future enhancements (AST with positions, better debugging)
- **JSONL Streaming Support**: Record-oriented JSON processing for data pipelines
  - Five core functions: `parse-jsonl-line`, `format-jsonl-line`, `read-jsonl`, `write-jsonl`, `append-jsonl-line`
  - Type-safe JSON ↔ M28 conversion (handles all M28 types: dicts, lists, strings, numbers, bools, nil)
  - Eager file loading with `read-jsonl` (loads entire file into memory)
  - Write modes: overwrite (default) or append for incremental processing
  - Automatic empty line handling (skipped during parsing)
  - Perfect for log processing, data streams, API responses, ETL pipelines
  - Seamless integration with path-ops (`get-path`), dict-ops (`select-keys`, `rename-keys`), merge-ops (`merge`, `deep-merge`)
  - Composable with functional operations (`filter`, `map`, `reduce`)
  - Implementation in builtin/jsonl.go with helper functions for type conversion
  - Test suite demonstrates all functionality (tests/test-jsonl.m28)
- **Top-Level Infix Operator Fix**: REPL now intuitively handles infix expressions
  - **Problem**: `1 + 2` at REPL returned `2` (last of three atoms), not `3`
  - **Solution**: Parser detects infix patterns at top level before wrapping in `(do ...)`
  - **Impact**: Natural REPL experience - `1 + 2` → `3` as users expect
  - Graceful fallback: if infix parsing fails, falls back to traditional behavior
  - 100% backward compatible: all existing code works unchanged
  - **Examples that now work naturally at REPL**:
    - `1 + 2` → `3` ✓ (was `2` before)
    - `5 - 3` → `2` ✓
    - `1 + 2 * 3` → `7` ✓ (correct precedence)
    - `2 ** 3` → `8` ✓
    - `5 > 3` → `True` ✓
  - Prefix notation still works: `(+ 1 2)` → `3`
  - Inside-paren infix still works: `(1 + 2)` → `3`
  - Implementation: 8-line addition to parser/parser.go Parse() method
  - Test coverage: tests/test-infix-repl-simple.m28
  - All 37 existing tests pass
- **Phase 2.5 Infix Operator Support**: Natural, Pythonic syntax for operators
  - Infix notation inside parentheses: `(x + y)` instead of `(+ x y)`
  - Smart context-aware parsing: detects infix vs prefix automatically
  - Pratt parser with Python-compatible operator precedence
  - 30+ operators: arithmetic (+, -, *, /, //, %, **), comparison (==, !=, <, >, <=, >=), logical (and, or, not, in), assignment (=, +=, -=, etc.)
  - Right-associativity for exponentiation and assignment
  - Graceful fallback: if infix parsing fails, falls back to prefix (handles edge cases like `(reduce + 0 lst)`)
  - Floor division operator (//) implementation with protocol-based dispatch
  - Updated tuple syntax: `(tuple ...)` instead of `%(...)` for clarity
  - 5 comprehensive example programs demonstrating real-world usage
  - 100% backward compatible - all existing prefix code works unchanged
  - All 37 tests passing
- **Complete Macro System**: Full Lisp-style macros with Pythonic syntax
  - **Phase 1**: `@macro` decorator implementation
    - Decorator syntax: `(@macro (def name (params) body))`
    - Macros receive unevaluated arguments (key difference from functions)
    - Macro expansion at call time with two-phase evaluation
    - `__macro__` attribute marks functions as macros
  - **Phase 2**: Quasiquote/unquote system
    - `(quasiquote expr)` - like quote but allows selective evaluation
    - `(unquote expr)` - evaluate expression within quasiquote
    - `(unquote-splicing expr)` - splice list elements
    - Recursive expansion with proper nesting support
  - **Phase 3**: Built-in utility macros
    - `unless` and `when` - conditional execution macros
    - `->` (thread-first) - thread value as first argument through pipeline
    - `->>` (thread-last) - thread value as last argument through pipeline
  - **Phase 4**: Reader macro syntax (syntactic sugar)
    - `` `expr `` for quasiquote (backtick)
    - `,expr` for unquote (comma)
    - `,@expr` for unquote-splicing (comma-at)
    - Parser-level transformations for cleaner macro code
    - Smart comma handling: commas followed by whitespace are separators, commas followed by non-whitespace are reader macros
  - Full test coverage with comprehensive examples
  - Complete documentation in docs/MACROS.md
- **Pythonic Path Operations**: Clojure-inspired nested data access with Python-style syntax
  - `get-path` - Get value at nested path with optional default: `(get-path user "address.city" "Unknown")`
  - `set-path` - Immutable set returns new structure: `(set-path data "users.0.name" "Alice")`
  - `update-path` - Apply function at path: `(update-path config "cache.ttl" (lambda (x) (* x 2)))`
  - `has-path?` - Check path existence: `(has-path? data "users.0.profile.email")`
  - String paths with dot notation: `"user.address.city"` parses to `["user", "address", "city"]`
  - Automatic numeric index parsing: `"users.0.name"` parses to `["users", 0, "name"]`
  - List paths for complex keys: `["users", "contact.info", 0]` handles keys with dots
  - Multi-tier data structure support: dicts (key access), lists/tuples (index access), objects (attribute access)
  - Immutable updates preserve original structures (functional programming style)
  - Comprehensive test suite with 15 test scenarios (tests/test-path-ops.m28)
- **Pythonic Dict Operations**: Functional dict manipulation with composable operations
  - `select-keys` - Extract subset of keys, skips missing: `(select-keys user ["name" "email"])`
  - `rename-keys` - Dual-mode renaming with collision detection: `(rename-keys data {"old": "new"})` or `(rename-keys data upper)`
  - `map-keys` - Transform all keys with function: `(map-keys dict (lambda (k) (+ "prefix_" k)))`
  - `map-values` - Transform all values with function: `(map-values prices (lambda (p) (* p 1.1)))`
  - `filter-keys` - Keep keys matching predicate: `(filter-keys data (lambda (k) (not (k.startswith "internal_"))))`
  - `filter-values` - Keep values matching predicate: `(filter-values scores (lambda (s) (>= s 90)))`
  - All operations immutable (return new dict, original unchanged)
  - Collision detection prevents duplicate keys in rename operations
  - Fully composable with threading macros: `(-> data (select-keys [...]) (rename-keys {...}) (map-values ...))`
  - Works seamlessly with nested structures and path operations
  - Comprehensive test suite with 13 test scenarios (tests/test-dict-ops.m28)
- **Pythonic Merge Operations**: Deep merge and transformations with predictable semantics
  - `merge` - Shallow merge multiple dicts: `(merge dict1 dict2 dict3)`
  - `deep-merge` - Recursive merge of nested structures: `(deep-merge config1 config2)`
  - `merge-with` - Custom conflict resolution: `(merge-with + dict1 dict2)` sums conflicting values
  - Later-wins semantics (simple, predictable, matches Python's `{**dict1, **dict2}`)
  - Type mismatch handling: later value replaces earlier (no errors, practical defaults)
  - List handling: later list replaces earlier (not concatenated, explicit behavior)
  - Immutable operations return new dicts (functional programming style)
  - Variadic arguments for merging unlimited dicts
  - Fully composable: `(-> base (deep-merge overrides) (select-keys [...]) (update-path ...))`
  - Replaced buggy old merge implementation that used deprecated Set() method
  - Comprehensive test suite with 16 test scenarios covering all edge cases (tests/test-merge-ops.m28)

### December 2024
- **S-String Implementation (Phases 1 & 2)**: Syntax strings for metaprogramming and code generation
  - Parser support for s"..." and rs"..." (raw s-strings)
  - Five interpolation types: {x} value, {=x} code, {*x} splice, {**x} dict-splice, {x#} gensym
  - Evaluator generates AST from templates with interpolation
  - Parse-time evaluation captures values from context
  - Comprehensive test suite demonstrating all features
  - Foundation for macro system (Phase 3: defmacro pending)
- **Large Function Refactoring**: Broke down all 100+ line functions into smaller, focused components
  - LoadModule() refactored from 107 lines into 12 functions with single responsibilities
  - FormatDocEntry() refactored from 81 lines into 11 focused formatting functions
  - Repr() refactored from 76 lines into 13 specialized representation functions
  - Improved testability, maintainability, and code clarity
  - All tests pass (100% success rate after refactoring)
- **Decimal.Decimal Implementation**: Full high-precision decimal arithmetic with Python API compatibility
  - Exact decimal representation (no float rounding errors)
  - Complete arithmetic and comparison operators
  - Rich method set: sqrt, quantize, normalize, as_tuple, to_integral
  - Context management with getcontext/setcontext
  - Mixed-type operations (Decimal + int/float)
  - Comprehensive test coverage
- **Dictionary and Set Comprehensions**: Full support for `{k: v for ...}` and `{x for ...}` with conditions
- **Frozenset Implementation**: Immutable, hashable set type with full method support
- **Type System Migration Complete**: All builtin functions migrated to validation framework and type helpers
- **F-String Enhancements**: Format specifications, alignment, conversion flags, self-documenting expressions
- **Argument unpacking**: `func(*args, **kwargs)` in function calls
- **Container Protocol Implementation**: All container protocols for built-in types
- **Bytes and ByteArray Implementation**: Full implementation with methods
- **Code Quality Improvements**: Replaced panic() with proper error handling

### November 2024
- **InitializeTypeRegistry Refactoring**: Split 1152-line function into 10 modular files
- **Module/Builtin Separation**: Moved 7 modules to lazy-loaded, kept math as builtin
- **GetAttr Deduplication**: Created MethodRegistry pattern, 40-50% code reduction

### October 2024
- **Type System Phase 2**: Protocol interfaces, TypeSwitch builder, dunder utilities
- **Builtin Duplicate Cleanup**: Eliminated all 41 duplicate registrations
- **Iterator Protocol**: Full `__iter__` and `__next__` implementation
- **Super() Implementation**: Both bare super and explicit super(Class, instance)

### Earlier 2024
- Enhanced f-strings with nested quotes
- Full dot notation and indexing support
- All string, list, dict, set methods
- Module system with local imports
- Keyword arguments and default parameters

## 📊 Progress Tracking

### Current Metrics
- **Code reduction achieved**: 36% average in migrated builtins
- **Test pass rate**: 100% M28 tests passing
- **Builtins reduced**: From 110 to 69 (removed duplicates)
- **Files refactored with MethodRegistry**: 10 types

### Target Metrics
- 🎯 < 10 cyclomatic complexity for all functions
- 🎯 > 80% test coverage for core packages
- 🎯 < 5% code duplication
- 🎯 No functions > 100 lines
- 🎯 Zero panic() calls in non-test code

## 📝 Notes

For detailed architecture plans and implementation strategies, see:
- Architecture evolution details in [ARCHITECTURE.md](ARCHITECTURE.md) (to be created)
- GetAttr refactoring pattern in [GETATTR_REFACTORING.md](GETATTR_REFACTORING.md)

Implementation priorities are marked with:
- 🟡 HIGH PRIORITY - Core language features
- 🟡 HIGH VALUE - High impact improvements
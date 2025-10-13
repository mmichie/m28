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
- [ ] Path-based access: `get-in`, `assoc-in`, `update-in`
- [ ] JSONL streaming support
- [ ] Record operations: `select-keys`, `rename-keys`
- [ ] Deep merge and transformations

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
- [ ] AST nodes with source locations
- [ ] Context-aware error reporting
- [ ] Stack traces with function names
- [ ] "Did you mean?" suggestions

## 🔧 Code Quality & Refactoring

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

**Medium Priority - Missing Builtins:** ✅ COMPLETE (except format)
- [ ] Implement `format()` builtin with `__format__(format_spec)` dunder method
  - **NOTE**: Naming conflict with existing sprintf-style format() in builtin/string_format.go
  - Python's builtin format(value, format_spec) differs from current format(template, *values)
  - Needs resolution: either rename existing format() or implement dual behavior
- [x] `reversed()` - Implemented with `__reversed__()` dunder method support (builtin/iteration.go)
- [x] `bytes()` (core/type_registry_primitives.go:307) - Added `__bytes__()` dunder method support

**Status:**
- ✅ `float()` - Already has `__float__()` support (builtin/types.go:199)
- ✅ `len()` - Already has `__len__()` support (builtin/collections.go:297)
- ✅ Operators - All arithmetic operators now use protocol layer

#### Python Dunder Methods - Missing Implementations

##### Currently Implemented ✅
Arithmetic: `__add__`, `__sub__`, `__mul__`, `__truediv__`, `__floordiv__`, `__mod__`, `__pow__`, `__neg__`, `__abs__`
Reflected: `__radd__`, `__rsub__`, `__rmul__`, `__rtruediv__`, `__rfloordiv__`, `__rmod__`, `__rpow__`
Comparison: `__eq__`, `__ne__`, `__lt__`, `__le__`, `__gt__`, `__ge__`
Container: `__len__`, `__getitem__`, `__setitem__`, `__delitem__`, `__contains__`, `__iter__`, `__next__`
Bitwise (partial): `__and__`, `__or__`, `__xor__` (sets only), `__hash__`
Context Manager: `__enter__`, `__exit__`
Conversion: `__int__`, `__float__`, `__str__`, `__repr__`, `__bool__`
Lifecycle: `__init__`, `__call__`
Metaclass: `__instancecheck__`, `__subclasscheck__`

##### High Priority - Core Operations 🔴

**Unary Operations:**
- [ ] `__pos__` - Unary positive (+x)
- [ ] `__invert__` - Bitwise NOT (~x)

**Bitwise Operations (for int types):**
- [ ] `__lshift__` - Left shift (<<)
- [ ] `__rshift__` - Right shift (>>)
- [ ] `__rand__` - Reflected AND
- [ ] `__ror__` - Reflected OR
- [ ] `__rxor__` - Reflected XOR
- [ ] `__rlshift__` - Reflected left shift
- [ ] `__rrshift__` - Reflected right shift

**Augmented Assignment (In-place Operations):**
- [ ] `__iadd__` - In-place addition (+=)
- [ ] `__isub__` - In-place subtraction (-=)
- [ ] `__imul__` - In-place multiplication (*=)
- [ ] `__itruediv__` - In-place true division (/=)
- [ ] `__ifloordiv__` - In-place floor division (//=)
- [ ] `__imod__` - In-place modulo (%=)
- [ ] `__ipow__` - In-place power (**=)
- [ ] `__iand__` - In-place AND (&=)
- [ ] `__ior__` - In-place OR (|=)
- [ ] `__ixor__` - In-place XOR (^=)
- [ ] `__ilshift__` - In-place left shift (<<=)
- [ ] `__irshift__` - In-place right shift (>>=)

**Attribute Access:**
- [ ] `__getattr__` - Fallback for undefined attributes
- [ ] `__getattribute__` - Intercept all attribute access
- [ ] `__setattr__` - Attribute assignment
- [ ] `__delattr__` - Attribute deletion
- [ ] `__dir__` - Directory listing for dir()

**Type Conversion:**
- [ ] `__bytes__` - Convert to bytes
- [ ] `__complex__` - Convert to complex number
- [ ] `__index__` - Convert to integer index (for slicing)
- [ ] `__format__` - Custom string formatting (for format())

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

### Parser Abstraction (Foundation)
- [ ] Define language-agnostic AST nodes in `core/ast/`
- [ ] Include source location info in every node for error reporting
- [ ] Design AST to support type annotations
- [ ] Create AST visitor pattern for transformations
- [ ] Create `Parser` interface with multiple implementations
- [ ] Move current S-expr parser to `parser/sexpr/`
- [ ] Add parser registry for file extension mapping
- [ ] Support for parsing strings, files, and streams
- [ ] Design a lower-level IR between AST and evaluation
- [ ] IR should be easier to optimize than AST
- [ ] Consider SSA form for future optimizations
- [ ] Support for type information in IR

### Error Handling Enhancement
- [ ] Add line/column info to all AST nodes
- [ ] Implement source span tracking (start/end positions)
- [ ] Keep original source text for error display
- [ ] Support for multi-file error traces
- [ ] Rich error types with structured information
- [ ] Error recovery in parser (continue after errors)
- [ ] Colorized error output with source highlights
- [ ] Error explanation system (like Rust's `--explain`)

### Type System Foundation
- [ ] Type AST nodes (for type annotations)
- [ ] Type inference infrastructure
- [ ] Gradual typing support (typed + untyped code)
- [ ] Generic/parametric types foundation
- [ ] Optional type checking phase
- [ ] Type error reporting using diagnostic framework
- [ ] Type-directed optimizations
- [ ] Runtime type checking for gradual typing

### Multiple Frontend Support
- [ ] Python 3.x grammar implementation
- [ ] Transpiler option (Python → S-expr)
- [ ] Native parser option using parser combinator
- [ ] Handle Python-specific features (decorators, etc.)
- [ ] Mandatory type annotations for typed variant
- [ ] Stricter semantics (immutability by default)
- [ ] Additional keywords for mutability opt-in
- [ ] Compile-time guarantees

### Optimization & Performance
- [ ] Design M28 HIR with dynamic operation support
- [ ] Include type guards and profiling points
- [ ] Support for gradual lowering to different backends
- [ ] Deoptimization points for returning to interpreter
- [ ] Bytecode VM with register-based design
- [ ] Inline caching for method lookups
- [ ] Quickening (bytecode specialization)
- [ ] Computed goto dispatch (if supported)
- [ ] Type annotation validator
- [ ] LLVM IR generation for typed functions
- [ ] FFI boundary between compiled and interpreted code
- [ ] Benchmark common numeric kernels
- [ ] Runtime type profiling infrastructure
- [ ] Hot path detection
- [ ] Speculative optimization with guards
- [ ] On-stack replacement (OSR) for hot loops

### Developer Experience & Tooling
- [ ] Basic LSP implementation
- [ ] Syntax highlighting
- [ ] Auto-completion
- [ ] Go-to-definition
- [ ] Inline error display
- [ ] Debug adapter protocol (DAP)
- [ ] Code formatter (like gofmt/black)
- [ ] Linter with configurable rules
- [ ] Documentation generator
- [ ] Package manager

### Community & Ecosystem
- [ ] Plugin system for extensions
- [ ] FFI for C extensions
- [ ] Web playground
- [ ] Comprehensive documentation
- [ ] Standard library expansion

## ✅ Recently Completed (2024-2025)

### January 2025
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
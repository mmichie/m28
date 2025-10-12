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

#### Protocol wrapper implementations
- [ ] DunderIterator wrapper (core/protocols/iterable.go:273)
- [ ] DunderNumeric wrapper (core/protocols/numeric.go:181)
- [ ] DunderIndexable wrapper (core/protocols/indexable.go:251)

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
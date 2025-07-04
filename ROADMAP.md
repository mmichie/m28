# M28 Language Roadmap

This is the single source of truth for M28 development.

## 🎯 Current Priorities

### 1. Module/Builtin Separation ✅ COMPLETE
Successfully separated modules from builtins:
- **Modules extracted**: os, json, pathlib, random, time, datetime, shutil
- **Kept as builtins**: len(), print(), type(), range(), type conversions, math (special case)
- **Benefits achieved**: Lazy loading, cleaner architecture, Python-like imports

### 1. Type System Phase 2 ✅ COMPLETE
- [x] Protocol interfaces (Numeric, Indexable, Container)
- [x] TypeSwitch builder for complex type handling
- [x] Dunder method utilities (CallDunder, HasDunder)
- [x] Operator migration to protocol-based dispatch

### 2. Macro System with S-Strings 🟡 HIGH PRIORITY
- [ ] S-string syntax: `s"(+ {x} 1)"` with interpolation
- [ ] `defmacro` for defining macros
- [ ] Hygienic macro support
- [ ] Built-in threading macros (`->`, `->>`)

### 3. Record Stream & JSON Processing 🟡 HIGH VALUE
- [ ] Path-based access: `get-in`, `assoc-in`, `update-in`
- [ ] JSONL streaming support
- [ ] Record operations: `select-keys`, `rename-keys`
- [ ] Deep merge and transformations

## ✅ Recently Completed

### Bytes and ByteArray Implementation
- Implemented `bytes` immutable byte sequence type
- Implemented `bytearray` mutable byte sequence type
- Added full indexing support through protocol adapters
- Implemented methods: decode(), hex(), append() (bytearray), extend() (bytearray), clear() (bytearray)
- Added string encode() method to convert strings to bytes
- Full support for iteration, slicing, and __contains__ protocol
- Proper __eq__ implementation for equality comparison

### Container Protocol Implementation
- Completed all container protocols for built-in types
- Lists, tuples, strings: __getitem__ and __contains__ (immutable by design)
- Dicts: full support for __getitem__, __setitem__, __delitem__, __contains__
- Fixed protocol adapters to match M28's immutability design

### Code Quality Improvements
- Cleaned up debug code in parser/parser.go
- Replaced panic() with log.Fatal() in initialization functions
- Removed unused code containing panic() calls
- Maintained panic() in Must* pattern functions (Go convention)

### InitializeTypeRegistry Refactoring
- Split 1152-line god function into 10 modular files
- Organized by type category: primitives, collections, objects, I/O, concurrent
- Fixed dict/set mutation semantics to match Python behavior
- Maintained 100% test compatibility throughout refactoring

### Module/Builtin Separation
- Moved 7 modules from builtin to lazy-loaded modules
- Created enhanced module loader supporting Go modules
- Fixed dict key abstraction leak (removed "s:" prefix in JSON)
- All modules now require import (matching Python)
- Math module kept as builtin for convenience

### Type System Improvements (Phase 1)
- **23 builtin files migrated** with 15-50% code reduction
- Created comprehensive type assertion helpers
- Established validation framework patterns
- 2 complex files deferred (essential_builtins.go, datetime.go)

### Builtin Duplicate Cleanup
- Eliminated all 41 duplicate registrations
- Reduced from 110 to 69 total builtins
- Each function now has single source of truth

### Core Language Features
- Enhanced f-strings with nested quotes
- Full dot notation and indexing support
- All string, list, dict, set methods
- Module system with local imports
- Keyword arguments and default parameters
- Argument unpacking in function calls (*args, **kwargs)

## 🚧 In Progress

### Python Protocol Implementation
- [x] Arithmetic protocols (`__add__`, `__sub__`, etc.) ✅ via Type System Phase 2
- [x] Comparison protocols (`__lt__`, `__le__`, etc.) ✅ via Type System Phase 2
- [x] Iterator protocol (`__iter__`, `__next__`) ✅ COMPLETE
- [x] Container protocols (`__getitem__`, `__setitem__`, `__delitem__`, `__contains__`) ✅ COMPLETE

### Better Error Messages
- [ ] AST nodes with source locations
- [ ] Context-aware error reporting
- [ ] Stack traces with function names
- [ ] "Did you mean?" suggestions

## ❌ Not Yet Implemented

### Language Features
- [x] Argument unpacking: `func(*args, **kwargs)` ✅ COMPLETE (function calls only, not parameter definitions)
- [ ] Multiple inheritance
- [x] Proper `super()` calls ✅ COMPLETE (supports both bare super and explicit super(Class, instance))
- [ ] Generator expressions
- [ ] Dictionary/set comprehensions
- [ ] Decorators with `@` syntax
- [ ] `eval()` and `exec()`

### Data Types
- [x] `bytes` and `bytearray` ✅ COMPLETE
- [ ] `complex` numbers
- [ ] `frozenset`
- [ ] `decimal.Decimal`
- [ ] Collections module types (deque, Counter, defaultdict)

### F-String Enhancements
- [ ] Format specifications: `f"{pi:.2f}"`
- [ ] Alignment: `f"{text:>10}"`
- [ ] Conversion flags: `f"{value!r}"`
- [ ] Self-documenting: `f"{x+y=}"`

## 🔧 Technical Debt

### Code Quality Improvements
- [x] **Refactor InitializeTypeRegistry god function** ✅ COMPLETE
  - [x] Split 1152-line function into 10 modular files by type category
  - [x] Fixed dict/set mutation semantics to match Python
  - [x] Removed old god function entirely (reduced file from 1154 to 24 lines)
  - [ ] Use data-driven approach with type definitions
  - [ ] Consider code generation for repetitive patterns
- [x] **Replace panic() with proper error handling** ✅ COMPLETE
  - [x] core/list_methods.go, dict_methods.go, set_methods.go - replaced with log.Fatal()
  - [x] Removed unused panic() in builtin/utilities.go
  - [x] Left intentional panic() in Must* pattern functions
- [x] **Deduplicate GetAttr implementations** ✅ COMPLETED
  - [x] Created MethodRegistry pattern in core/method_registry.go
  - [x] Refactored 10 types: Range, Generator, File, Task, Channel, BuiltinFunction, Path, etc.
  - [x] Achieved 40-50% code reduction in GetAttr methods
  - [ ] Document the refactoring pattern for contributors (see GETATTR_REFACTORING.md)
- [ ] **Standardize error handling patterns**
  - [ ] Define clear error types in core/error.go
  - [ ] Use consistent error wrapping throughout
- [x] **Clean up debug code** ✅ COMPLETE
  - [x] Removed commented debug prints in parser/parser.go
  - [x] No need for logging framework at this time
- [ ] **Fix import organization** (follow Go conventions)

### Unimplemented Features from TODOs
- [ ] **Protocol wrapper implementations**
  - [ ] DunderIterator wrapper (core/protocols/iterable.go:273)
  - [ ] DunderNumeric wrapper (core/protocols/numeric.go:181)
  - [ ] DunderIndexable wrapper (core/protocols/indexable.go:251)
- [ ] **Missing core functionality**
  - [ ] Sentinel iterator in iter() (builtin/iteration.go:65)
  - [ ] Dict initialization from pairs (core/type_registry.go:925)
  - [ ] Container protocol __len__ and __contains__ checks (core/protocols/container.go:176)

### Container Protocols Implementation
- [ ] Create Indexable protocol adapters for List, Dict, Tuple, String, Range
- [ ] Update indexing operations in eval/indexing.go
- [ ] Add DelItemForm for del operations
- [ ] Integrate with three-tier dispatch: dunder → protocol → type-specific
- [ ] Add comprehensive tests for protocol-based indexing

### Type System Migration (Ongoing)
- [ ] Complete migration of remaining builtin files to use type helpers
  - [ ] builtin/iteration.go - iterable type switches
  - [ ] builtin/attributes.go - object type checking
  - [ ] eval/evaluator.go - special forms validation
  - [ ] builtin/modules/math.go - numeric validation
- [ ] Migrate high-priority files with 40+ type checks:
  - [ ] builtin/string_search.go (~42 manual checks)
  - [ ] builtin/os.go (~35+ manual checks)
  - [ ] builtin/json.go (complex validation)
  - [ ] builtin/pathlib.go (~20+ type checks)

### Architecture Improvements
- [ ] Package reorganization (core package doing too much)
- [ ] Visitor pattern for AST
- [ ] Break down 100+ line functions:
  - [ ] core/module_loader_enhanced.go: LoadModule() - 107 lines
  - [x] ~~core/context_manager.go: FileContextManager.GetAttr() - 99 lines~~ ✅ Refactored
  - [ ] core/doc_registry.go: FormatDocEntry() - 81 lines
  - [ ] core/repr.go: Repr() - 76 lines
- [ ] Apply validation framework everywhere
- [ ] Consider using generics (Go 1.18+) for common patterns

### Testing & Quality
- [ ] Add Go unit tests (currently only M28 tests)
- [ ] Property-based testing for parser
- [ ] Performance benchmarks
- [ ] Documentation testing framework

## 📊 Success Metrics

**Completed**:
- ✅ 36% average code reduction in migrated builtins
- ✅ 100% test pass rate maintained
- ✅ All core Python syntax working

**Target**:
- 🎯 < 10 cyclomatic complexity for all functions
- 🎯 > 80% test coverage for core packages
- 🎯 < 5% code duplication
- 🎯 No functions > 100 lines
- 🎯 Zero panic() calls in non-test code

## 🏗️ Architecture Evolution

### Phase 1: Parser Abstraction (Foundation)
**Goal**: Separate parsing from evaluation to support multiple syntaxes

#### 1.1 Common AST Definition
- [ ] Define language-agnostic AST nodes in `core/ast/`
- [ ] Include source location info in every node for error reporting
- [ ] Design AST to support type annotations (even if not used yet)
- [ ] Create AST visitor pattern for transformations

#### 1.2 Parser Interface
- [ ] Create `Parser` interface with multiple implementations
- [ ] Move current S-expr parser to `parser/sexpr/`
- [ ] Add parser registry for file extension mapping
- [ ] Support for parsing strings, files, and streams

#### 1.3 Intermediate Representation (IR)
- [ ] Design a lower-level IR between AST and evaluation
- [ ] IR should be easier to optimize than AST
- [ ] Consider SSA form for future optimizations
- [ ] Support for type information in IR

### Phase 2: Error Handling Enhancement
**Goal**: Rust/Elm-quality error messages with helpful suggestions

#### 2.1 Source Tracking
- [ ] Add line/column info to all AST nodes
- [ ] Implement source span tracking (start/end positions)
- [ ] Keep original source text for error display
- [ ] Support for multi-file error traces

#### 2.2 Error Reporting System
- [ ] Rich error types with structured information
- [ ] Error recovery in parser (continue after errors)
- [ ] "Did you mean?" suggestions using edit distance
- [ ] Colorized error output with source highlights
- [ ] Error explanation system (like Rust's `--explain`)

### Phase 3: Type System Foundation
**Goal**: Prepare for gradual or mandatory typing

#### 3.1 Type Representation
- [ ] Type AST nodes (for type annotations)
- [ ] Type inference infrastructure
- [ ] Gradual typing support (typed + untyped code)
- [ ] Generic/parametric types foundation

#### 3.2 Type Checking Pipeline
- [ ] Optional type checking phase
- [ ] Type error reporting using diagnostic framework
- [ ] Type-directed optimizations
- [ ] Runtime type checking for gradual typing

### Phase 4: Multiple Frontend Support
**Goal**: Support Python syntax and custom typed variant

#### 4.1 Python Parser
- [ ] Python 3.x grammar implementation
- [ ] Transpiler option (Python → S-expr)
- [ ] Native parser option using parser combinator
- [ ] Handle Python-specific features (decorators, etc.)

#### 4.2 Typed Python Variant
- [ ] Mandatory type annotations
- [ ] Stricter semantics (immutability by default)
- [ ] Additional keywords for mutability opt-in
- [ ] Compile-time guarantees

### Phase 5: Optimization & Backends
**Goal**: Make M28 performant through various optimization techniques

#### 5.1 High-Level IR (HIR) Design
**Why**: LLVM IR doesn't work well for dynamic languages; need our own IR first
- [ ] Design M28 HIR with dynamic operation support
- [ ] Include type guards and profiling points
- [ ] Support for gradual lowering to different backends
- [ ] Deoptimization points for returning to interpreter

Example HIR for `(+ a b)`:
```
ADD_DYNAMIC %a, %b → %result
  guards: [
    if type(%a) == int && type(%b) == int → ADD_INT
    if type(%a) == str && type(%b) == str → CONCAT_STR
    else → CALL_METHOD %a.__add__(%b)
  ]
```

#### 5.2 Interpreter Optimizations
- [ ] Bytecode VM with register-based design
- [ ] Inline caching for method lookups
- [ ] Quickening (bytecode specialization)
- [ ] Computed goto dispatch (if supported)

#### 5.3 Typed Subset → LLVM
**Strategy**: Only compile fully-typed code to LLVM, keep dynamic parts interpreted
- [ ] Type annotation validator
- [ ] LLVM IR generation for typed functions
- [ ] FFI boundary between compiled and interpreted code
- [ ] Benchmark common numeric kernels

```python
# This compiles to LLVM
@compile
def fib(n: int) -> int:
    return n if n < 2 else fib(n-1) + fib(n-2)

# This stays interpreted  
def dynamic_func(x):
    return x.whatever()
```

#### 5.4 Profile-Guided Optimization
- [ ] Runtime type profiling infrastructure
- [ ] Hot path detection
- [ ] Speculative optimization with guards
- [ ] On-stack replacement (OSR) for hot loops

#### 5.5 Backend Targets
- [ ] **Interpreter** (current, keep as fallback)
- [ ] **Bytecode VM** (2-5x speedup)
- [ ] **LLVM** (typed code only, 50-200x for numeric)
- [ ] **Go codegen** (easier FFI, reuse Go's GC)
- [ ] **WASM** (via LLVM or direct)
- [ ] **JavaScript** (for browser embedding)

#### 5.6 Compilation Pipeline
```
┌─────────────┐
│  M28 Source │
└──────┬──────┘
       ↓
┌──────▼──────┐
│     AST     │
└──────┬──────┘
       ↓
┌──────▼──────┐
│   M28 HIR   │ ← High-level IR (dynamic ops)
└──────┬──────┘
       ├─────────────┬────────────┬─────────────┐
       ↓             ↓            ↓             ↓
┌──────▼──────┐ ┌───▼────┐ ┌────▼────┐ ┌──────▼──────┐
│ Interpreter │ │Bytecode│ │Type Spec│ │ LLVM (typed)│
└─────────────┘ └────────┘ └─────────┘ └─────────────┘
```

### Phase 6: Developer Experience
**Goal**: Professional tooling and debugging support

#### 6.1 Language Server Protocol (LSP)
- [ ] Basic LSP implementation
- [ ] Syntax highlighting
- [ ] Auto-completion
- [ ] Go-to-definition
- [ ] Inline error display

#### 6.2 Debugging & Tools
- [ ] Debug adapter protocol (DAP)
- [ ] Code formatter (like gofmt/black)
- [ ] Linter with configurable rules
- [ ] Documentation generator
- [ ] Package manager

## 🗺️ Long-term Vision

### Architecture Goals
```
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│   Python    │  │   S-Expr    │  │Typed Python │
│   Parser    │  │   Parser    │  │   Parser    │
└──────┬──────┘  └──────┬──────┘  └──────┬──────┘
       │                 │                 │
       └─────────────────┴─────────────────┘
                         │
                    ┌────▼────┐
                    │   AST   │
                    │ + Types │
                    └────┬────┘
                         │
                  ┌──────▼──────┐
                  │Type Checker │ (Optional)
                  └──────┬──────┘
                         │
                  ┌──────▼──────┐
                  │ Optimizer   │
                  └──────┬──────┘
                         │
                  ┌──────▼──────┐
                  │  Evaluator  │
                  │      or     │
                  │   Backend   │
                  └─────────────┘
```

### Implementation Timeline
- **Near Term (3-6 months)**: AST extraction, error improvements, parser interface
- **Medium Term (6-12 months)**: Python parser, type annotations, basic optimizations
- **Long Term (1-2 years)**: Full Python support, typed variant, LSP, native compilation

### Community & Ecosystem
- [ ] Plugin system for extensions
- [ ] FFI for C extensions
- [ ] Web playground
- [ ] Comprehensive documentation
- [ ] Standard library expansion

---

## Appendix: Feature Details

<details>
<summary>Completed Features (Click to expand)</summary>

### Core Language ✅
- S-expression syntax with Python semantics
- All primitive types and operators
- Control flow (if, for, while, try/except)
- Functions with *args, **kwargs, defaults
- Classes with inheritance
- Generators with yield
- Context managers
- Module system with imports

### Data Structures ✅
- Lists, dicts, tuples, sets
- List comprehensions
- All methods implemented
- Range objects

### Built-in Functions ✅
- Type operations: type, isinstance, issubclass
- Collections: len, sorted, reversed, enumerate
- Math: abs, min, max, sum, round, divmod
- Functional: map, filter, reduce
- I/O: print, input, open

### Advanced Features ✅
- REPL with readline support
- Tail call optimization
- Method chaining
- Property access
- Augmented assignment

</details>

<details>
<summary>Implementation Notes</summary>

### Module/Builtin Separation Strategy
1. Create proper module loading infrastructure
2. Move module code to separate packages
3. Update import system for lazy loading
4. Benchmark startup time improvements

### Type System Phase 2 Design
1. Define protocol interfaces in `core/protocols`
2. Implement TypeSwitch builder pattern
3. Create dunder method utilities
4. Migrate existing code to use protocols

### Macro System Implementation
1. S-string parser (2-3 weeks)
2. Macro definition and expansion (3-4 weeks)
3. Built-in macros (2-3 weeks)
4. Testing and documentation (1-2 weeks)

### Compilation Strategy Rationale
**Why not pure LLVM?** Dynamic languages need:
- Runtime type information
- Dynamic dispatch
- Reflection/introspection
- Deoptimization capability

**Our approach:**
1. Build M28 HIR that understands dynamic operations
2. Use LLVM only for typed, performance-critical code
3. Keep interpreter for dynamic features
4. Profile-guided optimization for hot paths

This follows successful implementations like V8, PyPy, and Julia rather than trying to force everything through LLVM.

</details>
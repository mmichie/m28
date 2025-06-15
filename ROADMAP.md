# M28 Language Roadmap

This is the single source of truth for M28 development.

## 🎯 Current Priorities

### 1. Module/Builtin Separation ✅ COMPLETE
Successfully separated modules from builtins:
- **Modules extracted**: os, json, pathlib, random, time, datetime, shutil
- **Kept as builtins**: len(), print(), type(), range(), type conversions, math (special case)
- **Benefits achieved**: Lazy loading, cleaner architecture, Python-like imports

### 1. Type System Phase 2 🔴 NEW PRIORITY
- [ ] Protocol interfaces (Numeric, Indexable, Container)
- [ ] TypeSwitch builder for complex type handling
- [ ] Dunder method utilities (CallDunder, HasDunder)
- [ ] Type error standardization

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

## 🚧 In Progress

### Python Protocol Implementation
- [ ] Arithmetic protocols (`__add__`, `__sub__`, etc.)
- [ ] Comparison protocols (`__lt__`, `__le__`, etc.)
- [ ] Container protocols (`__getitem__`, `__setitem__`)
- [ ] Iterator protocol (`__iter__`, `__next__`)

### Better Error Messages
- [ ] AST nodes with source locations
- [ ] Context-aware error reporting
- [ ] Stack traces with function names
- [ ] "Did you mean?" suggestions

## ❌ Not Yet Implemented

### Language Features
- [ ] Argument unpacking: `func(*args, **kwargs)`
- [ ] Multiple inheritance
- [ ] Proper `super()` calls
- [ ] Generator expressions
- [ ] Dictionary/set comprehensions
- [ ] Decorators with `@` syntax
- [ ] `eval()` and `exec()`

### Data Types
- [ ] `bytes` and `bytearray`
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

### Architecture Improvements
- [ ] Package reorganization
- [ ] Visitor pattern for AST
- [ ] Break down 100+ line functions
- [ ] Apply validation framework everywhere

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

## 🗺️ Long-term Vision

### Performance
- [ ] Bytecode compilation
- [ ] JIT compilation
- [ ] Parallel execution

### Developer Experience
- [ ] Language Server Protocol (LSP)
- [ ] Debugger support
- [ ] Package manager
- [ ] Standard library

### Community
- [ ] Plugin system
- [ ] FFI for C extensions
- [ ] Web playground
- [ ] Comprehensive documentation

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

</details>
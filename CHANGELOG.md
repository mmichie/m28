# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2025-06-15

### Added

#### Protocol System
- **Iterator Protocol** (`__iter__`, `__next__`) - enables custom iteration for user-defined types
- **Container Protocols** (`__getitem__`, `__setitem__`, `__delitem__`) - enables custom indexing and item manipulation
- **Comparison Protocols** (`__eq__`, `__lt__`, `__le__`, etc.) - custom comparison operations
- **Arithmetic Protocols** (`__add__`, `__sub__`, `__mul__`, `__div__`, etc.) - operator overloading
- **String Protocols** (`__repr__`, `__str__`) - custom string representations
- **Boolean Protocol** (`__bool__`) - custom truthiness evaluation  
- **Length Protocol** (`__len__`) - custom length for containers
- **Contains Protocol** (`__contains__`) - custom membership testing

#### Type System Enhancements
- Complete Type System Phase 2 implementation with protocols and TypeSwitch builder
- Type assertion helpers providing 40-50% code reduction in type-heavy functions
- Protocol-based dispatch for all operators (three-tier: dunder → protocol → type)
- Comprehensive validation framework for consistent error handling

#### Language Features
- **Multiple assignment** syntax: `(= x y 10)` assigns 10 to both x and y
- **Tuple unpacking** in for loops: `(for [k v] (dict.items) ...)`
- **Lazy range objects** matching Python's behavior - ranges are now lazy iterators
- **Slice object creator**: `(slice start stop step)` for advanced slicing
- **Custom exceptions**: `raise` and `error` functions for exception handling
- **Enhanced operators** with proper variadic support and operator overloading

#### Developer Experience  
- Full readline support in REPL with history and tab completion
- Source location tracking with LocatedValue type for better error messages
- Improved error reporting with contextual information
- 100% test pass rate (37/37 tests) with new test infrastructure

#### Module System Enhancements
- Support for explicit `.m28` extension in imports
- List syntax `[name1 name2]` in from imports now works correctly
- Proper module caching between imports with and without `.m28` extension
- Key parameter support for sorted(), min(), and max() functions

#### Architecture Improvements
- Module/builtin separation - modules now lazy-loaded for better startup time
- Complete migration to builder framework achieving 36% code reduction
- Modular builtin registration preventing duplicates
- Operator abstraction layer for cleaner code

### Changed
- Lists are now immutable by default (matching functional programming principles)
- Range objects are now lazy iterators instead of eager lists
- All builtin functions migrated to use new type assertion helpers
- Operators use protocol-based dispatch for extensibility

### Fixed
- `list(range(n))` now correctly converts lazy ranges to lists
- Exception handling in file contexts works properly
- Dictionary 'in' operator and key representation fixed
- Variadic arithmetic operations work correctly with operator overloading
- Set literal parsing issues resolved
- All duplicate builtin registrations eliminated (reduced from 110 to 69 builtins)
- Module loader properly normalizes module names for caching
- List literal syntax in import statements correctly extracts symbols

### Developer Notes
- This release focuses on making M28 more extensible through protocols
- The protocol system enables Python-style "duck typing" for custom types
- Type system improvements make the codebase more maintainable
- All changes maintain backwards compatibility except for list immutability

## [0.1.0] - 2024-01-06

### Added
- Initial release of M28 programming language
- Core language features:
  - S-expression syntax with Python semantics
  - Python-style data structures (lists, dicts, sets, tuples)
  - Function definitions with `def` keyword
  - Variable assignment with `=` operator
  - Control flow (if/elif/else, while, for)
  - Exception handling (try/except/finally)
  - List comprehensions
  - Lambda functions
  - Dot notation for property/method access
  - F-string formatting
  - Module system with import/export
  - Generator functions with yield
  - Context managers (with statement)
  - Async/await support
  - Class definitions with inheritance

- Built-in modules:
  - `math` - Mathematical functions and constants
  - `os` - Operating system interface
  - `time` - Time-related functions
  - `datetime` - Date and time handling
  - `random` - Random number generation
  - `json` - JSON parsing and generation
  - `pathlib` - Object-oriented filesystem paths
  - `shutil` - High-level file operations

- Development tools:
  - Interactive REPL with history and completion
  - Syntax highlighting in REPL
  - Error reporting with line numbers
  - Module hot-reloading

### Fixed
- Module import system now properly detects circular dependencies
- Dictionary dot notation (dict.property) works correctly
- All test suite passes (10/10 tests)
- All examples run successfully (28/33, with 5 requiring specific setup)

### Known Issues
- Parser hangs on symbols starting with `:` (use `as` instead of `:as` for imports)
- Some Python standard library modules not yet implemented
- Limited macro support compared to traditional Lisps

### Security
- Module loading restricted to designated paths
- No arbitrary code execution from strings without explicit eval

### Notes
This is the first public release of M28. The language is functional and suitable for 
educational use and small projects. Performance optimizations and additional standard
library modules are planned for future releases.
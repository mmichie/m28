# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
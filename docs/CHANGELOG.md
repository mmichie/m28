# M28 Documentation Changelog

## v0.1.0 Documentation Update

### Added
- Comprehensive installation guide (`docs/installation.md`)
- Consolidated module documentation (`docs/modules.md`)
- Updated quick reference with accurate v0.1.0 syntax

### Changed
- Updated all documentation to reflect current M28 syntax
- Fixed import syntax examples (removed colons, added quotes where needed)
- Updated dictionary access patterns to show dot notation
- Clarified list mutability behavior (methods return new lists)
- Updated class syntax examples (empty parent list required)
- Corrected comparison operator usage (== for equality in conditions)

### Removed
- Redundant documentation files:
  - `docs/module-import.md` (merged into modules.md)
  - `docs/module-exports.md` (merged into modules.md)  
  - `docs/enhanced-module-system.md` (merged into modules.md)
  - `docs/features/python-style-dicts.md` (content in dictionaries.md)

### Fixed
- List comprehension examples now use prefix notation inside comprehensions
- Module exports use `__exports__` not `__all__`
- Exception handling examples show current syntax
- For loops are documented as working (not "future feature")

### Notes
- Examples in `examples/` directory serve as authoritative syntax reference
- Some advanced features like decorators are planned but not yet implemented
- List methods (append, sort, etc.) return new lists rather than modifying in place
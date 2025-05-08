# M28 Module System Enhancement Roadmap

This document outlines the planned improvements for the M28 module system and namespace functionality.

## High Priority Tasks

### 1. Update Base Module Implementation
- [ ] Replace deprecated `ioutil.ReadFile` with `os.ReadFile`
- [ ] Fix consistency issues in module symbol resolution
- [ ] Implement proper error handling with detailed error messages
- [ ] Add tests for edge cases in module loading

### 2. Enhance Namespace Control
- [ ] Implement a module-level `__exports__` mechanism to control public API
- [ ] Add support for private symbols (prefixed with underscore)
- [ ] Create clear separation between module-local and exported symbols
- [ ] Add namespace introspection utilities

### 3. Implement Hierarchical Module Structure
- [ ] Add support for package directories (with `__init__.m28` files)
- [ ] Implement nested module resolution for `import` statements
- [ ] Create a proper module path resolution system
- [ ] Add support for relative imports (e.g., `from ..module import symbol`)

## Medium Priority Tasks

### 4. Improve Module Loading
- [ ] Make module search paths configurable (not hard-coded)
- [ ] Add support for a module path environment variable
- [ ] Implement circular import detection and prevention
- [ ] Add module reloading capability for development

### 5. Enhance Import Functionality
- [ ] Add support for aliased imports (e.g., `import module as alias`)
- [ ] Implement multi-symbol imports with aliasing
- [ ] Add wildcard imports with filtering
- [ ] Create an import hook system for extensibility

### 6. Module Documentation
- [ ] Define a standard for module docstrings
- [ ] Implement a doc-extraction system
- [ ] Add a help function for viewing module documentation
- [ ] Create tooling for generating documentation from modules

## Low Priority Tasks

### 7. Advanced Features
- [ ] Implement lazy loading for modules
- [ ] Add dynamic module creation
- [ ] Support for namespace packages (multiple directories)
- [ ] Create a plugin system based on the module system

### 8. Performance Optimizations
- [ ] Optimize module loading and caching
- [ ] Implement bytecode caching for modules
- [ ] Improve symbol resolution performance
- [ ] Reduce memory usage for modules

## Implementation Notes

### Design Principles
1. **Simplicity**: Keep the implementation simple and maintainable
2. **Pythonic**: Follow Python's conventions where appropriate
3. **Lisp-compatible**: Maintain compatibility with Lisp-style code
4. **Robust**: Include comprehensive error handling
5. **Extensible**: Design for future enhancements

### Backward Compatibility
- All changes should maintain backward compatibility with existing M28 code
- Deprecated features should be marked clearly
- Migration paths should be provided for any breaking changes

### Documentation Requirements
- Each new feature must be documented in a user-friendly way
- Examples should be provided for all functionality
- Documentation should cover both basic and advanced use cases
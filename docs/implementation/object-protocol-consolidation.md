# Object Protocol Consolidation Plan

## Overview

This document outlines the plan to consolidate the M28 codebase to fully utilize the Object Protocol and remove redundant legacy code patterns. This plan extends the work described in [unified-object-protocol.md](unified-object-protocol.md) and addresses the challenges identified in [object-protocol-implementation.md](../object-protocol-implementation.md).

## Current Status and Challenges

As outlined in the implementation status:

1. **Multiple Access Mechanisms**: Several code paths exist for object property access:
   - Legacy `DotAccessible` interface
   - Direct type-specific handling
   - New `ObjProtocol` interface
   - Optimized fast paths for specific types

2. **Mixed Implementation Patterns**: Both old and new approaches are used across the codebase:
   - `dot_notation.go` uses a cascade of access methods
   - Type-specific special handling in switch statements
   - Parallel implementations in `dot.go` and `enhanced_dot_forms.go`

3. **Performance vs. Consistency**: Optimizations for specific types create inconsistencies and maintenance challenges

## Consolidation Plan

This plan fits into Phase 3 of the [implementation roadmap](../specification/implementation-roadmap.md) and builds on the Object System work already in progress.

### Phase 1: Complete Core Adapters (Weeks 1-2)

1. **Audit Existing Adapters**
   - Verify all built-in types have proper ObjProtocol adapters
   - Add missing adapters for any unsupported types
   - Ensure all adapters implement the full interface correctly

2. **Enhance Adapter Performance**
   - Improve caching mechanism for adapter creation
   - Add direct fast paths for common operations
   - Benchmark adapter overhead vs. direct access

3. **Update Tests**
   - Extend object-protocol-test.m28 with comprehensive coverage
   - Add tests for edge cases and legacy compatibility

### Phase 2: Streamline Access Functions (Weeks 3-4)

1. **Simplify Core Access Functions**
   - Refactor `AccessObjectMember` to use only FastGetPropFrom
   - Refactor `SetObjectMember` to use only FastSetPropOn
   - Maintain backward compatibility through adapters

2. **Standardize Error Handling**
   - Use consistent error messages and types
   - Improve error contextualization for property access
   - Document error handling patterns

3. **Remove Type-Specific Handling**
   - Replace switch statements with adapter pattern
   - Use consistent access patterns for all types
   - Create proper adapters for special cases

### Phase 3: Legacy Interface Deprecation (Weeks 5-6)

1. **Deprecate Legacy Interfaces**
   - Mark `DotAccessible` interface as deprecated
   - Add migration guidance in code comments
   - Create backward compatibility adapters

2. **Update Implementation Requirements**
   - Update existing types to use ObjProtocol
   - Replace direct `DotAccessible` method calls with ObjProtocol equivalents
   - Keep compatibility shims for external code

3. **Document Migration Path**
   - Update documentation on correct property access patterns
   - Add examples of migrating from old to new approaches
   - Create tutorials for best practices

### Phase 4: Dot Notation Standardization (Weeks 7-8)

1. **Consolidate on Enhanced Forms**
   - Make enhanced_dot_forms.go the standard implementation
   - Update all code to use enhanced implementation
   - Deprecate original dot.go implementation

2. **Improve Form Evaluation**
   - Ensure proper evaluation context propagation
   - Optimize common dot notation patterns
   - Add specialized handling for method calls

3. **Add Advanced Dot Features**
   - Implement safer nested access patterns
   - Add optional access patterns (e.g., for nil-safe navigation)
   - Document best practices

### Phase 5: Performance Optimization (Weeks 9-10)

1. **Benchmark Key Operations**
   - Measure property access performance
   - Compare with baseline performance
   - Identify bottlenecks

2. **Reduce Indirection**
   - Minimize layers of function calls
   - Add specialized fast paths for common patterns
   - Optimize adapter creation and caching

3. **Final Testing**
   - Run comprehensive test suite
   - Verify backward compatibility
   - Check performance metrics

## Integration with Project Roadmap

This consolidation plan supports the following roadmap items:

1. **Object System (Phase 3)**: Completes the unified object interface
2. **Dot Notation (Phase 3)**: Improves property access via dot notation
3. **Type System (Phase 3-4)**: Enhances support for types through unified interfaces

## Backward Compatibility

Throughout the implementation, backward compatibility will be maintained:

1. Legacy interfaces will be deprecated but still supported
2. Existing code using old patterns will continue to work
3. Migration guidance will be provided in documentation

## Future Enhancements

After consolidation, these enhancements can be considered:

1. **Advanced Property Access Patterns**:
   - Optional chaining (`obj?.prop?.method()`)
   - Default values for missing properties
   - Computed property names

2. **Runtime Reflection Improvements**:
   - Better support for introspection
   - Dynamic property access and method calls
   - Enhanced type checking

3. **Performance Tuning**:
   - JIT-like optimizations for property access
   - Specialized handlers for hotspots
   - Context-aware optimizations

## Conclusion

This consolidation plan fills the gaps in the current object protocol implementation, eliminates redundant code patterns, and provides a solid foundation for future language features. By standardizing on a single, well-designed object protocol, we'll improve code maintainability, ensure consistent behavior, and enable more powerful language features.
# Unified Object Protocol Implementation Todo List

This document tracks the implementation tasks for the Unified Object Protocol described in [unified-object-protocol.md](./unified-object-protocol.md).

## Phase 1: Core Interfaces

- [ ] Define the `ObjectProtocol` interface in `core/object_protocol.go`
- [ ] Create the `BaseObject` implementation
- [ ] Implement the `Method` and `BoundMethod` interfaces
- [ ] Create the `Class` type implementation
- [ ] Build basic test cases for the core interfaces

## Phase 2: Type Implementations

- [ ] Update `PythonicDict` to implement `ObjectProtocol`
- [ ] Create `ObjectProtocol` implementations for strings
- [ ] Create `ObjectProtocol` implementations for lists and tuples
- [ ] Update `PythonicObject` to use the new protocol
- [ ] Implement attribute lookup and method dispatch for `Class`

## Phase 3: Evaluation Integration

- [ ] Refactor the dot notation handler in `evaluator.go`
- [ ] Update the special form handlers for class definition
- [ ] Modify the method call implementation
- [ ] Update property access implementation
- [ ] Clean up deprecated interfaces and functions

## Phase 4: Testing and Documentation

- [ ] Create comprehensive test suite for all object types
- [ ] Update documentation for classes and objects
- [ ] Add examples demonstrating the new protocol
- [ ] Benchmark performance and optimize critical paths

## Migration Plan

- [ ] Identify all places using the old interfaces
- [ ] Plan migration order to minimize breakage
- [ ] Update all builtin modules to use the new protocol
- [ ] Update example code to use the new protocol

## Future Enhancements

- [ ] Add metaclass support
- [ ] Implement property decorators
- [ ] Add support for operator overloading
- [ ] Implement Python-like descriptors
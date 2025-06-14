# M28 Codebase-Wide Improvement Plan

> **UPDATE**: The builtin package refactoring has been completed successfully. This document now focuses on remaining codebase-wide improvements beyond the builtin system.

## Executive Summary

The M28 codebase exhibits systemic architectural issues that go beyond the builtin package. While the builtin refactoring is complete (see ROADMAP.md for details), this document outlines remaining improvements needed across the broader codebase.

## Systemic Issues Identified

### 1. Error Handling Chaos
- **No standardized error handling** across packages
- **Mixed error creation patterns**: fmt.Errorf, errors.New, custom types
- **Inconsistent error messages** and formats
- **No error wrapping strategy** for context preservation

### 2. Massive Code Duplication
- **56+ files** contain identical argument validation patterns
- **51+ files** have repetitive type switch statements
- **No shared utilities** for common operations
- **Copy-paste programming** evident throughout

### 3. Architectural Confusion
- **No clear separation of concerns** - parsing, evaluation, and type logic mixed
- **Circular dependencies** between packages
- **Inconsistent abstractions** - some areas over-engineered, others under-engineered
- **No design patterns** applied consistently

### 4. Type System Issues
- **Type checking via assertions** rather than interfaces
- **No type hierarchy** for common behaviors
- **Repeated type conversion code** without helpers
- **Missing protocol interfaces** for Python-like behaviors

## Improvement Strategy

### Phase 1: Foundation Layer (Weeks 1-2) ✅ PARTIALLY COMPLETE

> **UPDATE**: The `common/errors` and `common/validation` packages have been created as part of the builtin refactoring. These foundations are now available for use across the codebase.

#### 1.1 Error Handling Package ✅ COMPLETE
The `common/errors` package has been created with standardized error types:

```go
// common/errors/types.go
package errors

type ErrorCode string

const (
    TypeError      ErrorCode = "TypeError"
    ValueError     ErrorCode = "ValueError"
    ArgumentError  ErrorCode = "ArgumentError"
    AttributeError ErrorCode = "AttributeError"
    RuntimeError   ErrorCode = "RuntimeError"
)

type M28Error struct {
    Code     ErrorCode
    Message  string
    Function string
    Context  map[string]interface{}
}

func (e *M28Error) Error() string {
    if e.Function != "" {
        return fmt.Sprintf("%s: %s: %s", e.Code, e.Function, e.Message)
    }
    return fmt.Sprintf("%s: %s", e.Code, e.Message)
}

// Builders for common error types
func NewTypeError(fn string, expected, got string) error {
    return &M28Error{
        Code:     TypeError,
        Function: fn,
        Message:  fmt.Sprintf("expected %s, got %s", expected, got),
    }
}

func NewArgumentError(fn string, expected, got int) error {
    return &M28Error{
        Code:     ArgumentError,
        Function: fn,
        Message:  fmt.Sprintf("takes %d argument(s), got %d", expected, got),
    }
}
```

#### 1.2 Validation Package ✅ COMPLETE
The `common/validation` package has been created with comprehensive validation utilities:

```go
// common/validation/args.go
package validation

import (
    "github.com/mmichie/m28/common/errors"
    "github.com/mmichie/m28/core"
)

type ArgValidator struct {
    FunctionName string
    Args         []core.Value
}

func NewValidator(fnName string, args []core.Value) *ArgValidator {
    return &ArgValidator{FunctionName: fnName, Args: args}
}

func (v *ArgValidator) ExactCount(n int) error {
    if len(v.Args) != n {
        return errors.NewArgumentError(v.FunctionName, n, len(v.Args))
    }
    return nil
}

func (v *ArgValidator) CountRange(min, max int) error {
    if len(v.Args) < min || len(v.Args) > max {
        return errors.NewArgumentError(v.FunctionName, min, len(v.Args))
    }
    return nil
}

func (v *ArgValidator) GetString(index int, paramName string) (string, error) {
    if index >= len(v.Args) {
        return "", errors.NewArgumentError(v.FunctionName, index+1, len(v.Args))
    }
    
    if str, ok := v.Args[index].(core.StringValue); ok {
        return string(str), nil
    }
    return "", errors.NewTypeError(v.FunctionName, "string", v.Args[index].Type())
}
```

### Phase 2: Type System Refactoring (Weeks 3-4)

#### 2.1 Protocol Interfaces
Define clear protocol interfaces:

```go
// core/protocols/indexable.go
package protocols

type Indexable interface {
    GetIndex(index core.Value) (core.Value, error)
    SetIndex(index, value core.Value) error
}

// core/protocols/iterable.go
type Iterable interface {
    Iterate() Iterator
}

type Iterator interface {
    Next() (core.Value, bool)
}

// core/protocols/callable.go
type Callable interface {
    Call(args []core.Value, ctx *core.Context) (core.Value, error)
}

// core/protocols/comparable.go
type Comparable interface {
    Compare(other core.Value) (int, error) // -1, 0, 1 like Python's __cmp__
}
```

#### 2.2 Type Checking Utilities
Create type checking helpers:

```go
// common/types/checks.go
package types

func IsIndexable(v core.Value) bool {
    _, ok := v.(protocols.Indexable)
    return ok
}

func IsIterable(v core.Value) bool {
    _, ok := v.(protocols.Iterable)
    return ok
}

func AsNumber(v core.Value) (float64, bool) {
    if num, ok := v.(core.NumberValue); ok {
        return float64(num), true
    }
    return 0, false
}

func RequireNumber(v core.Value, context string) (float64, error) {
    if num, ok := AsNumber(v); ok {
        return num, nil
    }
    return 0, errors.NewTypeError(context, "number", v.Type())
}
```

### Phase 3: Design Pattern Implementation (Weeks 5-6)

#### 3.1 Visitor Pattern for AST
Replace massive switch statements with visitor pattern:

```go
// parser/ast/visitor.go
package ast

type Visitor interface {
    VisitNumber(n *NumberNode) (core.Value, error)
    VisitString(s *StringNode) (core.Value, error)
    VisitSymbol(s *SymbolNode) (core.Value, error)
    VisitList(l *ListNode) (core.Value, error)
    VisitCall(c *CallNode) (core.Value, error)
    // ... other node types
}

type Node interface {
    Accept(v Visitor) (core.Value, error)
}

type NumberNode struct {
    Value float64
}

func (n *NumberNode) Accept(v Visitor) (core.Value, error) {
    return v.VisitNumber(n)
}
```

#### 3.2 Builder Pattern for Complex Objects
Create builders for complex object construction:

```go
// core/builders/class.go
package builders

type ClassBuilder struct {
    name       string
    bases      []core.Value
    methods    map[string]core.Value
    attributes map[string]core.Value
}

func NewClass(name string) *ClassBuilder {
    return &ClassBuilder{
        name:       name,
        methods:    make(map[string]core.Value),
        attributes: make(map[string]core.Value),
    }
}

func (b *ClassBuilder) WithBase(base core.Value) *ClassBuilder {
    b.bases = append(b.bases, base)
    return b
}

func (b *ClassBuilder) WithMethod(name string, fn core.Value) *ClassBuilder {
    b.methods[name] = fn
    return b
}

func (b *ClassBuilder) Build() (*core.ClassValue, error) {
    // Construct the class with validation
}
```

### Phase 4: Package Reorganization (Week 7)

Restructure packages for better separation of concerns:

```
m28/
├── cmd/              # Command line interface
├── common/           # Shared utilities
│   ├── errors/       # Error types and builders
│   ├── validation/   # Validation utilities
│   └── types/        # Type checking utilities
├── core/             # Core types and interfaces
│   ├── protocols/    # Protocol interfaces
│   ├── values/       # Value type implementations
│   └── context.go    # Execution context
├── language/         # Language implementation
│   ├── parser/       # Parsing logic
│   │   ├── ast/      # AST definitions
│   │   ├── lexer/    # Tokenization
│   │   └── parser.go # Main parser
│   ├── evaluator/    # Evaluation logic
│   │   ├── builtin/  # Built-in functions
│   │   ├── special/  # Special forms
│   │   └── eval.go   # Main evaluator
│   └── runtime/      # Runtime support
│       ├── modules/  # Module system
│       └── gc/       # Garbage collection (future)
├── stdlib/           # Standard library
│   ├── os/           # OS module
│   ├── math/         # Math module
│   └── ...           # Other modules
└── repl/             # REPL implementation
```

### Phase 5: Refactoring Complex Functions (Weeks 8-9)

#### 5.1 Function Decomposition
Break down complex functions into smaller, testable units:

```go
// Before: 100+ line function with mixed concerns
func evalFunctionCall(expr core.Value, ctx *core.Context) (core.Value, error) {
    // ... 100+ lines of mixed logic
}

// After: Separated concerns
func evalFunctionCall(expr core.Value, ctx *core.Context) (core.Value, error) {
    call, err := extractCallExpression(expr)
    if err != nil {
        return nil, err
    }
    
    fn, err := evaluateFunction(call.Function, ctx)
    if err != nil {
        return nil, err
    }
    
    args, err := evaluateArguments(call.Args, ctx)
    if err != nil {
        return nil, err
    }
    
    return applyFunction(fn, args, ctx)
}
```

#### 5.2 Strategy Pattern for Evaluation
Replace switch statements with strategies:

```go
// evaluator/strategies/strategy.go
type EvaluationStrategy interface {
    CanEvaluate(expr core.Value) bool
    Evaluate(expr core.Value, ctx *core.Context) (core.Value, error)
}

// evaluator/strategies/registry.go
type StrategyRegistry struct {
    strategies []EvaluationStrategy
}

func (r *StrategyRegistry) Evaluate(expr core.Value, ctx *core.Context) (core.Value, error) {
    for _, strategy := range r.strategies {
        if strategy.CanEvaluate(expr) {
            return strategy.Evaluate(expr, ctx)
        }
    }
    return nil, errors.NewRuntimeError("no evaluation strategy for type: " + expr.Type())
}
```

### Phase 6: Testing and Migration (Weeks 10-12)

#### 6.1 Testing Infrastructure
- Create comprehensive test suites for new packages
- Add integration tests to ensure compatibility
- Benchmark critical paths to ensure no performance regression

#### 6.2 Gradual Migration
- Migrate one package at a time
- Maintain backward compatibility during transition
- Use feature flags for new implementations

## Implementation Priorities

### ✅ Completed (from builtin refactoring)
1. Error handling standardization - `common/errors` package created
2. Validation utilities - `common/validation` package created
3. Function builders - `common/builders` package created

### High Priority (Do Next)
1. Type checking helpers - Reduces boilerplate across non-builtin code
2. Protocol interfaces - Improves type system
3. Package reorganization - Better architecture

### Medium Priority
1. Complex function refactoring - Improves maintainability
2. AST visitor pattern - Cleaner evaluation logic
3. Testing infrastructure - Better coverage

### Low Priority (Do Later)
1. Full visitor pattern - Nice to have but not critical
2. Builder patterns - Can be added incrementally
3. Strategy pattern - Advanced optimization

## Success Metrics

- **Code duplication reduced by 80%** - From hundreds of duplicated patterns to dozens
- **Average function length < 50 lines** - From many 100+ line functions
- **Test coverage > 80%** - Especially for core packages
- **Consistent error handling** - All errors follow standard format
- **Clear package boundaries** - No circular dependencies

## Risk Mitigation

1. **Backward Compatibility**
   - Maintain existing APIs during migration
   - Use deprecation warnings for changes
   - Provide migration guides

2. **Performance**
   - Benchmark before and after changes
   - Profile hot paths
   - Optimize where necessary

3. **Team Buy-in**
   - Document new patterns clearly
   - Provide examples and templates
   - Code reviews to ensure compliance

## Next Steps

1. ~~Review and approve this plan~~ ✅
2. ~~Set up new package structure~~ ✅ (common/ packages created)
3. ~~Implement error handling package~~ ✅ (common/errors)
4. ~~Create validation utilities~~ ✅ (common/validation)
5. ~~Begin migrating builtin package as proof of concept~~ ✅ (complete)
6. **CURRENT**: Apply patterns to non-builtin packages:
   - Start with parser package (type checking helpers)
   - Move to eval package (protocol interfaces)
   - Continue with special_forms and modules

## Conclusion

These systemic improvements will transform M28 from a working prototype into a maintainable, extensible language implementation. The investment in proper architecture will pay dividends as the language grows and evolves.
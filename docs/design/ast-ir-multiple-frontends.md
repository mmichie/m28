# AST and IR Design for Multiple Frontend Support

## Executive Summary

M28 currently has an implicit two-stage architecture (Parser → IR → Evaluator) but lacks a formal AST layer with source location tracking. This design proposes adding an explicit AST layer to enable:

1. **Multiple frontend languages** (Python, JSON DSLs, etc.) sharing the same runtime
2. **High-quality error messages** with precise source locations in original syntax
3. **IDE tooling support** (LSP, debugger, refactoring tools)
4. **Type system foundation** for gradual typing
5. **Optimization opportunities** through IR transformation passes

**Timeline:** 5-6 weeks for critical path
**Impact:** Foundation for all future language features and tooling
**Risk:** Medium (architectural change, but evaluator unchanged)

---

## Current Architecture

### What Works Well

```
Source Code (M28 with Pythonic sugar)
         ↓
┌─────────────────────────────────┐
│  Tokenizer (tokenizer.go)       │  ← Recently added, working great!
│  - Lexical analysis              │
│  - Token[] with positions        │
└─────────────────────────────────┘
         ↓
┌─────────────────────────────────┐
│  Parser (parser.go)              │
│  - Syntax analysis               │
│  - Desugaring (Pythonic → Lisp) │  ← Happens here!
│  - Returns core.Value            │
└─────────────────────────────────┘
         ↓
    core.Value (IR)
    S-expressions as ListValue
         ↓
┌─────────────────────────────────┐
│  Evaluator (eval/evaluator.go)  │  ← Pure, syntax-agnostic
│  - Interprets IR                 │
│  - Special forms                 │
│  - Function calls                │
└─────────────────────────────────┘
```

**Key Insight:** The evaluator is already completely syntax-agnostic! It only sees `core.Value` (S-expressions).

### The Problem: Missing AST Layer

**Current flow:**
```go
// Parser directly returns core.Value
func (p *Parser) Parse(input string) (core.Value, error) {
    // Tokenize
    tokens, _ := tokenizer.Tokenize()

    // Parse and desugar in one step
    // x = 10  →  (= x 10)
    // print("hello")  →  (print "hello")

    return ListValue{SymbolValue("="), ...}, nil  // Lost source location!
}
```

**Problems:**
1. **No source locations in IR** - Can't show "error at line 5, column 12"
2. **Lost original syntax** - Can't tell if user wrote `x = 10` (Python) or `(= x 10)` (Lisp)
3. **No type annotations** - Python's `def f(x: int) -> int:` loses type hints
4. **No comments preserved** - Documentation lost after parsing
5. **Hard to add new frontends** - Would need to duplicate all the desugaring logic

**Evidence of the problem:**
- `LocatedValue` was created but never used (see docs/design/located-value-analysis.md)
- Error messages show "line 0, column 0" for runtime errors
- No way to show Python syntax in error messages for Python code

---

## Proposed Architecture

### Three-Layer Design

```
┌────────────────────────────────────────────────────────────┐
│                   Frontend Layer                            │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Python     │  │  M28 Lisp    │  │  Future DSL  │     │
│  │   Parser     │  │  Parser      │  │  Parser      │     │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘     │
│         │                  │                  │             │
│         └──────────────────┴──────────────────┘             │
│                            ↓                                │
└────────────────────────────────────────────────────────────┘
                             ↓
┌────────────────────────────────────────────────────────────┐
│                    AST Layer (NEW!)                         │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  ASTNode interface with:                             │  │
│  │  - Source locations (file, line, column)             │  │
│  │  - Type annotations (for Python type hints)          │  │
│  │  - Comments (for documentation)                      │  │
│  │  - Syntax kind tag (Python | Lisp | Other)           │  │
│  │  - ToIR() method (lower to core.Value)               │  │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────┘
                             ↓ Lower to IR
┌────────────────────────────────────────────────────────────┐
│              IR Layer (core.Value + Metadata)               │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  S-expressions (ListValue) unchanged                 │  │
│  │  + Metadata table (locations, types, comments)       │  │
│  │  - Lightweight runtime representation                │  │
│  │  - Can reconstruct source from metadata              │  │
│  └──────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────┘
                             ↓
┌────────────────────────────────────────────────────────────┐
│                 Evaluator Layer (UNCHANGED)                 │
│  Works on core.Value (no knowledge of AST/syntax)          │
└────────────────────────────────────────────────────────────┘
```

### Key Components

#### 1. ASTNode Interface (core/ast/ast.go)

```go
package ast

// ASTNode represents a node in the abstract syntax tree
type ASTNode interface {
    core.Value  // Still a Value for backwards compatibility

    // AST-specific methods
    Location() *core.SourceLocation
    SyntaxKind() SyntaxKind
    Comments() []string
    TypeAnnotation() *TypeInfo

    // Lower to IR for evaluation
    ToIR() core.Value
}

// SyntaxKind identifies which frontend produced this AST
type SyntaxKind int
const (
    SyntaxLisp SyntaxKind = iota
    SyntaxPython
    SyntaxJSON
    SyntaxCustom
)

// Base implementation all AST nodes embed
type BaseNode struct {
    Loc     *core.SourceLocation
    Syntax  SyntaxKind
    Comment []string
    Type    *TypeInfo
}

func (b *BaseNode) Location() *core.SourceLocation { return b.Loc }
func (b *BaseNode) SyntaxKind() SyntaxKind { return b.Syntax }
func (b *BaseNode) Comments() []string { return b.Comment }
func (b *BaseNode) TypeAnnotation() *TypeInfo { return b.Type }
```

#### 2. Concrete AST Node Types

```go
// Identifier node
type Identifier struct {
    BaseNode
    Name string
}

func (i *Identifier) Type() core.Type { return core.SymbolType }
func (i *Identifier) String() string { return i.Name }
func (i *Identifier) ToIR() core.Value {
    return core.SymbolValue(i.Name)
}

// Literal node
type Literal struct {
    BaseNode
    Value core.Value
}

func (l *Literal) Type() core.Type { return l.Value.Type() }
func (l *Literal) String() string { return l.Value.String() }
func (l *Literal) ToIR() core.Value { return l.Value }

// S-expression node (function call, special form, etc.)
type SExpr struct {
    BaseNode
    Elements []ASTNode
}

func (s *SExpr) Type() core.Type { return core.ListType }
func (s *SExpr) String() string {
    parts := make([]string, len(s.Elements))
    for i, e := range s.Elements {
        parts[i] = e.String()
    }
    return "(" + strings.Join(parts, " ") + ")"
}

func (s *SExpr) ToIR() core.Value {
    vals := make(core.ListValue, len(s.Elements))
    for i, elem := range s.Elements {
        vals[i] = elem.ToIR()
    }
    return vals
}

// Function definition node
type DefForm struct {
    BaseNode
    Name       string
    Params     []Parameter
    Body       ASTNode
    ReturnType *TypeInfo  // NEW: from Python type hints
}

type Parameter struct {
    Name    string
    Type    *TypeInfo  // NEW: from Python type hints
    Default ASTNode
}

func (d *DefForm) ToIR() core.Value {
    // Build (def name (params) body)
    params := make(core.ListValue, len(d.Params))
    for i, p := range d.Params {
        params[i] = core.SymbolValue(p.Name)
    }

    return core.ListValue{
        core.SymbolValue("def"),
        core.SymbolValue(d.Name),
        params,
        d.Body.ToIR(),
    }
}
```

#### 3. Type Information Structure

```go
// TypeInfo represents type annotations (for gradual typing)
type TypeInfo struct {
    Name    string        // "int", "str", "List"
    Generic []TypeInfo    // For List[int], Dict[str, int]
    IsOptional bool       // For Optional[T]
}

// Examples:
// int                    → TypeInfo{Name: "int"}
// List[int]              → TypeInfo{Name: "List", Generic: []{Name: "int"}}
// Dict[str, int]         → TypeInfo{Name: "Dict", Generic: []{Name: "str"}, {Name: "int"}}
// Optional[str]          → TypeInfo{Name: "str", IsOptional: true}
```

#### 4. Metadata Table (core/metadata.go)

```go
// IRMetadata stores information about IR nodes that can't be in the Value itself
type IRMetadata struct {
    mu        sync.RWMutex
    locations map[core.Value]*core.SourceLocation
    types     map[core.Value]*TypeInfo
    comments  map[core.Value][]string
    syntax    map[core.Value]SyntaxKind
}

func (m *IRMetadata) SetLocation(val core.Value, loc *core.SourceLocation) {
    m.mu.Lock()
    defer m.mu.Unlock()
    m.locations[val] = loc
}

func (m *IRMetadata) GetLocation(val core.Value) *core.SourceLocation {
    m.mu.RLock()
    defer m.mu.RUnlock()
    return m.locations[val]
}

// Similar methods for types, comments, syntax kind...
```

**Why a separate table?**
- Keeps `core.Value` lightweight and unchanged
- No need for unwrapping wrappers
- Can track metadata for composite values (lists, dicts)
- Easy to clear/manage during optimization passes

---

## Implementation Plan

### Phase 1: Core AST Infrastructure (Week 1-2)

**Goal:** Add AST layer without breaking existing code

**Tasks:**
1. Create `core/ast/` package with ASTNode interface
2. Implement basic node types: Identifier, Literal, SExpr
3. Add SourceLocation to core (already exists, just needs usage)
4. Create IRMetadata table
5. Update Context to carry metadata

**Deliverables:**
- [ ] `core/ast/ast.go` - ASTNode interface and base types
- [ ] `core/ast/nodes.go` - Concrete node implementations
- [ ] `core/metadata.go` - Metadata table
- [ ] All tests still pass (no behavior change)

**Example:**
```go
// Before:
result, err := parser.Parse(code)  // Returns core.Value

// After:
ast, metadata, err := parser.Parse(code)  // Returns ASTNode + metadata
ir := ast.ToIR()                           // Lower to core.Value
result, err := eval.Eval(ir, ctx)          // Evaluate (unchanged!)
```

### Phase 2: Parser Refactoring (Week 3-4)

**Goal:** Update parser to build AST instead of directly returning IR

**Tasks:**
1. Refactor `parseAtomFromToken()` to return ASTNode
2. Refactor `parseExpr()` to return ASTNode
3. Add location tracking to every node creation
4. Preserve syntax kind (currently always Lisp)
5. Update all parse methods

**Deliverables:**
- [ ] Parser returns AST with full location info
- [ ] Metadata table populated during parsing
- [ ] All tests pass with improved error messages
- [ ] Error messages show exact source locations

**Example:**
```go
// Current:
func (p *Parser) parseExpr() (core.Value, error) {
    // ... parsing logic
    return core.ListValue{...}, nil  // No location!
}

// New:
func (p *Parser) parseExpr() (ast.ASTNode, error) {
    tok := p.currentToken()

    // ... parsing logic

    node := &ast.SExpr{
        BaseNode: ast.BaseNode{
            Loc: &core.SourceLocation{
                File: p.filename,
                Line: tok.Line,
                Col:  tok.Col,
            },
            Syntax: ast.SyntaxLisp,
        },
        Elements: elements,
    }

    return node, nil
}
```

### Phase 3: Enhanced Error Messages (Week 5)

**Goal:** Use AST locations to show beautiful error messages

**Tasks:**
1. Update error types to include SourceLocation
2. Enhance ErrorReporter to use metadata
3. Show original syntax in errors (Python vs Lisp)
4. Add source code context to all errors

**Deliverables:**
- [ ] All errors show file:line:col
- [ ] Source snippets with caret indicators
- [ ] Syntax-aware error formatting

**Example:**
```python
# Python code:
def divide(x, y):
    return x / y

divide(10, 0)

# Error output:
ZeroDivisionError: division by zero
  File "script.py", line 5, in <module>
    divide(10, 0)
    ^^^^^^^^^^^^^
  File "script.py", line 2, in divide
    return x / y
           ~~^~~
```

### Phase 4: Type Annotation Support (Week 6)

**Goal:** Preserve Python type hints through compilation

**Tasks:**
1. Add TypeInfo structure
2. Parse Python type annotations into TypeInfo
3. Store type info in metadata table
4. Add type checking validation (optional)

**Deliverables:**
- [ ] Type hints preserved in AST
- [ ] Type info available for IDE queries
- [ ] Foundation for gradual typing

**Example:**
```python
# Python with types:
def add(x: int, y: int) -> int:
    return x + y

# AST preserves types:
DefForm{
    Name: "add",
    Params: [
        {Name: "x", Type: &TypeInfo{Name: "int"}},
        {Name: "y", Type: &TypeInfo{Name: "int"}},
    ],
    ReturnType: &TypeInfo{Name: "int"},
    Body: ...,
}

# Can be queried by LSP/type checker
```

### Phase 5: Testing & Documentation (Week 6)

**Tasks:**
1. Comprehensive test suite for AST
2. Update all documentation
3. Migration guide for library authors
4. Performance benchmarking

**Deliverables:**
- [ ] Test coverage > 80%
- [ ] No performance regression
- [ ] Updated docs/ARCHITECTURE.md
- [ ] Example code showing AST usage

---

## Benefits

### 1. Multiple Frontend Support

**Python Frontend Example:**
```python
# Python code:
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

# Python parser builds AST:
DefForm{
    Name: "factorial",
    Params: [{Name: "n"}],
    Body: IfForm{
        Condition: BinOp{...},
        Then: Literal{1},
        Else: BinOp{...},
    },
}

# Lower to IR:
(def factorial (n)
  (if (<= n 1)
    1
    (* n (factorial (- n 1)))))

# Evaluate (same evaluator!)
```

**Both syntaxes share:**
- Runtime (evaluator)
- Standard library (builtins)
- Type system
- Optimization passes
- Debugging tools

### 2. Perfect Error Messages

```python
# Current (bad):
NameError: name 'prin' is not defined

# With AST (good):
NameError: name 'prin' is not defined
  File "script.py", line 10, in greet
    prin(f"Hello, {name}!")
    ^^^^
Did you mean: print?
```

### 3. IDE Support Foundation

- **Go to definition** - AST has location of every symbol
- **Hover tooltips** - Show type info from annotations
- **Refactoring** - Rename variables safely using AST
- **Auto-complete** - Use type hints for suggestions
- **Syntax highlighting** - Accurate, AST-based

### 4. Bidirectional Syntax Display

```python
# User writes Python:
result = [x**2 for x in range(10) if x % 2 == 0]

# Debugger can show Lisp view (for learning):
(= result
   (list-comp (** x 2)
              (for x (range 10))
              (if (== (% x 2) 0))))

# Both views backed by same AST
```

### 5. Type System Foundation

```python
# Python with types:
def process(data: List[Dict[str, int]]) -> int:
    return sum(d["value"] for d in data)

# Type checker can:
# 1. Validate argument types at call sites
# 2. Infer return types
# 3. Catch type errors before runtime
# 4. Enable optimizations (known types = fast path)
```

---

## Design Decisions

### Why Not Wrap Values with LocatedValue?

**Considered:**
```go
type LocatedValue struct {
    Value core.Value
    Location *SourceLocation
}
```

**Problems:**
1. Type assertion pollution - every type check needs unwrapping
2. Equality issues - `42` != `Located(42, line=5)`
3. Interface compliance - needs to implement every interface of wrapped value
4. Performance overhead - constant wrapping/unwrapping

**Better: Separate metadata table**
- Values unchanged
- No type system pollution
- Optional (only when needed)
- Easy to clear for optimization

### Why ASTNode Implements Value?

**Decision:** `ASTNode interface { core.Value; ... }`

**Rationale:**
- Backwards compatibility during migration
- Can pass AST nodes to existing functions temporarily
- `ToIR()` method makes lowering explicit
- Eventually can remove Value interface from ASTNode

### Why Three Layers Instead of Two?

**Alternative:** Parse directly to IR with metadata

**Why three layers:**
1. **AST is for tooling** (LSP, refactoring, type checking)
2. **IR is for evaluation** (lightweight, optimizable)
3. **Clean separation** - AST has full fidelity, IR is simplified
4. **Multiple IRs possible** - Could have HIR, MIR, LIR for optimization

### IR Design Philosophy

**Keep IR as S-expressions** (current `core.Value`)
- Simple and proven
- Universal (can represent any language construct)
- Easy to manipulate programmatically
- Macros work naturally on S-expressions
- Evaluator is already optimized for this

**Use metadata for non-semantic info**
- Source locations (debugging)
- Type annotations (optional checking)
- Comments (documentation)
- Original syntax (error messages)

---

## Migration Strategy

### Backwards Compatibility

**Goal:** Zero breaking changes for existing code

**Approach:**
1. Add AST layer alongside existing parser
2. Parser returns both AST and IR initially
3. Evaluator continues using IR (unchanged)
4. Gradually move error reporting to use AST
5. Eventually make AST primary, IR derived

**Example:**
```go
// Phase 1: Parse returns both
ast, ir, metadata, err := parser.Parse(code)
result, err := eval.Eval(ir, ctx)  // Still uses IR

// Phase 2: Parse returns AST, explicit lowering
ast, metadata, err := parser.Parse(code)
ir := ast.ToIR()  // Explicit
result, err := eval.Eval(ir, ctx)

// Phase 3: Context carries metadata automatically
ast, err := parser.Parse(code)
ctx := ctx.WithMetadata(metadata)
result, err := eval.Eval(ast.ToIR(), ctx)
```

### Testing Strategy

1. **Parallel testing** - Run all tests with both old and new parser
2. **Golden tests** - Verify AST structure matches expected
3. **Location tests** - Verify every node has correct location
4. **Error message tests** - Verify improved error output
5. **Performance tests** - No regression in eval speed

### Rollout Plan

**Week 1-2:** AST infrastructure in place, opt-in
**Week 3-4:** Parser refactored, AST primary
**Week 5:** Error messages using AST locations
**Week 6:** Type annotations working, documentation complete

---

## Performance Considerations

### Memory Impact

**AST layer adds:**
- SourceLocation per node (~24 bytes)
- Metadata table (~40 bytes overhead + entries)
- Type annotations (optional, ~32 bytes when present)

**Mitigation:**
- Only create AST during parsing (one-time cost)
- Lower to IR immediately after parsing
- Clear AST after lowering (GC reclaims memory)
- Metadata table only for debugging/error reporting

**Estimate:** +2-5 MB for typical program, negligible for real workloads

### Runtime Impact

**AST does NOT impact runtime:**
- Evaluator still works on IR (core.Value)
- No AST traversal during evaluation
- Metadata lookups only on errors (rare path)

**Benchmark targets:**
- Parse time: +10% acceptable (one-time cost)
- Eval time: 0% regression (same IR)
- Memory: +5% acceptable
- Error reporting: 20% slower acceptable (rare path)

### Optimization Opportunities

**AST enables optimizations:**
1. **Type-directed optimizations** - Use type annotations to specialize
2. **Constant folding** - Detect constant expressions in AST
3. **Dead code elimination** - Find unused definitions
4. **Inlining** - Inline small functions based on call graph
5. **Escape analysis** - Determine if values escape scope

---

## Future Extensions

### Once AST is in place, we can add:

1. **Bytecode compiler**
   - AST → Bytecode (like Python's .pyc)
   - Faster startup for large programs
   - More optimization opportunities

2. **JIT compilation**
   - AST → LLVM IR for hot functions
   - Type-specialized fast paths
   - Profile-guided optimization

3. **Static analysis**
   - Lint rules on AST
   - Security analysis
   - Complexity metrics

4. **Code transformations**
   - Auto-formatting
   - Automatic refactorings
   - Macro expansion visualization

5. **Language server protocol (LSP)**
   - Go to definition
   - Find references
   - Rename symbol
   - Hover documentation

---

## Risks and Mitigation

### Risk 1: Scope Creep
**Mitigation:** Start with minimal AST, iterate incrementally

### Risk 2: Performance Regression
**Mitigation:** Benchmark every phase, optimize hot paths

### Risk 3: Breaking Changes
**Mitigation:** Parallel implementation, gradual migration

### Risk 4: Complexity
**Mitigation:** Clear documentation, simple examples

### Risk 5: Team Bandwidth
**Mitigation:** 5-6 weeks is realistic, can be done incrementally

---

## Success Metrics

### Must Have (Critical)
- [ ] All tests pass with AST-based parser
- [ ] Error messages show source locations
- [ ] No eval performance regression
- [ ] Backwards compatible with existing code

### Should Have (Important)
- [ ] Type annotations preserved from Python
- [ ] Comments preserved in AST
- [ ] Documentation complete
- [ ] 80% test coverage of AST code

### Nice to Have
- [ ] Python frontend proof-of-concept
- [ ] LSP server foundation
- [ ] Optimization pass framework

---

## Conclusion

Adding an explicit AST layer is the foundation for M28's evolution into a multi-frontend language. It enables:

1. **Better user experience** - Perfect error messages
2. **Multiple syntaxes** - Python, Lisp, custom DSLs
3. **Developer tools** - LSP, debugger, refactoring
4. **Type system** - Gradual typing with annotations
5. **Optimizations** - HIR/MIR transformation passes

The design is pragmatic:
- **Minimal changes** to evaluator (zero)
- **Incremental migration** (backwards compatible)
- **Proven patterns** (AST → IR is standard)
- **Reasonable scope** (5-6 weeks)

**Recommendation:** Proceed with Phase 1 immediately. This is the foundation for everything on the roadmap.

---

## References

- [LocatedValue Analysis](./located-value-analysis.md) - Why we need separate metadata
- Python AST - [https://docs.python.org/3/library/ast.html](https://docs.python.org/3/library/ast.html)
- Rust's syn crate - [https://docs.rs/syn/latest/syn/](https://docs.rs/syn/latest/syn/)
- TypeScript compiler - [https://github.com/microsoft/TypeScript/wiki/Architectural-Overview](https://github.com/microsoft/TypeScript/wiki/Architectural-Overview)

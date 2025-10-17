# Phase B: Python AST Nodes - Design Document

## Overview

Phase B adds Python-specific AST node types to represent constructs that don't exist in the base Lisp syntax.

## Design Principles

1. **Clean separation**: Python nodes live in `core/ast/python_nodes.go`
2. **Consistent with existing nodes**: Embed BaseNode, implement ASTNode interface
3. **ToIR() desugaring**: Each node knows how to lower itself to S-expressions
4. **Complete Python support**: Cover all Python statement types

## New Node Types

### 1. ComprehensionForm

**Purpose**: List, dict, set comprehensions and generator expressions

```python
# List comprehension
[x*x for x in range(10) if x % 2 == 0]

# Dict comprehension
{k: v*2 for k, v in items.items() if v > 0}

# Set comprehension
{x for x in data if x > 10}

# Generator expression
(x*x for x in range(10))
```

**Structure**:
```go
type ComprehensionKind int
const (
    ListComp ComprehensionKind = iota
    DictComp
    SetComp
    GeneratorComp
)

type ComprehensionForm struct {
    BaseNode
    Kind       ComprehensionKind
    Element    ASTNode    // For list/set/generator: the expression
    KeyExpr    ASTNode    // For dict: key expression
    ValueExpr  ASTNode    // For dict: value expression
    Variable   string     // Loop variable
    Iterable   ASTNode    // What to iterate over
    Condition  ASTNode    // Optional filter (nil if none)
}
```

**ToIR() examples**:
```lisp
; [x*x for x in range(10) if x % 2 == 0]
(list-comp (lambda (x) (* x x))
           (range 10)
           (lambda (x) (== (% x 2) 0)))

; {k: v*2 for k, v in items.items()}
(dict-comp (lambda (k v) k)
           (lambda (k v) (* v 2))
           (items.items))
```

### 2. ForForm

**Purpose**: For loops with optional else clause

```python
for i in range(5):
    print(i)
else:
    print("done")
```

**Structure**:
```go
type ForForm struct {
    BaseNode
    Variable string       // Loop variable
    Iterable ASTNode      // Expression to iterate over
    Body     []ASTNode    // Loop body statements
    ElseBody []ASTNode    // Optional else clause
}
```

**ToIR() example**:
```lisp
; for i in range(5): print(i)
(for i (range 5)
  (print i))
```

### 3. WhileForm

**Purpose**: While loops with optional else clause

```python
while condition:
    do_something()
else:
    cleanup()
```

**Structure**:
```go
type WhileForm struct {
    BaseNode
    Condition ASTNode
    Body      []ASTNode
    ElseBody  []ASTNode
}
```

**ToIR() example**:
```lisp
; while x > 0: x -= 1
(while (> x 0)
  (= x (- x 1)))
```

### 4. WithForm

**Purpose**: Context managers (with statement)

```python
with open("file.txt") as f:
    data = f.read()

# Multiple context managers
with open("in.txt") as f, open("out.txt", "w") as g:
    g.write(f.read())
```

**Structure**:
```go
type WithItem struct {
    Context  ASTNode
    Variable string  // Optional (empty string if no 'as' clause)
}

type WithForm struct {
    BaseNode
    Items []WithItem
    Body  []ASTNode
}
```

**ToIR() example**:
```lisp
; with open("file.txt") as f: data = f.read()
(with (open "file.txt") f
  (= data (f.read)))
```

### 5. TryForm

**Purpose**: Exception handling

```python
try:
    risky()
except ValueError as e:
    handle_value_error(e)
except Exception:
    handle_other()
else:
    success()
finally:
    cleanup()
```

**Structure**:
```go
type ExceptClause struct {
    ExceptionType string    // Empty string for bare except
    Variable      string    // Empty string if no 'as' clause
    Body          []ASTNode
}

type TryForm struct {
    BaseNode
    TryBody       []ASTNode
    ExceptClauses []ExceptClause
    ElseBody      []ASTNode  // Optional
    FinallyBody   []ASTNode  // Optional
}
```

**ToIR() example**:
```lisp
; try/except/finally
(try
  (risky)
  (except ValueError e
    (handle-value-error e))
  (finally
    (cleanup)))
```

### 6. ClassForm

**Purpose**: Class definitions

```python
@dataclass
class Counter(BaseClass):
    def __init__(self, start: int = 0):
        self.value = start

    def increment(self) -> int:
        self.value += 1
        return self.value
```

**Structure**:
```go
type ClassForm struct {
    BaseNode
    Name       string
    Bases      []ASTNode      // Base classes
    Body       []ASTNode      // Methods and attributes (DefForm, AssignForm)
    Decorators []ASTNode      // Class decorators
}
```

**ToIR() example**:
```lisp
; class Counter(BaseClass): ...
(def-class Counter (BaseClass)
  (def __init__ (self start)
    (= self.value start))
  (def increment (self)
    (do
      (= self.value (+ self.value 1))
      self.value)))
```

### 7. Control Flow Statements

**Purpose**: break, continue, return, raise, pass

```python
break
continue
return value
raise Exception("error")
raise ValueError from cause
pass
```

**Structure**:
```go
type BreakForm struct {
    BaseNode
}

type ContinueForm struct {
    BaseNode
}

type ReturnForm struct {
    BaseNode
    Value ASTNode  // nil for bare return
}

type RaiseForm struct {
    BaseNode
    Exception ASTNode  // nil for bare raise (re-raise)
    Cause     ASTNode  // nil unless 'raise X from Y'
}

type PassForm struct {
    BaseNode
}
```

**ToIR() examples**:
```lisp
; break
(break)

; return value
(return value)

; raise ValueError("msg")
(raise (ValueError "msg"))

; pass
None  ; or could be (pass) special form
```

### 8. BlockForm (Helper)

**Purpose**: Sequence of statements (used in function bodies, loop bodies, etc.)

```go
type BlockForm struct {
    BaseNode
    Statements []ASTNode
}
```

**ToIR()**:
```lisp
; Multiple statements
(do
  stmt1
  stmt2
  stmt3)
```

### 9. Enhanced DefForm

**Purpose**: Add decorators to existing DefForm

```python
@property
@cache
def expensive_computation(self):
    return result
```

**Update to existing DefForm**:
```go
type DefForm struct {
    BaseNode
    Name       string
    Params     []Parameter
    Body       ASTNode
    ReturnType *TypeInfo
    Decorators []ASTNode  // NEW: List of decorator expressions
}
```

**ToIR() example**:
```lisp
; @decorator
; def foo(): pass

; Desugars to:
(= foo (decorator (def foo () None)))
```

## Constructor Functions

Each node type needs a constructor:

```go
func NewComprehensionForm(kind ComprehensionKind, ..., loc *SourceLocation, syntax SyntaxKind)
func NewForForm(variable string, iterable, body, loc, syntax)
func NewWhileForm(condition, body, loc, syntax)
func NewWithForm(items []WithItem, body, loc, syntax)
func NewTryForm(tryBody, excepts, loc, syntax)
func NewClassForm(name string, bases, body, decorators, loc, syntax)
func NewBreakForm(loc, syntax)
func NewContinueForm(loc, syntax)
func NewReturnForm(value, loc, syntax)
func NewRaiseForm(exception, cause, loc, syntax)
func NewPassForm(loc, syntax)
func NewBlockForm(statements, loc, syntax)
```

## ToIR() Implementation Strategy

1. **Comprehensions**: Use lambda functions for element and filter
2. **Loops**: Map directly to M28 for/while special forms
3. **With**: Map to with special form (may need to implement)
4. **Try**: Map to try special form with except clauses
5. **Class**: Map to def-class special form
6. **Control flow**: Map directly to corresponding special forms
7. **Block**: Wrap in (do ...)

## Implementation Order

1. ✅ Simple control flow (Break, Continue, Return, Raise, Pass)
2. ✅ BlockForm (helper for multi-statement bodies)
3. ✅ ForForm and WhileForm (loops)
4. ✅ ComprehensionForm (most complex)
5. ✅ WithForm (context managers)
6. ✅ TryForm (exception handling)
7. ✅ ClassForm (class definitions)
8. ✅ Update DefForm with Decorators

## Testing Strategy

Each node type needs tests for:
1. Construction
2. ToIR() lowering
3. Location tracking
4. SyntaxKind = Python

## File Organization

```
core/ast/
  ast.go              - Base types (existing)
  nodes.go            - Base nodes (existing)
  python_nodes.go     - Python-specific nodes (NEW)
  python_nodes_test.go - Tests (NEW)
```

## Success Criteria

- [ ] All Python node types implemented
- [ ] All ToIR() methods working correctly
- [ ] Comprehensive test coverage (>20 tests)
- [ ] Zero regressions in existing tests
- [ ] Ready for Phase C (parser implementation)

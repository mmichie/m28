# Python Frontend Design

## Overview

With the AST layer (Phases 1-3) complete, we have the foundation to add a full Python frontend. This document explores what that would look like.

## Current State

**What we already have:**
- AST infrastructure with SyntaxKind tracking
- Metadata table for source locations
- Pythonic syntax sugar already working:
  - `print(42)` → `(print 42)` ✅
  - `x = 10` → `(= x 10)` ✅
  - `def foo(x): expr` → `(def foo (x) expr)` ✅
  - Infix operators: `x * 2` → `(* x 2)` ✅
- Error messages with location tracking

## Architecture

```
Python Source Code
       ↓
Python Tokenizer (indentation-aware)
       ↓
Python Parser → AST (SyntaxKind = Python)
       ↓
AST.ToIR() → S-expressions (core.Value)
       ↓
Evaluator (unchanged!)
```

## Concrete Examples

### Example 1: Function Definition

**Python:**
```python
def factorial(n: int) -> int:
    """Calculate factorial recursively."""
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)
```

**AST:**
```go
DefForm{
  Name: "factorial",
  Params: []Parameter{
    {Name: "n", Type: &TypeInfo{Name: "int"}},
  },
  ReturnType: &TypeInfo{Name: "int"},
  Body: IfForm{
    Condition: SExpr([
      Identifier("<="),
      Identifier("n"),
      Literal(1),
    ]),
    ThenBranch: Literal(1),
    ElseBranch: SExpr([
      Identifier("*"),
      Identifier("n"),
      SExpr([
        Identifier("factorial"),
        SExpr([Identifier("-"), Identifier("n"), Literal(1)]),
      ]),
    ]),
  },
  SyntaxKind: SyntaxPython,
  Location: &SourceLocation{File: "fact.py", Line: 1, Column: 1},
  Comments: []string{"Calculate factorial recursively."},
}
```

**IR (after ToIR()):**
```lisp
(def factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

### Example 2: List Comprehension

**Python:**
```python
squares = [x * x for x in range(10) if x % 2 == 0]
```

**AST:**
```go
AssignForm{
  Target: Identifier("squares"),
  Value: ComprehensionForm{  // New AST node type
    Element: SExpr([Identifier("*"), Identifier("x"), Identifier("x")]),
    Variable: "x",
    Iterable: SExpr([Identifier("range"), Literal(10)]),
    Condition: SExpr([
      Identifier("=="),
      SExpr([Identifier("%"), Identifier("x"), Literal(2)]),
      Literal(0),
    ]),
    Kind: ListComprehension,
  },
  SyntaxKind: SyntaxPython,
}
```

**IR (after ToIR()):**
```lisp
(= squares
   (list-comp (lambda (x) (* x x))
              (range 10)
              (lambda (x) (== (% x 2) 0))))
```

### Example 3: Class Definition

**Python:**
```python
class Counter:
    def __init__(self, start=0):
        self.value = start

    def increment(self):
        self.value += 1
        return self.value
```

**AST:**
```go
ClassForm{  // New AST node type
  Name: "Counter",
  Methods: []DefForm{
    DefForm{
      Name: "__init__",
      Params: []Parameter{
        {Name: "self"},
        {Name: "start", Default: Literal(0)},
      },
      Body: AssignForm{
        Target: DotExpr{Object: Identifier("self"), Property: "value"},
        Value: Identifier("start"),
      },
    },
    DefForm{
      Name: "increment",
      Params: []Parameter{{Name: "self"}},
      Body: SExpr([
        Identifier("do"),
        AssignForm{
          Target: DotExpr{Object: Identifier("self"), Property: "value"},
          Value: SExpr([
            Identifier("+"),
            DotExpr{Object: Identifier("self"), Property: "value"},
            Literal(1),
          ]),
        },
        DotExpr{Object: Identifier("self"), Property: "value"},
      ]),
    },
  },
  SyntaxKind: SyntaxPython,
}
```

**IR (after ToIR()):**
```lisp
(def-class Counter ()
  (def __init__ (self start)
    (= self.value start))

  (def increment (self)
    (do
      (= self.value (+ self.value 1))
      self.value)))
```

### Example 4: For Loop

**Python:**
```python
for i in range(5):
    print(i)
```

**AST:**
```go
ForForm{  // New AST node type
  Variable: "i",
  Iterable: SExpr([Identifier("range"), Literal(5)]),
  Body: SExpr([Identifier("print"), Identifier("i")]),
  SyntaxKind: SyntaxPython,
}
```

**IR (after ToIR()):**
```lisp
(for i (range 5)
  (print i))
```

## New AST Node Types Needed

```go
// ComprehensionForm represents list/dict/set comprehensions
type ComprehensionForm struct {
    BaseNode
    Element   ASTNode              // What to compute: x*x
    Variable  string               // Loop variable: x
    Iterable  ASTNode              // What to iterate: range(10)
    Condition ASTNode              // Optional filter: x % 2 == 0
    Kind      ComprehensionKind    // List, Dict, Set
}

// ForForm represents for loops
type ForForm struct {
    BaseNode
    Variable string    // Loop variable
    Iterable ASTNode   // What to iterate over
    Body     ASTNode   // Loop body
}

// WhileForm represents while loops
type WhileForm struct {
    BaseNode
    Condition ASTNode
    Body      ASTNode
}

// WithForm represents context managers (with statement)
type WithForm struct {
    BaseNode
    Context ASTNode   // The context manager expression
    Alias   string    // Optional 'as' variable
    Body    ASTNode
}

// TryForm represents try/except/finally
type TryForm struct {
    BaseNode
    TryBody     ASTNode
    ExceptClauses []ExceptClause
    FinallyBody   ASTNode  // Optional
}

type ExceptClause struct {
    ExceptionType string   // Optional type filter
    Variable      string   // Optional 'as' variable
    Body          ASTNode
}

// ClassForm represents class definitions
type ClassForm struct {
    BaseNode
    Name       string
    Bases      []ASTNode     // Parent classes
    Methods    []DefForm     // Method definitions
    Attributes []AssignForm  // Class attributes
}

// DotExpr already exists but may need enhancement for chaining
```

## Python Tokenizer Requirements

```go
// parser/python_tokenizer.go

type PythonTokenizer struct {
    source      string
    pos         int
    line        int
    column      int
    indentStack []int  // Track indentation levels
}

// Key differences from Lisp tokenizer:
// 1. Whitespace-sensitive (indentation matters)
// 2. Newlines are significant
// 3. Colons introduce blocks
// 4. INDENT/DEDENT tokens for block structure
```

**Token types needed:**
- `TOKEN_INDENT` - Increase indentation level
- `TOKEN_DEDENT` - Decrease indentation level
- `TOKEN_NEWLINE` - Significant newline (not just whitespace)
- `TOKEN_COLON` - Block introducer
- All existing tokens (identifiers, numbers, strings, operators, etc.)

## Python Parser Structure

```go
// parser/python_parser.go

type PythonParser struct {
    tokens   []Token
    pos      int
    filename string
}

func (p *PythonParser) ParseToAST(source string) (ast.ASTNode, *core.IRMetadata, error) {
    // 1. Tokenize with indentation tracking
    tokenizer := NewPythonTokenizer(source)
    tokens, err := tokenizer.Tokenize()

    // 2. Parse module (top-level statements)
    astNode, err := p.parseModule()

    // 3. Populate metadata
    metadata := core.NewIRMetadata()
    p.populateMetadata(astNode, metadata)

    return astNode, metadata, nil
}

func (p *PythonParser) parseStatement() (ast.ASTNode, error) {
    tok := p.currentToken()

    switch tok.Type {
    case TOKEN_DEF:
        return p.parseFunctionDef()
    case TOKEN_CLASS:
        return p.parseClassDef()
    case TOKEN_IF:
        return p.parseIfStatement()
    case TOKEN_FOR:
        return p.parseForLoop()
    case TOKEN_WHILE:
        return p.parseWhileLoop()
    case TOKEN_WITH:
        return p.parseWithStatement()
    case TOKEN_TRY:
        return p.parseTryStatement()
    case TOKEN_RETURN:
        return p.parseReturnStatement()
    // ... etc
    default:
        // Expression statement or assignment
        return p.parseExpressionStatement()
    }
}

func (p *PythonParser) parseBlock() ([]ast.ASTNode, error) {
    // Expect INDENT token
    if !p.matchToken(TOKEN_INDENT) {
        return nil, fmt.Errorf("expected indent")
    }
    p.advanceToken()

    var statements []ast.ASTNode

    // Parse statements until DEDENT
    for !p.matchToken(TOKEN_DEDENT) {
        stmt, err := p.parseStatement()
        if err != nil {
            return nil, err
        }
        statements.append(statements, stmt)
    }

    p.advanceToken() // consume DEDENT
    return statements, nil
}
```

## Error Messages with Python Frontend

**When an error occurs in Python code:**

```python
# test.py
def calculate(x):
    result = x * 2
    print(undefined_var)  # Error here!
    return result
```

**Error output:**
```
NameError: name 'undefined_var' is not defined [Python syntax]
at test.py:3:11

File "test.py", line 3:
  1 | def calculate(x):
  2 |     result = x * 2
> 3 |     print(undefined_var)
              ^
  4 |     return result

Did you mean: result?
```

Notice:
- `[Python syntax]` indicates original source language
- Line numbers match Python source (not transformed Lisp)
- Source snippet shows Python code (not S-expressions)
- Column points to exact token in Python

## Implementation Phases

### Phase A: Extended Tokenizer (1 week)
- [ ] Implement indentation tracking
- [ ] Add INDENT/DEDENT token generation
- [ ] Handle significant newlines
- [ ] Comprehensive tokenizer tests

### Phase B: Statement Parsing (2 weeks)
- [ ] Parse function definitions (enhanced)
- [ ] Parse class definitions
- [ ] Parse if/elif/else chains
- [ ] Parse for/while loops
- [ ] Parse with statements
- [ ] Parse try/except/finally

### Phase C: Expression Enhancements (1 week)
- [ ] List/dict/set comprehensions
- [ ] Generator expressions
- [ ] Lambda expressions
- [ ] Ternary operator (x if cond else y)
- [ ] Slice notation (list[1:5])

### Phase D: AST Lowering (1 week)
- [ ] Implement ToIR() for all new AST nodes
- [ ] Test desugaring of Python constructs
- [ ] Verify evaluator compatibility

### Phase E: Integration (1 week)
- [ ] File extension detection (.py vs .m28)
- [ ] Parser selection based on extension
- [ ] Error message routing
- [ ] End-to-end tests

## Benefits

1. **True Python Compatibility**: Write actual Python, not "Pythonic Lisp"
2. **Better Error Messages**: Show errors in original Python syntax
3. **Type Hints**: Preserve Python type annotations for tooling
4. **IDE Support**: Foundation for Python LSP server
5. **Gradual Migration**: Run Python and M28 in same codebase
6. **Learning Curve**: Python developers can use familiar syntax

## Examples of Mixed Usage

```python
# utils.py (Python frontend)
def fibonacci(n: int) -> int:
    """Calculate nth Fibonacci number."""
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)

# main.m28 (M28 Lisp frontend)
(import utils)

# Call Python function from M28
(print (utils.fibonacci 10))

# Both compile to same IR!
```

## Key Insight

The beautiful part: **Both Python and M28 Lisp compile to the same S-expression IR**. The evaluator doesn't know or care which frontend was used. The AST layer we built makes this seamless:

- Python AST → ToIR() → S-expressions → Evaluate
- Lisp AST → ToIR() → S-expressions → Evaluate
- Same runtime, same semantics, different syntax!

## Current Syntax Sugar vs Full Python Frontend

**Current (Syntax Sugar):**
- Limited to expression-level transformations
- Still fundamentally Lisp with Python-like calls
- Can't handle Python-specific control flow
- No indentation-based blocks

**Full Python Frontend:**
- Complete Python syntax support
- Proper statement parsing
- Indentation-based blocks
- Python-native constructs (comprehensions, with, etc.)
- True `.py` file support

## Next Steps

To implement a full Python frontend, we would:

1. Create `parser/python_tokenizer.go` with indentation tracking
2. Create `parser/python_parser.go` with statement parsing
3. Add new AST node types in `core/ast/python_nodes.go`
4. Implement ToIR() methods for Python-specific constructs
5. Add file extension detection in main.go
6. Write comprehensive Python→AST→IR tests

The AST infrastructure is ready. We just need to build the Python-specific parsing layer on top!

# Phase C: Python Parser - Design Document

## Overview

Phase C implements a recursive descent parser that converts Python tokens (from Phase A) into Python AST nodes (from Phase B), which then desugar to S-expression IR via ToIR().

## Architecture

```
Python Source Code
       ↓
PythonTokenizer (Phase A)
       ↓
[TOKEN_DEF, TOKEN_IDENTIFIER("foo"), ...]
       ↓
PythonParser (Phase C) ← WE ARE HERE
       ↓
Python AST Nodes (Phase B)
       ↓ .ToIR()
S-expression IR
       ↓
Evaluator (existing)
```

## Parser Structure

### Core Components

```go
type PythonParser struct {
    tokens   []Token
    current  int

    // Error handling
    errors   []error
    panicMode bool
}

func NewPythonParser(tokens []Token) *PythonParser
func (p *PythonParser) Parse() ([]ast.ASTNode, error)
```

### Parsing Methods Hierarchy

```
Parse() - Entry point, parses module (list of statements)
  ↓
parseStatement() - Dispatches to specific statement parsers
  ├─ parseDefStatement()
  ├─ parseClassStatement()
  ├─ parseIfStatement()
  ├─ parseForStatement()
  ├─ parseWhileStatement()
  ├─ parseTryStatement()
  ├─ parseWithStatement()
  ├─ parseReturnStatement()
  ├─ parseBreakStatement()
  ├─ parseContinueStatement()
  ├─ parsePassStatement()
  ├─ parseRaiseStatement()
  └─ parseExpressionStatement() (assignment or bare expression)

parseExpression() - Expression parsing with precedence climbing
  ↓
parseComparison() → parseAddition() → parseMultiplication() → parseUnary() → parsePrimary()
  ↓
parsePrimary() - Atoms: identifiers, literals, parens, comprehensions
  ├─ parseIdentifier()
  ├─ parseLiteral() (numbers, strings, True/False/None)
  ├─ parseListLiteral() [1, 2, 3] or list comprehension
  ├─ parseDictLiteral() {"k": "v"} or dict comprehension
  ├─ parseSetLiteral() {1, 2, 3} or set comprehension
  ├─ parseParenthesized() (expr) or generator expression
  └─ parseLambda() (lambda x: x*2)

parseBlock() - Handles INDENT...statements...DEDENT
parseDecorators() - Handles @decorator sequences
parseParameters() - Function parameter list with types and defaults
parseTypeAnnotation() - Type hints (: int, -> str)
```

## Expression Parsing Strategy

### Precedence Climbing

Use precedence climbing for binary operators:

```
Precedence levels (lowest to highest):
1. or
2. and
3. not
4. in, not in, is, is not, <, <=, >, >=, !=, ==
5. |
6. ^
7. &
8. <<, >>
9. +, -
10. *, /, //, %
11. unary -, +, ~
12. **
13. postfix: (), [], .
```

### Example: Parsing `x + y * 2`

```
parseExpression()
  parseComparison()
    parseAddition()
      parseMultiplication() → x
      see +
      parseMultiplication()
        parseUnary() → y
        see *
        parseUnary() → 2
        return (* y 2)
      return (+ x (* y 2))
```

## Statement Parsing Examples

### 1. Function Definition

```python
@decorator
def foo(x: int, y: int = 10) -> int:
    return x + y
```

**Parsing steps:**
1. `parseDecorators()` → collect [@decorator]
2. Expect TOKEN_DEF
3. Expect TOKEN_IDENTIFIER("foo")
4. `parseParameters()` → collect [(x, int, nil), (y, int, 10)]
5. Expect TOKEN_ARROW, `parseTypeAnnotation()` → int
6. Expect TOKEN_COLON, TOKEN_NEWLINE, TOKEN_INDENT
7. `parseBlock()` → collect statements until DEDENT
8. Create DefForm with decorators

### 2. Class Definition

```python
@dataclass
class Counter(BaseClass):
    def __init__(self, start: int = 0):
        self.value = start
```

**Parsing steps:**
1. `parseDecorators()` → [@dataclass]
2. Expect TOKEN_CLASS
3. Expect TOKEN_IDENTIFIER("Counter")
4. Parse base classes in parens
5. Expect TOKEN_COLON, TOKEN_NEWLINE, TOKEN_INDENT
6. `parseBlock()` → parse method definitions
7. Create ClassForm

### 3. If Statement

```python
if x > 0:
    print("positive")
elif x < 0:
    print("negative")
else:
    print("zero")
```

**Parsing steps:**
1. Expect TOKEN_IF
2. `parseExpression()` → condition
3. Expect TOKEN_COLON, `parseBlock()` → then branch
4. Check for TOKEN_ELIF (recursive)
5. Check for TOKEN_ELSE
6. Create IfForm

### 4. For Loop with Else

```python
for i in range(10):
    if i == 5:
        break
else:
    print("completed")
```

**Parsing steps:**
1. Expect TOKEN_FOR
2. Expect TOKEN_IDENTIFIER (loop variable)
3. Expect TOKEN_IN
4. `parseExpression()` → iterable
5. Expect TOKEN_COLON, `parseBlock()` → body
6. Check for TOKEN_ELSE, `parseBlock()` → else body
7. Create ForForm

### 5. Try/Except/Finally

```python
try:
    risky()
except ValueError as e:
    handle(e)
except:
    fallback()
finally:
    cleanup()
```

**Parsing steps:**
1. Expect TOKEN_TRY, TOKEN_COLON, `parseBlock()` → try body
2. Loop while TOKEN_EXCEPT:
   - Parse exception type (optional)
   - Parse TOKEN_AS, variable (optional)
   - Expect TOKEN_COLON, `parseBlock()` → except body
3. Check for TOKEN_ELSE, `parseBlock()` → else body
4. Check for TOKEN_FINALLY, `parseBlock()` → finally body
5. Create TryForm

### 6. List Comprehension

```python
[x*x for x in range(10) if x % 2 == 0]
```

**Parsing steps:**
1. See TOKEN_LBRACKET
2. `parseExpression()` → element expression
3. Check for TOKEN_FOR (it's a comprehension!)
4. Expect TOKEN_IDENTIFIER → loop variable
5. Expect TOKEN_IN, `parseExpression()` → iterable
6. Check for TOKEN_IF, `parseExpression()` → condition
7. Expect TOKEN_RBRACKET
8. Create ComprehensionForm(ListComp, ...)

## Block Parsing

Blocks are delimited by INDENT/DEDENT tokens:

```python
if condition:     # TOKEN_COLON, TOKEN_NEWLINE
    stmt1         # TOKEN_INDENT
    stmt2
    stmt3
next_stmt         # TOKEN_DEDENT
```

**Implementation:**

```go
func (p *PythonParser) parseBlock() []ast.ASTNode {
    // Expect COLON NEWLINE INDENT
    p.expect(TOKEN_COLON)
    p.expect(TOKEN_NEWLINE)
    p.expect(TOKEN_INDENT)

    statements := []ast.ASTNode{}

    for !p.check(TOKEN_DEDENT) && !p.isAtEnd() {
        statements = append(statements, p.parseStatement())

        // Consume optional newlines between statements
        for p.check(TOKEN_NEWLINE) {
            p.advance()
        }
    }

    p.expect(TOKEN_DEDENT)
    return statements
}
```

## Decorator Parsing

```python
@decorator1
@decorator2(arg)
def foo():
    pass
```

**Implementation:**

```go
func (p *PythonParser) parseDecorators() []ast.ASTNode {
    decorators := []ast.ASTNode{}

    for p.check(TOKEN_AT) {
        p.advance() // consume @

        // Parse decorator expression (identifier or call)
        decorator := p.parsePrimary()

        // If followed by (, it's a decorator call
        if p.check(TOKEN_LPAREN) {
            decorator = p.parseCall(decorator)
        }

        decorators = append(decorators, decorator)
        p.expect(TOKEN_NEWLINE)
    }

    return decorators
}
```

## Type Annotation Parsing

```python
def foo(x: int, y: List[str] = []) -> Dict[str, int]:
    pass
```

**Implementation:**

```go
func (p *PythonParser) parseTypeAnnotation() *ast.TypeInfo {
    if !p.check(TOKEN_COLON) && !p.check(TOKEN_ARROW) {
        return nil
    }

    p.advance() // consume : or ->

    // Parse type name
    name := p.expect(TOKEN_IDENTIFIER).Lexeme

    // Check for generic types: List[int], Dict[str, int]
    var generics []*ast.TypeInfo
    if p.check(TOKEN_LBRACKET) {
        p.advance()
        for {
            generics = append(generics, p.parseTypeAnnotation())
            if !p.check(TOKEN_COMMA) {
                break
            }
            p.advance()
        }
        p.expect(TOKEN_RBRACKET)
    }

    return &ast.TypeInfo{
        Name:    name,
        Generic: generics,
    }
}
```

## Error Handling Strategy

### Panic Mode Recovery

When encountering an error:
1. Record the error
2. Enter panic mode
3. Synchronize to the next statement boundary
4. Continue parsing

```go
func (p *PythonParser) synchronize() {
    p.panicMode = false

    for !p.isAtEnd() {
        // Found a statement boundary
        if p.previous().Type == TOKEN_NEWLINE {
            return
        }

        switch p.peek().Type {
        case TOKEN_DEF, TOKEN_CLASS, TOKEN_IF, TOKEN_FOR,
             TOKEN_WHILE, TOKEN_TRY, TOKEN_WITH, TOKEN_RETURN:
            return
        }

        p.advance()
    }
}

func (p *PythonParser) error(message string) {
    if p.panicMode {
        return // Don't cascade errors
    }

    p.panicMode = true
    tok := p.peek()
    err := fmt.Errorf("Parse error at line %d: %s", tok.Line, message)
    p.errors = append(p.errors, err)
}
```

## Implementation Order

### Phase C.1: Core Structure (Day 1)
- [ ] Create `parser/python_parser.go`
- [ ] Implement `PythonParser` struct and basic methods
- [ ] Implement token navigation (advance, peek, check, expect)
- [ ] Implement error handling and synchronization

### Phase C.2: Expression Parsing (Day 1-2)
- [ ] Implement precedence climbing for binary operators
- [ ] Parse literals (numbers, strings, True/False/None)
- [ ] Parse identifiers
- [ ] Parse parenthesized expressions
- [ ] Parse list/dict/set literals
- [ ] Parse function calls `foo(args)`
- [ ] Parse attribute access `obj.attr`
- [ ] Parse subscripting `list[0]`

### Phase C.3: Simple Statements (Day 2)
- [ ] Parse return, break, continue, pass, raise
- [ ] Parse expression statements (assignments)
- [ ] Parse augmented assignments (+=, -=, etc.)

### Phase C.4: Block Statements (Day 3)
- [ ] Implement `parseBlock()` with INDENT/DEDENT
- [ ] Parse if/elif/else
- [ ] Parse for loops (with optional else)
- [ ] Parse while loops (with optional else)

### Phase C.5: Function & Class Definitions (Day 3-4)
- [ ] Parse function parameters with types and defaults
- [ ] Parse function definitions
- [ ] Parse decorators
- [ ] Apply decorators to functions
- [ ] Parse class definitions
- [ ] Apply decorators to classes

### Phase C.6: Advanced Statements (Day 4)
- [ ] Parse try/except/finally/else
- [ ] Parse with statements (single and multiple)

### Phase C.7: Comprehensions (Day 4-5)
- [ ] Parse list comprehensions
- [ ] Parse dict comprehensions
- [ ] Parse set comprehensions
- [ ] Parse generator expressions

### Phase C.8: Testing & Polish (Day 5)
- [ ] Write comprehensive tests for each construct
- [ ] Test error recovery
- [ ] Test nested structures
- [ ] Integration tests with tokenizer and AST

## Test Strategy

### Unit Tests

Test each parsing function individually:

```go
func TestParseLiteral(t *testing.T) {
    tokens := []Token{
        {Type: TOKEN_NUMBER, Value: core.NumberValue(42)},
        {Type: TOKEN_EOF},
    }
    parser := NewPythonParser(tokens)
    node := parser.parsePrimary()

    literal, ok := node.(*ast.Literal)
    assert.True(t, ok)
    assert.Equal(t, core.NumberValue(42), literal.Value)
}
```

### Integration Tests

Test complete programs:

```go
func TestParseSimpleFunction(t *testing.T) {
    source := `def add(x, y):
    return x + y`

    tokenizer := NewPythonTokenizer(source)
    tokens, err := tokenizer.Tokenize()
    assert.NoError(t, err)

    parser := NewPythonParser(tokens)
    nodes, err := parser.Parse()
    assert.NoError(t, err)
    assert.Len(t, nodes, 1)

    defForm, ok := nodes[0].(*ast.DefForm)
    assert.True(t, ok)
    assert.Equal(t, "add", defForm.Name)
}
```

### Error Recovery Tests

Test that parser recovers from errors:

```go
func TestErrorRecovery(t *testing.T) {
    source := `def foo(:  # Syntax error
def bar():
    return 1`

    // Should report error but continue and parse bar()
    parser := parseSource(source)
    assert.Len(t, parser.errors, 1)  // One error
    assert.Len(t, parser.nodes, 1)   // But parsed bar()
}
```

## Success Criteria

- [ ] All Python statement types parse correctly
- [ ] All Python expression types parse correctly
- [ ] Decorators work for functions and classes
- [ ] Type annotations are captured
- [ ] Comprehensions desugar to ComprehensionForm
- [ ] Blocks using INDENT/DEDENT work correctly
- [ ] Error recovery allows continued parsing
- [ ] 50+ comprehensive tests pass
- [ ] Integration with tokenizer works end-to-end
- [ ] Generated AST nodes produce correct IR via ToIR()

## Example End-to-End Flow

**Input Python:**
```python
def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

**Tokenizer output:**
```
TOKEN_DEF, TOKEN_IDENTIFIER("factorial"), TOKEN_LPAREN,
TOKEN_IDENTIFIER("n"), TOKEN_COLON, TOKEN_IDENTIFIER("int"),
TOKEN_RPAREN, TOKEN_ARROW, TOKEN_IDENTIFIER("int"), TOKEN_COLON,
TOKEN_NEWLINE, TOKEN_INDENT, TOKEN_IF, ...
```

**Parser output (AST):**
```go
&ast.DefForm{
    Name: "factorial",
    Params: []ast.Parameter{
        {Name: "n", Type: &ast.TypeInfo{Name: "int"}},
    },
    ReturnType: &ast.TypeInfo{Name: "int"},
    Body: &ast.IfForm{...},
}
```

**IR output (via ToIR()):**
```lisp
(def factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

## Next Steps

After Phase C is complete, we'll have a working Python-to-IR compiler ready for Phase D (integration with the evaluator).

# S-Strings Design Document

## Overview

S-strings (Syntax strings) are a metaprogramming feature that enables code generation through template-like syntax with interpolation. They provide a clean, intuitive way to construct M28 code programmatically, primarily for use in macros but also available as a general-purpose code generation tool.

**Status**: Design Phase
**Target Version**: M28 v0.2.0
**Related**: Macro System, Quasiquoting

## Motivation

### Current Problem

Without s-strings, creating code templates requires verbose construction:

```lisp
# Manual code construction - verbose and error-prone
(defmacro unless (condition body)
  (list 'if (list 'not condition) body 'None))

# Hard to read, easy to make mistakes
# No visual correspondence to the code being generated
```

### With S-Strings

S-strings provide a cleaner, more maintainable approach:

```lisp
# With s-strings - clear and concise
(defmacro unless (condition body)
  s"(if (not {condition}) {body} None)")

# Looks like the code it generates
# Visual structure matches output structure
```

## Core Design Decisions

Based on research of similar features in Julia, Clojure, Scala, Rust, and traditional Lisps, we adopt the following design:

| Feature | Syntax | Rationale |
|---------|--------|-----------|
| **Value Interpolation** | `{x}` | Insert value at parse-time (like Julia) |
| **Code Interpolation** | `{=x}` | Insert code/symbol literally |
| **Sequence Splicing** | `{*args}` | Python-style unpacking with `*` |
| **Dict Splicing** | `{**kwargs}` | Python-style dict unpacking |
| **Auto-gensym** | `{x#}` | Clojure-style hygiene with `#` |
| **Escape Braces** | `\{` `\}` | Standard escape syntax |
| **Raw Strings** | `rs"..."` | No interpolation at all |
| **Evaluation Time** | Parse-time | Capture values immediately |

## Syntax Specification

### Basic S-String Syntax

```ebnf
s-string     ::= "s" string-literal
               | "s'" string-literal-single
raw-string   ::= "rs" string-literal
               | "rs'" string-literal-single

interpolation ::= "{" expr "}"              # Value interpolation
                | "{=" expr "}"             # Code interpolation
                | "{*" expr "}"             # Sequence splice
                | "{**" expr "}"            # Dict splice
                | "{" identifier "#}"       # Auto-gensym

escape        ::= "\{" | "\}" | "\\" | "\"" | "\n" | "\t"
```

### Interpolation Forms

#### 1. Value Interpolation: `{expr}`

Evaluates `expr` and inserts its value into the generated code:

```lisp
(= x 10)
(= code s"(+ {x} 1)")
# Result: (+ 10 1)

(= name "Alice")
(= greeting s"(print (+ \"Hello \" {name}))")
# Result: (print (+ "Hello " "Alice"))
```

#### 2. Code Interpolation: `{=expr}`

Inserts the code/symbol without evaluating:

```lisp
(= var 'x)
(= code s"(+ {=var} 1)")
# Result: (+ x 1)  ← symbol x, not its value

(= param-name 'count)
(= func s"(def (increment {=param-name}) (+ {=param-name} 1))")
# Result: (def (increment count) (+ count 1))
```

#### 3. Sequence Splicing: `{*expr}`

Unpacks a sequence into multiple arguments (Python-style):

```lisp
(= args [1 2 3])
(= code s"(sum {*args})")
# Result: (sum 1 2 3)

(= list1 [1 2])
(= list2 [4 5])
(= combined s"(list 0 {*list1} 3 {*list2} 6)")
# Result: (list 0 1 2 3 4 5 6)
```

#### 4. Dict Splicing: `{**expr}`

Unpacks a dictionary into keyword arguments:

```lisp
(= options {"precision" 2 "rounding" "ROUND_HALF_UP"})
(= code s"(Context {**options})")
# Result: (Context precision=2 rounding="ROUND_HALF_UP")
```

#### 5. Auto-gensym: `{name#}`

Creates a unique symbol for macro hygiene (Clojure-style):

```lisp
(defmacro square (x)
  s"(let (tmp# {x})
     (* tmp# tmp#))")

# Expands to something like:
# (let (tmp_G_42 10) (* tmp_G_42 tmp_G_42))

# Each {tmp#} in the same s-string gets the SAME unique name
# Different s-strings get DIFFERENT unique names
```

### Escape Sequences

Standard escape sequences work inside s-strings:

```lisp
s"literal \{ brace"           # → literal { brace
s"newline \n here"            # → newline with actual newline
s"quote \" inside"            # → quote " inside
s"backslash \\ here"          # → backslash \ here

# Escaped interpolation - not evaluated
s"show \{x\} as literal"     # → show {x} as literal
```

### Raw S-Strings

Raw s-strings disable all interpolation:

```lisp
(= x 10)
(= code rs"(+ {x} 1)")
# Result: (+ {x} 1)  ← literal braces, no interpolation

# Useful for templates, documentation, or meta-programming
(= template rs"Use {var} for interpolation")
# Result: Use {var} for interpolation
```

## Semantics

### Evaluation Timing

S-strings evaluate interpolations **at parse-time** (when the s-string is created):

```lisp
(= x 10)
(= code s"(+ {x} 1)")  # {x} evaluated NOW, captures 10
(= x 20)
(print code)           # Still prints: (+ 10 1)
```

This is the most intuitive behavior and matches:
- Julia's `:()` with `$`
- Template literals in most languages
- Expected behavior for macros

### Return Value

S-strings return **AST nodes** (expression objects), not strings:

```lisp
(= code s"(+ 1 2)")
(type code)  # → "list" or "expr" (not "string")
(eval code)  # → 3
```

### Scope and Binding

Interpolations capture values from the enclosing lexical scope:

```lisp
(def (make-incrementer n)
  s"(lambda (x) (+ x {n}))")

(= inc5 (make-incrementer 5))
# inc5 = (lambda (x) (+ x 5))  ← n=5 is captured

(= inc10 (make-incrementer 10))
# inc10 = (lambda (x) (+ x 10))  ← n=10 is captured
```

### Nested Interpolation

S-strings can be nested, with inner s-strings evaluated first:

```lisp
(= inner 5)
(= outer s"(+ {s"(* {inner} 2)"} 3)")
# Inner evaluates: s"(* {inner} 2)" → (* 5 2) → 10
# Outer evaluates: s"(+ {10} 3)" → (+ 10 3)

# Or preserve as code:
(= outer s"(+ {=s"(* {inner} 2)"} 3)")
# Result: (+ (* 5 2) 3)
```

### Auto-gensym Scope

Auto-gensym symbols (`{name#}`) maintain consistency within a single s-string:

```lisp
# Same s-string - same generated name
s"(let (x# 10) (+ x# x#))"
# → (let (x_G_42 10) (+ x_G_42 x_G_42))

# Different s-strings - different names
(= s1 s"(let (x# 10) x#)")
(= s2 s"(let (x# 20) x#)")
# s1: (let (x_G_42 10) x_G_42)
# s2: (let (x_G_43 20) x_G_43)
```

## Comparison with Other Languages

### Julia
```julia
# Julia
x = 10
ex = :($x + 1)  # :(10 + 1)

# M28 equivalent
(= x 10)
s"{x} + 1"  # Note: M28 uses prefix notation
```

### Clojure
```clojure
; Clojure
(def x 10)
`(+ ~x 1)      ; (+ 10 1)
`(+ ~@lst 1)   ; Unquote-splice

; M28 equivalent
(= x 10)
s"(+ {x} 1)"      # (+ 10 1)
s"(+ {*lst} 1)"   # Splice
```

### Rust quote!
```rust
// Rust
let name = ...;
quote! {
    impl MyTrait for #name {
        fn method(&self) { }
    }
}

// M28 equivalent
(= name ...)
s"(class {name} ()
    (def (method self) ...))"
```

### Python f-strings (inspiration)
```python
# Python f-strings (runtime, not code gen)
x = 10
s = f"value is {x}"  # "value is 10"

# M28 s-strings (compile-time code gen)
(= x 10)
s"(print {x})"  # (print 10) - generates code
```

## Use Cases and Examples

### 1. Simple Macros

```lisp
# Unless macro
(defmacro unless (condition body)
  s"(if (not {condition})
       {body}
       None)")

(unless (< x 0)
  (print "x is non-negative"))

# Expands to:
# (if (not (< x 0))
#     (print "x is non-negative")
#     None)
```

### 2. Debugging Macros

```lisp
(defmacro debug (expr)
  s"(begin
      (print \"Evaluating: {=expr}\")
      (= result# {expr})
      (print \"Result: \" result#)
      result#)")

(debug (+ 2 3))
# Prints: Evaluating: (+ 2 3)
# Prints: Result: 5
# Returns: 5
```

### 3. Class Generation

```lisp
(defmacro defstruct (name fields)
  s"(class {=name} ()
      (def (__init__ self {*fields})
        {*[(s"(= self.{=f} {=f})") for f in fields]})))"

(defstruct Point [x y])

# Expands to:
# (class Point ()
#   (def (__init__ self x y)
#     (= self.x x)
#     (= self.y y)))
```

### 4. Threading Macros

```lisp
(defmacro -> (x forms)
  (if (empty? forms)
      x
      s"(-> {s"({(first forms)} {x})"} {(rest forms)})"))

(-> 5
    (+ 3)
    (* 2)
    (- 1))

# Expands to: (- (* (+ 5 3) 2) 1)
# Evaluates to: 15
```

### 5. Code Generation from Data

```lisp
# Generate accessor functions from field list
(def (make-accessors class-name fields)
  (map (lambda (field)
         s"(def (get-{=field} obj)
             obj.{=field})")
       fields))

(= accessors (make-accessors 'Person ['name 'age 'email]))
# Generates: get-name, get-age, get-email functions
```

### 6. DSL Construction

```lisp
# HTML-like DSL
(defmacro html (tag attrs body)
  s"(dict
      \"tag\" {=tag}
      \"attrs\" {attrs}
      \"children\" {body})")

(html div {"class" "container"}
  [(html h1 {} ["Title"])
   (html p {} ["Content"])])
```

## Edge Cases and Error Handling

### 1. Malformed Interpolation

```lisp
# Missing closing brace
s"(+ {x 1)"  # SyntaxError: Unclosed interpolation

# Invalid expression
s"(+ {&&&} 1)"  # SyntaxError: Invalid expression in interpolation

# Empty interpolation
s"(+ {} 1)"  # SyntaxError: Empty interpolation
```

### 2. Type Errors in Splicing

```lisp
# Non-sequence splice
(= x 10)
s"(sum {*x})"  # TypeError: Cannot splice non-sequence type 'number'

# Non-dict dict-splice
(= x [1 2])
s"(func {**x})"  # TypeError: Cannot dict-splice non-dict type 'list'
```

### 3. Gensym in Wrong Context

```lisp
# Auto-gensym outside macro
(= x s"(let (tmp# 10) tmp#)")
# Warning: Auto-gensym outside macro context may not be necessary

# But still works (generates unique name)
```

### 4. Nested Escapes

```lisp
# Multiple escape levels
s"outer \{inner {s"nested \{deep\}"}\}"
# Requires careful escape tracking
```

### 5. Splicing Empty Collections

```lisp
(= empty-list [])
s"(sum {*empty-list})"
# Result: (sum)  ← valid, just no arguments
```

## Implementation Plan

### Phase 1: Parser Extensions (Week 1)

**Files to modify:**
- `parser/parser.go` - Add s-string token recognition
- `parser/fstring.go` - Reuse/extend interpolation parsing

**Tasks:**
1. Add `s"..."` and `s'...'` token recognition
2. Add `rs"..."` for raw s-strings
3. Parse interpolation expressions `{...}`
4. Distinguish interpolation types: `{x}`, `{=x}`, `{*x}`, `{**x}`, `{x#}`
5. Handle escape sequences `\{`, `\}`
6. Build AST nodes for s-strings

**Test cases:**
```lisp
s"(+ 1 2)"                    # Basic s-string
s"(+ {x} 1)"                  # Value interpolation
s"(+ {=x} 1)"                 # Code interpolation
s"(sum {*args})"              # Splice
s"(let (tmp# 10) tmp#)"       # Auto-gensym
s"literal \{ brace"           # Escape
rs"no {interpolation} here"   # Raw string
```

### Phase 2: AST Node Types (Week 1)

**Files to create/modify:**
- `core/ast.go` (new) or extend existing AST types
- `core/value.go` - Add s-string-specific value types if needed

**New AST node types:**
```go
type SStringNode struct {
    Template  string
    Interps   []Interpolation
    Raw       bool  // true for rs"..."
}

type Interpolation struct {
    Type     InterpolationType  // Value, Code, Splice, DictSplice, Gensym
    Expr     Value              // The expression to interpolate
    Position int                // Position in template
}

type InterpolationType int
const (
    InterpValue     InterpolationType = iota  // {x}
    InterpCode                                // {=x}
    InterpSplice                              // {*x}
    InterpDictSplice                          // {**x}
    InterpGensym                              // {x#}
)
```

### Phase 3: Evaluator (Week 2)

**Files to modify:**
- `eval/evaluator.go` - Add s-string evaluation
- `core/context.go` - Gensym counter/registry

**Implementation:**
1. Evaluate interpolations in current context
2. Build resulting expression AST
3. Handle splicing (flatten sequences)
4. Generate unique names for gensyms
5. Track gensym names within single s-string

**Key functions:**
```go
func EvaluateSString(node *SStringNode, ctx *Context) (Value, error)
func ProcessInterpolation(interp Interpolation, ctx *Context) (Value, error)
func SpliceSequence(vals []Value) []Value
func GenerateGensym(name string, ctx *Context) SymbolValue
```

### Phase 4: Macro Integration (Week 2)

**Files to modify:**
- `eval/evaluator.go` - Ensure macros can return s-strings
- `special_forms/defmacro.go` (if separate)

**Tasks:**
1. Allow s-strings in macro bodies
2. Ensure proper evaluation order
3. Test macro expansion with s-strings
4. Validate hygiene with gensyms

### Phase 5: Testing (Week 3)

**Test files to create:**
- `tests/sstring-basic.m28` - Basic interpolation
- `tests/sstring-splice.m28` - Splicing tests
- `tests/sstring-gensym.m28` - Hygiene tests
- `tests/sstring-macros.m28` - Macro integration
- `tests/sstring-edge-cases.m28` - Error handling

**Test coverage:**
- All interpolation types
- Nested s-strings
- Error conditions
- Macro usage
- Gensym uniqueness
- Escape sequences

### Phase 6: Documentation (Week 3)

**Documentation to create:**
- User guide: `docs/features/s-strings.md`
- Tutorial: `docs/tutorial.md` (update)
- Examples: `examples/metaprogramming/`
- API docs: Update function documentation

### Phase 7: Examples and Validation (Week 4)

**Example programs:**
1. Common macros (unless, when, cond)
2. Threading macros (->, ->>)
3. DSL examples (HTML, SQL)
4. Code generation examples
5. Debugging utilities

## Future Extensions

### 1. Pattern Matching in S-Strings

```lisp
# Match and destructure in interpolation
s"(case {expr}
    ({pattern} {body}))"
```

### 2. Conditional Interpolation

```lisp
# Optional sections
s"(def (func x) {if debug s"(print x)" else ""})"
```

### 3. Iteration in S-Strings

```lisp
# Repeat pattern for each item
s"(sum {#(square {item})* for item in items})"
```

### 4. Type-safe S-Strings

```lisp
# Validate generated code structure
s"(def (func) -> int {body})" : FunctionType
```

### 5. Multi-line S-Strings with Indentation

```lisp
# Heredoc-style s-strings
s"""
(class MyClass ()
  (def (method self)
    {body}))
"""
```

## Migration Path

### From Current Quasiquote (if exists)

```lisp
# Before (hypothetical quasiquote)
`(+ ,x 1)

# After (s-strings)
s"(+ {x} 1)"
```

### From Manual Construction

```lisp
# Before
(list '+ x 1)

# After
s"(+ {x} 1)"
```

## Alternatives Considered

### 1. Traditional Quasiquote (`` ` `` and `,`)
- **Pros**: Standard in Lisps, familiar
- **Cons**: Hard to see backtick, comma; doesn't fit M28's Python inspiration
- **Decision**: S-strings more visible and familiar to broader audience

### 2. Template Strings with `$`
- **Pros**: Like Julia, JavaScript
- **Cons**: `$` not as clear for splicing
- **Decision**: Braces more explicit, can use modifiers (`{*x}`)

### 3. Function-based Construction
- **Pros**: No new syntax
- **Cons**: Verbose, hard to read
- **Decision**: S-strings provide better UX

## Open Questions

1. **Should s-strings work in REPL immediately?**
   - Likely yes for testing, but document evaluation timing clearly

2. **Error messages: show s-string source or expanded code?**
   - Both? Show template with error location + expanded code

3. **Should we support format specifications like f-strings?**
   - `s"value is {x:.2f}"` → Probably not initially, keep it simple

4. **Interaction with docstrings?**
   - Should docstrings support s-string interpolation?
   - Probably not - keep docs static

5. **Performance implications?**
   - S-strings evaluated at parse-time, so minimal runtime cost
   - Macro expansion happens once
   - Need benchmarks to validate

## References

- Julia Metaprogramming: https://docs.julialang.org/en/v1/manual/metaprogramming/
- Clojure Macros: https://www.braveclojure.com/writing-macros/
- Rust quote! crate: https://docs.rs/quote/
- Scala 3 Quotes: https://docs.scala-lang.org/scala3/guides/macros/quotes.html
- Python f-strings: https://peps.python.org/pep-0498/

## Version History

- **v0.1** - 2024-12-XX - Initial design document

# M28 Compiler Architecture

## Executive Summary

M28 is designed as a **multi-frontend, multi-backend language platform** that supports multiple syntaxes (Python, Lisp, future languages) and multiple execution strategies (interpreter, bytecode, native compilation). The architecture uses a **High-Level Intermediate Representation (HIR)** as the universal pivot point between frontends and backends.

**Key Design Principles:**
- **Language-neutral core** - HIR represents semantics, not syntax
- **Clean layer separation** - Frontends, HIR, and backends are independent
- **Incremental migration** - Add frontends/backends without breaking existing code
- **Performance path** - Clear evolution from interpreted → bytecode → native
- **Both static and dynamic** - Single architecture supports both typing disciplines

**Current State (Nov 2024):**
- ✅ M28 Lisp frontend with Pythonic sugar
- ✅ Tree-walking interpreter backend
- ✅ AST layer with source locations
- 🚧 HIR layer (using core.Value, needs formalization)
- 📋 Bytecode VM backend (designed, not implemented)
- 📋 Python frontend (designed, partially implemented)
- 📋 LLVM/WASM backends (future)

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Layer 1: Frontends](#layer-1-frontends)
3. [Layer 2: HIR (High-Level IR)](#layer-2-hir-high-level-ir)
4. [Layer 3: Backends](#layer-3-backends)
5. [Static vs Dynamic Languages](#static-vs-dynamic-languages)
6. [Why Custom HIR (Not LLVM)](#why-custom-hir-not-llvm)
7. [Migration Strategy](#migration-strategy)
8. [Implementation Guide](#implementation-guide)
9. [Performance Evolution](#performance-evolution)
10. [References](#references)

---

## Architecture Overview

### The Big Picture

```
┌──────────────────────────────────────────────────────────┐
│                    FRONTENDS                             │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐              │
│  │  Python  │  │   M28    │  │  Future  │              │
│  │  Parser  │  │  Parser  │  │ Language │              │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘              │
│       │             │              │                     │
│       └─────────────┴──────────────┘                    │
│                     │                                    │
│              Language-Specific AST                       │
│         (Python AST, M28 AST, etc.)                      │
└──────────────────────────────────────────────────────────┘
                      │
                      ↓ Lower to HIR
┌──────────────────────────────────────────────────────────┐
│                 HIR (High-Level IR)                       │
│                                                          │
│  Language-neutral representation of program semantics   │
│  - Operations: Call, Binding, Conditional, Loop, etc.   │
│  - Optional type information (for gradual typing)       │
│  - Optimizable (constant folding, DCE, inlining)        │
│  - Not LLVM - higher level, easier to work with         │
└──────────────────────────────────────────────────────────┘
                      │
                      ↓ Backend lowering
┌──────────────────────────────────────────────────────────┐
│                    BACKENDS                              │
│                                                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐    │
│  │Interpreter  │  │  Bytecode   │  │    LLVM     │    │
│  │ (current)   │  │     VM      │  │  (future)   │    │
│  │   1x        │  │   3-10x     │  │   50-100x   │    │
│  └─────────────┘  └─────────────┘  └─────────────┘    │
│                                                          │
│  ┌─────────────┐                                        │
│  │    WASM     │                                        │
│  │  (future)   │                                        │
│  │  portable   │                                        │
│  └─────────────┘                                        │
└──────────────────────────────────────────────────────────┘
```

### Key Insight: No M×N Problem

Without a pivot IR, supporting M frontends and N backends requires M×N compilers:

```
Bad: Direct compilation
Python  →  Interpreter
Python  →  Bytecode
Python  →  LLVM
M28     →  Interpreter
M28     →  Bytecode
M28     →  LLVM
= 6 compilers for 2 languages × 3 backends
```

With HIR as pivot, you only need M+N components:

```
Good: HIR as pivot
Python  ↘
M28     → HIR → Interpreter
Future  ↗     → Bytecode
              → LLVM
= 3 frontends + 3 backends = 6 components (vs 9)
```

---

## Layer 1: Frontends

### What is a Frontend?

A frontend is responsible for:
1. **Parsing** source code into a language-specific AST
2. **Lowering** the AST to HIR
3. **Preserving** source locations, type hints, comments

### Frontend Interface

```go
// frontend/common/interface.go
package common

import "github.com/mmichie/m28/hir"

// Frontend represents a language frontend
type Frontend interface {
    Name() string                        // "python", "m28", "javascript"
    Parse(source string) (AST, error)    // Parse source → language AST
}

// AST represents a language-specific abstract syntax tree
type AST interface {
    Lower() (*hir.Module, error)         // Lower to HIR
    Language() string                    // Which language produced this
}
```

### M28 Lisp Frontend

**Status:** ✅ Working (current implementation)

```
Source: (def add (a b) (+ a b))
   ↓ parser/parser.go
M28 AST with Pythonic sugar support
   ↓ ast.ToIR()
HIR (currently core.Value, needs formalization)
```

**Features:**
- S-expression syntax: `(+ 1 2)`
- Pythonic sugar: `print("hello")` → `(print "hello")`
- Infix operators: `x * 2` → `(* x 2)`
- Assignment: `x = 10` → `(= x 10)`

**Directory structure (proposed):**
```
frontend/m28/
├── parser.go       # M28 parser (from parser/parser.go)
├── ast.go          # M28-specific AST nodes
├── lower.go        # M28 AST → HIR lowering
└── desugar.go      # Pythonic sugar → canonical M28
```

### Python Frontend

**Status:** 🚧 Partially implemented

```
Source: def add(a, b): return a + b
   ↓ parser/python_parser.go
Python AST (python_nodes.go)
   ↓ ast.ToIR()
HIR (same as M28!)
```

**Features:**
- Full Python syntax (indentation-based)
- Type hints: `def f(x: int) -> int:`
- Comprehensions: `[x*x for x in range(10)]`
- Classes: `class Foo: ...`
- Context managers: `with open(f) as file:`

**Directory structure (proposed):**
```
frontend/python/
├── tokenizer.go    # Indentation-aware tokenizer
├── parser.go       # Python statement parser
├── ast.go          # Python-specific AST nodes (from core/ast/python_nodes.go)
├── lower.go        # Python AST → HIR lowering
└── desugar.go      # Python features → HIR primitives
```

### Adding a New Frontend (e.g., JavaScript)

**Steps:**
1. Create `frontend/javascript/` directory
2. Implement parser (tokens → JS AST)
3. Implement lowering (JS AST → HIR)
4. That's it! Works with all backends automatically

**Example:**
```go
// frontend/javascript/lower.go

func (jsAST *JSFunctionDecl) Lower() *hir.Function {
    return &hir.Function{
        Name: jsAST.Name,
        Params: lowerParams(jsAST.Params),
        Body: lowerBody(jsAST.Body),
        // HIR doesn't care that this came from JavaScript!
    }
}
```

---

## Layer 2: HIR (High-Level IR)

### What is HIR?

HIR (High-Level Intermediate Representation) is M28's **language-neutral representation** of program semantics. It's the pivot point between frontends and backends.

**Design goals:**
- **Language-neutral** - No Python-isms, no Lisp-isms
- **Semantic** - Represents what the code does, not how it's written
- **High-level** - Easier than LLVM, harder than AST
- **Optimizable** - Clean structure for optimization passes
- **Executable** - Can be interpreted directly
- **Lowerable** - Can be compiled to bytecode/LLVM/WASM

### HIR Node Types

```go
// hir/operation.go
package hir

// Module is the top-level compilation unit
type Module struct {
    Name      string
    Functions []*Function
    Globals   []*Binding
    Source    *SourceInfo
}

// Function represents a callable
type Function struct {
    Name       string
    Params     []*Parameter
    Body       Operation      // Single operation (often Block)
    ReturnType *TypeInfo      // Optional (for gradual typing)
    Source     *SourceInfo
}

type Parameter struct {
    Name    string
    Type    *TypeInfo      // Optional
    Default Operation      // Optional
}

// Operation is the fundamental HIR node
type Operation interface {
    OpType() OpKind
    TypeInfo() *TypeInfo   // Optional type info
    Source() *SourceInfo   // Source location
}

// Core operation types
type (
    // Literals
    IntLit    struct { Value int64 }
    FloatLit  struct { Value float64 }
    StringLit struct { Value string }
    BoolLit   struct { Value bool }
    NoneLit   struct {}

    // Collections
    ListLit   struct { Elements []Operation }
    DictLit   struct { Pairs [][2]Operation }
    TupleLit  struct { Elements []Operation }
    SetLit    struct { Elements []Operation }

    // Variables
    LoadVar   struct { Name string }
    StoreVar  struct { Name string; Value Operation }

    // Control flow
    Call      struct {
        Function Operation
        Args     []Operation
        Kwargs   map[string]Operation
    }

    If        struct {
        Condition Operation
        Then      Operation
        Else      Operation
    }

    Block     struct {
        Ops []Operation
    }

    Return    struct {
        Value Operation
    }

    Loop      struct {
        Iterator  Operation
        LoopVar   string
        Body      Operation
    }

    // Type guards (for optimization)
    Guard     struct {
        Value    Operation
        Expected *TypeInfo
    }
)

// Type information (for gradual typing)
type TypeInfo struct {
    Kind       TypeKind           // Any, Concrete, Generic, Union
    Concrete   *ConcreteType      // For static types: int, str, MyClass
    Generic    []TypeConstraint   // For generics: List[T]
    Runtime    bool               // True = check at runtime, False = compile-time
}
```

### Example: Python → HIR

**Python source:**
```python
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)
```

**HIR:**
```go
&hir.Function{
    Name: "factorial",
    Params: []*Parameter{{Name: "n", Type: nil}},
    Body: &hir.If{
        Condition: &hir.Call{
            Function: &hir.LoadVar{Name: "<="},
            Args: []Operation{
                &hir.LoadVar{Name: "n"},
                &hir.IntLit{Value: 1},
            },
        },
        Then: &hir.Return{
            Value: &hir.IntLit{Value: 1},
        },
        Else: &hir.Return{
            Value: &hir.Call{
                Function: &hir.LoadVar{Name: "*"},
                Args: []Operation{
                    &hir.LoadVar{Name: "n"},
                    &hir.Call{
                        Function: &hir.LoadVar{Name: "factorial"},
                        Args: []Operation{
                            &hir.Call{
                                Function: &hir.LoadVar{Name: "-"},
                                Args: []Operation{
                                    &hir.LoadVar{Name: "n"},
                                    &hir.IntLit{Value: 1},
                                },
                            },
                        },
                    },
                },
            },
        },
    },
}
```

### Example: M28 → HIR

**M28 source:**
```lisp
(def factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

**HIR:** *Exactly the same as Python!*

This is the key insight: **different syntaxes → same HIR**.

### HIR Optimization Passes

HIR's clean structure enables optimization:

```go
// hir/passes/constant_fold.go

// Before optimization:
&hir.Call{
    Function: &hir.LoadVar{Name: "+"},
    Args: []Operation{
        &hir.IntLit{Value: 1},
        &hir.IntLit{Value: 2},
    },
}

// After constant folding:
&hir.IntLit{Value: 3}

// hir/passes/inline.go

// Before inlining:
&hir.Call{
    Function: &hir.LoadVar{Name: "double"},  // def double(x): x * 2
    Args: []Operation{&hir.IntLit{Value: 5}},
}

// After inlining:
&hir.Call{
    Function: &hir.LoadVar{Name: "*"},
    Args: []Operation{
        &hir.IntLit{Value: 5},
        &hir.IntLit{Value: 2},
    },
}

// Then constant fold again:
&hir.IntLit{Value: 10}
```

---

## Layer 3: Backends

### What is a Backend?

A backend is responsible for:
1. **Executing** HIR (interpreter)
2. **Compiling** HIR to bytecode/native code
3. **Optimizing** for its execution model

### Backend Interface

```go
// backend/common/interface.go
package common

import "github.com/mmichie/m28/hir"

// Backend represents a compilation/execution target
type Backend interface {
    Name() string                                     // "interpreter", "bytecode", "llvm"
    Execute(module *hir.Module) (Result, error)      // Execute HIR

    // Capabilities
    RequiresTypeInfo() bool                           // True for AOT compilers
    SupportsGradualTyping() bool                      // Can use optional type hints
}

type Result struct {
    Value  interface{}   // Result value (backend-specific)
    Output string        // Any output produced
}
```

### Interpreter Backend

**Status:** ✅ Working (current implementation)

**How it works:**
```go
// backend/interpreter/eval.go

func Eval(op hir.Operation, ctx *Context) (Value, error) {
    switch op := op.(type) {
    case *hir.IntLit:
        return IntValue(op.Value), nil

    case *hir.LoadVar:
        return ctx.Get(op.Name)

    case *hir.Call:
        fn := Eval(op.Function, ctx)
        args := evalArgs(op.Args, ctx)
        return Call(fn, args, ctx)

    case *hir.If:
        cond := Eval(op.Condition, ctx)
        if ToBool(cond) {
            return Eval(op.Then, ctx)
        }
        return Eval(op.Else, ctx)
    }
}
```

**Performance:** 1x (baseline)

**Pros:**
- Simple, easy to debug
- Direct mapping from HIR
- Good for REPL, development

**Cons:**
- Slow (tree-walking overhead)
- Type checks every operation
- Heavy GC pressure

**Directory structure (current):**
```
eval/                    # Should become backend/interpreter/
├── evaluator.go         # Main eval loop
├── function_params.go   # Parameter binding
├── class_forms.go       # Class evaluation
└── ...
```

### Bytecode VM Backend

**Status:** 📋 Designed (see mir-bytecode-vm.md)

**How it works:**
```
HIR → Bytecode Compiler → Bytecode (.m28c)
                              ↓
                         VM Execution
```

**Sample bytecode:**
```
factorial:
  LOAD_VAR      r0, "n"
  LOAD_CONST_I  r1, 1
  LE            r2, r0, r1
  JUMP_IF_FALSE r2, REC
    RETURN        r0
  REC:
    LOAD_VAR      r3, "factorial"
    LOAD_VAR      r4, "n"
    LOAD_CONST_I  r5, 1
    SUB           r6, r4, r5
    CALL_1        r7, r3, r6
    LOAD_VAR      r8, "n"
    MUL           r9, r8, r7
    RETURN        r9
```

**Performance:** 3-10x (vs interpreter)

**Pros:**
- Much faster than tree-walking
- Type specialization opportunities
- Can cache compiled code (.m28c files)
- Foundation for JIT

**Cons:**
- More complex than interpreter
- Compilation overhead (amortized)
- Debugging harder (but solvable with source maps)

**Directory structure (proposed):**
```
backend/bytecode/
├── compiler.go          # HIR → Bytecode
├── vm.go                # Bytecode execution
├── opcodes.go           # Instruction definitions
├── optimize.go          # Bytecode optimization passes
└── serialize.go         # .m28c format
```

### LLVM Backend

**Status:** 📋 Future

**How it works:**
```
HIR → LLVM Lowering → LLVM IR → LLVM Optimization → Native code
```

**Performance:** 50-100x (vs interpreter) for compute-heavy code

**Pros:**
- Native code performance
- Free optimizations from LLVM
- Cross-platform compilation

**Cons:**
- Complex integration
- Long compilation times
- Harder to debug
- Needs runtime support for dynamic features

**When to build:**
- After bytecode VM is working
- When performance is critical (numerical, games)
- When you want ahead-of-time compilation

**Directory structure (future):**
```
backend/llvm/
├── codegen.go           # HIR → LLVM IR
├── runtime.go           # Runtime support (GC, etc.)
└── optimize.go          # LLVM-specific optimizations
```

### WASM Backend

**Status:** 📋 Future

**How it works:**
```
HIR → WASM Lowering → WASM Module → Browser/WASI runtime
```

**Performance:** 10-50x (vs interpreter), portable

**Pros:**
- Runs in browsers
- Portable (Linux, Mac, Windows, WASI)
- Good performance
- Sandboxed by default

**Cons:**
- Still needs runtime support
- Limited debugging
- WASM has restrictions (linear memory, etc.)

**When to build:**
- After bytecode VM
- For web deployment
- For portable distribution

**Directory structure (future):**
```
backend/wasm/
├── codegen.go           # HIR → WASM
├── runtime.go           # WASM runtime support
└── bindings.go          # JS/WASI bindings
```

---

## Static vs Dynamic Languages

### The Challenge

**Dynamic languages** (Python, M28, Ruby):
- Types determined at runtime
- Variables can change type
- Duck typing
- Flexible but slower

**Static languages** (TypeScript, Rust, Go):
- Types determined at compile-time
- Type checking before execution
- Explicit types
- Less flexible but faster

**Question:** Can one architecture handle both?

**Answer:** Yes! With optional type information in HIR.

### HIR with Optional Types

```go
type TypeInfo struct {
    Kind       TypeKind           // Any, Concrete, Generic, Union
    Concrete   *ConcreteType      // Specific type (if known)
    Runtime    bool               // True = check at runtime
}

type TypeKind int
const (
    TypeAny TypeKind = iota        // Dynamic: anything goes
    TypeConcrete                   // Static: int, str, MyClass
    TypeGeneric                    // Static: List[T]
    TypeUnion                      // Static or gradual: int | str
)
```

### Example: Same HIR, Different Types

**Python (dynamic):**
```python
def greet(name):
    return f"Hello, {name}"
```

**HIR:**
```go
&hir.Function{
    Name: "greet",
    Params: []*Parameter{
        {Name: "name", Type: &TypeInfo{Kind: TypeAny}},  // No type constraint
    },
    ReturnType: &TypeInfo{Kind: TypeAny},
    Body: /* ... */,
}
```

**TypeScript (static):**
```typescript
function greet(name: string): string {
    return `Hello, ${name}`;
}
```

**HIR:**
```go
&hir.Function{
    Name: "greet",
    Params: []*Parameter{
        {Name: "name", Type: &TypeInfo{
            Kind: TypeConcrete,
            Concrete: &ConcreteType{Name: "string"},
            Runtime: false,  // Already type-checked by TypeScript
        }},
    },
    ReturnType: &TypeInfo{
        Kind: TypeConcrete,
        Concrete: &ConcreteType{Name: "string"},
        Runtime: false,
    },
    Body: /* ... */,
}
```

**Key insight:** Same HIR structure, different type annotations!

### How Backends Use Type Info

**Interpreter:**
```go
func (e *Evaluator) evalCall(op *hir.Call) (Value, error) {
    fn := e.eval(op.Function)
    args := e.evalArgs(op.Args)

    // Dynamic: Always check types at runtime
    if op.TypeInfo() == nil || op.TypeInfo().Runtime {
        if err := checkTypes(fn, args); err != nil {
            return nil, err
        }
    }
    // Static: Types already checked, skip

    return call(fn, args)
}
```

**Bytecode VM:**
```go
func (c *Compiler) compileCall(op *hir.Call) []Instruction {
    instrs := []Instruction{}

    if op.TypeInfo() != nil && !op.TypeInfo().Runtime {
        // Static: Direct call (fast!)
        instrs = append(instrs, CALL_DIRECT)
    } else {
        // Dynamic: Dynamic dispatch (slower but flexible)
        instrs = append(instrs, CALL_DYNAMIC)
    }

    return instrs
}
```

**LLVM Backend:**
```go
func (g *Generator) genCall(op *hir.Call) llvm.Value {
    if op.TypeInfo() != nil && !op.TypeInfo().Runtime {
        // Static: Generate optimal native code
        // - No runtime type checks
        // - Direct call
        // - Can inline
        return g.genStaticCall(op)
    } else {
        // Dynamic: Use runtime support
        // - Include type tags
        // - Indirect call
        // - Runtime checks
        return g.genDynamicCall(op)
    }
}
```

### Gradual Typing

Python with type hints gets the best of both worlds:

```python
# Python with type hints
def add(x: int, y: int) -> int:
    return x + y

# Can optionally enforce at runtime
```

**HIR:**
```go
&hir.Function{
    Params: []*Parameter{
        {Name: "x", Type: &TypeInfo{
            Kind: TypeConcrete,
            Concrete: &ConcreteType{Name: "int"},
            Runtime: true,  // Python checks at runtime (not compile-time)
        }},
    },
}
```

Backends can:
- **Interpreter:** Check types at runtime (Python semantics)
- **Bytecode VM:** Add type guards, specialize hot paths
- **LLVM:** Generate fast path with guards, slow path for type errors

---

## Why Custom HIR (Not LLVM)

### The Question

Should M28 use LLVM IR directly instead of inventing a custom HIR?

### The Answer: Custom HIR Now, LLVM Later

**Reasons:**

#### 1. LLVM IR is Too Low-Level

**Example: Python `x = [1, 2, 3]` in LLVM:**
```llvm
; Allocate Python list object
%list_ptr = call ptr @PyList_New(i64 3)

; Create integer objects
%int1 = call ptr @PyLong_FromLong(i64 1)
%int2 = call ptr @PyLong_FromLong(i64 2)
%int3 = call ptr @PyLong_FromLong(i64 3)

; Add to list
call void @PyList_SetItem(ptr %list_ptr, i64 0, ptr %int1)
call void @PyList_SetItem(ptr %list_ptr, i64 1, ptr %int2)
call void @PyList_SetItem(ptr %list_ptr, i64 2, ptr %int3)

; Store in variable
store ptr %list_ptr, ptr %x_slot
```

**Custom HIR:**
```go
&hir.StoreVar{
    Name: "x",
    Value: &hir.ListLit{
        Elements: []Operation{
            &hir.IntLit{Value: 1},
            &hir.IntLit{Value: 2},
            &hir.IntLit{Value: 3},
        },
    },
}
```

Much simpler! HIR is at the right abstraction level.

#### 2. LLVM Requires SSA Form

**SSA (Static Single Assignment):** Every variable assigned exactly once.

**Python code:**
```python
x = 1
x = x + 1
print(x)
```

**LLVM IR requires:**
```llvm
%x1 = add i64 0, 1
%x2 = add i64 %x1, 1
call void @print(i64 %x2)
```

This is painful for dynamic languages where variables change frequently!

**Custom HIR:**
```go
&hir.StoreVar{Name: "x", Value: &hir.IntLit{Value: 1}}
&hir.StoreVar{Name: "x", Value: &hir.Call{...}}  // x = x + 1
&hir.Call{Function: "print", Args: []{&hir.LoadVar{Name: "x"}}}
```

Natural representation of mutable variables.

#### 3. LLVM is Statically Typed

LLVM requires you to specify types everywhere (`i64`, `i32`, `ptr`).

M28 is dynamically typed. We'd need to:
- Box everything in tagged unions
- Build entire Python object model in LLVM
- Implement GC in LLVM
- Handle dynamic dispatch

This is a **lot of work** and makes iteration slow.

#### 4. Custom HIR Enables Rapid Iteration

**Time to implement:**
- Custom HIR: ~1 week (100-500 lines)
- LLVM integration: ~3-6 months (5000+ lines)

**Development cycle:**
- Custom HIR: Change IR → test → iterate (minutes)
- LLVM: Change lowering → recompile → debug LLVM crashes → iterate (hours)

#### 5. Real-World Evidence

Languages that started with custom IR:

| Language | Initial IR | Later added LLVM? |
|----------|-----------|------------------|
| Python   | Bytecode  | ✅ (PyPy, Pyston) |
| Ruby     | Bytecode  | ✅ (TruffleRuby) |
| JavaScript | Bytecode | ✅ (WebKit) |
| Julia    | LLVM directly | ⚠️ Took years, very complex |

**Pattern:** Start simple (bytecode), add LLVM when needed for performance.

### The Migration Path

```
Phase 1: Custom HIR + Interpreter (now)
   - Fast iteration
   - ~1000 lines of code
   - Works for development

Phase 2: Custom HIR + Bytecode VM (next)
   - 3-10x faster
   - ~3000 lines of code
   - Production-ready

Phase 3: Custom HIR → LLVM Backend (later)
   - 50-100x faster for compute
   - ~5000+ lines of code
   - When you need peak performance
```

### HIR Designed for LLVM Lowering

Even though we're not using LLVM now, HIR is designed to lower cleanly:

```go
// hir/operation.go - Structured for LLVM lowering

type If struct {
    Condition Operation
    Then      Operation
    Else      Operation
}

// Later: backend/llvm/codegen.go
func (g *Generator) genIf(op *hir.If) llvm.Value {
    // HIR's explicit structure maps cleanly to LLVM basic blocks
    condBB := g.createBlock("cond")
    thenBB := g.createBlock("then")
    elseBB := g.createBlock("else")
    mergeBB := g.createBlock("merge")

    // Generate condition
    cond := g.gen(op.Condition)
    g.builder.CreateCondBr(cond, thenBB, elseBB)

    // Generate branches...
}
```

HIR's clean structure (expressions, explicit control flow) makes LLVM lowering straightforward when you're ready.

---

## Migration Strategy

### Current State (Nov 2024)

```
Parser → core.Value (acting as HIR) → Evaluator
```

**Problems:**
- `core.Value` does double duty (runtime values + IR)
- Can't optimize IR without affecting runtime
- Hard to add backends

### Phase 1: Formalize HIR (1-2 months)

**Goal:** Split IR from runtime values

**Tasks:**
1. Create `hir/` package with operation types
2. Create `hir.Op → core.Value` adapter (for compatibility)
3. Update parser to emit HIR (instead of core.Value directly)
4. Keep evaluator unchanged (use adapter)

**Result:**
```
Parser → HIR → Adapter → core.Value → Evaluator
```

**Code:**
```go
// hir/operation.go
type IntLit struct { Value int64 }
func (i *IntLit) ToCoreValue() core.Value {
    return core.NewNumber(float64(i.Value))
}

// Evaluator uses core.Value (unchanged)
func Eval(v core.Value, ctx *Context) (core.Value, error) {
    // Existing code works!
}
```

**Deliverables:**
- [ ] `hir/operation.go` - Core HIR types (~200 lines)
- [ ] `hir/builder.go` - HIR construction helpers (~100 lines)
- [ ] `hir/adapter.go` - HIR → core.Value conversion (~100 lines)
- [ ] Update parser to emit HIR
- [ ] All tests pass (behavior unchanged)

### Phase 2: Optimize HIR (1 month)

**Goal:** Add optimization passes on HIR

**Tasks:**
1. Implement constant folding pass
2. Implement dead code elimination
3. Implement inline simple functions
4. Add optimization flags to CLI

**Result:**
```
Parser → HIR → Optimize → Adapter → core.Value → Evaluator
```

**Code:**
```go
// hir/passes/constant_fold.go
func ConstantFold(module *Module) {
    // (+ 1 2) → 3
}

// hir/passes/inline.go
func InlineSimpleFunctions(module *Module) {
    // Replace small function calls with body
}

// main.go
if *optimize {
    module = passes.ConstantFold(module)
    module = passes.Inline(module)
}
```

**Deliverables:**
- [ ] `hir/passes/constant_fold.go` (~100 lines)
- [ ] `hir/passes/dead_code.go` (~100 lines)
- [ ] `hir/passes/inline.go` (~200 lines)
- [ ] Benchmarks showing optimization benefits

### Phase 3: Separate Frontend (1-2 months)

**Goal:** Clean frontend/backend separation

**Tasks:**
1. Move parser code to `frontend/m28/`
2. Move Python parser to `frontend/python/`
3. Add frontend abstraction
4. File extension detection (.py vs .m28)

**Result:**
```
frontend/python/ → HIR → Optimize → Backend
frontend/m28/    ↗
```

**Directory structure:**
```
frontend/
├── common/
│   └── interface.go     # Frontend interface
├── m28/
│   ├── parser.go
│   ├── ast.go
│   └── lower.go
└── python/
    ├── parser.go
    ├── ast.go
    └── lower.go

hir/
├── operation.go         # HIR types
├── builder.go           # Construction helpers
└── passes/              # Optimization passes

backend/
├── common/
│   └── interface.go     # Backend interface
└── interpreter/
    ├── eval.go
    └── runtime.go
```

**Deliverables:**
- [ ] Clean frontend/HIR/backend separation
- [ ] Both Python and M28 frontends working
- [ ] File extension detection
- [ ] All tests passing

### Phase 4: Bytecode Backend (2-3 months)

**Goal:** Add bytecode VM for performance

**Tasks:**
1. Design instruction set
2. Implement HIR → Bytecode compiler
3. Implement bytecode VM
4. Add optimization (type specialization)
5. Add .m28c serialization

**Result:**
```
Frontend → HIR → Optimize → Bytecode Compiler → VM
                         ↘ Interpreter (fallback)
```

**Deliverables:**
- [ ] `backend/bytecode/compiler.go`
- [ ] `backend/bytecode/vm.go`
- [ ] 3-10x speedup on benchmarks
- [ ] .m28c file format
- [ ] CLI commands (compile, disassemble)

### Phase 5: LLVM Backend (Future, 6+ months)

**Goal:** Native code compilation

**Tasks:**
1. Implement HIR → LLVM IR lowering
2. Build runtime support (GC, objects)
3. Integrate LLVM optimization passes
4. Support ahead-of-time compilation

**Result:**
```
Frontend → HIR → Optimize → LLVM Codegen → LLVM → Native code
                         ↘ Bytecode VM
                         ↘ Interpreter
```

**Deliverables:**
- [ ] `backend/llvm/codegen.go`
- [ ] `backend/llvm/runtime.go`
- [ ] 50-100x speedup for numerical code
- [ ] Standalone executables

### Phase 6: WASM Backend (Future, 3-6 months)

**Goal:** Web and portable deployment

**Tasks:**
1. Implement HIR → WASM lowering
2. Build WASM runtime bindings
3. Browser deployment
4. WASI support

**Result:**
```
Frontend → HIR → WASM Codegen → WASM module → Browser/WASI
```

**Deliverables:**
- [ ] `backend/wasm/codegen.go`
- [ ] Browser demo
- [ ] WASI support for server deployment

---

## Implementation Guide

### How to Add a Frontend

**Example: Adding a Ruby frontend**

1. **Create directory structure:**
```bash
mkdir -p frontend/ruby
cd frontend/ruby
```

2. **Create Ruby AST types:**
```go
// frontend/ruby/ast.go
package ruby

type RubyAST struct {
    Statements []Statement
}

type DefStatement struct {
    Name   string
    Params []string
    Body   []Statement
}

// etc...
```

3. **Implement parser:**
```go
// frontend/ruby/parser.go
package ruby

func Parse(source string) (*RubyAST, error) {
    // Tokenize Ruby code
    // Parse into Ruby AST
    return ast, nil
}
```

4. **Implement HIR lowering:**
```go
// frontend/ruby/lower.go
package ruby

func (ast *RubyAST) Lower() (*hir.Module, error) {
    module := &hir.Module{Name: "main"}

    for _, stmt := range ast.Statements {
        op := lowerStatement(stmt)
        module.AddOperation(op)
    }

    return module, nil
}

func lowerStatement(stmt Statement) hir.Operation {
    switch s := stmt.(type) {
    case *DefStatement:
        return &hir.Function{
            Name: s.Name,
            Params: lowerParams(s.Params),
            Body: lowerBody(s.Body),
        }
    // etc...
    }
}
```

5. **Register frontend:**
```go
// main.go

func selectFrontend(filename string) frontend.Frontend {
    switch filepath.Ext(filename) {
    case ".py":
        return frontend.NewPythonFrontend()
    case ".m28":
        return frontend.NewM28Frontend()
    case ".rb":
        return frontend.NewRubyFrontend()  // <-- Add this
    default:
        return frontend.NewM28Frontend()
    }
}
```

6. **Done!** Ruby code now works with all backends (interpreter, bytecode, LLVM, etc.)

### How to Add a Backend

**Example: Adding a JavaScript backend**

1. **Create directory structure:**
```bash
mkdir -p backend/javascript
cd backend/javascript
```

2. **Implement HIR → JS lowering:**
```go
// backend/javascript/codegen.go
package javascript

import "github.com/mmichie/m28/hir"

type Generator struct {
    output strings.Builder
}

func (g *Generator) Generate(module *hir.Module) (string, error) {
    for _, fn := range module.Functions {
        g.genFunction(fn)
    }
    return g.output.String(), nil
}

func (g *Generator) genFunction(fn *hir.Function) {
    g.output.WriteString(fmt.Sprintf("function %s(", fn.Name))

    // Parameters
    for i, param := range fn.Params {
        if i > 0 {
            g.output.WriteString(", ")
        }
        g.output.WriteString(param.Name)
    }

    g.output.WriteString(") {\n")
    g.genOperation(fn.Body)
    g.output.WriteString("}\n")
}

func (g *Generator) genOperation(op hir.Operation) {
    switch op := op.(type) {
    case *hir.IntLit:
        g.output.WriteString(fmt.Sprintf("%d", op.Value))

    case *hir.Call:
        g.genOperation(op.Function)
        g.output.WriteString("(")
        for i, arg := range op.Args {
            if i > 0 {
                g.output.WriteString(", ")
            }
            g.genOperation(arg)
        }
        g.output.WriteString(")")

    case *hir.Return:
        g.output.WriteString("return ")
        g.genOperation(op.Value)
        g.output.WriteString(";\n")

    // etc...
    }
}
```

3. **Implement backend interface:**
```go
// backend/javascript/backend.go

type JSBackend struct{}

func (b *JSBackend) Name() string {
    return "javascript"
}

func (b *JSBackend) Execute(module *hir.Module) (common.Result, error) {
    gen := &Generator{}
    jsCode, err := gen.Generate(module)
    if err != nil {
        return common.Result{}, err
    }

    // Could execute with Node.js or just return code
    return common.Result{
        Output: jsCode,
    }, nil
}
```

4. **Register backend:**
```go
// main.go

func selectBackend(name string) backend.Backend {
    switch name {
    case "interpreter":
        return backend.NewInterpreter()
    case "bytecode":
        return backend.NewBytecodeVM()
    case "javascript":
        return backend.NewJSBackend()  // <-- Add this
    default:
        return backend.NewInterpreter()
    }
}
```

5. **Use it:**
```bash
# Compile M28 to JavaScript
m28 --backend=javascript factorial.m28 > factorial.js

# Run with Node.js
node factorial.js
```

### How to Add an Optimization Pass

**Example: Adding loop unrolling**

```go
// hir/passes/unroll.go
package passes

func UnrollLoops(module *Module) {
    for _, fn := range module.Functions {
        fn.Body = unrollOperation(fn.Body)
    }
}

func unrollOperation(op Operation) Operation {
    switch op := op.(type) {
    case *Loop:
        // If loop count is small constant, unroll
        if count := getConstantIterCount(op); count > 0 && count < 4 {
            return unrollLoop(op, count)
        }
        return op

    case *Block:
        for i, child := range op.Ops {
            op.Ops[i] = unrollOperation(child)
        }
        return op

    default:
        return op
    }
}

func unrollLoop(loop *Loop, count int) Operation {
    // Replace loop with repeated body
    ops := make([]Operation, count)
    for i := 0; i < count; i++ {
        ops[i] = cloneAndSubstitute(loop.Body, loop.LoopVar, IntLit{Value: int64(i)})
    }
    return &Block{Ops: ops}
}
```

**Use it:**
```go
// main.go

if *optimize {
    module = passes.ConstantFold(module)
    module = passes.UnrollLoops(module)      // <-- Add this
    module = passes.DeadCodeElimination(module)
}
```

---

## Performance Evolution

### Expected Performance at Each Stage

| Stage | Backend | Speedup | When | Use Case |
|-------|---------|---------|------|----------|
| 1 | Interpreter | 1x | Now | Development, REPL |
| 2 | Bytecode VM | 3-10x | Next 3-6 mo | Production, general use |
| 3 | LLVM AOT | 50-100x | Future | Numerical, games, servers |
| 4 | LLVM JIT | 100-200x | Future | Long-running processes |

### Benchmark Example: Fibonacci(35)

```python
def fibonacci(n):
    if n <= 1:
        return n
    return fibonacci(n-1) + fibonacci(n-2)
```

**Projected performance:**
- Interpreter: 5000ms (baseline)
- Bytecode VM: 500-1000ms (5-10x)
- Bytecode VM + type guards: 200-400ms (12-25x)
- LLVM: 50-100ms (50-100x)
- C (reference): 40ms

### When to Use Each Backend

**Interpreter:**
- REPL sessions
- Debugging
- Quick scripts
- Development

**Bytecode VM:**
- Production web services
- CLI tools
- General applications
- Default for most code

**LLVM:**
- Numerical computation
- Game engines
- High-performance servers
- Batch processing

**WASM:**
- Web applications
- Sandboxed execution
- Portable deployment
- Cross-platform tools

---

## References

### Related M28 Docs

- [AST and IR Design](./ast-ir-multiple-frontends.md) - AST layer (being superseded by this doc)
- [MIR Bytecode VM](./mir-bytecode-vm.md) - Bytecode backend design
- [Python Frontend](./python-frontend-design.md) - Python frontend specifics

### External Resources

**Compiler Architecture:**
- [LLVM Architecture](https://llvm.org/docs/LangRef.html)
- [GCC Internals](https://gcc.gnu.org/onlinedocs/gccint/)
- [Crafting Interpreters](https://craftinginterpreters.com/) - Excellent book on bytecode VMs

**Multi-Frontend Examples:**
- [GraalVM Truffle](https://www.graalvm.org/latest/graalvm-as-a-platform/language-implementation-framework/) - Framework for multiple languages
- [LLVM](https://llvm.org/) - C, C++, Rust, Swift all use LLVM IR
- [JVM](https://en.wikipedia.org/wiki/Java_virtual_machine) - Java, Kotlin, Scala, Clojure share JVM

**Language-Specific:**
- [Python AST](https://docs.python.org/3/library/ast.html)
- [Python Bytecode](https://docs.python.org/3/library/dis.html)
- [PyPy Architecture](https://doc.pypy.org/en/latest/architecture.html) - Python on LLVM

**IR Design:**
- [Sea of Nodes](https://darksi.de/d.sea-of-nodes/) - Modern IR design
- [SSA Form](https://en.wikipedia.org/wiki/Static_single-assignment_form)
- [CPS](https://en.wikipedia.org/wiki/Continuation-passing_style) - Alternative IR style

---

## Appendices

### Appendix A: HIR Complete Reference

See `hir/operation.go` for complete HIR node definitions.

### Appendix B: Bytecode Instruction Set

See [MIR Bytecode VM Design](./mir-bytecode-vm.md) for complete instruction set.

### Appendix C: Migration Checklist

**Phase 1: Formalize HIR**
- [ ] Create `hir/` package
- [ ] Define operation types
- [ ] Implement HIR → core.Value adapter
- [ ] Update parser to emit HIR
- [ ] All tests pass

**Phase 2: Optimize HIR**
- [ ] Constant folding pass
- [ ] Dead code elimination
- [ ] Simple inlining
- [ ] Benchmark improvements

**Phase 3: Separate Frontend**
- [ ] Move to `frontend/` structure
- [ ] Clean interfaces
- [ ] File extension detection
- [ ] Multiple frontends working

**Phase 4: Bytecode Backend**
- [ ] Instruction set design
- [ ] HIR → Bytecode compiler
- [ ] VM implementation
- [ ] .m28c serialization
- [ ] 3-10x speedup achieved

**Phase 5: LLVM Backend (Future)**
- [ ] HIR → LLVM IR lowering
- [ ] Runtime support
- [ ] Integration tests
- [ ] 50-100x speedup for numerical code

---

**Document Version:** 1.0
**Last Updated:** November 2024
**Status:** Living document - will be updated as architecture evolves

# MIR Bytecode VM Design

## Executive Summary

This document proposes adding a **Mid-Level IR (MIR) bytecode virtual machine** to M28 as an intermediate execution tier between the current tree-walking interpreter and future native compilation (LLVM/WASM). The MIR bytecode VM will provide 3-10x performance improvements while maintaining full compatibility with M28's dynamic semantics.

**Key Benefits:**
- **Fast execution** - Register-based VM with type specialization
- **Optimizable** - Profile-guided optimization, constant folding, dead code elimination
- **Portable** - Serializable bytecode format (.m28c files)
- **JIT-ready** - Clean path to further compilation (WASM, LLVM)
- **Incremental** - Mix interpreted and compiled code seamlessly

**Timeline:** 8-12 weeks for full implementation
- Week 1-2: Core instruction set and compiler
- Week 3-4: VM implementation
- Week 5-6: Optimization passes
- Week 7-8: Integration and testing
- Week 9-12: Performance tuning and polish

**Impact:** Foundation for high-performance execution, multi-backend compilation, and future JIT compilation.

---

## Table of Contents

1. [Motivation](#motivation)
2. [Design Goals](#design-goals)
3. [Architecture Overview](#architecture-overview)
4. [Instruction Set Architecture](#instruction-set-architecture)
5. [Bytecode Compilation](#bytecode-compilation)
6. [Virtual Machine](#virtual-machine)
7. [Optimization Strategies](#optimization-strategies)
8. [Serialization Format](#serialization-format)
9. [Integration with M28](#integration-with-m28)
10. [Performance Expectations](#performance-expectations)
11. [Implementation Plan](#implementation-plan)
12. [Risks and Mitigations](#risks-and-mitigations)
13. [Future Extensions](#future-extensions)

---

## Motivation

### Current State: Tree-Walking Interpreter

M28 currently uses a pure tree-walking interpreter that evaluates S-expression IR directly:

```go
func Eval(expr core.Value, ctx *core.Context) (core.Value, error) {
    switch v := expr.(type) {
    case core.NumberValue:
        return v, nil  // Self-evaluating
    case core.SymbolValue:
        return ctx.Lookup(string(v))  // Variable lookup
    case core.ListValue:
        // Function call or special form
        return evalFunctionCall(v, ctx)
    }
}
```

**Problems:**
1. **Performance overhead** - Every operation requires type checks and dispatch
2. **Memory allocation** - Heavy GC pressure from intermediate values
3. **No optimization** - Same code paths every time
4. **Hard to JIT** - Tree structure doesn't map well to machine code

### Why Bytecode?

Bytecode provides a sweet spot between interpretation and compilation:

```
Tree-walking → Bytecode VM → JIT Compilation → Native Code
(1x speed)     (3-10x)        (10-50x)          (50-100x)
```

**Examples from other languages:**
- **Python (CPython):** Uses bytecode VM, ~5x faster than pure AST walking
- **Lua:** Bytecode VM achieves near-native performance for many workloads
- **JavaScript (V8):** Bytecode (Ignition) as intermediate before TurboFan JIT

---

## Design Goals

### Primary Goals

1. **Performance** - 3-10x speedup over tree-walking for typical workloads
2. **Compatibility** - 100% compatible with existing M28 semantics
3. **Debuggability** - Full source mapping, stack traces, breakpoint support
4. **Portability** - Serializable bytecode format for caching/distribution
5. **Optimizability** - Support for type specialization, inlining, constant folding

### Non-Goals

1. **Not replacing interpreter** - Tree-walking remains for debugging, cold code
2. **Not native compilation** - That's a separate future project (LLVM/WASM)
3. **Not breaking changes** - Fully backward compatible

---

## Architecture Overview

### Three-Tier Execution Strategy

```
┌─────────────────────────────────────────────┐
│  Source Code (Python/M28)                   │
└────────────────┬────────────────────────────┘
                 ↓
┌─────────────────────────────────────────────┐
│  AST Layer (already implemented)            │
└────────────────┬────────────────────────────┘
                 ↓ ToIR()
┌─────────────────────────────────────────────┐
│  HIR (High-level IR - S-expressions)        │
│  - Current tree-walking target              │
│  - Good for: macros, REPL, metaprogramming  │
└────────────────┬────────────────────────────┘
                 ↓ Compile
┌─────────────────────────────────────────────┐
│  MIR (Mid-level IR - Bytecode) ← NEW!       │
│  - Register-based VM                        │
│  - Type-specialized operations              │
│  - Good for: fast execution, optimization   │
└────────────────┬────────────────────────────┘
                 ↓ Execute
┌─────────────────────────────────────────────┐
│  Virtual Machine                            │
│  - Dispatch loop                            │
│  - Type feedback collection                 │
│  - Profile-guided optimization              │
└─────────────────────────────────────────────┘
```

### Multi-Backend Vision (Future)

```
          HIR (S-expressions)
                 ↓
          ┌──────┴──────┐
          ↓             ↓
    MIR Bytecode    WASM Compiler
          ↓             ↓
        VM          WASM Runtime
                        ↓
                  Native (JIT)
```

---

## Instruction Set Architecture

### Design Decision: Register Machine

**Stack Machine vs Register Machine:**

| Aspect | Stack Machine | Register Machine |
|--------|---------------|------------------|
| Code density | ★★★★★ | ★★★☆☆ |
| Execution speed | ★★★☆☆ | ★★★★★ |
| JIT-friendliness | ★★★☆☆ | ★★★★★ |

**Decision: Register machine** - Modern VMs (Lua 5.0+, V8 Ignition) use registers for better performance.

### Instruction Format

```go
type Instruction struct {
    Opcode   Opcode     // 2 bytes (operation type)
    Dest     Register   // 2 bytes (destination register)
    Arg1     Operand    // 8 bytes (first argument)
    Arg2     Operand    // 8 bytes (second argument)
    LineInfo uint32     // 4 bytes (source line number)
}
// Total: 24 bytes (cache-friendly on 64-bit systems)

type Opcode uint16      // 65k possible opcodes (need ~200)
type Register uint16    // 65k virtual registers per frame

type Operand struct {
    Type OperandType   // 1 byte
    Data uint64        // 8 bytes (value or index)
}

type OperandType byte
const (
    OP_REGISTER  OperandType = iota  // Data = register index
    OP_CONSTANT                       // Data = constant pool index
    OP_IMMEDIATE                      // Data = immediate value
    OP_LABEL                          // Data = instruction offset
)
```

### Core Opcode Categories

**1. Constants & Variables (0x00-0x0F)**
```
OP_LOAD_CONST       r0, constants[idx]
OP_LOAD_CONST_I     r0, immediate_value
OP_LOAD_NIL         r0
OP_LOAD_TRUE/FALSE  r0
OP_LOAD_VAR         r0, "name"
OP_STORE_VAR        "name", r0
```

**2. Arithmetic - Generic (0x10-0x1F)**
```
OP_ADD              r0, r1, r2    # Generic (runtime type check)
OP_SUB              r0, r1, r2
OP_MUL              r0, r1, r2
OP_DIV              r0, r1, r2
OP_FLOORDIV         r0, r1, r2
OP_MOD              r0, r1, r2
OP_POW              r0, r1, r2
OP_NEG              r0, r1
```

**3. Arithmetic - Specialized (0x20-0x2F)**
```
OP_ADD_INT          r0, r1, r2    # Fast path for ints
OP_ADD_FLOAT        r0, r1, r2    # Fast path for floats
OP_ADD_STRING       r0, r1, r2    # String concatenation
OP_SUB_INT          r0, r1, r2
OP_MUL_INT          r0, r1, r2
OP_INCREMENT        r0            # r0 = r0 + 1
OP_DECREMENT        r0            # r0 = r0 - 1
```

**4. Comparisons (0x40-0x4F)**
```
OP_EQ               r0, r1, r2    # r0 = (r1 == r2)
OP_NE               r0, r1, r2
OP_LT               r0, r1, r2
OP_LE               r0, r1, r2
OP_GT               r0, r1, r2
OP_GE               r0, r1, r2
OP_EQ_INT           r0, r1, r2    # Specialized for ints
OP_IN               r0, r1, r2    # r0 = (r1 in r2)
```

**5. Control Flow (0x60-0x6F)**
```
OP_JUMP             offset
OP_JUMP_IF_TRUE     r0, offset
OP_JUMP_IF_FALSE    r0, offset
OP_FOR_ITER         r0, iter, offset  # r0 = next(iter) or jump
OP_GET_ITER         r0, r1            # r0 = iter(r1)
```

**6. Function Calls (0x70-0x7F)**
```
OP_CALL_0           r0, func           # r0 = func()
OP_CALL_1           r0, func, arg1     # r0 = func(arg1)
OP_CALL_2           r0, func, arg1     # r0 = func(arg1, arg2)
OP_CALL_N           r0, func, count    # r0 = func(arg1..argN)
OP_CALL_METHOD      r0, obj, method, args
OP_RETURN           r0
OP_YIELD            r0                 # For generators
```

**7. Object/Attribute Access (0x80-0x8F)**
```
OP_GET_ATTR         r0, obj, "attr"   # r0 = obj.attr
OP_SET_ATTR         obj, "attr", val  # obj.attr = val
OP_GET_ITEM         r0, obj, key      # r0 = obj[key]
OP_SET_ITEM         obj, key, val     # obj[key] = val
OP_GET_SLICE        r0, obj, start, end
```

**8. Collection Building (0x90-0x9F)**
```
OP_BUILD_LIST       r0, [r1, r2, ..., rN]
OP_BUILD_TUPLE      r0, [r1, r2, ..., rN]
OP_BUILD_DICT       r0, {k1:v1, k2:v2, ...}
OP_BUILD_SET        r0, {r1, r2, ..., rN}
OP_LIST_APPEND      list, item
OP_DICT_ADD         dict, key, value
```

**9. Type Guards (0xA0-0xAF)**
```
OP_GUARD_INT        r0                # assert is_int(r0) else deopt
OP_GUARD_FLOAT      r0
OP_GUARD_STRING     r0
OP_GUARD_TYPE       r0, expected_type
OP_CHECK_TYPE       r0, r1, type      # r0 = isinstance(r1, type)
```

**10. Exception Handling (0xB0-0xBF)**
```
OP_RAISE            exception
OP_SETUP_EXCEPT     handler_offset
OP_SETUP_FINALLY    handler_offset
OP_POP_EXCEPT
```

**Full opcode listing:** See appendix for complete 200+ opcodes.

---

## Bytecode Compilation

### Compiler Architecture

```go
type Compiler struct {
    chunk     *BytecodeChunk
    regAlloc  *RegisterAllocator
    constants map[string]int      // constant → pool index
    names     map[string]int      // name → names index
    labels    map[string]int      // label → PC
}

type BytecodeChunk struct {
    // Metadata
    Name       string
    Filename   string
    FirstLine  int

    // Code
    Code       []Instruction

    // Constants pool
    Constants  []core.Value

    // Names
    Names      []string
    VarNames   []string
    CellVars   []string      // Closure variables
    FreeVars   []string      // Free variables

    // Parameters
    ArgCount   int
    Flags      FunctionFlags

    // Debug info
    LineTable  []LineEntry

    // Optimization
    TypeFeedback map[int]*TypeProfile
}
```

### Compilation Examples

#### Example 1: Arithmetic

```scheme
; M28 Source
(+ a b)

; HIR (S-expression)
(+ a b)

; MIR Bytecode
LOAD_VAR      r0, "a"
LOAD_VAR      r1, "b"
ADD           r2, r0, r1    ; Generic add
RETURN        r2

; With type inference (if a, b known to be int):
LOAD_VAR      r0, "a"
LOAD_VAR      r1, "b"
GUARD_INT     r0            ; Type guard
GUARD_INT     r1
ADD_INT       r2, r0, r1    ; Specialized add
RETURN        r2
```

#### Example 2: Conditional

```scheme
; M28 Source
(if (< x 10)
  (print "small")
  (print "large"))

; MIR Bytecode
LOAD_VAR      r0, "x"
LOAD_CONST_I  r1, 10
LT            r2, r0, r1
JUMP_IF_FALSE r2, ELSE_LABEL
  LOAD_VAR      r3, "print"
  LOAD_CONST    r4, "small"
  CALL_1        r5, r3, r4
  JUMP          END_LABEL
ELSE_LABEL:
  LOAD_VAR      r6, "print"
  LOAD_CONST    r7, "large"
  CALL_1        r8, r6, r7
END_LABEL:
```

#### Example 3: Function Definition

```scheme
; M28 Source
(def fibonacci (n)
  (if (<= n 1)
    n
    (+ (fibonacci (- n 1))
       (fibonacci (- n 2)))))

; Compiled to nested BytecodeChunk
fibonacci:
  ArgCount: 1
  VarNames: ["n"]
  Code:
    LOAD_VAR      r0, "n"
    LOAD_CONST_I  r1, 1
    LE            r2, r0, r1
    JUMP_IF_FALSE r2, REC_LABEL
      RETURN        r0
    REC_LABEL:
      LOAD_VAR      r3, "fibonacci"
      LOAD_VAR      r4, "n"
      LOAD_CONST_I  r5, 1
      SUB           r6, r4, r5
      CALL_1        r7, r3, r6

      LOAD_VAR      r8, "fibonacci"
      LOAD_VAR      r9, "n"
      LOAD_CONST_I  r10, 2
      SUB           r11, r9, r10
      CALL_1        r12, r8, r11

      ADD           r13, r7, r12
      RETURN        r13
```

### Register Allocation

**Strategy:** Unlimited virtual registers with simple stack allocation

```go
type RegisterAllocator struct {
    nextReg Register
    freeList []Register
}

func (ra *RegisterAllocator) Allocate() Register {
    if len(ra.freeList) > 0 {
        // Reuse freed register
        r := ra.freeList[len(ra.freeList)-1]
        ra.freeList = ra.freeList[:len(ra.freeList)-1]
        return r
    }
    // Allocate new register
    r := ra.nextReg
    ra.nextReg++
    return r
}

func (ra *RegisterAllocator) Free(r Register) {
    ra.freeList = append(ra.freeList, r)
}
```

**Note:** No limit on register count - VM maps virtual registers to actual storage.

---

## Virtual Machine

### VM Architecture

```go
type VM struct {
    // Execution state
    frames      []*Frame
    frameIndex  int

    // Global state
    globals     map[string]core.Value
    builtins    map[string]core.Value

    // Optimization
    typeFeedback map[*BytecodeChunk]map[int]*TypeProfile
    hotThreshold int
}

type Frame struct {
    chunk      *BytecodeChunk
    pc         int                      // Program counter
    registers  []core.Value             // Register file
    locals     map[string]core.Value    // Local variables
    closure    []core.Value             // Closure variables
    parent     *Frame
}
```

### Execution Loop

```go
func (vm *VM) run() (core.Value, error) {
    frame := vm.frames[vm.frameIndex]

    for frame.pc < len(frame.chunk.Code) {
        instr := frame.chunk.Code[frame.pc]

        // Type feedback collection (for optimization)
        if vm.shouldProfile(frame, frame.pc) {
            vm.recordTypeFeedback(frame, instr)
        }

        // Dispatch
        switch instr.Opcode {
        case OP_LOAD_CONST:
            idx := int(instr.Arg1.Data)
            frame.registers[instr.Dest] = frame.chunk.Constants[idx]

        case OP_ADD:
            left := frame.registers[getReg(instr.Arg1)]
            right := frame.registers[getReg(instr.Arg2)]
            result, err := vm.add(left, right)  // Generic add
            if err != nil {
                return nil, err
            }
            frame.registers[instr.Dest] = result

        case OP_ADD_INT:
            // Fast path - no type check!
            left := frame.registers[getReg(instr.Arg1)].(core.NumberValue)
            right := frame.registers[getReg(instr.Arg2)].(core.NumberValue)
            frame.registers[instr.Dest] = left + right

        case OP_JUMP_IF_FALSE:
            cond := frame.registers[getReg(instr.Arg1)]
            if !core.IsTruthy(cond) {
                frame.pc = int(instr.Arg2.Data) - 1
            }

        case OP_CALL_1:
            fn := frame.registers[getReg(instr.Arg1)]
            arg := frame.registers[getReg(instr.Arg2)]
            result, err := vm.call(fn, []core.Value{arg})
            if err != nil {
                return nil, err
            }
            frame.registers[instr.Dest] = result

        case OP_RETURN:
            returnVal := frame.registers[instr.Dest]
            vm.frameIndex--
            if vm.frameIndex < 0 {
                return returnVal, nil  // End of execution
            }
            frame = vm.frames[vm.frameIndex]
            // Store return value in caller's destination

        // ... 200+ more opcodes ...
        }

        frame.pc++
    }

    return core.Nil, nil
}
```

### Function Calls

```go
func (vm *VM) call(fn core.Value, args []core.Value) (core.Value, error) {
    switch f := fn.(type) {
    case *core.BuiltinFunction:
        // Call native Go function
        return f.Call(args, vm.getCurrentContext())

    case *BytecodeChunk:
        // Call bytecode function
        return vm.callBytecode(f, args)

    case core.Callable:
        // Generic callable
        return f.Call(args, vm.getCurrentContext())
    }

    return nil, &core.TypeError{Expected: "callable", Got: fn.Type()}
}

func (vm *VM) callBytecode(chunk *BytecodeChunk, args []core.Value) (core.Value, error) {
    // Arity check
    if len(args) != chunk.ArgCount {
        return nil, fmt.Errorf("expected %d args, got %d", chunk.ArgCount, len(args))
    }

    // Create new frame
    newFrame := &Frame{
        chunk:     chunk,
        pc:        0,
        registers: make([]core.Value, 256),  // Pre-allocate
        locals:    make(map[string]core.Value),
        parent:    vm.frames[vm.frameIndex],
    }

    // Bind arguments to parameters
    for i, arg := range args {
        paramName := chunk.VarNames[i]
        newFrame.locals[paramName] = arg
    }

    // Push frame
    vm.frameIndex++
    if vm.frameIndex >= len(vm.frames) {
        vm.frames = append(vm.frames, newFrame)
    } else {
        vm.frames[vm.frameIndex] = newFrame
    }

    // Continue execution in new frame
    return vm.run()
}
```

---

## Optimization Strategies

### 1. Type Specialization via Profiling

**Approach:** Profile types at runtime, specialize hot paths

```go
type TypeProfile struct {
    SeenTypes map[core.Type]int
    TotalCalls int
}

func (vm *VM) recordTypeFeedback(frame *Frame, instr Instruction) {
    if instr.Opcode == OP_ADD {
        left := frame.registers[getReg(instr.Arg1)]
        right := frame.registers[getReg(instr.Arg2)]

        profile := vm.getProfile(frame.chunk, frame.pc)
        profile.SeenTypes[left.Type()]++
        profile.SeenTypes[right.Type()]++
        profile.TotalCalls++
    }
}

func (o *Optimizer) Optimize(chunk *BytecodeChunk, feedback map[int]*TypeProfile) {
    for pc, profile := range feedback {
        if profile.TotalCalls < 100 {
            continue  // Not hot enough
        }

        // Check if 95%+ are same type
        if dominant, ratio := profile.GetDominantType(); ratio > 0.95 {
            instr := chunk.Code[pc]

            // Insert type guard + specialized instruction
            chunk.InsertBefore(pc, Instruction{
                Opcode: OP_GUARD_INT,
                Dest:   getReg(instr.Arg1),
            })

            chunk.Code[pc+1] = Instruction{
                Opcode: OP_ADD_INT,  // Specialized!
                Dest:   instr.Dest,
                Arg1:   instr.Arg1,
                Arg2:   instr.Arg2,
            }
        }
    }
}
```

### 2. Constant Folding

**Detect compile-time constants and evaluate eagerly**

```go
func (o *Optimizer) ConstantFold(chunk *BytecodeChunk) {
    for i := 2; i < len(chunk.Code); i++ {
        instr := chunk.Code[i]

        // Pattern: LOAD_CONST r0, c1
        //          LOAD_CONST r1, c2
        //          ADD        r2, r0, r1
        // →        LOAD_CONST r2, (c1 + c2)

        if instr.Opcode == OP_ADD {
            prev1 := chunk.Code[i-2]
            prev2 := chunk.Code[i-1]

            if prev1.Opcode == OP_LOAD_CONST && prev2.Opcode == OP_LOAD_CONST {
                c1 := chunk.Constants[prev1.Arg1.Data]
                c2 := chunk.Constants[prev2.Arg1.Data]

                if n1, ok := c1.(core.NumberValue); ok {
                    if n2, ok := c2.(core.NumberValue); ok {
                        result := n1 + n2
                        idx := chunk.AddConstant(result)

                        // Replace 3 instructions with 1
                        chunk.Code[i-2] = Instruction{
                            Opcode: OP_LOAD_CONST,
                            Dest:   instr.Dest,
                            Arg1:   Operand{Type: OP_CONSTANT, Data: uint64(idx)},
                        }
                        chunk.Code[i-1] = Instruction{Opcode: OP_NOP}
                        chunk.Code[i] = Instruction{Opcode: OP_NOP}
                    }
                }
            }
        }
    }
}
```

### 3. Dead Code Elimination

**Remove instructions whose results are never used**

```go
func (o *Optimizer) DeadCodeElimination(chunk *BytecodeChunk) {
    // Build liveness analysis
    liveness := o.computeLiveness(chunk)

    for i := 0; i < len(chunk.Code); i++ {
        instr := chunk.Code[i]

        // If result register is never read, remove instruction
        if !liveness.IsLive(i, instr.Dest) && !hasSideEffects(instr) {
            chunk.Code[i] = Instruction{Opcode: OP_NOP}
        }
    }
}
```

### 4. Inline Caching

**Cache method lookups and attribute access**

```go
// Before optimization
OP_GET_ATTR  r0, obj, "method"    // Lookup every time

// After inline caching
OP_LOAD_ATTR_CACHED  r0, obj, offset, "method"
// First call: lookup and cache offset
// Subsequent calls: direct offset access (if type matches)
```

---

## Serialization Format

### File Format (.m28c)

```
M28C Bytecode File Format v1.0

┌─────────────────────────────────────┐
│ Header (32 bytes)                   │
│  - Magic: "M28C" (4 bytes)          │
│  - Version: uint16                  │
│  - Flags: uint16                    │
│  - Timestamp: int64                 │
│  - Source Hash: [16]byte (MD5)      │
└─────────────────────────────────────┘
┌─────────────────────────────────────┐
│ Metadata Section                    │
│  - Name: string                     │
│  - Filename: string                 │
│  - FirstLine: uint32                │
│  - ArgCount: uint32                 │
│  - Flags: uint32                    │
└─────────────────────────────────────┘
┌─────────────────────────────────────┐
│ Code Section                        │
│  - Instruction Count: uint32        │
│  - Instructions: [Instruction]      │
└─────────────────────────────────────┘
┌─────────────────────────────────────┐
│ Constants Section                   │
│  - Constant Count: uint32           │
│  - Constants: [core.Value]          │
└─────────────────────────────────────┘
┌─────────────────────────────────────┐
│ Names Section                       │
│  - Names: [string]                  │
│  - VarNames: [string]               │
│  - CellVars: [string]               │
│  - FreeVars: [string]               │
└─────────────────────────────────────┘
┌─────────────────────────────────────┐
│ Debug Section (optional)            │
│  - Line Table: [PC -> Line]         │
│  - Source Map: [PC -> (file,line)]  │
└─────────────────────────────────────┘
```

### Serialization Example

```go
func (chunk *BytecodeChunk) Serialize() ([]byte, error) {
    buf := new(bytes.Buffer)

    // Write header
    buf.Write([]byte("M28C"))
    binary.Write(buf, binary.LittleEndian, uint16(1))      // version
    binary.Write(buf, binary.LittleEndian, uint16(0))      // flags
    binary.Write(buf, binary.LittleEndian, time.Now().Unix())
    buf.Write(make([]byte, 16))  // hash placeholder

    // Write metadata
    writeString(buf, chunk.Name)
    writeString(buf, chunk.Filename)
    binary.Write(buf, binary.LittleEndian, uint32(chunk.FirstLine))
    binary.Write(buf, binary.LittleEndian, uint32(chunk.ArgCount))
    binary.Write(buf, binary.LittleEndian, uint32(chunk.Flags))

    // Write code
    binary.Write(buf, binary.LittleEndian, uint32(len(chunk.Code)))
    for _, instr := range chunk.Code {
        binary.Write(buf, binary.LittleEndian, instr)
    }

    // Write constants, names, debug info...

    return buf.Bytes(), nil
}
```

### Usage

```bash
# Compile M28 to bytecode
$ m28 compile fibonacci.m28 -o fibonacci.m28c

# Run bytecode
$ m28 run fibonacci.m28c

# Disassemble bytecode
$ m28 disassemble fibonacci.m28c
```

---

## Integration with M28

### Transparent Integration

```go
// eval/evaluator.go
func Eval(expr core.Value, ctx *core.Context) (core.Value, error) {
    // Check if bytecode compilation is enabled
    if ctx.UseBytecode {
        // Try to compile to bytecode
        compiler := mir.NewCompiler("<eval>")
        chunk, err := compiler.Compile(expr)

        if err != nil {
            // Fall back to tree-walking
            return evalTreeWalk(expr, ctx)
        }

        // Execute bytecode
        vm := mir.GetVM()  // Reuse VM instance
        vm.SetContext(ctx)
        return vm.Execute(chunk)
    }

    // Original tree-walking interpreter
    return evalTreeWalk(expr, ctx)
}
```

### Configuration

```go
// Enable bytecode compilation
ctx.UseBytecode = true
ctx.BytecodeOptLevel = 2  // 0 = none, 1 = basic, 2 = aggressive

// Compile threshold
ctx.CompileThreshold = 10  // Compile after 10 interpretations
```

### Mixed Execution

```go
// Some functions interpreted, some compiled
func (vm *VM) call(fn core.Value, args []core.Value) (core.Value, error) {
    switch f := fn.(type) {
    case *BytecodeChunk:
        // Bytecode - fast
        return vm.callBytecode(f, args)
    case *eval.UserFunction:
        // Interpreted - slower, but works
        return f.Call(args, ctx)
    }
}
```

---

## Performance Expectations

### Benchmark Targets

| Workload | Tree-walk | Bytecode | Target Speedup |
|----------|-----------|----------|----------------|
| Fibonacci(30) | 1000ms | 300ms | 3-4x |
| List sum (1M) | 500ms | 100ms | 5x |
| Dict operations | 200ms | 50ms | 4x |
| Function calls | 100ms | 30ms | 3x |
| String manipulation | 150ms | 40ms | 3-4x |

### Memory Overhead

- **Bytecode size:** 5-10x smaller than AST
- **VM overhead:** ~2-4KB per call frame
- **Optimization data:** ~1KB per hot function
- **.m28c file:** ~60% of source size (with debug info), ~30% without

### Compilation Time

- **Simple function:** <1ms
- **Complex function:** 5-10ms
- **Module (1000 LOC):** 50-100ms

**Trade-off:** Compilation cost amortized over multiple executions.

---

## Implementation Plan

### Phase 1: Core Infrastructure (Week 1-2)

**Deliverables:**
- [ ] Instruction set definition (`mir/opcodes.go`)
- [ ] Instruction and BytecodeChunk types (`mir/types.go`)
- [ ] Basic compiler scaffold (`mir/compiler.go`)
- [ ] Basic VM scaffold (`mir/vm.go`)
- [ ] Unit tests for core types

**Test:** Compile and execute simple expressions (constants, variables)

### Phase 2: Compiler Implementation (Week 3-4)

**Deliverables:**
- [ ] Expression compilation (arithmetic, comparisons)
- [ ] Control flow compilation (if, loops)
- [ ] Function definition compilation
- [ ] Function call compilation
- [ ] Register allocation
- [ ] Constant pool management

**Test:** Compile fibonacci, factorial, list comprehensions

### Phase 3: VM Implementation (Week 5-6)

**Deliverables:**
- [ ] Complete opcode dispatch
- [ ] Stack frame management
- [ ] Variable lookup (locals, globals, closure)
- [ ] Function call mechanism
- [ ] Exception handling
- [ ] Debug support (stack traces)

**Test:** Execute all compiled programs, compare with tree-walking

### Phase 4: Optimization (Week 7-8)

**Deliverables:**
- [ ] Type feedback collection
- [ ] Type specialization
- [ ] Constant folding
- [ ] Dead code elimination
- [ ] Inline caching (basic)

**Test:** Performance benchmarks show 3-5x speedup

### Phase 5: Serialization (Week 9-10)

**Deliverables:**
- [ ] Bytecode serialization (.m28c format)
- [ ] Bytecode deserialization
- [ ] CLI commands (compile, disassemble)
- [ ] Bytecode verification

**Test:** Save, load, and execute bytecode files

### Phase 6: Integration & Testing (Week 11-12)

**Deliverables:**
- [ ] Integrate with main evaluator
- [ ] Transparent fallback to tree-walking
- [ ] Configuration options
- [ ] Performance benchmarking suite
- [ ] Documentation

**Test:** Full M28 test suite passes with bytecode enabled

---

## Risks and Mitigations

### Risk 1: Complexity

**Risk:** Bytecode VM adds significant complexity
**Mitigation:**
- Start with minimal instruction set (~50 opcodes)
- Extensive unit tests for each opcode
- Keep tree-walking as fallback
- Gradual rollout (opt-in initially)

### Risk 2: Performance Not Meeting Targets

**Risk:** May not achieve 3-5x speedup
**Mitigation:**
- Profile early and often
- Focus on hot paths first
- Type specialization for common cases
- Can still benefit from 2x speedup

### Risk 3: Compatibility Issues

**Risk:** Bytecode semantics differ from tree-walking
**Mitigation:**
- Extensive test coverage
- Side-by-side execution validation
- Fuzzing to find edge cases
- Keep tree-walking as reference implementation

### Risk 4: Debugging Harder

**Risk:** Bytecode harder to debug than source
**Mitigation:**
- Full source mapping
- Disassembler tool
- Interactive debugger
- Option to disable bytecode compilation

### Risk 5: Maintenance Burden

**Risk:** Two execution engines to maintain
**Mitigation:**
- Shared core types and semantics
- Automated cross-validation tests
- Focus on tree-walking for new features initially
- Bytecode compilation can lag (graceful degradation)

---

## Future Extensions

### After Initial Implementation

1. **Bytecode Optimizer Pipeline**
   - Loop unrolling
   - Inlining
   - Escape analysis
   - Tail call optimization

2. **JIT Compilation**
   - Compile bytecode → native code (LLVM)
   - OSR (on-stack replacement) for hot loops
   - Profile-guided optimization

3. **Multi-threading**
   - Multiple VMs in parallel
   - Shared constant pools
   - Lock-free optimization

4. **Advanced Inline Caching**
   - Polymorphic inline caches
   - Megamorphic cache fallback
   - Hidden class tracking

5. **Bytecode-to-WASM Compiler**
   - MIR bytecode → WebAssembly
   - Run M28 in browsers at near-native speed
   - Portable deployment

6. **Bytecode Verification**
   - Type safety checks
   - Stack depth verification
   - Control flow validation
   - Security sandboxing

---

## Appendix A: Complete Opcode Reference

See separate document: `mir-opcodes-reference.md`

## Appendix B: Compiler Algorithm Details

See separate document: `mir-compiler-algorithms.md`

## Appendix C: Performance Benchmarks

See separate document: `mir-performance-benchmarks.md`

## Appendix D: Comparison with Other VMs

| Feature | M28 MIR | CPython | Lua 5.4 | V8 Ignition |
|---------|---------|---------|---------|-------------|
| Architecture | Register | Stack | Register | Register |
| Registers | Unlimited | - | 256 | 256 |
| Type guards | Yes | No | No | Yes |
| Inline caching | Planned | Limited | No | Yes |
| JIT | Future | No | Yes (LuaJIT) | Yes |
| Serialization | Yes | Yes (.pyc) | Yes | No |

---

## References

- [CPython Bytecode](https://docs.python.org/3/library/dis.html)
- [Lua 5.0 Register VM](https://www.lua.org/doc/jucs05.pdf)
- [V8 Ignition Interpreter](https://v8.dev/blog/ignition-interpreter)
- [PyPy JIT Compiler](https://pypy.org/)
- [Crafting Interpreters - Bytecode VM](https://craftinginterpreters.com/a-bytecode-virtual-machine.html)

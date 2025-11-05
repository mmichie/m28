# M28 Design Documents

This directory contains design documents for M28's architecture and features.

## Primary Architecture Document

📘 **[Compiler Architecture](./compiler-architecture.md)** - **START HERE**

The comprehensive architecture guide covering:
- Multi-frontend, multi-backend design
- HIR (High-Level IR) as pivot point
- How to add frontends and backends
- Static vs dynamic language support
- Migration strategy from current state to full architecture

## Backend Designs

- **[MIR Bytecode VM](./mir-bytecode-vm.md)** - Detailed bytecode VM design (register-based, 3-10x speedup)

## Frontend Designs

**Python Frontend:**
- **[Python Frontend Design](./python-frontend-design.md)** - Overview of Python syntax support
- **[Python Frontend Progress](./python-frontend-progress.md)** - Implementation status tracker
- **[Phase B: Python Nodes](./phase-b-python-nodes-design.md)** - Python-specific AST node design
- **[Phase C: Python Parser](./phase-c-python-parser-design.md)** - Python parser implementation

## Feature Designs

- **[Type Enhancement Plan](./type-enhancement-plan.md)** - Type system improvements
- **[S-Strings](./s-strings.md)** - Metaprogramming with syntax strings
- **[Tuples and Functional Lists](./tuple-and-functional-lists.md)** - Tuple/list design decisions

## Document Organization

```
compiler-architecture.md    ← Main architecture (read this first!)
│
├── Backend designs
│   └── mir-bytecode-vm.md
│
├── Frontend designs
│   ├── python-frontend-design.md
│   ├── python-frontend-progress.md
│   ├── phase-b-python-nodes-design.md
│   └── phase-c-python-parser-design.md
│
└── Feature designs
    ├── type-enhancement-plan.md
    ├── s-strings.md
    └── tuple-and-functional-lists.md
```

## Quick Links

**Want to understand M28's architecture?** → Start with [Compiler Architecture](./compiler-architecture.md)

**Want to add a new language frontend?** → See "How to Add a Frontend" in [Compiler Architecture](./compiler-architecture.md#how-to-add-a-frontend)

**Want to add a new backend?** → See "How to Add a Backend" in [Compiler Architecture](./compiler-architecture.md#how-to-add-a-backend)

**Want to implement Python support?** → See [Python Frontend Design](./python-frontend-design.md)

**Want to build the bytecode VM?** → See [MIR Bytecode VM](./mir-bytecode-vm.md)

**Working on type system?** → See [Type Enhancement Plan](./type-enhancement-plan.md)

## Contributing

When adding a new design document:

1. Add it to this README in the appropriate section
2. Link to it from the main architecture doc if relevant
3. Keep docs focused - split large topics into multiple docs
4. Include examples and code snippets
5. Mark status (Design, In Progress, Implemented)

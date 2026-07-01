# M28 Design Documents

Design documents for M28's architecture and features. Work items referenced
in these docs live in the beads tracker (`bd show <id>`, `bd ready`).

## Primary architecture document

**[Compiler Architecture](./compiler-architecture.md)** — START HERE. v2
(2026-07): multi-frontend, single-runtime platform; resolver + typed IR as the
compiler; resolved tree-walk execution; consolidation debt register;
performance and compliance strategy; what we are explicitly not building.

## Execution designs

- **[Bytecode VM](./mir-bytecode-vm.md)** — SUPERSEDED as a speed tier
  (vmspike, commit b276449). Retained as design constraints for a future
  frame-based tier for resumable semantics (M28-8jo, deferred).

## Frontend designs (historical — the Python frontend shipped)

These are retained as design records; the implementation is live
(`parser/python_tokenizer.go`, `parser/python_parser*.go`, `core/ast`).

- **[Python Frontend Design](./python-frontend-design.md)**
- **[Python Frontend Progress](./python-frontend-progress.md)**
- **[Phase B: Python Nodes](./phase-b-python-nodes-design.md)**
- **[Phase C: Python Parser](./phase-c-python-parser-design.md)**

## Feature designs

- **[S-Strings](./s-strings.md)** — metaprogramming with syntax strings.
  Status: proposed, not implemented.
- **[Type Enhancement Plan](./type-enhancement-plan.md)** — tuple/list work
  shipped; remaining type-system direction is tracked under epic M28-b20f.
- **[Tuples and Functional Lists](./tuple-and-functional-lists.md)** —
  historical; implemented.

## Contributing

When adding a design document:

1. Add it to this README in the appropriate section.
2. Put a status line at the top (Proposed / In progress / Implemented /
   Superseded) and keep it current — stale "north star" docs actively mislead.
3. Reference beads issue IDs for the work items the doc creates.
4. No emojis in docs.

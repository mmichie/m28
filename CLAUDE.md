# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build/Test Commands
- Build: `make build`
- Run all tests: `make test`
- Run REPL: `make run`
- Run single test: `./bin/m28 tests/[test-file.lisp]`
- Clean: `make clean`
- Install deps: `make deps`

## Code Style Guidelines
- **Go Style**: Follow standard Go conventions
  - Use camelCase for function names
  - Group imports properly (stdlib first, then third-party)
  - Return errors as second return value
  - Descriptive error messages
- **Types**: Use types defined in core/types.go for Lisp values
- **Validation**: Validate arguments before processing
- **Naming**:
  - Packages: lowercase (builtin, core, special_forms)
  - Functions: camelCase
- **Error Handling**: Return errors rather than panicking when possible
- **Comments**: Document complex logic or non-obvious behaviors

When modifying the codebase, maintain consistency with existing patterns and style.
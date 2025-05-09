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

When modifying the codebase, maintain consistency with existing patterns and
style.  Look for opportunities to consolidate similar code or refactor for
clarity.  If you encounter a complex function, consider breaking it down into
smaller, more manageable pieces.  Always run tests after making changes to
ensure nothing is broken.  Don't create new files unless necessary; instead,
reuse existing ones when possible.  If you need to create a new file, follow the
naming conventions and structure of the existing files.  If you are unsure about
a change, ask for clarification or guidance.  If you are adding new features,
consider how they will be tested and document them accordingly.  If you are
adding new dependencies, ensure they are necessary and do not bloat the
project.  If you are unsure about a dependency, ask for clarification or
guidance.  If you are adding new tests, ensure they are comprehensive and
cover all edge cases.  If you are unsure about a test, ask for clarification or
guidance.  If you are adding new documentation, ensure it is clear and
concise.  If you are unsure about a documentation change, ask for
clarification or guidance.  If you are adding new examples, ensure they are
clear and concise.  If you are unsure about an example, ask for clarification
or guidance.  If you are adding new benchmarks, ensure they are clear and
concise.


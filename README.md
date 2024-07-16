# M28 Lisp Interpreter

M28 is a Lisp interpreter that supports core Lisp functionality, including
arithmetic operations, list manipulation, control structures, and user-defined
functions. It features a REPL (Read-Eval-Print Loop) for interactive use and can
execute Lisp code from files.

## Installation

To use M28, follow these steps:

1. Ensure you have Go installed on your system.
2. Clone the repository: `git clone https://github.com/mmichie/m28.git`
3. Navigate to the project directory: `cd m28`
4. Build the project: `make`

## Usage

### REPL Mode

To start the M28 Lisp REPL, run:

```
./bin/m28
```

In the REPL, you can enter Lisp expressions and see their evaluated results.

### Execute a File

To execute a Lisp file with the `.m28` extension:

```
./bin/m28 path/to/your/file.m28
```

### Evaluate an Expression

To evaluate a single Lisp expression:

```
./bin/m28 "(+ 1 2 3)"
```

## Main Features

- **Arithmetic Operations**: Basic arithmetic (`+`, `-`, `*`, `/`, `%`)
- **Comparison Operations**: Numeric comparisons (`<`, `>`, `=`, `>=`, `<=`, `!=`)
- **List Operations**: List manipulation (`car`, `cdr`, `cons`, `list`, `length`)
- **Logic Operations**: Boolean logic (`and`, `or`, `not`)
- **Control Structures**: Conditionals and loops (`if`, `cond`, `case`, `do`)
- **Function Definition**: Lambda expressions and named functions
- **Special Forms**: `quote`, `define`, `lambda`, `let`, `set!`
- **Type Checking**: Functions to check types (`number?`, `string?`, `symbol?`, `list?`)
- **Error Handling**: Built-in error reporting

## Dependencies

- `github.com/chzyer/readline` for REPL functionality

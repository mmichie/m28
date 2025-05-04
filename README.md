# M28: A Pythonic Lisp Interpreter

M28 is a Lisp interpreter with Python-inspired syntax and features, combining the expressiveness of Lisp with familiar Python semantics. It features a REPL (Read-Eval-Print Loop) for interactive use and can execute code from files.

## Installation

To use M28, follow these steps:

1. Ensure you have Go installed on your system
2. Clone the repository: `git clone https://github.com/mmichie/m28.git`
3. Navigate to the project directory: `cd m28`
4. Build the project: `make build`

## Usage

### REPL Mode

To start the M28 Lisp REPL, run:

```bash
make run
# or directly
./bin/m28
```

In the REPL, you can enter expressions and see their evaluated results.

### Execute a File

To execute a file with the `.m28` extension:

```bash
./bin/m28 path/to/your/file.m28
```

### Run Tests

To run the language test suite:

```bash
make m28-test
```

## Main Features

- **Python-Like Syntax**: Familiar syntax for Python developers with Lisp structure
- **Arithmetic Operations**: Basic arithmetic (`+`, `-`, `*`, `/`, `%`, `**`)
- **Comparison Operations**: Numeric comparisons (`==`, `!=`, `<`, `>`, `>=`, `<=`)
- **Data Types**: Numbers, strings, lists, dictionaries, booleans, None
- **Variable Assignment**: Using the `=` operator
- **Function Definition**: Define functions with `def` and lambda expressions
- **Control Structures**: Conditionals and loops (`if`, `for`, `while`)
- **List Operations**: List creation, access, and manipulation
- **Dictionary Support**: Python-style dictionary literals `{"key": value}` and dictionary functions
- **Keyword Arguments**: Support for function keyword arguments via dictionaries
- **Type Checking**: Functions to check types (`number?`, `string?`, `symbol?`, `list?`)

## Documentation

For more detailed documentation, see the following guides:

- [M28 Documentation](docs/m28-documentation.md): Complete language guide
- [Pythonic Lisp How-to](docs/pythonic-lisp-howto.md): Quick introduction to the language
- [Dictionary Usage](docs/dictionary-usage.md): Guide to working with dictionaries

## Examples

M28 comes with numerous example programs in the `examples/` directory to demonstrate language features.

Basic example:

```lisp
# Define a function to greet someone
(def (greet name greeting)
    (= message (+ greeting ", " name "!"))
    (return message))

# Call the function
(print (greet "Alice" "Hello"))
```

## Dependencies

- Go standard library (no external dependencies for runtime)
- Development dependencies are managed through `go.mod`

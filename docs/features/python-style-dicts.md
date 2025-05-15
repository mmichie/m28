# Python-Style Dictionary Literals in M28

M28 now supports Python-style dictionary literals using curly braces, making the language more familiar to Python developers while enhancing readability and expressiveness.

## Basic Syntax

Python-style dictionary literals allow you to create dictionaries with a concise, familiar syntax:

```lisp
# Empty dictionary
{}

# Dictionary with string keys
{"name": "Alice", "age": 30, "city": "New York"}

# Dictionary with numeric keys
{1: "one", 2: "two", 3: "three"}

# Dictionary with mixed value types
{"string": "text", "number": 42, "boolean": True, "null": None}
```

This syntax is identical to Python's dictionary literals, making it very intuitive for Python developers.

> For comprehensive tests and examples, see the consolidated test file:
> - `tests/dict-test.m28` - Contains all essential dictionary tests
> 
> Run the tests with: `./run-tests.sh` or `./bin/m28 tests/dict-test.m28`

## Nested Dictionaries

You can nest dictionaries to create complex data structures:

```lisp
# Nested dictionaries
{"user": {"name": "Bob", "profile": {"age": 25, "interests": ["coding", "music"]}}}

# Configuration example
{"server": {"host": "localhost", "port": 8080}, "database": {"uri": "postgres://localhost/db"}}
```

## Usage with Variable Assignment

Use dictionary literals with variable assignment:

```lisp
# Using = for assignment
(= person {"name": "Alice", "age": 30})
(= config {"debug": True, "log_level": "info"})

# Using def for definition
(def user {"id": 123, "username": "bob"})
```

## Accessing Values

Access dictionary values using the `get` function or dot notation:

```lisp
# With get function
(get person "name")  # Returns: "Alice"

# With default value for missing keys
(get person "country" "Unknown")  # Returns: "Unknown" if key doesn't exist

# With dot notation (if supported)
person.name  # Returns: "Alice"
config.server.host  # For nested dictionaries
```

## Dictionary as Function Arguments

Use dictionaries as keyword arguments to functions:

```lisp
# Print with custom separator
(print "Hello" "World" {"sep": "-"})  # Prints: Hello-World

# Configure logging
(setup_logging {"level": "debug", "format": "json"})
```

## Implementation Notes

### Key Types

Dictionary keys must be hashable types:
- Strings: `"name"`, `"user_id"`
- Numbers: `1`, `42`, `3.14`
- Symbols: `True`, `False`, `None`

Other types are automatically converted to strings when used as keys.

### Quote Handling

Both double-quoted and single-quoted strings work as keys:

```lisp
# Double-quoted keys
{"name": "Alice"}

# Single-quoted keys
{'age': 30}
```

### Dictionary Methods

Dictionary objects support these methods:
- `keys` - Get all keys
- `values` - Get all values
- `items` - Get key-value pairs
- `get` - Get value with optional default
- `has_key` - Check if key exists
- `update` - Update with another dictionary

## Comparison with Traditional Syntax

M28 supports both traditional functional dict creation and the new Python-style literal syntax:

```lisp
# Traditional syntax
(= user (dict "name" "Alice" "age" 30))

# Python-style syntax
(= user {"name": "Alice", "age": 30})
```

The Python-style syntax offers improved readability, especially for complex or nested dictionaries.

## Best Practices

- Use Python-style dictionary literals for better readability and familiarity
- Stick to simple, hashable types for keys when possible
- Use consistent quoting style for keys (either single or double quotes)
- Leverage nested dictionaries for structured configuration
- Consider using dot notation for cleaner access to dictionary values
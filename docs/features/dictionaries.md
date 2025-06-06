# Dictionaries in M28

This guide explains how to work with dictionaries in the M28 language, including Python-style dictionary literals and dictionary operations.

## Contents
1. [Creating Dictionaries](#creating-dictionaries)
2. [Accessing Dictionary Values](#accessing-dictionary-values)
3. [Dictionary Operations](#dictionary-operations)
4. [Dictionary as Keyword Arguments](#dictionary-as-keyword-arguments)
5. [Examples](#examples)
6. [Dictionary Methods Reference](#dictionary-methods-reference)
7. [Implementation Notes](#implementation-notes)

## Creating Dictionaries

M28 supports two main methods for creating dictionaries:

### Dictionary Literals (Python-style)

```lisp
# Create a dictionary with key-value pairs
(= person {"name": "John", "age": 30, "city": "New York"})

# Empty dictionary
(= empty {})

# Dictionary with numeric keys
(= counts {1: "one", 2: "two", 3: "three"})

# Dictionary with mixed value types
(= mixed {"string": "text", "number": 42, "boolean": True, "null": None})

# Nested dictionaries
(= nested {"user": {"name": "Bob", "profile": {"age": 25}}})

# Dictionary with list values
(= with_lists {"colors": ["red", "green", "blue"], "numbers": [1, 2, 3]})
```

This syntax is identical to Python's dictionary literals, making it very familiar to Python developers.

### Dict Function

```lisp
# Create a dictionary with the dict function
(= person (dict "name" "Jane" "age" 25 "city" "Boston"))
```

The dict function takes alternating keys and values as arguments.

## Accessing Dictionary Values

M28 provides multiple ways to access dictionary values:

### Dot Notation (Read Only)
```lisp
# Direct property access
person.name  # Returns: "John"
person.age   # Returns: 30
```

### Get Method
```lisp
# Using the get method
(person.get "name")  # Returns: "John"

# With a default value for missing keys
(person.get "country" "Unknown")  # Returns: "Unknown" if "country" key doesn't exist
```

## Dictionary Operations

### Checking Dictionary Size

```lisp
# Get the number of entries in a dictionary
(len person)  # Returns: 3 for the example above
```

### Dictionary Keys and Values

```lisp
# Get all keys
(person.keys)  # Returns: ["name", "age", "city"]

# Get all values
(person.values)  # Returns: ["John", 30, "New York"]

# Get key-value pairs
(person.items)  # Returns: [["name", "John"], ["age", 30], ["city", "New York"]]
```

### Checking for Keys

```lisp
# Check if key exists
(in "name" person)  # Returns: True
(in "email" person)  # Returns: False
```

### Modifying Dictionaries

```lisp
# Set a value using index assignment
(= person["email"] "john@example.com")

# Update with another dictionary
(person.update {"country": "USA", "zip": "10001"})

# Remove a key
(person.pop "age")  # Removes the key and returns its value
(person.pop "missing" "default")  # Returns default if key doesn't exist

# Set value only if key doesn't exist
(person.setdefault "status" "active")

# Clear all entries
(person.clear)  # Removes all key-value pairs

# Create a copy
(= person_copy (person.copy))
```

## Dictionary as Keyword Arguments

One powerful feature of dictionaries is using them as keyword arguments for functions, similar to Python:

```lisp
# Print with custom separator
(print "Hello" "World" {"sep": "-"})  # Prints: Hello-World

# Print with custom end string (no newline)
(print "Hello" {"end": ""})  # Prints: Hello (with no newline)

# Both separator and end
(print "a" "b" "c" {"sep": "|", "end": "!\n"})  # Prints: a|b|c!
```

## Examples

### Example 1: Student Records

```lisp
# Create individual student records
(= alice {"name": "Alice", "grade": 95})
(= bob {"name": "Bob", "grade": 87})
(= charlie {"name": "Charlie", "grade": 91})

# Create a list of students
(= students (list alice bob charlie))

# Access student information
(print "First student name:" (get (get students 0) "name"))  # Alice
(print "Second student grade:" (get (get students 1) "grade"))  # 87

# Loop through students
(for student students
  (print (+ (get student "name") " earned a grade of " (get student "grade"))))
```

### Example 2: Configuring Function Behavior

```lisp
(def (process_data data options)
  # Get option values with defaults
  (= verbose (get options "verbose" False))
  (= max_items (get options "max_items" 10))
  
  # Use the options in processing
  (if verbose
      (print "Processing data with max_items:" max_items))
  
  # Process data...
)

# Call with different options
(process_data some_data {"verbose": True, "max_items": 5})
(process_data other_data {"verbose": False})
```

### Example 3: Nested Dictionaries

```lisp
# Create a nested dictionary structure
(= config {
  "server": {
    "host": "localhost",
    "port": 8080,
    "debug": True
  },
  "database": {
    "uri": "postgresql://user:pass@localhost/db",
    "pool_size": 5,
    "timeout": 30
  },
  "logging": {
    "level": "info",
    "file": "/var/log/app.log"
  }
})

# Access nested values
(print "Server host:" (get (get config "server") "host"))
```

## Dictionary Methods Reference

Here's a complete reference of all dictionary methods available in M28:

| Method | Description | Example | Return Value |
|--------|-------------|---------|--------------|
| `get` | Get value with optional default | `(dict.get d "key" "default")` | Value or default |
| `set` | Set key to value | `(dict.set d "key" "value")` | Dictionary (for chaining) |
| `keys` | Get all keys | `(dict.keys d)` | List of keys |
| `values` | Get all values | `(dict.values d)` | List of values |
| `items` | Get all key-value pairs | `(dict.items d)` | List of [key, value] lists |
| `has_key` | Check if key exists | `(dict.has_key d "key")` | True or False |
| `contains?` | Check if key exists (alias) | `(dict.contains? d "key")` | True or False |
| `update` | Update with another dict | `(dict.update d other_dict)` | None |
| `pop` | Remove key and return value | `(dict.pop d "key" "default")` | Value or default |
| `popitem` | Remove and return a key-value pair | `(dict.popitem d)` | [key, value] list |
| `setdefault` | Get value, set if not exists | `(dict.setdefault d "key" "default")` | Value or default |
| `clear` | Remove all items | `(dict.clear d)` | None |
| `copy` | Create a shallow copy | `(dict.copy d)` | New dictionary |

## Implementation Notes

- Dictionary keys can be strings, numbers, symbols, or any other hashable value
- Dictionary values can be any valid M28 value
- Dictionaries are implemented with thread-safe operations for concurrent access
- Dictionary keys are sorted in a consistent order for iteration
- Each dictionary has a strong reference to the evaluator, ensuring persistence
- When used as keyword arguments, dictionaries must be the last argument
- Dictionaries are implemented using the `PythonicDict` type in the M28 core
- The object protocol integration allows using common properties like `len` and `size`
- Dictionary string representation follows Python-style format: `{"key": value, ...}`
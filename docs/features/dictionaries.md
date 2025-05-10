# Dictionaries in M28

This guide explains how to work with dictionaries in the M28 language, including Python-style dictionary literals and dictionary operations.

## Contents
1. [Creating Dictionaries](#creating-dictionaries)
2. [Accessing Dictionary Values](#accessing-dictionary-values)
3. [Dictionary Operations](#dictionary-operations)
4. [Dictionary as Keyword Arguments](#dictionary-as-keyword-arguments)
5. [Examples](#examples)
6. [Implementation Notes](#implementation-notes)

## Creating Dictionaries

M28 supports two main methods for creating dictionaries:

### Dictionary Literals (Python-style)

```lisp
# Create a dictionary with key-value pairs
(= person {"name": "John", "age": 30, "city": "New York"})
```

This syntax is similar to Python's dictionary literals, making it familiar to Python developers.

### Dict Function

```lisp
# Create a dictionary with the dict function
(= person (dict "name" "Jane" "age" 25 "city" "Boston"))
```

The dict function takes alternating keys and values as arguments.

## Accessing Dictionary Values

Use the `get` function to access dictionary values:

```lisp
# Basic access
(get person "name")  # Returns: "John"

# With a default value for missing keys
(get person "country" "Unknown")  # Returns: "Unknown" if "country" key doesn't exist
```

With dot notation:

```lisp
# Dot notation (if supported)
(print person.name)  # Returns: "John"
```

## Dictionary Operations

### Checking Dictionary Size

```lisp
# Get the number of entries in a dictionary
(len person)  # Returns: 3 for the example above
```

### Dictionary Methods

```lisp
# Get all keys
(dict.keys person)  # Returns: ["name", "age", "city"]

# Get all values
(dict.values person)  # Returns: ["John", 30, "New York"]

# Check if key exists
(dict.has_key person "name")  # Returns: True

# Update dictionary
(dict.update person {"country": "USA"})
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
# With dot notation
(print "Database URI:" config.database.uri)
```

## Implementation Notes

- Dictionary keys can be strings, numbers, or other immutable types
- Dictionary values can be any valid M28 value
- Dictionaries maintain insertion order (like Python 3.7+)
- When used as keyword arguments, dictionaries must be the last argument

### Dictionary Methods

M28 dictionaries support these methods (accessed via dot notation or dict functions):

| Method | Description | Example |
|--------|-------------|---------|
| `keys` | Get list of keys | `(dict.keys my_dict)` |
| `values` | Get list of values | `(dict.values my_dict)` |
| `items` | Get key-value pairs | `(dict.items my_dict)` |
| `get` | Get value with default | `(dict.get my_dict "key" "default")` |
| `update` | Update with another dict | `(dict.update my_dict {"key": "value"})` |
| `has_key` | Check if key exists | `(dict.has_key my_dict "key")` |
| `pop` | Remove and return value | `(dict.pop my_dict "key")` |
| `clear` | Remove all items | `(dict.clear my_dict)` |

### Comparing with Python

M28's dictionary implementation is designed to feel familiar to Python users:

| Python | M28 |
|--------|-----|
| `d = {"key": value}` | `(= d {"key": value})` |
| `d = dict(key=value)` | `(= d (dict "key" value))` |
| `d["key"]` | `(get d "key")` |
| `d.get("key", default)` | `(get d "key" default)` |
| `len(d)` | `(len d)` |
| `print("a", "b", sep="-")` | `(print "a" "b" {"sep": "-"})` |
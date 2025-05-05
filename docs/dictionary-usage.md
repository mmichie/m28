# Dictionaries in M28

This guide explains how to work with dictionaries in the M28 language, including the newly added Python-style dictionary literals and keyword arguments.

## Creating Dictionaries

M28 supports two main methods for creating dictionaries:

### 1. Dictionary Literals (Python-style)

```lisp
# Create a dictionary with key-value pairs
(= person {"name": "John", "age": 30, "city": "New York"})
```

This syntax is similar to Python's dictionary literals, making it familiar to Python developers.

### 2. Dict Function

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

## Dictionary Operations

### Checking Dictionary Size

```lisp
# Get the number of entries in a dictionary
(len person)  # Returns: 3 for the example above
```

### Dictionary as Keyword Arguments

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

# Note: The following loop syntax may vary based on implementation
# In concept, iterating through student records would look like:
# (for student students
#   (print (get student "name")))
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

## Dictionary Conversion

Convert between dictionaries and other data structures:

```lisp
# List of key-value pairs to dictionary
(= pairs (list (list "a" 1) (list "b" 2)))
(= d (dict pairs))

# Dictionary to list of keys
(= keys (dict_keys d))

# Dictionary to list of values
(= values (dict_values d))
```

## Implementation Notes

- Dictionary keys can be strings, numbers, or other immutable types
- Dictionary values can be any valid M28 value
- Dictionaries maintain insertion order (like Python 3.7+)
- When used as keyword arguments, dictionaries must be the last argument

## Comparing with Python

M28's dictionary implementation is designed to feel familiar to Python users:

| Python | M28 |
|--------|-----|
| `d = {"key": value}` | `(= d {"key": value})` |
| `d = dict(key=value)` | `(= d (dict "key" value))` |
| `d["key"]` | `(get d "key")` |
| `d.get("key", default)` | `(get d "key" default)` |
| `len(d)` | `(len d)` |
| `print("a", "b", sep="-")` | `(print "a" "b" {"sep": "-"})` |
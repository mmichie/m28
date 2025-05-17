# M28 Dot Notation Quick Reference

## Syntax Options

M28 supports two forms of dot notation:

### Functional Form

```lisp
(dot object "property-name")
(. object "property-name")  # Shorthand form
```

### Nested Access

```lisp
(dot object "prop1" "prop2" "prop3")  # Multi-segment path
(. (. (. object "prop1") "prop2") "prop3")  # Nested calls
```

## Common Operations

### Dictionary Access

```lisp
(= person {"name": "Alice", "age": 30})

# Get a property
(= name (. person "name"))  # Returns "Alice"

# Set a property
(= (. person "age") 31)
```

### Nested Dictionary Access

```lisp
(= data {"user": {"profile": {"email": "user@example.com"}}})

# Access nested data
(= email (dot data "user" "profile" "email"))
```

### Method Calls

```lisp
(= obj {"method": (lambda (self arg) (+ arg 1))})

# Call a method
(= result (. obj "method" obj 5))  # Returns 6
```

### List Operations

```lisp
(= items [1, 2, 3, 4, 5])

# Get list properties
(= length (. items "length"))  # Number of items
(= first (. items "first"))    # First item
(= last (. items "last"))      # Last item 
```

## Error Handling

```lisp
# Handle missing properties
(try
  (= value (. obj "missing-prop"))
  (catch e
    (print "Property not found:" e)))

# Handle nil objects
(try
  (= value (. nil "anything"))
  (catch e
    (print "Cannot access nil:" e)))
```

## Tips

1. Use dot notation for cleaner, more readable code
2. For computed property names, use the functional form
3. For static property names, the shorthand `.` form is more concise
4. When working with complex nested structures, use multi-segment paths
5. Remember to handle potential errors when accessing dynamic properties
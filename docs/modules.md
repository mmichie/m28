# M28 Module System

M28 provides a Python-inspired module system for organizing and reusing code. This guide covers creating, importing, and using modules.

## Creating Modules

Modules in M28 are files with `.m28` extension containing M28 code. You can define variables, functions, and classes that can be imported and used in other code.

### Basic Module Structure

```lisp
# math_utils.m28

# Module metadata (convention)
(= __name__ "math_utils")
(= __version__ "1.0.0")

# Public functions
(def square (x)
  (* x x))

(def cube (x)
  (* x x x))

# Private helper (convention: underscore prefix)
(def _validate_number (x)
  (if (not (number? x))
      (raise "Input must be a number")))

# Export control
(= __exports__ (list "square" "cube"))
```

## Importing Modules

### Basic Import

```lisp
# Import a module
(import "math_utils")

# Use imported functions
(print (math_utils.square 5))     # 25
(print (math_utils.cube 3))       # 27
```

### Import with Alias

```lisp
# Import with a shorter name
(import "math_utils" as mu)

(print (mu.square 5))             # 25
(print (mu.cube 3))               # 27
```

### Import Specific Names

You can import specific names from a module using the `from` clause with list syntax:

```lisp
# Import only specific functions/variables
(import "math_utils" from [square cube])

# Now you can use them directly without the module prefix
(print (square 5))    # 25
(print (cube 3))      # 27
```

### Wildcard Import

Import all exported names into the current namespace:

```lisp
# Import all exported symbols
(import "math_utils" from *)

# All exported names are now available directly
(print (square 4))    # 16
```

**Note:** Only symbols listed in `__exports__` or non-private symbols (not starting with `_`) are imported.

### Import with .m28 Extension

You can explicitly include the `.m28` extension when importing:

```lisp
# Both of these work and refer to the same module
(import "math_utils")
(import "math_utils.m28")

# With alias
(import "math_utils.m28" as mu)
```

The module system automatically handles the extension and ensures proper caching.

### Import from Package

```lisp
# Import from a package directory
(import "mypackage.submodule")
(import "mypackage.utils" as utils)
```

## Export Control

### Using __exports__

The `__exports__` list controls which symbols are accessible when the module is imported:

```lisp
# my_module.m28
(= PUBLIC_CONSTANT 42)
(= _PRIVATE_VAR "internal")

(def public_function (x)
  (* x 2))

(def _private_helper ()
  "Internal use only")

# Only these will be accessible
(= __exports__ (list 
  "PUBLIC_CONSTANT"
  "public_function"
))
```

When imported:
```lisp
(import "my_module")

# These work
(print my_module.PUBLIC_CONSTANT)      # 42
(print (my_module.public_function 5))  # 10

# These fail - not exported
(print my_module._PRIVATE_VAR)         # Error
(print (my_module._private_helper))    # Error
```

### Private Symbol Convention

Symbols starting with underscore (`_`) are considered private by convention:
- They won't be included in wildcard imports (if implemented)
- They signal internal implementation details
- Tools may hide them in documentation

## Module Search Path

M28 looks for modules in these locations (in order):
1. Current directory
2. Directory of the main script being run
3. Directories in `M28_PATH` environment variable

```bash
# Set module search path
export M28_PATH="/home/user/m28libs:/usr/local/lib/m28"
```

## Module Metadata

Modules automatically have access to special variables:

```lisp
# Inside a module
(print __name__)    # Module name or "__main__" if run directly
(print __file__)    # Full path to the module file

# Main guard pattern
(if (== __name__ "__main__")
  (do
    (print "Running as main program")
    (run-tests)))
```

## Best Practices

### 1. Use Clear Naming
```lisp
# Good: descriptive module names
(import "string_utils")
(import "database_connection")

# Bad: vague names
(import "utils")
(import "stuff")
```

### 2. Control Exports
```lisp
# Always define __exports__ for public APIs
(= __exports__ (list
  "connect"
  "query"
  "close"
))
```

### 3. Document Public APIs
```lisp
(def connect (host port)
  "Connect to database at given host and port.
   Returns a connection object."
  ...)
```

### 4. Use Package Structure
```
project/
├── main.m28
├── lib/
│   ├── __init__.m28
│   ├── database.m28
│   └── utils.m28
└── tests/
    └── test_database.m28
```

### 5. Avoid Circular Imports
Structure your modules to avoid circular dependencies. If A imports B, B should not import A.

## Common Patterns

### Configuration Module
```lisp
# config.m28
(= DEBUG True)
(= DATABASE_URL "postgresql://localhost/mydb")
(= MAX_CONNECTIONS 100)

(= __exports__ (list "DEBUG" "DATABASE_URL" "MAX_CONNECTIONS"))
```

### Utility Module
```lisp
# utils.m28
(def read_json (filename)
  (with (open filename "r") as f
    (json.loads (f.read))))

(def write_json (data filename)
  (with (open filename "w") as f
    (f.write (json.dumps data))))

(= __exports__ (list "read_json" "write_json"))
```

### Class Module
```lisp
# models.m28
(class User ()
  (def __init__ (self name email)
    (= self.name name)
    (= self.email email))
  
  (def __str__ (self)
    f"User({self.name}, {self.email})"))

(class Post ()
  (def __init__ (self title content author)
    (= self.title title)
    (= self.content content)
    (= self.author author)))

(= __exports__ (list "User" "Post"))
```

## Troubleshooting

### Module Not Found
- Check the file exists and has `.m28` extension
- Verify the module is in the search path
- Use absolute imports for clarity

### Import Errors
- Ensure no circular dependencies
- Check for syntax errors in the module
- Verify __exports__ includes needed symbols

### Name Conflicts
- Use aliases to avoid naming conflicts
- Prefer specific imports over wildcard imports
- Use package structure to namespace modules
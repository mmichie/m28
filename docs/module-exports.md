# Module Exports in M28

This document describes the `__exports__` mechanism in M28, which allows module authors to control which symbols are exposed to importers.

## Overview

The module system now supports two key features for better namespace control:

1. **Module-level `__exports__` mechanism** - Explicitly control which symbols are exported from a module
2. **Private symbol convention** - Symbols starting with a single underscore (`_`) are considered private

These features help create cleaner, more maintainable code by separating internal implementation details from the public API.

## Usage

### Defining Exports

To control which symbols are exported from a module, define an `__exports__` list:

```lisp
# module.m28
(= PUBLIC_VAR 42)
(= _PRIVATE_VAR "internal use only")

(def (public_function x)
  (* x 2))

(def (_private_helper)
  "Internal helper function")

# Define the public API
(= __exports__ (list
  "PUBLIC_VAR"
  "public_function" 
))
```

### Importing a Module

When importing a module with `__exports__`, only the symbols listed in `__exports__` are accessible:

```lisp
(import "module")

# These work
(print module.PUBLIC_VAR)           # 42
(print (module.public_function 10)) # 20

# These will fail - not exported
(print module._PRIVATE_VAR)         # Error
(print (module._private_helper))    # Error
```

### Wildcard Imports

Wildcard imports also respect the `__exports__` list:

```lisp
(import (from "module" import *))

# These work
(print PUBLIC_VAR)           # 42
(print (public_function 10)) # 20

# These will fail - not exported
(print _PRIVATE_VAR)         # Error
(print (_private_helper))    # Error
```

## Private Symbols

Even without an `__exports__` list, symbols starting with an underscore are considered private and are not exported:

```lisp
# module_without_exports.m28
(= PUBLIC_VAR 42)
(= _PRIVATE_VAR "internal use only")

# No __exports__ list defined
```

```lisp
(import "module_without_exports")

# This works
(print module_without_exports.PUBLIC_VAR) # 42

# This will fail - private symbol
(print module_without_exports._PRIVATE_VAR) # Error
```

## Rules for Symbol Export

The following rules determine which symbols are exported:

1. If `__exports__` is defined:
   - Only symbols listed in `__exports__` are accessible to importers
   - Private symbols (starting with `_`) are only exported if explicitly listed

2. If `__exports__` is not defined:
   - All symbols are exported, except:
   - Private symbols (starting with `_`) are never exported

3. Special symbols always accessible:
   - Module metadata (`__name__`, `__file__`, `__exports__`)
   - Special forms and built-in functions

## Best Practices

1. **Always define `__exports__`** to make your module's public API explicit.
2. **Use underscore prefix** for internal implementation details.
3. **Keep the public API minimal** - export only what users need.
4. **Document exported symbols** with comments explaining their purpose.
5. **Group related functionality** into modules with clear, focused APIs.

## Example Module

Here's a well-structured module example:

```lisp
# math_utils.m28

# Module metadata
(= MODULE_NAME "math_utils")
(= VERSION "1.0.0")

# Public functions
(def (add x y)
  (+ x y))

(def (multiply x y)
  (* x y))

# Private helper function
(def (_validate_input x)
  (if (< x 0)
      (raise "Invalid input: number must be positive")
      x))

# Public function that uses private helper
(def (square_root x)
  (let ((validated (_validate_input x)))
    (sqrt validated)))

# Define the public API
(= __exports__ (list
  "MODULE_NAME"
  "VERSION" 
  "add"
  "multiply"
  "square_root"
))
```
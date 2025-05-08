# Enhanced Module System for M28

This document describes the enhanced module system in M28, which provides better control over module imports, namespace management, and module metadata.

## Module Basics

Modules in M28 are still files with `.m28` extension, but now they have enhanced capabilities:

```lisp
# example_module.m28

# Module metadata (convention)
(= MODULE_NAME "example_module")
(= VERSION "1.0.0")
(= AUTHOR "M28 Team")

# Public function
(def (greet name)
  (+ "Hello, " name "!"))

# Public data
(= CONFIG (dict "version" VERSION))
```

## Module Import

To import a module, use the same `import` special form:

```lisp
(import "example_module")
```

## New Features

### 1. Module Metadata

Modules automatically have access to built-in metadata:

- `__name__` - The name of the module
- `__file__` - The full path to the module file

```lisp
(import "example_module")
(print "Module name:" example_module.__name__)
(print "Module file:" example_module.__file__)
```

### 2. Export Control with `__exports__`

You can control which symbols are exported from your module by defining an `__exports__` list:

```lisp
# Define public API in the module
(= __exports__ (list
  "MODULE_NAME"
  "VERSION"
  "greet"
  "CONFIG"
))
```

When a module defines `__exports__`, only the listed symbols will be accessible to importers. This keeps your module's implementation details private.

### 3. Module Utility Functions

The enhanced module system provides several utility functions:

#### `add_module_path`

Adds a directory to the module search path:

```lisp
(add_module_path "./my_modules")
```

#### `get_module_paths`

Returns the current module search paths:

```lisp
(print "Search paths:" (get_module_paths))
```

#### `module_info`

Returns information about a loaded module:

```lisp
(print "Module info:" (module_info "example_module"))
```

The returned dictionary includes:
- `name` - Module name
- `path` - Full file path
- `load_time` - When the module was loaded
- `dependencies` - Modules imported by this module
- `exports` - List of exported symbols (if defined)

#### `reload_module`

Reloads a module from disk, refreshing its contents:

```lisp
(reload_module "example_module")
```

This is useful during development when you modify a module and want to see the changes without restarting M28.

### 4. Package Support

The enhanced module system now supports packages - directories with an `__init__.m28` file:

```
my_package/
  __init__.m28
  submodule.m28
```

Importing the package automatically loads the `__init__.m28` file:

```lisp
(import "my_package")
```

### 5. Improved Error Messages

Module import errors now provide more detailed information, including:
- The module search paths that were checked
- More specific error messages for different types of failures

## Best Practices

1. **Always Define `__exports__`**: Explicitly list what parts of your module are public API
2. **Use Module Metadata**: Include standard metadata like `MODULE_NAME`, `VERSION`, etc.
3. **Prefix Private Symbols**: Use underscore prefix for internal symbols (e.g., `_internal_helper`)
4. **Organize Related Code**: Use packages for organizing related modules
5. **Document Your Modules**: Include comments explaining the module's purpose and usage

## Example: Complete Module with Export Control

```lisp
# Well-structured module with export control
# my_module.m28

# Module metadata
(= MODULE_NAME "my_module")
(= VERSION "1.0.0")
(= AUTHOR "M28 Team")

# Public functions
(def (add x y)
  (+ x y))

(def (multiply x y)
  (* x y))

# Private helper (not exported)
(def (_validate_input x)
  (if (< x 0)
      (raise "Invalid input: number must be positive")
      x))

# Public API - only these symbols will be exported
(= __exports__ (list
  "MODULE_NAME"
  "VERSION"
  "AUTHOR"
  "add"
  "multiply"
))
```

Importing and using this module:

```lisp
(import "my_module")

# These work
(print "Version:" my_module.VERSION)
(print "Addition:" (my_module.add 2 3))

# This would fail - _validate_input is not exported
# (my_module._validate_input 5)
```
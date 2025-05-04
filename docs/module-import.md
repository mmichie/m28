# M28 Module System

The M28 language includes a Python-inspired module system for organizing and reusing code. This document explains how to create, import, and use modules in M28.

## Creating Modules

Modules in M28 are simply files with `.m28` extension containing M28 code. You can define variables, functions, and data structures in a module that can be imported and used in other code.

Example module (`example_module.m28`):

```lisp
# Define module variables
(= MODULE_NAME "example_module")
(= VERSION "1.0")

# Define module functions
(def (greet name)
  (+ "Hello, " name "!"))

# Define module data structures
(= CONFIG (dict
  "version" VERSION
  "debug" False
  "paths" (dict
    "data" "./data"
    "logs" "./logs"
  )
))
```

## Importing Modules

To import a module, use the `import` special form:

```lisp
# Basic import
(import "example_module")
```

This imports the module and makes it available under the base name of the module file. If the module path is `path/to/example_module.m28`, it will be available as `example_module`.

## Accessing Module Contents

Once a module is imported, you can access its contents using the `module_get` function:

```lisp
# Get a module variable
(= version (module_get example_module "VERSION"))
(print "Module version:" version)

# Get a module function
(= greet (module_get example_module "greet"))
(print (greet "World"))

# Get a module dictionary
(= config (module_get example_module "CONFIG"))
(print "Debug mode:" (get config "debug"))
```

For nested dictionaries, you can chain `get` calls:

```lisp
# Access nested dictionary
(= config (module_get example_module "CONFIG"))
(= paths (get config "paths"))
(print "Data path:" (get paths "data"))
```

## Module Path Resolution

When importing a module, the system searches for the module in the following locations:

1. The exact path provided (if it ends with `.m28`)
2. Current directory (`.`)
3. Tests directory (`./tests`)
4. Modules directory (`./modules`)
5. Examples directory (`./examples`)
6. System-wide module directories (`/usr/local/lib/m28/modules`, `/usr/lib/m28/modules`)

## Best Practices

1. **Organize related functionality in modules**: Group related functions and data together in a module.
2. **Document your modules**: Include comments that explain what the module does.
3. **Provide constants**: Define constants at the top of your module for version, author, etc.
4. **Export an API**: Explicitly define what your module exposes by creating an API dictionary.

Example API dictionary:

```lisp
# Define the module's API
(= API (dict
  "greet" greet
  "VERSION" VERSION
  "CONFIG" CONFIG
))
```

This makes it clear to users what functions and variables are meant to be used externally.

## Example: Complete Module

Here's a complete example of a well-structured module:

```lisp
# Module: math_utils.m28
# Description: Provides mathematical utility functions
# Author: M28 Team
# Version: 1.0.0

# Define module metadata
(= MODULE_NAME "math_utils")
(= VERSION "1.0.0")
(= AUTHOR "M28 Team")

# Define module functions
(def (factorial n)
  (if (< n 2)
      1
      (* n (factorial (- n 1)))))

(def (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))

# Export API
(= API (dict
  "factorial" factorial
  "fibonacci" fibonacci
  "VERSION" VERSION
  "AUTHOR" AUTHOR
))
```

And how to use it:

```lisp
# Import the module
(import "math_utils")

# Get the API
(= math_api (module_get math_utils "API"))

# Use functions from the API
(= factorial (get math_api "factorial"))
(print "5! =" (factorial 5))

# Or directly get functions from the module
(= fibonacci (module_get math_utils "fibonacci"))
(print "Fibonacci(8) =" (fibonacci 8))
```
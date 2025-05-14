# Module Exports and Private Symbols

This document describes the implementation of module exports and private symbol handling in M28.

## Overview

The module system in M28 now supports two key features for better namespace control:

1. **Module-level `__exports__` mechanism** - Explicitly control which symbols are exported from a module
2. **Private symbol convention** - Symbols starting with a single underscore (`_`) are considered private

These features help module authors create cleaner, more maintainable code by separating internal implementation details from the public API.

## Implementation Details

### 1. Module Exports

When a module defines an `__exports__` list, only the symbols listed in it will be accessible to importers:

```lisp
(= __exports__ (list
  "PUBLIC_VAR"
  "public_function"
))
```

The filter is applied during module loading in the `LoadModule` function, which calls `filterModuleExports()` to process the module's contents based on the export rules.

The filtering logic:

1. If a module defines `__exports__`, only symbols in that list (plus metadata) are accessible
2. If no `__exports__` list exists, all symbols except private ones (prefixed with `_`) are accessible
3. Metadata (symbols with `__` prefix) are always accessible regardless of export settings
4. Dot notation handlers are preserved for module access

### 2. Private Symbols

Symbols starting with a single underscore (`_`) are considered private and are:

- Hidden from importers by default
- Only accessible if explicitly listed in `__exports__`
- Not included in wildcard imports (`from module import *`)

This convention allows module authors to clearly indicate which functions, variables, and classes are intended for internal use only.

### 3. Wildcard Imports

The wildcard import mechanism (`from module import *`) has been updated to:

- Respect the `__exports__` list if defined, importing only exported symbols
- Skip private symbols unless they are explicitly listed in `__exports__`

Wildcard imports can be further filtered with the `except` clause:

```lisp
(import (from module import * except symbol1 symbol2))
```

## Examples

### Defining Exports

```lisp
# Define public and private items
(= PUBLIC_VAR 42)
(= _PRIVATE_VAR "hidden implementation detail")

(def (public_function x)
  (+ x 2))

(def (_private_helper)
  "Internal helper function")

# Define exports explicitly
(= __exports__ (list
  "PUBLIC_VAR"
  "public_function"
))
```

### Importing a Module

```lisp
# Standard import
(import "my_module")

# These work
(print my_module.PUBLIC_VAR)
(print (my_module.public_function 40))

# This fails - _PRIVATE_VAR is not exported
# (print my_module._PRIVATE_VAR)
```

### Wildcard Import

```lisp
# Import all exported symbols
(import (from "my_module" import *))

# These work 
(print PUBLIC_VAR)
(print (public_function 40))

# This fails - _PRIVATE_VAR is not exported
# (print _PRIVATE_VAR)
```

## Future Improvements

Potential enhancements to consider:

1. Improved error messages when accessing non-exported symbols
2. Support for explicitly exposing private symbols in `__exports__`
3. Better tooling for inspecting available module exports
4. Performance optimizations for large modules with many export filters
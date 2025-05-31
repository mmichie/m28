# F-String Enhancements in M28

## Overview

M28 now supports enhanced f-strings with several Python-compatible features, with a focus on the most commonly needed functionality.

## Implemented Features

### 1. Nested Quotes (✅ IMPLEMENTED - Priority #1)
The most requested feature - ability to use different quote types inside f-string expressions:

```m28
# Using single quotes inside double-quoted f-string
(= person {"name": "Alice", "age": 30})
(print f"Name: {person['name']}")  # Works!

# Using double quotes inside single-quoted f-string  
(print f'Name: {person["name"]}')  # Also works!
```

### 2. Basic Interpolation (✅ Already Supported)
- Variables: `f"Hello, {name}"`
- Expressions: `f"Sum: {(+ 2 3)}"`
- Method calls: `f"{text.upper()}"`
- Function calls: `f"{(len items)}"`

### 3. Complex Expressions (✅ Supported)
- List comprehensions: `f"Doubled: {[x * 2 for x in nums]}"`
- Nested access: `f"{data['users'][0]['name']}"`
- Math expressions: `f"Result: {(* (+ a b) 2)}"`

### 4. Escaped Braces (✅ Supported)
- Literal braces: `f"{{not interpolated}}"`
- Mixed: `f"{value} and {{literal}}"`

### 5. Unicode Support (✅ Supported)
- Full Unicode: `f"Emoji: {emoji}"` where emoji = "🐍🎉"
- International text: Works with Chinese, Greek, etc.

## Not Yet Implemented

### 1. Format Specifications
Python-style format specs like:
- `f"{pi:.2f}"` - Decimal precision
- `f"{num:04d}"` - Zero padding
- `f"{text:>10}"` - Alignment
- `f"{value:,}"` - Thousands separator

### 2. Conversion Flags
- `f"{value!r}"` - repr() conversion
- `f"{value!s}"` - str() conversion  
- `f"{value!a}"` - ascii() conversion

### 3. Self-Documenting Expressions
- `f"{expr=}"` - Shows both expression and value

## Implementation Details

The enhanced f-string parser (`parseFStringEnhancedSimple`) provides:
1. **Proper quote nesting** - Tracks quote context to allow alternating quote types
2. **Expression parsing** - Creates temporary parser for expressions
3. **Escape handling** - Properly handles `\\`, `\{`, `\}`, etc.

## Usage Examples

```m28
# Complex nested data with different quotes
(= data {"users": [{"name": "Alice", "prefs": {"color": "blue"}}]})
(print f"User's favorite: {data['users'][0]['prefs']['color']}")

# Multiple expressions
(= x 10)
(= y 20)
(print f"x={x}, y={y}, sum={(+ x y)}")

# List operations
(= items [1, 2, 3, 4, 5])
(print f"Filtered: {[x for x in items if (> x 3)]}")
```

## Technical Notes

The implementation uses a two-phase approach:
1. **Expression extraction** - Handles nested quotes and braces correctly
2. **Expression parsing** - Uses a temporary parser instance for clean parsing

This ensures that complex expressions with nested quotes are handled properly without interfering with the main parser state.

## Future Enhancements

While format specifications would be nice to have, the current implementation covers the most critical use cases. The nested quotes feature alone resolves many of the f-string failures that were blocking example files from running.
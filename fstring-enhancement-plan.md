# F-String Enhancement Plan for M28

## Current State

M28's f-strings currently support:
- ✅ Basic variable interpolation: `f"Hello, {name}"`
- ✅ Expression evaluation: `f"Sum: {(+ 2 3)}"`
- ✅ Method calls: `f"{text.upper()}"`
- ✅ Simple indexing: `f"{list[0]}"`
- ✅ Multiple expressions: `f"x={x}, y={y}"`
- ✅ Escaped braces: `f"\{literal\}"`

## Python F-String Features to Implement

### 1. Format Specifications (High Priority)
Python syntax: `{value:format_spec}`

Examples:
- `f"{pi:.2f}"` - Float with 2 decimal places
- `f"{num:04d}"` - Integer padded with zeros
- `f"{value:>10}"` - Right-aligned in 10 chars
- `f"{text:^20}"` - Center-aligned in 20 chars
- `f"{num:+}"` - Show sign for positive numbers
- `f"{num:,}"` - Thousands separator
- `f"{value!r}"` - repr() instead of str()
- `f"{value!s}"` - str() conversion
- `f"{value!a}"` - ascii() conversion

### 2. Nested Quotes (High Priority)
Allow different quote types inside expressions:
- `f"{dict['key']}"` - Single quotes inside double
- `f'{dict["key"]}'` - Double quotes inside single
- `f"""{dict["key"]}"""` - Triple quotes

### 3. Self-Documenting Expressions (Medium Priority)
Python 3.8+ feature: `{expr=}`
- `f"{x + y=}"` → `"x + y=8"`
- Shows both expression and result

### 4. Complex Expressions (Medium Priority)
- Conditional expressions: `f"{x if x > 0 else 'negative'}"`
- List comprehensions: `f"{[x*2 for x in range(3)]}"`
- Lambda functions: `f"{(lambda x: x*2)(5)}"`
- Nested f-strings: `f"{f'{x}'*3}"`

### 5. Special Characters (Low Priority)
- `{{` and `}}` for literal braces
- Unicode: `f"\u{code}"`
- Raw strings: `fr"path\to\file"`

### 6. Date/Time Formatting (Low Priority)
With format specs:
- `f"{date:%Y-%m-%d}"`
- `f"{time:%H:%M:%S}"`

## Implementation Strategy

### Phase 1: Core Parser Enhancements
1. **Nested Quote Support**
   - Track quote context in f-string expressions
   - Allow alternating quote types
   - Handle escape sequences properly

2. **Format Specification Parser**
   - Parse `:format_spec` after expressions
   - Parse `!conversion` flags
   - Pass format info to formatter

### Phase 2: Format Specification Implementation
1. **Number Formatting**
   - Decimal places: `.2f`
   - Padding: `05d`
   - Signs: `+`, `-`, ` `
   - Thousands separator: `,`
   - Binary/octal/hex: `b`, `o`, `x`

2. **String Formatting**
   - Alignment: `<`, `>`, `^`
   - Padding character
   - Truncation: `.10`

3. **Type Conversions**
   - `!r` → repr()
   - `!s` → str()
   - `!a` → ascii()

### Phase 3: Advanced Features
1. **Self-documenting expressions (`=`)**
   - Capture expression text
   - Format as "expr=value"

2. **Complex expressions**
   - Handle multi-line expressions
   - Support all Python expressions

### Phase 4: Optimizations
1. **Compile-time optimization**
   - Pre-compile format strings
   - Cache parsed format specs

2. **Performance**
   - Efficient string building
   - Minimize allocations

## Technical Changes Required

### Parser (parser.go)
1. Enhance `parseFString()` to:
   - Handle nested quotes
   - Parse format specifications
   - Support `=` suffix
   - Better error messages

2. New functions:
   - `parseFormatSpec()` - Parse `:format`
   - `parseConversion()` - Parse `!conv`
   - `trackQuoteNesting()` - Handle quotes

### Formatter (builtin/string.go)
1. Enhance `StrFormatFunc()` to:
   - Accept format specifications
   - Apply formatting rules
   - Handle conversions

2. New functions:
   - `formatNumber()` - Number formatting
   - `formatString()` - String formatting
   - `applyConversion()` - Type conversions

### Core Types
1. New format spec type:
   ```go
   type FormatSpec struct {
       Fill      rune
       Align     rune  // '<', '>', '^', '='
       Sign      rune  // '+', '-', ' '
       Alt       bool  // '#' flag
       Zero      bool  // '0' flag
       Width     int
       Precision int
       Type      rune  // 'f', 'd', 's', etc.
   }
   ```

## Testing Strategy

Create comprehensive test suite covering:
1. All format types and combinations
2. Edge cases (empty specs, invalid formats)
3. Performance benchmarks
4. Compatibility with Python behavior

## Priority Order

1. **Nested quotes** - Fixes many current failures
2. **Basic format specs** - Most commonly used
3. **Number formatting** - High utility
4. **String alignment** - Common use case
5. **Self-documenting** - Nice developer feature
6. **Advanced expressions** - Less common
7. **Special cases** - Edge features
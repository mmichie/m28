# M28 Infix Notation Examples

This directory contains examples showcasing M28's new Pythonic infix operator syntax.

## What's New?

M28 now supports **infix notation** for operators inside parentheses! Write natural, readable expressions like:

```lisp
(= result (2 + 3 * 4))        # Result: 14 (respects precedence)
(= is_valid (x > 5 and y < 10))  # Natural comparisons
(= total (price * quantity + tax))  # Mathematical expressions
```

## Features

- ✅ **Natural syntax**: `(a + b)` instead of `(+ a b)`
- ✅ **Operator precedence**: `(2 + 3 * 4)` = 14 (multiplication first)
- ✅ **Right-associativity**: `(2 ** 3 ** 2)` = 512 (exponentiation)
- ✅ **All contexts**: Works in loops, conditionals, function args, literals
- ✅ **Backward compatible**: Prefix syntax still works everywhere
- ✅ **30+ operators**: Arithmetic, comparison, logical, and more

## Examples

### 1. **infix-calculator.m28**
Mathematical calculations with natural syntax.

```bash
./bin/m28 examples/infix-calculator.m28
```

**Highlights:**
- Basic arithmetic with precedence
- Temperature conversions
- Quadratic formula solver
- Circle area/circumference calculations

### 2. **infix-conditionals.m28**
Conditional logic and comparisons.

```bash
./bin/m28 examples/infix-conditionals.m28
```

**Highlights:**
- Grade calculator with range checking
- Temperature warning system
- Access control with complex conditions
- Leap year checker

### 3. **infix-algorithms.m28**
Classic algorithms with readable expressions.

```bash
./bin/m28 examples/infix-algorithms.m28
```

**Highlights:**
- Fibonacci sequence generator
- Prime number finder
- Factorial calculator
- Collatz conjecture
- Binary search

### 4. **infix-data-processing.m28**
Data analysis and processing.

```bash
./bin/m28 examples/infix-data-processing.m28
```

**Highlights:**
- Student grade analysis with statistics
- Sales revenue calculations
- Temperature data analysis
- Shopping cart with tax

### 5. **infix-game-logic.m28**
Game development scenarios.

```bash
./bin/m28 examples/infix-game-logic.m28
```

**Highlights:**
- RPG character stats
- Combat simulation
- Health regeneration
- Experience/level up system
- 2D collision detection

## Supported Operators

### Arithmetic
- `+`, `-`, `*`, `/`, `//` (floor division), `%`, `**` (power)

### Comparison
- `==`, `!=`, `<`, `>`, `<=`, `>=`

### Logical
- `and`, `or`, `not`, `in`

### Assignment
- `=`, `+=`, `-=`, `*=`, `/=`, `//=`, `%=`, `**=`

## Syntax Rules

1. **Infix inside parens**: `(x + y)` uses infix, `(+ x y)` uses prefix
2. **Auto-detection**: If 2nd element is an operator → infix, else → prefix
3. **Precedence**: Follows Python's operator precedence
4. **Grouping**: Use nested parens to control evaluation order

## Examples

```lisp
# Infix (new style)
(= result (2 + 3 * 4))           # 14
(= valid (x > 0 and y < 100))    # Boolean logic
(= area (pi * radius ** 2))      # Complex formula

# Prefix (still works!)
(= result (+ 2 (* 3 4)))         # 14
(= valid (and (> x 0) (< y 100))) # Boolean logic
(= area (* pi (** radius 2)))    # Complex formula

# Mixed (best of both!)
(= result (+ (x * 2) (y / 3)))   # Use what's clearest
(for i in (range 0 n)            # Infix in range
  (if (> i 5)                    # Infix comparison
    (print (i ** 2))))           # Infix power
```

## Tips

- Use infix for math and comparisons → more readable
- Use prefix for function composition → more Lispy
- Mix and match for clarity!
- Precedence follows Python (multiplication before addition, etc.)
- Use extra parens when unsure: `((a + b) * (c + d))`

## Backward Compatibility

**All existing M28 code still works!** The infix syntax is purely additive. Your old prefix code will continue to work exactly as before.

---

**Happy coding with M28's new Pythonic syntax! 🐍✨**

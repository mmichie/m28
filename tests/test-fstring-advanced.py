print("Testing advanced f-string features...")

# Test 1: Conversion flags (!s, !r, !a)
name = "Alice"
print(f"String: {name!s}")
print(f"Repr: {name!r}")
print(f"ASCII: {name!a}")

# Test 2: Self-documenting expressions (=)
x = 10
y = 20
print(f"{x=}")
print(f"{x + y=}")
print(f"{x * 2=}")

# Test 3: Combination of conversion and format spec
value = 3.14159
print(f"{value!s}")
print(f"{value:.2f}")

# Test 4: Width and alignment
num = 42
print(f"{num:>10}")  # Right align in 10 chars
print(f"{num:<10}")  # Left align
print(f"{num:^10}")  # Center align
print(f"{num:0>10}") # Right align with zeros

# Test 5: Sign options
pos = 5
neg = -5
print(f"{pos:+}")   # Always show sign
print(f"{neg:+}")
print(f"{pos: }")   # Space for positive

# Test 6: Number types
n = 42
print(f"{n:d}")    # Decimal
print(f"{n:x}")    # Hex lowercase
print(f"{n:X}")    # Hex uppercase
print(f"{n:o}")    # Octal
print(f"{n:b}")    # Binary

# Test 7: Percentage
ratio = 0.75
print(f"{ratio:%}")
print(f"{ratio:.1%}")

# Test 8: Scientific notation
big = 1234567.89
print(f"{big:e}")
print(f"{big:E}")

# Test 9: Thousands separator
large = 1000000
print(f"{large:,}")
print(f"{large:_}")

print("\n✓ All advanced f-string tests passed!")

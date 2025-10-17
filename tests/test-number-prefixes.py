# Test number literal prefixes

print("Testing number literal prefixes...")

# Test 1: Binary literals
binary1 = 0b1010
binary2 = 0b11111111
binary3 = 0B0
print("Binary: 0b1010=", binary1, ", 0b11111111=", binary2, ", 0B0=", binary3)

# Test 2: Octal literals
octal1 = 0o755
octal2 = 0o77
octal3 = 0O0
print("Octal: 0o755=", octal1, ", 0o77=", octal2, ", 0O0=", octal3)

# Test 3: Hexadecimal literals
hex1 = 0xFF
hex2 = 0x10
hex3 = 0xDEADBEEF
hex4 = 0X0
print("Hex: 0xFF=", hex1, ", 0x10=", hex2, ", 0xDEADBEEF=", hex3, ", 0X0=", hex4)

# Test 4: Mixed case
hex_lower = 0xabcd
hex_upper = 0XABCD
bin_upper = 0B1010
oct_upper = 0O777
print("Mixed case: 0xabcd=", hex_lower, ", 0XABCD=", hex_upper, ", 0B1010=", bin_upper, ", 0O777=", oct_upper)

# Test 5: Verify values
print("Verify: 0b1010 == 10:", 0b1010 == 10)
print("Verify: 0o755 == 493:", 0o755 == 493)
print("Verify: 0xFF == 255:", 0xFF == 255)

# Test 6: Use in expressions
result = 0xFF + 0b1111
print("0xFF + 0b1111 =", result)

# Test 7: Common use cases
color_red = 0xFF0000
permissions = 0o644
flags = 0b1101
print("Color:", color_red, ", Permissions:", permissions, ", Flags:", flags)

print("✓ Number prefix tests complete")

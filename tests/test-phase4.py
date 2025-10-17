# Comprehensive Phase 4 Integration Test
# Tests chained assignment, number prefixes, and multiple assignment together

print("=== Phase 4 Feature Integration Test ===\n")

# Test 1: Chained assignment with number prefixes
print("Test 1: Chained with hex")
r = g = b = 0xFF
print("r=", r, ", g=", g, ", b=", b)

# Test 2: Multiple assignment with binary
print("\nTest 2: Unpack binary values")
flags1, flags2 = 0b1010, 0b1100
print("flags1=", flags1, ", flags2=", flags2)

# Test 3: Swap with octal
print("\nTest 3: Swap octal values")
perm1 = 0o755
perm2 = 0o644
perm1, perm2 = perm2, perm1
print("perm1=", perm1, ", perm2=", perm2)

# Test 4: Simple chained with values
print("\nTest 4: Chained number assignments")
a = b = c = d = 42
print("a=", a, ", b=", b, ", c=", c, ", d=", d)

# Test 5: Hex colors with unpacking
print("\nTest 5: Color components")
red = 0xFF0000
green = 0x00FF00
blue = 0x0000FF
r, g, b = red, green, blue
print("RGB:", r, g, b)

# Test 6: Binary flags with chained assignment
print("\nTest 6: Flag operations")
flag_a = flag_b = flag_c = 0b0001
print("Flags:", flag_a, flag_b, flag_c)

# Test 7: Multiple assignment with number prefixes
print("\nTest 7: Unpack hex and binary")
x, y = 0x10, 0b10
print("x=", x, ", y=", y)

print("\n✓ All Phase 4 integration tests passed!")

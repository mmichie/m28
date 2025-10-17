print("Testing any() and all() builtins...")

# Test any()
assert any([False, True, False]) == True, "any with one True"
assert any([False, False, False]) == False, "any with all False"
assert any([True, True, True]) == True, "any with all True"
assert any([]) == False, "any with empty list"
assert any([0, 1, 2]) == True, "any with numbers"
assert any([0, 0, 0]) == False, "any with all zeros"

# Test all()
assert all([True, True, True]) == True, "all with all True"
assert all([True, False, True]) == False, "all with one False"
assert all([False, False, False]) == False, "all with all False"
assert all([]) == True, "all with empty list"
assert all([1, 2, 3]) == True, "all with non-zero numbers"
assert all([1, 0, 3]) == False, "all with a zero"

# Test with generators
assert any(x > 5 for x in range(10)) == True, "any with generator"
assert all(x < 10 for x in range(10)) == True, "all with generator"
assert all(x < 5 for x in range(10)) == False, "all with generator (false)"

print("\n✓ All any()/all() tests passed!")

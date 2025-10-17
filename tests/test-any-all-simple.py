print("Testing any() and all() builtins...")

# Test any()
print("Testing any()...")
result1 = any([False, True, False])
print(f"any([False, True, False]) = {result1}")
assert result1 == True, "any with one True"

result2 = any([False, False, False])
print(f"any([False, False, False]) = {result2}")
assert result2 == False, "any with all False"

result3 = any([True, True, True])
print(f"any([True, True, True]) = {result3}")
assert result3 == True, "any with all True"

result4 = any([])
print(f"any([]) = {result4}")
assert result4 == False, "any with empty list"

# Test all()
print("\nTesting all()...")
result5 = all([True, True, True])
print(f"all([True, True, True]) = {result5}")
assert result5 == True, "all with all True"

result6 = all([True, False, True])
print(f"all([True, False, True]) = {result6}")
assert result6 == False, "all with one False"

result7 = all([False, False, False])
print(f"all([False, False, False]) = {result7}")
assert result7 == False, "all with all False"

result8 = all([])
print(f"all([]) = {result8}")
assert result8 == True, "all with empty list"

print("\n✓ All any()/all() tests passed!")

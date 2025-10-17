print("Testing walrus operator (:=)...")

# Test 1: Basic assignment in if condition
if (n := 10) > 5:
    print(f"n = {n}, which is greater than 5")

# Test 2: Using walrus in while loop
data = [1, 2, 3, 4, 5]
i = 0
while (val := data[i] if i < len(data) else 0) > 0:
    print(f"Value: {val}")
    i += 1

# Test 3: Walrus with function call
def get_length(lst):
    return len(lst)

items = ["a", "b", "c"]
if (length := get_length(items)) > 2:
    print(f"List has {length} items, which is more than 2")

# Test 4: Walrus in list comprehension
numbers = [1, 2, 3, 4, 5, 6]
squares_gt_10 = [y for x in numbers if (y := x * x) > 10]
print(f"Squares greater than 10: {squares_gt_10}")

# Test 5: Multiple walrus operators
if (a := 5) > 3 and (b := 10) > 8:
    print(f"Both conditions met: a={a}, b={b}")

print("\n✓ All walrus operator tests passed!")

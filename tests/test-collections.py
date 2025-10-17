# Test collections module

from collections import Counter, defaultdict, deque

print("Testing Counter...")

# Test Counter creation and counting
c = Counter([1, 2, 2, 3, 3, 3])
print(f"Counter from list: {c}")

# Test most_common
most_common = c.most_common(2)
print(f"Most common 2: {most_common}")

# Test Counter access
print(f"Count of 3: {c[3]}")
print(f"Count of missing: {c[99]}")  # Should return 0

# Test Counter update
c.update([1, 1, 4])
print(f"After update: {c}")

# Test total
total = c.total()
print(f"Total count: {total}")

print("\nTesting defaultdict...")

# Test defaultdict with list factory
dd = defaultdict(list)
dd["a"].append(1)
dd["a"].append(2)
dd["b"].append(3)
print(f"defaultdict with list: {dd}")

# Test defaultdict with int factory (use custom function since int() requires args in M28)
def zero():
    return 0

dd2 = defaultdict(zero)
dd2["x"] = dd2["x"] + 1
dd2["y"] = dd2["y"] + 5
print(f"defaultdict with int: {dd2}")

print("\nTesting deque...")

# Test deque creation
dq = deque([1, 2, 3])
print(f"Initial deque: {dq}")

# Test append and appendleft
dq.append(4)
dq.appendleft(0)
print(f"After append/appendleft: {dq}")

# Test pop and popleft
right = dq.pop()
left = dq.popleft()
print(f"Popped right: {right}, left: {left}")
print(f"After pops: {dq}")

# Test extend and extendleft
dq.extend([5, 6])
print(f"After extend: {dq}")

dq.extendleft([-1, -2])
print(f"After extendleft: {dq}")

# Test rotate
dq2 = deque([1, 2, 3, 4, 5])
dq2.rotate(2)
print(f"After rotate(2): {dq2}")

dq2.rotate(-3)
print(f"After rotate(-3): {dq2}")

# Test clear
dq.clear()
print(f"After clear: {dq}")

# Test maxlen
dq3 = deque([1, 2, 3], 5)
dq3.append(4)
dq3.append(5)
dq3.append(6)  # Should push out 1
print(f"Deque with maxlen=5 after 3 appends: {dq3}")

print("\n✓ All collections tests completed!")

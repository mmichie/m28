# Subsystem: big-integer arithmetic (values exceed 2**53).
# Exercises the arbitrary-precision multiply/mod path rather than the
# float64-backed fast path.
acc = 1
mod = 10 ** 30 + 7
for i in range(1, 10000):
    acc = (acc * i) % mod
print(acc)

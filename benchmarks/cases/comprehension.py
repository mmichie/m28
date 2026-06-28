# Subsystem: list comprehension + builtin sum over an iterator.
N = 2000
total = 0
for _ in range(N):
    total = total + sum([x * x for x in range(100)])
print(total)

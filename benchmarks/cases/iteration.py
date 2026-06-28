# Subsystem: iterator protocol via a generator function (yield).
def squares(n):
    i = 0
    while i < n:
        yield i * i
        i = i + 1

total = 0
for _ in range(10):
    for v in squares(1000):
        total = total + v
print(total)

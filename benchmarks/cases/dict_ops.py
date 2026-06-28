# Subsystem: dict insert / lookup / iteration.
N = 7000
d = {}
for i in range(N):
    k = i % 1000
    d[k] = d.get(k, 0) + i
s = 0
for k in d:
    s = s + d[k]
print(s)

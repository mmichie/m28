# Subsystem: list build / append / index.
N = 5000
xs = []
for i in range(N):
    xs.append(i * 2)
s = 0
for i in range(len(xs)):
    s = s + xs[i]
print(s)

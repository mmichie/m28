# Subsystem: string building, concatenation, and join.
N = 10000
parts = []
for i in range(N):
    parts.append("item-" + str(i))
joined = "".join(parts)
print(len(joined))

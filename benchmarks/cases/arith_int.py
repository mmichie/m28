# Subsystem: integer arithmetic dispatch.
# Exercises the operator resolver and number.__add__/__mul__/__mod__ paths in
# a tight loop with no allocation of large objects.
N = 12000
total = 0
for i in range(N):
    total = (total + i * 3 - 1) % 1000000
print(total)

# Subsystem: exception raise/catch machinery in a loop.
N = 10000
caught = 0
for i in range(N):
    try:
        if i % 2 == 0:
            raise ValueError("even")
        x = 1
    except ValueError:
        caught = caught + 1
print(caught)

# Subsystem: function-call / recursion overhead.
# Each call creates a new Context (scope frame); this measures call dispatch,
# argument binding, and frame setup/teardown.
def fib(n):
    if n < 2:
        return n
    return fib(n - 1) + fib(n - 2)

print(fib(18))

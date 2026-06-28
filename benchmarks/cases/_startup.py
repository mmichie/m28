# Baseline: interpreter startup + parse of a trivial program.
# The harness subtracts this from every other case to isolate steady-state
# work from per-process startup cost. Keep this case as close to empty as
# possible.
x = 1
print(x)

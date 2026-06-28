# Subsystem: variable resolution (Context.lookupWithDepth).
# A hot loop that references several locals per iteration, stressing name
# lookup through the scope chain.
def run():
    a = 1
    b = 2
    c = 3
    d = 4
    e = 5
    total = 0
    for i in range(150000):
        total = total + a + b + c + d + e + i
    return total

print(run())

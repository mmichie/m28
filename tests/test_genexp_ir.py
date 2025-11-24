# Test what IR is produced for nested generator expression
import ast
import dis

# Simple genexp
code1 = compile("(x*2 for x in range(3))", "<string>", "eval")
print("Simple genexp bytecode:")
dis.dis(code1)

# Nested genexp
code2 = compile("(x*y for x in range(3) for y in range(3))", "<string>", "eval")
print("\nNested genexp bytecode:")
dis.dis(code2)

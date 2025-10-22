"""Standalone bool tests based on CPython test_bool.py"""

print("=" * 60)
print("Bool Tests - Based on CPython test_bool.py")
print("=" * 60)

passed = 0
failed = 0

def test(name, condition, expected=True):
    global passed, failed
    result = condition == expected
    if result:
        passed += 1
        print(f"✓ {name}")
    else:
        failed += 1
        print(f"✗ {name}: got {condition}, expected {expected}")

# test_repr
print("\ntest_repr:")
test("repr(False)", repr(False), 'False')
test("repr(True)", repr(True), 'True')
test("eval(repr(False)) is False", eval(repr(False)) is False)
test("eval(repr(True)) is True", eval(repr(True)) is True)

# test_str
print("\ntest_str:")
test("str(False)", str(False), 'False')
test("str(True)", str(True), 'True')

# test_int
print("\ntest_int:")
test("int(False)", int(False), 0)
test("int(False) is not False", int(False) is not False)
test("int(True)", int(True), 1)
test("int(True) is not True", int(True) is not True)

# test_float
print("\ntest_float:")
test("float(False)", float(False), 0.0)
test("float(False) is not False", float(False) is not False)
test("float(True)", float(True), 1.0)
test("float(True) is not True", float(True) is not True)

# test_math
print("\ntest_math:")
test("+False", +False, 0)
test("+False is not False", +False is not False)
test("-False", -False, 0)
test("-False is not False", -False is not False)
test("abs(False)", abs(False), 0)
test("abs(False) is not False", abs(False) is not False)

test("+True", +True, 1)
test("+True is not True", +True is not True)
test("-True", -True, -1)
test("abs(True)", abs(True), 1)
test("abs(True) is not True", abs(True) is not True)

test("False+2", False+2, 2)
test("True+2", True+2, 3)
test("2+False", 2+False, 2)
test("2+True", 2+True, 3)

test("False+False", False+False, 0)
test("False+False is not False", (False+False) is not False)
test("False+True", False+True, 1)
test("False+True is not True", (False+True) is not True)
test("True+False", True+False, 1)
test("True+False is not True", (True+False) is not True)
test("True+True", True+True, 2)

test("True-True", True-True, 0)
test("True-True is not False", (True-True) is not False)
test("False-False", False-False, 0)
test("False-False is not False", (False-False) is not False)
test("True-False", True-False, 1)
test("True-False is not True", (True-False) is not True)
test("False-True", False-True, -1)

test("True*1", True*1, 1)
test("False*1", False*1, 0)
test("1*True", 1*True, 1)
test("1*False", 1*False, 0)

test("True*2", True*2, 2)
test("False*2", False*2, 0)
test("2*True", 2*True, 2)
test("2*False", 2*False, 0)

# test_boolean_ops
print("\ntest_boolean_ops:")
test("False and False", (False and False) is False)
test("False and True", (False and True) is False)
test("True and False", (True and False) is False)
test("True and True", (True and True) is True)

test("False or False", (False or False) is False)
test("False or True", (False or True) is True)
test("True or False", (True or False) is True)
test("True or True", (True or True) is True)

test("not False", (not False) is True)
test("not True", (not True) is False)

# test_comparisons
print("\ntest_comparisons:")
test("False < True", False < True)
test("False <= True", False <= True)
test("False <= False", False <= False)
test("True > False", True > False)
test("True >= False", True >= False)
test("True >= True", True >= True)
test("False == False", False == False)
test("True == True", True == True)
test("False != True", False != True)

# Summary
print("\n" + "=" * 60)
print(f"Tests passed: {passed}")
print(f"Tests failed: {failed}")
print(f"Success rate: {100 * passed / (passed + failed):.1f}%")
print("=" * 60)

if failed == 0:
    print("\n✅ All tests passed!")
else:
    print(f"\n❌ {failed} test(s) failed")

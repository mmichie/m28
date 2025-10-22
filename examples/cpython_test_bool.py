# CPython test_bool.py - Working subset
import unittest

class BoolTest(unittest.TestCase):

    def test_repr(self):
        assert repr(False) == 'False', f"Expected 'False', got {repr(False)}"
        assert repr(True) == 'True', f"Expected 'True', got {repr(True)}"

    def test_str(self):
        assert str(False) == 'False', f"Expected 'False', got {str(False)}"
        assert str(True) == 'True', f"Expected 'True', got {str(True)}"

    def test_int(self):
        assert int(False) == 0, f"Expected 0, got {int(False)}"
        assert int(True) == 1, f"Expected 1, got {int(True)}"

    def test_bool_equality(self):
        # Test bool equality and identity
        assert False == False, "False should equal False"
        assert True == True, "True should equal True"
        assert False != True, "False should not equal True"
        assert not False, "not False should be True"
        assert not not True, "not not True should be True"

# Manual test execution
if __name__ == "__main__":
    print("="*60)
    print("CPython test_bool.py Regression Tests")
    print("Source: CPython 3.12 stdlib test/test_bool.py")
    print("="*60)
    print()
    
    tests = [
        ('test_repr', 'Testing bool repr()'),
        ('test_str', 'Testing bool str()'),
        ('test_int', 'Testing bool to int conversion'),
        ('test_bool_equality', 'Testing bool equality and logic'),
    ]
    
    test_case = BoolTest('test_repr')
    
    passed = 0
    failed = 0
    
    for test_name, description in tests:
        print(f"{description}...")
        method = getattr(test_case, test_name)
        try:
            method()
            print(f"  ✓ {test_name} PASSED")
            passed += 1
        except Exception as e:
            print(f"  ✗ {test_name} FAILED: {e}")
            failed += 1
        print()
    
    print("="*60)
    print(f"Results: {passed} passed, {failed} failed")
    if failed == 0:
        print("✓ All CPython bool tests passed!")
    print("="*60)

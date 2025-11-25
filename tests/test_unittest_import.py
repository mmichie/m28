# Test that unittest module can be imported with _locale support
print("Testing unittest import with _locale support...")

try:
    import unittest
    print("✓ unittest imported successfully!")

    # Try to create a simple test case
    class SimpleTest(unittest.TestCase):
        def test_example(self):
            self.assertEqual(1, 1)

    print("✓ Can create unittest.TestCase subclass!")
    print("✓ _locale module successfully supports unittest!")

except ImportError as e:
    print(f"✗ Failed to import unittest: {e}")
except Exception as e:
    print(f"✗ Error with unittest: {e}")

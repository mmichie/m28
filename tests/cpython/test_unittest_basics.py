"""
Validate that Python's unittest framework runs in M28.

This file exercises the M28 unittest stub (see /unittest.py at project root).
The tests themselves are standard Python that would run unmodified under CPython.

If this file passes, we have a working baseline for porting real CPython test
suite files (test_list.py, test_dict.py, etc.) over to run on M28.
"""

import unittest


class AssertionTests(unittest.TestCase):
    """Cover every assertion method we implement."""

    def test_assertEqual(self):
        self.assertEqual(1, 1)
        self.assertEqual("hello", "hello")
        self.assertEqual([1, 2], [1, 2])
        self.assertEqual({"a": 1}, {"a": 1})

    def test_assertNotEqual(self):
        self.assertNotEqual(1, 2)
        self.assertNotEqual("a", "b")

    def test_assertTrue_assertFalse(self):
        self.assertTrue(1)
        self.assertTrue([1])
        self.assertTrue("x")
        self.assertFalse(0)
        self.assertFalse([])
        self.assertFalse("")
        self.assertFalse(None)

    def test_assertIs_assertIsNot(self):
        x = [1, 2]
        y = x
        z = [1, 2]
        self.assertIs(x, y)
        self.assertIsNot(x, z)

    def test_assertIsNone(self):
        self.assertIsNone(None)
        self.assertIsNotNone(0)
        self.assertIsNotNone("")

    def test_assertIn(self):
        self.assertIn(2, [1, 2, 3])
        self.assertNotIn(99, [1, 2, 3])
        self.assertIn("a", "abc")
        self.assertIn("k", {"k": 1})

    def test_assertIsInstance(self):
        self.assertIsInstance(1, int)
        self.assertIsInstance("hi", str)
        self.assertIsInstance([1], list)
        self.assertNotIsInstance(1, str)

    def test_assertComparisons(self):
        self.assertGreater(2, 1)
        self.assertGreaterEqual(2, 2)
        self.assertLess(1, 2)
        self.assertLessEqual(1, 1)

    def test_assertRaises_context(self):
        with self.assertRaises(ZeroDivisionError):
            _ = 1 / 0
        with self.assertRaises(KeyError):
            _ = {}['missing']

    def test_assertRaises_callable(self):
        def boom():
            raise ValueError("nope")
        self.assertRaises(ValueError, boom)

    def test_assertRaisesRegex(self):
        with self.assertRaisesRegex(ValueError, "bad"):
            raise ValueError("bad value")


class LifecycleTests(unittest.TestCase):
    """Verify setUp / tearDown / addCleanup ordering."""

    log = []

    @classmethod
    def setUpClass(cls):
        cls.log.append("setUpClass")

    @classmethod
    def tearDownClass(cls):
        cls.log.append("tearDownClass")

    def setUp(self):
        self.log.append("setUp")

    def tearDown(self):
        self.log.append("tearDown")

    def test_first(self):
        self.log.append("test_first")
        self.addCleanup(lambda: self.log.append("cleanup_first"))

    def test_second(self):
        self.log.append("test_second")


class SkipTests(unittest.TestCase):
    @unittest.skip("intentionally skipped")
    def test_always_skipped(self):
        self.fail("should not be reached")

    @unittest.skipIf(True, "condition true")
    def test_skip_if_true(self):
        self.fail("should not be reached")

    @unittest.skipUnless(False, "condition false")
    def test_skip_unless_false(self):
        self.fail("should not be reached")

    def test_runtime_skip(self):
        self.skipTest("skipping at runtime")


class ExpectedFailureTests(unittest.TestCase):
    @unittest.expectedFailure
    def test_expected_failure(self):
        self.assertEqual(1, 2)


if __name__ == '__main__':
    unittest.main(verbosity=2)
